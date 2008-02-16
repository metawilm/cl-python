;; -*- package: clpython.app.repl; readtable: py-ast-user-readtable -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Read-Eval-Print loop 

(defpackage :clpython.app.repl
  (:documentation "Python read-eval-print loop")
  (:use :common-lisp :clpython :clpython.parser)
  (:export #:repl #:*repl-compile* #:*repl-prof*)
  (:import-from :clpython #:with-matching)
  (:import-from :clpython.ast #:suite-stmt-p #:module-stmt-p))

(in-package :clpython.app.repl)
(in-syntax *ast-user-readtable*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'clpython.app.repl:repl :clpython))

;;; Restarts

(defun goto-python-top-level ()
  (let ((r (find-restart 'return-python-toplevel)))
    (if r
	(invoke-restart r)
      (warn "There is no Python REPL running."))))

(setf (top-level:alias "ptl")
  #'goto-python-top-level)

(setf (top-level:alias "rc")
  #'retry-repl-comp)

(defun retry-repl-eval ()
  (let ((r (find-restart 'retry-repl-eval)))
    (if r
	(invoke-restart r)
      (warn "There is no Python REPL running."))))

(setf (top-level:alias "re")
  #'retry-repl-eval)


(defvar _   *the-none* "The last value evaluated by REPL")
(defvar __  *the-none* "The second-last value evaluated by REPL")
(defvar ___ *the-none* "The third-last value evaluated by REPL")

(defvar *repl-prof* nil
  "Execution of each expression is profiled according to this setting.
Possible values: :time :ptime :space :pspace nil")

(defvar *repl-compile* t
  "Whether code typed in the REPL is compiled before running.")

(defvar *ignore-copied-prompts* t
  "Whether to remove initial `>>>' and `...' on the input line.
If true, previous input can be copy-pasted as new input easily.")
                                
(defvar *repl-doc* "
In the Python interpreter:
     :help          => print (this) help
     :q             => quit
  <command>         => execute Python or Lisp <command>
  <space><command>  => execute Lisp <command>

In the Lisp debugger:
     :ptl           => back to Python top level
     :re            => retry the last (failed) Python command

Relevant Lisp variables (exported from package :clpython.app.repl):
   *repl-compile*  => whether source code is compiled into assembly
                      before running
   *repl-prof*     => profile execution of Python code
                      value must be one of:
                         :time    = like (TIME ...)
                         :ptime   = time call graph
                         :space   = list of allocations
                         :pspace  = space call graph
                         nil      = no profiling
")

(defvar *repl-mod* nil "The REPL module (for debugging)")
(defvar *prompts* '(">>> " "... "))

(defun profile (f kind)
  "Call F in profiling context.
KIND can be :ptime, :time, :space, :pspace or NIL."
  (ecase kind
    (:ptime  (prof:with-profiling (:type :time)
               (funcall f))
             (terpri)
             (prof:show-call-graph))
    (:time  (prog1 (time (funcall f))
              (terpri)))
    (:space  (prof:with-profiling
                 (:type :space :count t) (funcall f))
             (terpri)
             (prof:show-flat-profile))
    (:pspace (prof:with-profiling (:type :space)
               (funcall f))
             (terpri)
             (prof:show-call-graph))
    ((nil)   (funcall f))))


(defun repl ()
  (clpython::with-python-compiler-style-warnings
      (with-ast-user-readtable ()
        (repl-1))))

(defvar *object-repr-char-limit* 300
  "At most this many characters are printed for an object represenation in the REPL (NIL = infinite)")
(defvar *truncation-explain* t)

(defun print-string-truncated (s)
  (check-type s string)
  (if (and (integerp *object-repr-char-limit*)
           (> *object-repr-char-limit* 0)
           (> (length s) *object-repr-char-limit*))
      (progn (write-string (subseq s 0 *object-repr-char-limit*))
             (write-string "...")
             (when *truncation-explain*
               (terpri)
               (format t ";; String representation truncated after ~A characters (of ~A) due to ~S."
                       *object-repr-char-limit* (length s) '*object-repr-char-limit*)
               (setf *truncation-explain* nil)))
    (write-string s))
  (values))
  
(defun repl-1 ()
  (let* ((*repl-mod* (make-module))
         (clpython::*habitat* (clpython::make-habitat))
         (*truncation-explain* t)
	 acc)
    
    (dolist (x '(_ __ ___))
      (setf (py-attr* *repl-mod* x) *the-none*))
    (setf (py-attr* *repl-mod* "__name__") "__main__")
    
    (labels ((print-cmds ()
	       (format t *repl-doc*))
	     
	     (remember-value (val)
               ;; Make last three return values available as _, __, ___
               ;; for both Python (repl module namespace) and Lisp (dynamic vars).
               (when val ;; don't save NIL (which can be return value for Lisp eval)
                 (shiftf ___ __ _ val)
                 (shiftf (py-attr* *repl-mod* '___)
                         (py-attr* *repl-mod* '__)
                         (py-attr* *repl-mod* '_)
                         val)))

	     (run-ast-func (suite)
               (let ((f `(lambda ()
                           (clpython::with-this-module-context (,*repl-mod*)
                             ,suite))))
                 (when *repl-compile*
                   (setf f (compile nil f)))
                 f))
	     
             (nice-one-line-input-abbrev (total)
               (check-type total string)
               (loop while (and (plusp (length total))
                                (char= (aref total 0) #\Newline))
                   do (setf total (subseq total 1)))
               (loop while (and (plusp (length total))
                                (char= (aref total (1- (length total))) #\Newline))
                   do (setf total (subseq total 0 (- (length total) 1))))
               (let ((ix (position #\Newline total)))
                 (cond (ix
                        (setf total (concatenate 'string (subseq total 0 (min ix 30)) " ...")))
                       ((> (length total) 30)
                        (setf total (concatenate 'string (subseq total 0 30) "...")))))
               total)
             
	     (eval-print-ast (ast total)
               (with-matching (ast ([module-stmt] ?suite))
		 (assert (suite-stmt-p ?suite))
		 (let ((vals (multiple-value-list
                              (block :val 
                                (loop (let ((helper-func (run-ast-func ?suite)))
                                        (loop (with-simple-restart
                                                  (retry-repl-eval "Retry the expression: \"~A\" [:re]"
                                                                   (nice-one-line-input-abbrev total))
                                                (return-from :val
                                                  (profile helper-func *repl-prof*))))))))))
                   (when (car vals) ;; skip NIL
                     (remember-value (car vals))
                     (block repr
                       (loop
                         (with-simple-restart
                             (:continue "Retry printing the object.")
                           ;; Write string with quotes around it; convert other objects
                           ;; using __str__ and print without quotes.
                           (loop for val in vals
                               do (if (stringp val)
                                      (print-string-truncated (py-repr val))
                                    (let ((str-val (py-str-string val :circle t)))
                                      (print-string-truncated (py-val->string str-val)))) 
                                  (write-char #\Newline)))
                         (return-from repr)))))))
	     
	     (handle-as-python-code (total &key print-error (ast-finished :maybe))
               ;; Return T if this succeeded somehow, i.e. parsing as Lisp
               ;; should not be attempted.
               
               ;; When first char is a space, always treat it as Lisp code
               (when (or (zerop (length total))
                         (char= (char total 0) #\Space))
                 (return-from handle-as-python-code nil))
               
               (flet ((return-syntax-error (err)
                        (when print-error
                          (format t ";; Python parse failed:  ~A~%" err))
                        (return-from handle-as-python-code :syntax-error)))
                 
                 (let ((ast (handler-case (parse total)
                              ;; Try to parse input into AST 
                              ;;  - If that fails due to unexpected EOF, we still intend to parse
                              ;;    as Python code, therefore return T.
                              ;;  - If that fails due to syntax error, we give up.
                              ;; 
                              ;; If lines start with (copied) `>>>'/`...', then parsing is tried
                              ;; after removing those.
                              ({UnexpectedEofError} () (return-from handle-as-python-code t))
			      ({SyntaxError} (err)
                                (or (when *ignore-copied-prompts*
                                      (multiple-value-bind (new-str changed)
                                          (remove-interpreter-prompts total *prompts*)
                                        (when changed
                                          (handler-case (parse new-str)
                                            ({UnexpectedEofError} ()
                                              (return-from handle-as-python-code t))
                                            (error (err2)
                                              (return-syntax-error err2))
                                            (:no-error (val)
                                              (setf total new-str)
                                              val)))))
                                    (return-syntax-error err))))))
                   (when ast
                     (when (eq ast-finished :maybe)
                       (assert (module-stmt-p ast))
                       (with-matching (ast ([module-stmt] ?suite-stmt))
                         (with-matching (?suite-stmt ([suite-stmt] ?items))
                           (assert (listp ?items)) ;; ?items can be multiple, e.g. in the case of "a=3; b=4".
                           (when (ast-complete-p (car (last ?items)))
                             (setf ast-finished t)))))
                     (when (eq ast-finished t)
                       (eval-print-ast ast total)
                       (setf acc nil)))
                   (return-from handle-as-python-code t))))
	     
	     (handle-as-lisp-code (total &key print-error)
               ;; Returns whether actually handled
	       (multiple-value-bind (lisp-form error)
                   (ignore-errors (with-standard-io-syntax
                                    ;; Bind package, so symbols _, __, ___ are present.
                                    (let ((*package* #.*package*))
                                      (read-from-string total nil nil))))
                 (cond ((and (null lisp-form)
                             (typep error 'error))
                        ;; Could not parse as lisp
                        (when print-error
                          (format t ";; Lisp parse failed:  ~A~%" error))
                        nil)
                       
		       ((member lisp-form '(nil def class for while if try))
                        ;; Multi-line Python form
                        nil)
                       
		       ((and (symbolp lisp-form)
			     (> (length (symbol-name lisp-form)) 1)
			     (char= (aref (symbol-name lisp-form) 0) #\@))
                        ;; Reading function decorator
                        nil)
		       
		       (t (multiple-value-bind (res err) 
			      (ignore-errors (multiple-value-list (eval lisp-form)))
			    (if (and (null res)
				     (typep err 'condition))
				(progn (when print-error
                                         (format t ";; Lisp eval failed:  ~A~%" err))
                                       nil)
			      (progn
				(remember-value (car res))
				(dolist (r res)
				  (write r)
				  (write-char #\Newline))
				(setf acc nil)
                                t)))))))

             (input-available-p ()
               (let ((ch (read-char-no-hang *standard-input*)))
                 (when ch
                   (prog1 t (unread-char ch))))))
      
      (loop
	  initially (format t "[CLPython -- type `:q' to quit, `:help' for help]~%")
	  do (loop 
	       (with-simple-restart (return-python-toplevel "Return to Python top level [:ptl]")
		 (setf acc ())
		 (loop 
		   (locally (declare (special *stdout-softspace*))
		     (setf *stdout-softspace* (py-bool nil)))
                   
                   (unless (input-available-p)
                     ;; When copy-pasting multiple lines of Python source code into the REPL,
                     ;; prevent several prompts being printed below the copied code.
                     (write-string (nth (if acc 1 0) *prompts*) t)
                     (force-output t))
                   
		   (let ((x (read-line)))
		     (cond
                      ((and (> (length x) 0)
			    (char= (aref x 0) #\:))
		       (multiple-value-bind (cmd ix)
			   (read-from-string (string-downcase x))
			 (declare (ignore ix))
			 (case cmd
			   (:help (print-cmds))
			   (:q    (return-from repl-1 (values)))
			   (t     (warn "Unknown command: ~S" cmd)))))
		      
		      ((string= x "")
                       (let ((total (apply #'concatenate 'string (nreverse acc))))
			 (setf acc ())
			 (loop
			   (restart-case
                               (progn (or (eq t (handle-as-python-code total :print-error t :ast-finished t))
                                          (handle-as-lisp-code total :print-error t))
                                      (return))
			     (try-parse-again ()
				 :report "Parse string again into AST")))))
		      
		      (t (push (concatenate 'string x (string #\Newline)) acc)
			 (let* ((total (apply #'concatenate 'string (reverse acc))))
                           (ecase (handle-as-python-code total)
                             ((t) ) ;; handled)
                             ((nil) (handle-as-lisp-code total))
                             (:syntax-error (unless (handle-as-lisp-code total)
                                              (handle-as-python-code total :print-error t) ;; print python error
                                              (handle-as-lisp-code total :print-error t) ;; print lisp error
                                              (format t ";; Current input is therefore ignored.~%")
                                              (setf acc nil)))))))))))))))

(defun remove-interpreter-prompts (str prompts)
  "Remove all `>>>' and `...' at the start of lines. ~
Useful when re-parsing copied interpreter input."
  (check-type str string)
  (let ((new (concatenate 'string #.(string #\Newline) str))
        changed)
    (dolist (eol-ch '(#\Newline #\Return))
      (dolist (p prompts)
        (let ((prompt (concatenate 'string (string eol-ch) (string p))))
          (loop for ix = (search prompt new :test 'string=)
              while ix do (setf changed t)
                          (replace new new :start1 (1+ ix) :start2 (+ ix (length prompt)))
                          (setf new (subseq new 0 (- (length new) (length prompt))))))))
    (values new changed)))


;; Displaying stacktrace, todo
#+(or) 
(defun print-traceback ()
  (let ((frames (loop for f = (debug:newest-break-frame) then (next-frame f)
                    while f
                    when (debugger:frame-visible-p f)
                    collect f)))
    (dolist (f frames)
      (format t "tb> ~A ~S~%" (debugger:frame-type f) (debugger:frame-function f)))))

#+(or)
(defmacro with-stack-trace-restart (&body body)
  `(restart-bind ((:stacktrace 'print-traceback))
     ,@body))
       