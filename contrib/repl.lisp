;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.APP.REPL; Readtable: PY-AST-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Read-Eval-Print loop 

(in-package :clpython.app.repl)
(in-syntax *ast-user-readtable*)

;;; Restarts

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *repl-restart-aliases*
      '(("pt" . return-python-toplevel)
        ("re" . retry-repl-eval)))
  
  #+allegro
  (defun abbrev-for-restart (r &optional fmt)
    (let ((str (or (car (rassoc r *repl-restart-aliases*))
                   (error "No such Python repl restart: ~A." r))))
      (if fmt (format nil fmt str) str)))
)

(defun try-invoke-restart (rname)
  (let ((r (find-restart rname)))
    (if r
        (invoke-restart r)
      (warn "There is no Python REPL running."))))

(defmacro with-repl-toplevel-aliases (&body body)
  #+allegro
  `(progn (progn ,@(loop for (abbrev . restart) in *repl-restart-aliases*
                       collect `(setf (top-level:alias ,abbrev)
                                  (lambda () (try-invoke-restart ',restart)))))
          (unwind-protect (progn ,@body)
            (with-output-to-string (*terminal-io*) ;; suppress "removed `pt' alias" messages
              (tpl:remove-alias ,@(mapcar #'car *repl-restart-aliases*)))))
  #-allegro
  `(progn ,@body))

(defvar _   *the-none* "The last value evaluated by REPL")
(defvar __  *the-none* "The second-last value evaluated by REPL")
(defvar ___ *the-none* "The third-last value evaluated by REPL")

(defvar *repl-prof* nil
  "Execution of each expression is profiled according to this setting.
Possible values: :time :ptime :space :pspace nil")

(defvar *repl-compile* nil
  "Whether code typed in the REPL is compiled before running.")

(defvar *ignore-copied-prompts* t
  "Whether to remove initial `>>>' and `...' on the input line.
If true, previous input can be copy-pasted as new input easily.")

(defparameter *doc* (concatenate 'string
                           "CLPython - an implementation of Python in Common Lisp
http://common-lisp.net/project/clpython

CLPython is licensed under the terms of the Lisp Lesser GNU Public License
\(http://opensource.franz.com/preamble.html), known as the LLGPL. The LLGPL
consists of a preamble (see above URL) and the LGPL.  Where these conflict,
the preamble takes precedence. CLPython is referenced in the preamble as
the \"LIBRARY.\"~%"
                     "
Keyboard commands in this Python interpreter:
     :h             => print this help
     :q             => quit
  <form>            => execute Python or Lisp form
  <space><form>     => execute Lisp form
"
                     #+allegro
                     (format nil "
Restart shortcuts in the Lisp debugger:
     :~A            => back to Python top level
     :~A            => retry evaluation of the last Python command
" 
                             (abbrev-for-restart 'return-python-toplevel)
                             (abbrev-for-restart 'retry-repl-eval))
                     
                     (format nil "
Relevant Lisp variables (exported from package ~A):
   *repl-compile*   => whether source code is compiled into assembly
                       before running (current value: ~A)
   *repl-prof*      => profile execution of Python code (current value: ~A)
                       value must be one of:
                         :time    = like (TIME ...)
                         :ptime   = time call graph
                         :space   = list of allocations
                         :pspace  = space call graph
                         nil      = no profiling
"
                             #.(package-name *package*)
                             *repl-compile*          
                             *repl-prof*)))

(defvar *repl-mod* nil "The REPL module (for debugging)")
(defvar *prompts* '(">>> " "... "))

(defun profile (f kind)
  "Call F in profiling context.
KIND can be :ptime, :time, :space, :pspace or NIL."
  (ecase kind
    (:ptime  #-allegro (error "todo")
             #+allegro (progn (prof:with-profiling (:type :time)
                                (funcall f))
                              (terpri)
                              (prof:show-call-graph)))
    (:time   (prog1 (time (funcall f))
               (terpri)))
    (:space  #-allegro (error "todo")
             #+allegro (progn (prof:with-profiling
                                  (:type :space :count t) (funcall f))
                              (terpri)
                              (prof:show-flat-profile)))
    (:pspace #-allegro (error "todo")
             #+allegro (progn (prof:with-profiling (:type :space)
                                (funcall f))
                              (terpri)
                              (prof:show-call-graph)))
    ((nil)   (funcall f))))

(defmacro maybe-with-ldb-backend (&body body)
  #+clpython-source-level-debugging
  `(unwind-protect (excl::with-ldb-backend (:python) ,@body)
     
     ;; Delete all temp breakpoints upon quitting the REPL to ease debugging.
     ;; XXXX This removes also all non-Python breakpoints.
     (excl::delete-temp-breakpoints-except))
  #-clpython-source-level-debugging
  `(progn ,@body))

(defun repl (&rest options)
  (clpython::%reset-import-state)

  ;; This is a kind of hack to make importing modules a lot faster, while we
  ;; don't really record source location information.
  (let (#+allegro (excl:*load-xref-info* nil)
        #+allegro (excl:*record-xref-info* nil))
  
    (format t "Welcome to CLPython, an implementation of Python in Common Lisp.~%")
    (format t "Running on: ~A ~A~%" (lisp-implementation-type) (lisp-implementation-version))
    (format t "REPL shortcuts: `:q' = quit, `:h' = help.~%")
    (clpython::maybe-warn-set-search-paths nil)
    (with-repl-toplevel-aliases
        (clpython.parser:with-source-locations
            (with-ast-user-readtable ()
              (let ((clpython::*muffle-sbcl-compiler-notes* t))
                (maybe-with-ldb-backend
                 (apply #'repl-1 options))))))))

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

(defvar *repl-module-globals*)

(defun repl-1 (&key cmd-args lisp-exit)
  (let* ((*repl-module-globals* (clpython::make-eq-hash-table))
         (mod-namespace (clpython::make-hash-table-ns
                          :dict-form '*repl-module-globals*
                          :scope :module
                          :parent (clpython::make-builtins-namespace)))
         (clpython:*habitat* (clpython:make-habitat :cmd-line-args cmd-args))
         (*truncation-explain* t)
	 acc)
    (declare (special clpython:*habitat*))
    (labels ((print-cmds ()
	       (format t *doc*))
	     (remember-value (val)
               ;; Make last three return values available as _, __, ___
               ;; for both Python (repl module namespace) and Lisp (dynamic vars).
               (when val ;; don't save NIL (which can be return value for Lisp eval)
                 (shiftf ___ __ _ val)
                 (eval `(progn ,(clpython::ns.write-form mod-namespace '{_} '_)
                               ,(clpython::ns.write-form mod-namespace '{__} '__)
                               ,(clpython::ns.write-form mod-namespace '{___} '___)))))
             (clear-history ()
               (loop repeat 3 do (remember-value *the-none*)))
	     (run-ast-func (suite)
               (lambda () (let ((clpython::*module-namespace* mod-namespace))
                            (clpython::run-python-ast suite
                                                      :module-globals *repl-module-globals*
                                                      :compile *repl-compile*))))
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
		 (assert (ast-p ?suite '[suite-stmt]))
		 (let ((vals (multiple-value-list
                              (block :val 
                                (loop (let ((helper-func (run-ast-func ast)))
                                        (loop (with-simple-restart
                                                  (retry-repl-eval (concatenate 'string "Retry the expression: \"~A\"" #+allegro " ~A.")
                                                                   (nice-one-line-input-abbrev total)
                                                                   #+allegro (abbrev-for-restart 'retry-repl-eval "(:~A)"))
                                                (return-from :val
                                                  (profile helper-func *repl-prof*))))))))))
                   (when (car vals) ;; skip NIL
                     (remember-value (car vals))
                     (block repr
                       (unless (and (null (cdr vals))
                                    (clpython::none-p (car vals))) 
                         (loop
                           (with-simple-restart
                               (:continue "Retry printing the object.")
                             ;; Write string with quotes around it; convert other objects
                             ;; using __str__ and print without quotes.
                             (loop for val in vals
                                 do (if (stringp val)
                                        (print-string-truncated (py-repr val))
                                      (multiple-value-bind (str-val error)
                                          (ignore-errors (py-str-string val :circle t))
                                        (if str-val
                                            (print-string-truncated (py-val->string str-val))
                                          (progn (format t ";; Warning: printing ~A gave:~%;;   ~A~%" val error)
                                                 (format t "~A" val)))))
                                    (write-char #\Newline)))
                           (return-from repr))))))))
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
                              ({UnexpectedEofError} (c)
                                (if (eq ast-finished t)
                                    (error c)
                                  (return-from handle-as-python-code t)))
			      ({SyntaxError} (err)
                                (or (when *ignore-copied-prompts*
                                      (multiple-value-bind (new-str changed)
                                          (remove-interpreter-prompts total *prompts*)
                                        (when changed
                                          (handler-case (values (parse new-str))
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
                       (assert (ast-p ast '[module-stmt]))
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
                       
		       ((or (null lisp-form)
                            (and (symbolp lisp-form)
                                 (member lisp-form (mapcar #'second clpython.parser::*multi-line-statements*)
                                         :test 'string=)))
                        ;; Start of a multi-line Python form
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
             #+(or allegro sbcl)
             (input-available-p ()
               (let ((ch (read-char-no-hang *standard-input*)))
                 (when ch
                   (prog1 t (unread-char ch)))))

             (exit-repl ()
               (if lisp-exit
                   (clpython.util:quit)
                 (return-from repl-1 (values)))))
      (clear-history)
      (loop 
        (with-simple-restart (return-python-toplevel
                              (concatenate 'string "Return to Python top level" #+allegro " ~A.")
                              #+allegro (abbrev-for-restart 'return-python-toplevel "(:~A)"))
          (setf acc ())
          (loop 
            (locally (declare (special *stdout-softspace*))
              (setf *stdout-softspace* (py-bool nil)))
            
            (unless (checking-reader-conditionals
                     #+(or allegro sbcl) (input-available-p)
                     #-(or allegro sbcl) nil)
              ;; When copy-pasting multiple lines of Python source code into the REPL,
              ;; prevent several prompts being printed below the copied code.
              (format t (nth (if acc 1 0) *prompts*))
              (force-output *standard-output*)) ;; stream T would mean *terminal-io*

            (let ((x (read-line *standard-input* nil 'eof)))
              ;; XXX Let future debugger interface use *debug-io*?
              (cond
               ((eq x 'eof)
                (terpri *standard-output*)
                (exit-repl))
               ((and (> (length x) 0)
                     (char= (aref x 0) #\:))
                (multiple-value-bind (cmd ix)
                    (read-from-string (string-downcase x))
                  (declare (ignore ix))
                  (case cmd
                    (:h
                     (print-cmds))
                    (:q
                     (exit-repl))
                    ((:time :ptime :space :pspace)
                     (if (eq *repl-prof* cmd)
                         (progn (setf *repl-prof* nil)
                                (format t ";; Stopped profiling.~%"))
                       (progn (setf *repl-prof* cmd)
                              (format t ";; Set profiling to ~A.~%" cmd))))
                    (t
                     (warn "Unknown command: ~S" cmd)))))
               
               ((and (string= x "") #+allegro (not (input-available-p)))
                (let ((total (apply #'concatenate 'string (nreverse acc))))
                  (setf acc ())
                  (loop
                    (restart-case
                        (progn (or (eq t (handle-as-python-code total :print-error t :ast-finished t))
                                   (handle-as-lisp-code total :print-error t))
                               (return))
                      (try-parse-again ()
                          :report "Parse string again into AST")))))
               
               ((and (> (length x) 0)
                     (char= (aref x 0) #\#))
                ;; skip comment line
                )
               
               (t (push (concatenate 'string x (string #\Newline)) acc)
                  (let* ((total (apply #'concatenate 'string (reverse acc))))
                    (ecase (handle-as-python-code total)
                      ((t) ) ;; handled)
                      ((nil) (handle-as-lisp-code total))
                      (:syntax-error (unless (handle-as-lisp-code total)
                                       (handle-as-python-code total :print-error t) ;; print python error
                                       (handle-as-lisp-code total :print-error t) ;; print lisp error
                                       (format t ";; Current input is therefore ignored.~%")
                                       (setf acc nil))))))))))))))

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
                          (setf new (subseq new 0 (1+ (- (length new) (length prompt)))))))))
    (values new changed)))
