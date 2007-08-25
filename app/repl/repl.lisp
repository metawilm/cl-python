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
  (:use :common-lisp :clpython :clpython.parser )
  (:export #:repl #:*repl-compile* #:*repl-prof*)
  (:import-from :clpython #:with-matching #:with-perhaps-matching))

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

(defun retry-repl-comp ()
  (let ((r (find-restart 'retry-repl-comp)))
    (if r
	(invoke-restart r)
      (warn "There is no Python REPL running."))))

(setf (top-level:alias "rc")
  #'retry-repl-comp)

(defun retry-repl-eval ()
  (let ((r (find-restart 'retry-repl-eval)))
    (if r
	(invoke-restart r)
      (warn "There is no Python REPL running."))))

(setf (top-level:alias "re")
  #'retry-repl-eval)


(defvar _   *the-none*) ;; the last value evaluated by REPL
(defvar __  *the-none*) ;; second-last
(defvar ___ *the-none*) ;; third-last

(defvar *repl-prof* nil
  "Execution of each expression is profiled according to this setting.
Possible values: :time :ptime :space :pspace nil")

(defvar *repl-compile* nil
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
   *repl-prof*     => profile all Python commands
                      value must be one of:
                         :time    = like (TIME ...)
                         :ptime   = time call graph
                         :space   = list of allocations
                         :pspace  = space call graph
                         nil      = no profiling
")

(defvar *repl-mod* nil "The REPL module (for debugging)")

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
  (let* ((repl-mod (make-module))
	 (*repl-mod* repl-mod)
	 (dyn-globals (slot-value repl-mod 'dyn-globals))
	 (clpython::*habitat* (clpython::make-habitat))
	 acc)
    
    (setf (gethash '{_}        dyn-globals) *the-none*
	  (gethash '{__}       dyn-globals) *the-none*
	  (gethash '{___}      dyn-globals) *the-none*
	  (gethash '{__name__} dyn-globals) "__main__")
    
    (labels ((print-cmds ()
	       (format t *repl-doc*))
	     
	     (remember-value (val)
               ;; Make last three return values available as _, __, ___
               ;; for both Python (repl module namespace) and Lisp (dynamic vars).
               (when val ;; don't save NIL (which can be return value for Lisp eval)
                 (shiftf ___ __ _ val)
                 (shiftf (gethash '{___} dyn-globals)
                         (gethash '{__}  dyn-globals)
                         (gethash '{_}   dyn-globals)
                         val)))

	     (run-ast-func (suite)
               (let ((f `(lambda ()
                           (clpython::with-this-module-context (,repl-mod)
                             ,suite))))
                 (when *repl-compile*
                   (setf f (compile nil f)))
                 f))
	     
	     (eval-print-ast (ast)
               (with-matching (ast ([module-stmt] ?suite))
		 (assert (eq (car ?suite) '[suite-stmt]))
		 (let ((vals (multiple-value-list
                              (block :val
                                (loop
                                  (let ((helper-func (run-ast-func ?suite)))
                                    (loop
                                      (with-simple-restart
                                          (retry-repl-eval
                                           "Retry the execution the compiled REPL command. [:re]")
                                        (return-from :val
                                          (profile helper-func *repl-prof*))))))))))
                   (when (car vals) ;; skip NIL
                     (remember-value (car vals))
                     (block :repr
                       (loop
                         (with-simple-restart
                             (:continue "Retry printing the object.")
                           ;; Write string with quotes around it; convert other objects
                           ;; using __str__ and print without quotes.
                           (loop for val in vals
                               do (if (stringp val)
                                      (write-string (py-repr val)) 
                                    (let ((str-val (py-str-string val)))
                                      (write-string (py-val->string str-val)))) 
                                  (write-char #\Newline)))
                         (return-from :repr)))))))
	     
	     (handle-as-python-code (total &key print-error ast-finished)
               ;; Return T if this succeeded somehow, i.e. parsing as Lisp
               ;; should not be attempted.
               ;; When first char is a space, always treat it as Lisp code
               (when (or (zerop (length total))
                         (char= (char total 0) #\Space))
                 (return-from handle-as-python-code nil))
               
               (flet ((remove-prompts (str)
                        (let ((new (concatenate 'string (format nil "~A" #\Newline) str))
                              changed)
                          (dolist (eol-ch '(#\Newline #\Return))
                            (dolist (p '(">>> " "... "))
                              (let ((prompt (format nil "~A~A" eol-ch p)))
                                (loop for ix = (search prompt new :test 'string=)
                                    while ix
                                    do (setf changed t)
                                       (replace new new :start1 (1+ ix) :start2 (+ ix (length prompt)))
                                       (setf new (subseq new 0 (- (length new) (length prompt))))))))
                          (values new changed)))
                      
                      (return-syntax-error (err)
                        (when print-error
                          (format t ";; Python parse failed:  ~A~%" err))
                        (return-from handle-as-python-code :syntax-error)))
                 
                 (let ((ast (handler-case (parse-python-string total)
                              ({UnexpectedEofError} () (return-from handle-as-python-code t))
			      ({SyntaxError} (err)
                                (or (when *ignore-copied-prompts*
                                      (multiple-value-bind (new-str changed)
                                          (remove-prompts total)
                                        (when changed
                                          (handler-case (parse-python-string new-str)
                                            ({UnexpectedEofError} ()
                                              (return-from handle-as-python-code t))
                                            (error (err2)
                                              (return-syntax-error err2))))))
                                    (return-syntax-error err))))))
                   (when ast
                     (with-matching (ast ([module-stmt] ?suite-stmt))
                       (with-matching (?suite-stmt ([suite-stmt] ?items))
                         (when (cond (ast-finished 
                                      t)
                                     
                                     ((not (listp (car ?items)))
                                      t)
                                   
                                   ((> (length ?items) 1)
                                    nil) ;; todo?

                                   ;; Special A.I. to recognize these common cases:
                                   ;;  1. def f(): ...; return 42
                                   ;;     (`return' at the end of function body)
                                   ;;  2. def f(): if c: return X; else: return Y
                                   ;;     (`return' in every `if' clause; as in `fact')
                                   ((with-perhaps-matching
                                        ((car ?items)
                                         ([funcdef-stmt] ?decorators ([identifier-expr] ?fname)
                                                         ?fargs
                                                         ([suite-stmt] ?stmts)))
                                      (or (eq (caar (last ?stmts)) '[return-stmt]) ;; 1.
                                          (with-perhaps-matching ;; 2.
                                              ((car (last ?stmts))
                                               ([if-stmt] ?if-clauses ?else-clause))
                                            (and (every (lambda (c)
                                                          (with-perhaps-matching (c (?cond ([suite-stmt] ?stmts)))
                                                            (eq (caar (last ?stmts)) '[return-stmt])))
                                                        ?if-clauses)
                                                 ?else-clause ;; otherwise stmt perhaps not finished
                                                 (with-perhaps-matching (?else-clause ([suite-stmt] ?stmts))
                                                   (eq (caar (last ?stmts)) '[return-stmt]))))))
                                    t)
                                   
                                   ((member (caar ?items)
                                            '([classdef-stmt] ;; Wait for empty line
                                              [for-in-stmt]   ;; for perhaps multi-line
                                              [funcdef-stmt]  ;; statements
                                              [if-stmt]
                                              [try-except-stmt]
                                              [try-finally-stmt]
                                              [while-stmt] ))
                                    nil)
                                   (t t))
                           
                           (eval-print-ast ast)
                           (setf acc nil))))
                     
                     (return-from handle-as-python-code t)))))
	     
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
                                t))))))))
      
      (loop
	  initially (format t "[CLPython -- type `:q' to quit, `:help' for help]~%")
	  do (loop 
	       (with-simple-restart (return-python-toplevel "Return to Python top level [:ptl]")
		 (setf acc ())
		 (loop 
		   (locally (declare (special *stdout-softspace*))
		     (setf *stdout-softspace* (py-bool nil)))
		   (format t (if acc "... " ">>> "))
		   (let ((x (read-line)))
		     (cond
		      
		      ((and (> (length x) 0)
			    (char= (aref x 0) #\:))
		       (multiple-value-bind (cmd ix)
			   (read-from-string x)
			 (declare (ignore ix))
			 (case cmd
			   (:help  (print-cmds))
			   (:q     (return-from repl 'Bye))
			   (t      (warn "Unknown command: ~S" cmd)))))
		      
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
  