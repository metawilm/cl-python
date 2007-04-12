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
  (:export #:repl #:*repl-prof* ))

(in-package :clpython.app.repl)
(in-syntax *ast-user-readtable*)


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

(defvar *repl-doc* "
In the Python interpreter:

     :help          => print (this) help
     :q             => quit

  <command>         => execute Python or Lisp <command>
  <space><command>  => execute Lisp <command>


In the Lisp debugger:

     :ptl           => back to Python top level
     :re            => retry the last (failed) Python command


Relevant Lisp variables:

   *repl-prof*     => profile all Python commands
                      value must be one of:
                         :time    = like (TIME ...)
                         :ptime   = time call graph
                         :space   = list of allocations
                         :pspace  = space call graph
                         nil      = no profiling
")

(defvar *repl-mod* nil)

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
	       (shiftf ___ __ _ val)
	       (shiftf (gethash '___ dyn-globals)
		       (gethash '__  dyn-globals)
		       (gethash '_   dyn-globals)
		       val))

	     (run-ast-func (suite)
	       #+(or)(warn "AST: ~S" suite)
	       (compile nil `(lambda ()
			       (declare (optimize (debug 3)))
			       (clpython::with-this-module-context (,repl-mod)
				 ,suite))))
	     
	     (eval-print-ast (ast)
	       (destructuring-bind (module-stmt suite) ast
		 (assert (eq module-stmt '[module-stmt]))
		 (assert (eq (car suite) '[suite-stmt]))

		 (let ((vals (multiple-value-list
			     (block :val
			      (loop
				(let ((helper-func (run-ast-func suite)))
				  (loop
				    (with-simple-restart
					(retry-repl-eval
					 "Retry the execution the compiled REPL command. [:re]")
				      (return-from :val
					(ecase *repl-prof*
					  (:ptime  (prof:with-profiling (:type :time)
						     (funcall helper-func))
						   (terpri)
						   (prof:show-call-graph))
					  (:time  (prog1 (time (funcall helper-func))
						    (terpri)))
					  (:space  (prof:with-profiling
						       (:type :space :count t) (funcall helper-func))
						   (terpri)
						   (prof:show-flat-profile))
					  (:pspace (prof:with-profiling (:type :space)
						     (funcall helper-func))
						   (terpri)
						   (prof:show-call-graph))
					  ((nil)   (funcall helper-func))))))))))))
		   (when (car vals) ;; Don't remember NIL
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
	     
	     (handle-as-python-code (total)
	       ;; Return T if this \"succeeded\" somehow, i.e. parsing as Lisp
	       ;; should not be attempted.
	       ;; When first char is a space, always treat it as Lisp code
	       (unless (and (> (length total) 0)
			    (char= (char total 0) #\Space))
		 
		 (let ((ast (handler-case (parse-python-string total)
			      ({UnexpectedEofError} () (return-from handle-as-python-code t))
			      ({SyntaxError} () nil))))
		   
		   (when ast
		     (destructuring-bind (module-stmt (suite-stmt items)) ast
		       (assert (eq module-stmt '[module-stmt]))
		       (assert (eq suite-stmt '[suite-stmt]))
		       (when (and (= (length items) 1)
				  (or (not (listp (car items)))
				      (not 
				       (member (caar items)
					       '([classdef-stmt] ;; Wait for empty line
						 [for-in-stmt]   ;; for perhaps multi-line
						 [funcdef-stmt]  ;; statements
						 [if-stmt]
						 [try-except-stmt]
						 [try-finally-stmt]
						 [while-stmt] )))))
			 (eval-print-ast ast)
			 (setf acc nil)))
		     (return-from handle-as-python-code t)))))
	     
	     (handle-as-lisp-code (total)
	       (let ((lisp-form
		      (ignore-errors (with-standard-io-syntax
				       ;; Bind package, so symbols _, __, ___ are present.
				       (let ((*package* #.*package*))
					 (read-from-string total nil nil))))))
		 
		 (cond ((null lisp-form) ) ;; could not parse as lisp
		       
		       ((member lisp-form '(nil def class for while if try)) )
		       ;; ignore, it's a multiline python code
					  
		       ((and (symbolp lisp-form)
			     (> (length (symbol-name lisp-form)) 1)
			     (char= (aref (symbol-name lisp-form) 0) #\@)) )
		       ;; Reading function decorator
		       
		       (t (multiple-value-bind (res err) 
			      (ignore-errors (multiple-value-list (eval lisp-form)))
			    (if (and (null res)
				     (typep err 'condition))
				(format t ";; Eval as Lisp failed: ~A~%" err)
			      (progn
				(remember-value (car res))
				(dolist (r res)
				  (write r)
				  (write-char #\Newline))
				(setf acc nil)))))))))
      
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
			       (let ((ast (parse-python-string total)))
				 (eval-print-ast ast)
				 (return))
			     (try-parse-again ()
				 :report "Parse string again into AST")))))
		      
		      (t (push (concatenate 'string x (string #\Newline)) acc)
			 (let* ((total (apply #'concatenate 'string (reverse acc))))
			   (or (handle-as-python-code total)
			       (handle-as-lisp-code total)))))))))))))
  