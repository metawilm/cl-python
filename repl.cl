(in-package :python)


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

(defvar *repl-mod*)

(defvar _   nil) ;; the last value evaluated by REPL
(defvar __  nil) ;; second-last
(defvar ___ nil) ;; third-last

(defvar *repl-prof* nil)

(defun repl ()
  (setf *repl-mod* (make-module))
  (clrhash *py-modules*)
  (let* ((dyn-globals (slot-value *repl-mod* 'dyn-globals)))
    
    (declare (special *the-none*))
    (setf (gethash '_   dyn-globals) *the-none*
	  (gethash '__  dyn-globals) *the-none*
	  (gethash '___ dyn-globals) *the-none*
	  (gethash '__name__ dyn-globals) "__main__")
    
    (labels ((print-cmds-1 (cmds)
	       (loop for (cmd expl) in cmds do (format t "  ~19A: ~A~%" cmd expl)))
	     (print-cmds ()
	       (format t "~%In the Python interpreter:~%")
	       (print-cmds-1 '((":help"             "print (this) help")
			       (":q"                "quit")
			       ("<command>"         "execute Python or Lisp <command>")
			       ("<space><command>"  "execute Lisp <command>")))
	       (format t "~%In the Lisp debugger:~%")
	       (print-cmds-1 '((":ptl" "back to Python top level")
			       (":re"  "retry the last (failed) Python command")))
	       (format t "~%"))
	     
	     (remember-value (val)
	       ;; Make last three return values available as _, __, ___
	       ;; for both Python (repl module namespace) and Lisp (dynamic vars).
	       (shiftf ___ __ _ val)
	       (shiftf (gethash '___ dyn-globals)
		       (gethash '__  dyn-globals)
		       (gethash '_   dyn-globals)
		       val))

	     (eval-print-ast (ast)
	       (destructuring-bind (module-stmt suite) ast
		 (assert (eq module-stmt 'module-stmt))
		 (assert (eq (car suite) 'suite-stmt))

		 (let ((val (block :val
			      (loop
				(let ((helper-func
					 (compile nil `(lambda ()
							 (declare (optimize (debug 3)))
							 (with-this-module-context (,*repl-mod*)
							   ,suite)))))
				    (loop
				      (with-simple-restart
					  (retry-repl-eval
					   "Retry the execution the compiled REPL command. [:re]")
					(return-from :val
					  (case *repl-prof*
					    (:ptime (prof:with-profiling (:type :time)
						      (funcall helper-func))
						    (terpri)
						    (prof:show-call-graph))
					    (:time (prog1 (time (funcall helper-func))
						     (terpri)))
					    (:space (prof:with-profiling
							(:type :space :count t) (funcall helper-func))
						    (terpri)
						    (prof:show-flat-profile))
					    (:pspace (prof:with-profiling (:type :space)
						       (funcall helper-func))
						     (terpri)
						     (prof:show-call-graph))
					    (t (funcall helper-func)))))))))))
		   (when val
		     (remember-value val)
		     (block :repr
		       (loop
			 (with-simple-restart
			     (:continue "Retry printing the object.")
			   ;; Write string with quotes around it; convert other objects
			   ;; using __str__ and print without quotes.
			   (if (stringp val)
			       (write-string (py-repr val)) 
			     (let ((str-val (py-str-string val)))
			       (write-string (py-val->string str-val)))) 
			   (write-char #\Newline))
			 (return-from :repr))))))))
    
      (loop
	  initially (format t "[CLPython -- type `:q' to quit, `:help' for help]~%")
	  do (loop 
	       (with-simple-restart (return-python-toplevel "Return to Python top level [:ptl]")
		 (loop with acc = ()
		     do #+(or)(format t "acc: ~S~%" acc)
			(locally (declare (special *stdout-softspace*))
			  (setf *stdout-softspace* (py-bool nil)))
			(format t (if acc "... " ">>> "))
			(let ((x (read-line)))
			  (cond
			 
			   ((string= x ":help")        (print-cmds))
			   ((string= x ":q")           (return-from repl 'Bye))
			 
			   ((string= x "")
			    (let ((total (apply #'concatenate 'string (nreverse acc))))
			      (setf acc ())
			      (loop
				(restart-case
				    (progn
				      (let ((ast (parse-python-string total)))
					(eval-print-ast ast)
					(return)))
				  (try-parse-again ()
				      :report "Parse string again into AST")
				  (recompile-grammar ()
				      :report "Recompile grammar"
				    (compile-file "parser")
				    (load "parser"))))))
			 
			   (t (push (concatenate 'string x (string #\Newline))
				    acc)

			      ;; Try to parse; if that returns a "simple" AST
			      ;; (like just inspecting the value of a variable), the
			      ;; input is complete and there's no need to wait for
			      ;; an empty line.
			    
			      (let* ((total (apply #'concatenate 'string (reverse acc))))
				(block :try-parse

				  ;; try to parse as Python code first
				  ;;  but when first char is a space, always treat it as Lisp code
				  (unless (and (> (length total) 0)
					       (char= (char total 0) #\Space))
				    (let ((ast (ignore-errors (parse-python-string total))))
				      (when ast
					(destructuring-bind (module-stmt (suite-stmt items)) ast
					  (assert (eq module-stmt 'module-stmt))
					  (assert (eq suite-stmt 'suite-stmt))
					  (when (and (= (length items) 1)
						     (or (not (listp (car items)))
							 (not 
							  (member (caar items)
								  '(classdef-stmt ;; Wait for empty line
								    for-in-stmt   ;; for perhaps multi-line
								    funcdef-stmt  ;; statements
								    if-stmt
								    try-except-stmt
								    try-finally-stmt
								    while-stmt)))))
					    (eval-print-ast ast)
					    (setf acc nil)))
					(return-from :try-parse))))
				
				  ;; try to parse as Lisp code second
				  (let ((lisp-form (ignore-errors (with-standard-io-syntax
								    ;; Bind package, so symbols _, __, ___
								    ;; are present.
								    (let ((*package* #.*package*))
								      (read-from-string total nil nil))))))
				    (when (and lisp-form
					       (not (member lisp-form '(def class for while if try)))) 
				      (multiple-value-bind (res err) 
					  (ignore-errors (eval lisp-form))
					(if err
					    (format t ";; Evaluation as Lisp failed: ~A~%" err)
					  (progn
					    (remember-value res)
					    (write res)
					    (write-char #\Newline)
					    (setf acc nil))))))))))))))))))

(defun prof (f kind)
  (ecase kind
    (:t (prof:with-profiling (:type :time :count t) (funcall f))
	   (terpri)
	   (prof:show-flat-profile))
    (:tg (prof:with-profiling (:type :time) (funcall f))
	 (terpri)
	 (prof:show-call-graph))
    (:s (prof:with-profiling (:type :space) (funcall f))
	(terpri)
	(prof:show-flat-profile))
    (:sg (prof:with-profiling (:type :space) (funcall f))
	 (terpri)
	 (prof:show-call-graph))
    (:time (time (funcall f)))))


;; stuff (possibly) used by emacs

(defun run-py-code-string (str)
  (let ((ast (parse-python-string str)))
    (run-ast ast)))
 
(defun run-ast (ast)
  (when (eq (car ast) 'module-stmt)
    (setq ast (cadr ast)))
  
  (let ((func (compile nil `(lambda ()
			      (with-this-module-context (,*repl-mod*)
				,ast)))))
    (funcall func)))


(defun mod-make-fasl (mod-name)
  (let* ((ast (parse-python-file (format nil "~A.py" mod-name)))
	 (func (compile nil `(lambda () ,ast)))
	 (fasl-file (format nil "~A.fasl" mod-name)))
    (excl:fasl-write func fasl-file t t)))

(defun mod-fasl (mod-name)
  (let ((fasl-file (format nil "~A.fasl" mod-name)))
    (car (excl:fasl-read fasl-file))))
    
(defun dummy (&rest args)
  (warn "dummy: ~A" args))
