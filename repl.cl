(in-package :python)

(defparameter *show-ast* nil)
(defvar *last-val* nil)


(defun goto-python-top-level ()
  (let ((r (find-restart 'return-python-toplevel)))
    (if r
	(invoke-restart r)
      (warn "There is no Python REPL running."))))

(setf (top-level:alias "ptl")
  #'goto-python-top-level)

(defvar @) ;; local Python variable value, in the REPL debugger 

(defun show-python-locals (&optional name)
  (unless *scope*
    (warn "No Python REPL running")
    (return-from show-python-locals))
  (let ((alist (dict->alist *scope*)))
    (if name
	(loop for (k . v) in alist
	    when (string= (symbol-name k) (symbol-name name))
	    do (format t "~A~%" v)
	       (setf @ v))
      (loop for (k . v) in alist 
	  do (format t "~S~8T~S~%" k v)))))

(setf (top-level:alias "ploc")
  #'show-python-locals)

(defun repl ()
  (declare (special *scope* *sys.modules* *initial-sys.modules* *None*))
  
  (format t "[CLPython -- type `:q' to quit, `:help' for help]~%")
  (setf *sys.modules* (dict-copy *initial-sys.modules*))  ;; or LET ?
  (loop
    (let ((*scope* (make-namespace :name "repl ns" :builtins t)))
      (namespace-bind *scope* '__name__ "__main__")
      (loop
	(with-simple-restart (return-python-toplevel "Return to Python top level [:ptl]")
	  (let ((acc ()))
	    (flet ((show-ast (ast)
		     (when *show-ast*
		       (format t "AST: ~S~%" ast)))
		   
		   (eval-print-ast (ast)
		     (loop (restart-case
			       (let ((ev-ast (py-eval ast)))
				 (assert (eq (car ev-ast) :file-input))
				 (when (> (length ev-ast) 1)
				   (let ((ev (car (last ev-ast))))
				     
					
				     ;; don't print value if it's None
				     (unless (member ev (list *None* nil) :test 'eq)
				       (format t "~A~%" (call-attribute-via-class ev '__repr__))
				       (namespace-bind *scope* '_ ev)
				       (setf *last-val* ev))))
				 (return-from eval-print-ast))
			     (retry-py-eval ()
				 :report "Retry (py-eval AST)" ())))))
	      (loop
		(format t (if acc
			      "... "
			    ">>> "))
		(let ((x (read-line)))
		  (cond
		 
		   ((string= x ":help")
		    (flet ((print-cmds (cmds)
			     (loop for (cmd expl) in cmds
				 do (format t "  ~13A: ~A~%" cmd expl))))
		      (format t "~%In the Python interpreter:~%")
		      (print-cmds '((":help" "print (this) help")
				    (":lisp EXPR" "evaluate a Lisp expression")
				    (":ns" "print current namespace")
				    (":show-ast" "print the AST of inputted Python code")
				    (":no-show-ast" "don't print the AST")
				    (":q" "quit")
				    ("_" "Python variable `_' is bound to the value of the last expression")))
		      (format t "~%In the Lisp debugger:~%")
		      (print-cmds '((":ploc" "Python local variables")
				    (":ploc VAR" "Value of local variable VAR, bound to @")
				    (":ptl" "back to Python top level")))
		      (format t "~%")))
		 
		   ((string= x ":acc")         (format t "~S" acc))
		   ((string= x ":q")           (return-from repl 'Bye))
		   ((string= x ":show-ast")    (setf *show-ast* t))
		   ((string= x ":no-show-ast") (setf *show-ast* nil))
		   ((string= x ":ns")          (format t "~&REPL namespace:~%~S~&"
						       (dict->alist *scope*)))
		   ((string= x ":d") (describe *last-val*))

		   ((and (>= (length x) 5)
			 (string= (subseq x 0 5) ":lisp"))
		    (format t "~A~%"
			    (eval (read-from-string (subseq x 6)))))

		   ((string= x "")
		    (let ((total (apply #'concatenate 'string (reverse acc))))
		      (setf acc ())
		      (loop
			(restart-case
			    (progn
			      (let ((ast (parse-python-string total)))
				(show-ast ast)
				(eval-print-ast ast)
				(return)))
			  (try-parse-again ()
			      :report "Parse string again into AST")
			  (recompile-grammar ()
			      :report "Recompile grammar"
			    (compile-file "parsepython")
			    (load "parsepython"))))))
		 
		   (t  
		    (push (concatenate 'string x (string #\Newline))
			  acc)

		    ;; Try to parse; if that returns a "simple" AST
		    ;; (just inspecting the value of a variable), the
		    ;; input is complete and ther's no need to wait for
		    ;; an empty line.
		  
		    (let* ((total (apply #'concatenate 'string (reverse acc)))
			   (ast (ignore-errors (parse-python-string total))))
		      (when ast
			(assert (eq (first ast) 'file-input))
			(when (and (= (length ast) 2)
				   (member (caar (second ast)) '(testlist assign-expr) :test 'eq))
			  (show-ast ast)
			  (eval-print-ast ast)
			  (setf acc nil)))))))))))))))
