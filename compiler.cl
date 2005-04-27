(in-package :python)

(defun annotate-ast (ast)
  (walk-py-ast ast #'annotate-ast-1))

(defun annotate-ast-1 (ast &key value target)
  (case (car ast)
    (funcdef (annotate-function ast))
    (class   (annotate-class ast))
    (lambda  (annotate-function ast :lambda t))))

(defun annotate-function (ast &key lambda)
  ;; ignore lambda for now
  (multiple-value-bind (params locals globals)
      (function-get-locals-globals suite params)
    
	))

(defun function-get-vars (ast)
  "Given AST of a function (FUNCDEF), returns ~@
     PARAMS LOCALS OUTER-SCOPE GLOBALS ~@
   where PARAMS the formal parameters, LOCALS the local variables, ~
   OUTER-SCOPE the variables to be found in an enclosing scope (which ~
   is possibly the global module namespace), GLOBALS the variables to ~
   be found in the module namespace."
  
  ;; XXX lambda
  (assert (eq (car ast) 'funcdef))
  
  (destructuring-bind
      (fname (pos-key-params *-param **-param) suite) (cdr ast)
    (declare (ignore fname))
    (let ((params (append (loop for p in pos-key-params
			      if (consp p) collect (car p)
			      else collect p)
			  (when *-param (list *-param))
			  (when **-param (list **-param))))
	  (locals ())
	  (globals ())
	  (outer-scope ()))

      (walk-py-ast
       suite
       (lambda (form &key value target)
	 (case (car form)
	   ((funcdef class lambda) (values t t))
	   (identifier (let ((name (second form)))
			 
			 (when value
			   (unless (or (member name params)
				       (member name locals)
				       (member name globals))
			     (pushnew name outer-scope)))
			 
			 (when target
			   (when (member name outer-scope)
			     (error "SyntaxError: local variable ~
                                     ~A referenced before assignment" name))
			   (unless (or (member name params)
				       (member name locals)
				       (member name globals))
			     (pushnew name locals))))
		       
		       form)
	   
	   (global (dolist (name (second form))
		     
		     (when (member name params)
		       (error "SyntaxError: function param ~
                               ~A declared `global'" name))
		     
		     (when (or (member name locals)
			       (member name outer-scope))
		       (error "SyntaxError: variable ~A used ~
                               before declared `global'" name))
		     
		     (pushnew name globals))
		   form)
	   
	   (t form)))
       :walk-lists-only t)
      
      (values params locals outer-scope globals))))