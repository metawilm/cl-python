(in-package :python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Keep track of built-in names shadowed by the user.
;; 

(defvar *builtin-ix-hash* (make-hash-table :test 'eq))
(defvar *builtin-ix-fixed* nil)
(defvar *num-builtins* 0)
(defvar *ever-shadowed-builtins* nil)

(defgeneric get-builtin-ix (bi-name)
  (:method ((bi-name symbol)) (gethash bi-name *builtin-ix-hash*)))

(defgeneric add-builtin-ix (bi-name)
  (:method ((bi-name symbol)) 
	   (when *builtin-ix-fixed* 
	     (error "builtin-ix is fixed already"))
	   (let ((ix (gethash bi-name *builtin-ix-fixed*)))
	     (if ix 
		 (progn (warn "built-in ~A was already registered" bi-name)
			ix)
	       (prog1 *num-builtins*
		 (setf (gethash bi-name *builtin-ix-hash*) *num-builtins*)
		 (incf *num-builtins*))))))

(defgeneric builtin-overridden-flag (bi-name)
  (:method ((bi-name symbol))
	   (unless *ever-shadowed-builtins*
	     (error "builtin-ix is not fixed yet (call BUILTIN-IX-FIXED first)"))
	   (let ((ix (or (gethash bi-name *builtin-ix-hash*)
			 (error "not registered as a built-in: ~A" bi-name))))
	     (aref *ever-shadowed-builtins* ix))))

(defgeneric (setf builtin-overridden-flag) (bi-name new-val)
  (:method ((bi-name symbol) &optional (value 1))
	   (unless *ever-shadowed-builtins*
	     (error "builtin-ix is not fixed yet (call BUILTIN-IX-FIXED first)"))
	   (setf (aref *ever-shadowed-builtins*
		       (or (gethash bi-name *builtin-ix-hash*)
			   (error "not registered as a built-in: ~A" bi-name))) new-val)))

(defun fix-builtin-ix ()
  (if *builtin-ix-fixed*
      (warn "builtin-ix was already fixed")
    (setf *builtin-ix-fixed* t
	  *ever-shadowed-builtins* (make-array *num-builtins*
					       :element-type 'bit :initial-element 0))))

(defun builtin-ix-fixed-p ()
  *builtin-ix-fixed*)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

(defun funcdef-get-vars (ast)
  "Given AST of a function (FUNCDEF), returns
     PARAMS LOCALS OUTER-SCOPE GLOBALS
where: PARAMS : the formal parameters
       LOCALS : the local variables
       OUTER-SCOPE : the variables to be found in an enclosing scope (possibly
                     the global module namespace)
      GLOBALS : the variables to be found in the global module namespace."
  
  (assert (eq (car ast) 'funcdef)) ;; XXX lambda
    
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

      (with-sub-ast ((form &key value target) t suite)
	(case (car form)
	  (funcdef `(XXX function name is like other variables))
	  (class   `(XXX class name is variable)) 
	  (lambda (values nil t)) ;; a lambda has no name
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
      
      (values params locals outer-scope globals))) )












