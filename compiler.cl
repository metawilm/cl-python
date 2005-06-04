(in-package :python)

;; Each node in the s-expression returned by the
;; parse-python-{file,string} corresponds to a macro defined below
;; that generates the corresponding Lisp code.
;; 
;; Note that each AST node has a name ending in "-expr" or
;; "-stmt". There no is separate package for those symbols (should
;; there be?)


(defvar *scope* nil "Current execution namespace.")

(defmacro with-py-error-handlers (&body body)
  `(handler-bind
       ((division-by-zero
	 (lambda (c)
	   (declare (ignore c))
	   (py-raise 'ZeroDivisionError "Division or modulo by zero")))
	
	#+allegro
	(excl:synchronous-operating-system-signal
	 (lambda (c)
	   (when (string= (slot-value c 'excl::format-control)
			  "~1@<Stack overflow (signal 1000)~:@>")
	     (py-raise 'RuntimeError "Stack overflow"))))
	
	#+allegro
	(excl:interrupt-signal
	 (lambda (c)
	   (let ((fa (slot-value c 'excl::format-arguments)))
	     (when (and (listp fa)
			(string= (second fa) "Keyboard interrupt"))
	       (py-raise 'KeyboardInterrupt "Keyboard interrupt")))))
	
	;; more?
	)
     ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; AST macros
;; 

(defmacro module-stmt (items)
  ;; register module (its name, the namespace, etc)
  (let* ((*scope* (make-namespace))
	 (module (make-module :namespace *scope*)))
    (progn ,@items)))

(defmacro suite-stmt (stmts)
  (progn ,@stmts))

(defmacro funcdef-stmt ..)
(defmacro classdef-stmt ..)

(defmacro identifier-expr (name)
  `(namespace-lookup ,name))

(defmacro assign-expr (val targets)
  `(let ((val ,val))
     ,@(loop for target in targets
	   collect `(setf ,target ,val))))

(defun (setf identifier-expr) (val name)
  (setf (namespace-lookup ,name
			  
(defun (setf attributeref-expr) (val)
  
  
(defmacro augassign-expr (op place val)
  (assert (member (car place) '(tuple subscription-expr
				attributeref-expr identifier-expr)))
  `(let (place-obj-1 place-obj-2)
     ,(case (car place)
	(tuple (py-raise 'SyntaxError ;; perhaps check in parser
			 "Augmented assignment to multiple places not possible ~
                          (got: ~A)" `(,place ,op ,val)))
	
	((subscription-expr attributeref-expr)
	 `(setf place-obj-1 ,(second place)
		place-obj-2 ,(third place))))
     
     (let* ((ev-val ,val)
	    (place-val-now ,(ecase (car place)
			      (identifier-expr ,place)
			      ((attributeref-expr subscription-expr)
			       `(,(car place) place-obj-1 place-obj-2)))))
       
       ,(multiple-value-bind (py-@= py-@) (lookup-inplace-op-func op)
	  `(or (funcall ,py-@= place-val-now ev-val) ;; returns true iff __i@@@__ found
	       (let ((new-val (funcall ,py-@ place-val-now ev-val)))
		 (assign-expr ev-val (,(ecase (car place)
					 (identifier-expr ,place)
					 ((attributeref-expr subscription-expr)
					  `(,(car place) place-obj-1 place-obj-2)))))))))))

(defmacro import-stmt ..)
(defmacro import-from-stmt ..)
(defmacro raise-stmt ..)

(defmacro if-stmt (if-clauses else-clause)
  `(cond ,@(loop for (cond body) in if-clauses
	       collect `((py-val->lisp-bool ,cond) ,body))))

(defmacro for-in-stmt (target source suite else-suite)
  `(catch 'break
     (let* ((take-else t)
	    (f (lambda (x)
		  (setf take-else nil)
		  (assign-expr x (,target))
		  (catch 'continue
		    ,suite))))
       (declare (dynamic-extent f))
       (map-over-py-object f ,source))
     (when (and take-else ,else-suite)
       ,eval-suite)))

(defmacro while-stmt (test suite else-suite)
  `(tagbody
    :break
     (loop
	 with take-else = t
	 while (py-val->lisp-bool ,test)
	 do (tagbody
	     :continue
	      (setf take-else nil)
	      ,suite)
	 finally (when (and (not taken) ,else-suite)
		   ,else-suite)))) 

(defmacro break-stmt ()
  `(go :break))

(defmacro continue-stmt ()
  `(go :continue))

(defmacro return-stmt (val)
  `(return-from :function-body val))
       
(defmacro yield-stmt ..)
(defmacro try-except-stmt ..)
(defmacro try-finally-stmt ..)
(defmacro print-stmt ..)
(defmacro global-stmt ..)
(defmacro exec-stmt ..)
(defmacro assert-stmt ..)

(defmacro binary-expr ..)
(defmacro binary-lazy-expr ..)
(defmacro unary-expr ..)
(defmacro comparison-expr ..)
(defmacro tuple-expr ..)
(defmacro list-expr ..)
(defmacro list-compr-expr ..)
(defmacro dict-expr ..)
(defmacro backticks-expr ..)
(defmacro labmda-expr ..)
(defmacro call-expr ..)
(defmacro subscription-expr ..)
(defmacro attributeref-expr ..)
(defmacro slice-expr ..)
(defmacro generator-expr ..)

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


(destructuring-bind (&key entry-assumed exit-set)
    (referenced-vars-simple ast :assumed '(x y))
  )
  
(referenced-variables (ast)
		      



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

(defun funcdef-vars (ast)
  "Given AST of a function (FUNCDEF), returns
     PARAMS LOCALS OUTER-SCOPE GLOBALS
where: PARAMS : the formal parameters
       LOCALS : the local variables
       OUTER-SCOPE : the variables to be found in an enclosing scope 
                     (possibly the global module namespace)
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












