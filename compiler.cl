(in-package :python)

;; Each node in the s-expression returned by the
;; parse-python-{file,string} corresponds to a macro defined below
;; that generates the corresponding Lisp code.
;; 
;; Note that each such AST node has a name ending in "-expr" or
;; "-stmt". There no is separate package for those symbols (should
;; there be?)


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
;; Lexical variables that keep internal Python state hidden from the
;; user take the form +NAME+ while local variables that should not
;; clash with Python's local variables take the form .NAME. 
;; 
;; The Python compiler uses its own kind of declaration to keep state
;; in generated code. The declaration is named PYDECL.

#+allegro 
(eval-when (:compile :load :eval)
  (sys:define-declaration pydecl (declaration env) nil :declare)
  (defun get-pydecl (var env)
    (cdr (assoc var (sys:declaration-information 'pydecl e) :test #'eq))))



(defstruct comp-namespace kind locals)
;; kind: one of (:module :function :class)

(defun module-scope (scopes-list) 
  (assert scopes-list)
  (let ((ms (car (last scopes-list))))
    (assert (eq (comp-namespace-kind ms) :module))
    ms))


(defmacro module-stmt (items)
  
  ;; A module is translated into one lambda that creates and returns a
  ;; module object. Executing the lambda might have side effects (like
  ;; things being printed, other modules being modified, or perhaps a
  ;; hard disk getting formatted).
  
  (let ((gv (module-global-vars items)))
    
    `(lambda ()
       (let* ((+module-scope+ (make-module-namespace))
	      (+module+ (make-module :namespace module-scope))
	      (+module-global-var-names+ (make-array ,(length gv) :initial-contents ',gv))
	      (+module-global-var-vals+  (make-array ,(length gv) :initial-element :unbound)))
	 
	 (locally (declare (pydecl (:module-global-var-names
				    ,(make-array (length gv) :initial-contents ',gv))
				   (:module-lexical-visible-vars ())
				   (:inside-function nil)))
	   ,@items
	   +module+))))) ;; XXX if executing module failed, where to catch error?

(defmacro suite-stmt (stmts)
  `(progn ,@stmts))

(defmacro funcdef-stmt (decorators
			name (&whole formal-args pos-args key-args *-arg **-arg)
			suite
			&environment e)
  
  (assert (eq (car name) 'identifier))
  
  (multiple-value-bind (arg-names local-names outer-scope-names global-names)
      (funcdef-vars params suite)
    
    ;; Determine closed-over variables (present as local in enclosing
    ;; functions). The other variables are globals.

    (multiple-value-bind (closed-over-names global-names)
	(loop
	    with lex-vars = (get-pydecl :module-lexical-visible-vars e)
	    
	    for os-name in outer-scope-names
	    if (member os-name lex-vars) 
	    collect os-name into closed-overs
	    else collect os-name into globals
	    
	    finally (return (values closed-overs globals)))
      
      (multiple-value-bind (simple-func-args destruct-statement)
	  ;; replace: def f( (x,y), z):  ..
	  ;;; by:     def f( _tmp , z):  (x,y) = _tmp; ..
	  (simplify-arguments formal-args)
	
	`(let ((.func.
		(make-py-function
		 :name ',(second name)
		 :locals ',local-names
		 :function (lambda ,simple-func-args
			     (let ,(loop for loc in locals collect `(,loc :unbound))
			       ,destruct-statement
			       (locally
				   (declare 
				    (pydecl (:function-variables (:global ',global-names)
								 (:closed-over ',closed-overs)
								 (:local ',local-names))
					    (:inside-function t)))
				 ,suite))))))
	   (assign-expr .func. (,name)))))))

(defmacro assign-expr (val targets &environment e)
  ;; XXX check order of evaluation
  (let* ((in-func (get-pydecl :inside-function e))
	 (func-vars (and in-func (get-pydecl :function-variables e)))
	 (func-var-globals (and func-vars (cdr (assoc :global func-vars))))
	 (func-var-locals (and func-vars (cdr (assoc :local func-vars))))
	 (func-var-closed-over (and func-vars (cdr (assoc :closed-over func-vars)))))
    
    `(let ((.val. ,val))
       ,@(loop for tg in targets collect
	       (ecase (car tg)
		 (identifier (let ((name (second tg)))
			       (cond ((or (member name func-var-locals)
					  (member name func-var-closed-over))
				      `(setq ,name .val))
				     
				    ((member name func-var-globals)
				     (let ((ix (position name (get-pydecl :module-global-var-names))))
				       (assert ix () "Variable ~A is global (?), but no ~
                                                      entry in :module-global-var-names ~A ?!"
					       name (get-pydecl :module-global-var-names))
				       
				       `(let ((old-val (svref +module-global-var-vals+ ,ix)))
					  
					  (cond ((typep old-val 'user-object)
						 (if (py-data-descriptor-p old-val) ;; __set__
						     (py-call-attribute-via-class old-val '__set__
										  .val.)
						   (setf (svref +module-global-var-vals+ ,ix) .val.)))
						
						(t
						 (setf (svref +module-global-var-vals+ ,ix) .val.))))))
				    
				    ((or (member name func-var-locals)
					 (member name func-var-closed-over))
				     (let ((old-val ,name))
				       `(cond ((typep old-val 'user-object)
					       (if (py-data-descriptor-p old-val) ;; __set__
						   (py-call-attribute-via-class old-val '__set__
										.val.)
						 (setf ,name .val.)))
						
					      (t
					       (setf ,name .val.)))))
				    
				    (t (error :unexpected)))))
		
		 (subscription-expr (error 'todo))
		 (attributeref-expr (error 'todo)))))))
			
(defmacro classdef-stmt ..)

(defmacro identifier-expr (name)
  (break "identifier at top-level?"))

; #+(or)
; (if *inside-function-body*
;     (if (member name *
; 		`(namespace-lookup ,name))
	
; 	(defmacro assign-expr (val targets)
	  
;   `(let ((val ,val))
;      ,@(loop for target in targets
; 	   collect (ecase (car target)
; 		     (identifier 
			   
; 			   `(setf ,target ,val))))

;(defun (setf identifier-expr) (val name)
;  (setf (namespace-lookup ,name
			  
;(defun (setf attributeref-expr) (val)
  

(defmacro augassign-expr (op place val)
  (assert (member (car place) '(tuple subscription-expr
				attributeref-expr identifier-expr)))
  
  `(let ,(when (member (car place) '(attributeref-expr subscription-expr))
	   '(place-obj-1 place-obj-2))
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
					 (identifier-expr place)
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
     (let* ((.take-else. t)
	    (.f. (lambda (.x.)
		  (setf .take-else. nil)
		  (assign-expr .x. (,target))
		  (catch 'continue
		    ,suite))))
       (declare (dynamic-extent .f.))
       (map-over-py-object .f. ,source))
     (when (and .take-else. ,else-suite)
       ,else-suite)))

(defmacro while-stmt (test suite else-suite)
  `(tagbody
    :break
     (loop
	 with .take-else. = t
	 while (py-val->lisp-bool ,test)
	 do (tagbody
	     :continue
	      (setf .take-else. nil)
	      ,suite)
	 finally (when (and .take-else. ,else-suite)
		   ,else-suite)))) 

(defmacro break-stmt ()
  `(go :break))

(defmacro continue-stmt ()
  `(go :continue))

(defmacro return-stmt (val)
  `(return-from :function-body ,val))
       
(defmacro yield-stmt ..)
(defmacro try-except-stmt ..)
(defmacro try-finally-stmt ..)
(defmacro print-stmt ..)

(defmacro global-stmt (names)
  (if *inside-function-body*
      (loop for n in names do (push n *function-globals*))
    (warn "Bogus `global' statement: not inside a function")))
   
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

