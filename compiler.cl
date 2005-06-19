(in-package :python)

;; Each node in the s-expression returned by the
;; parse-python-{file,string} corresponds to a macro defined below
;; that generates the corresponding Lisp code.
;; 
;; Note that each such AST node has a name ending in "-expr" or
;; "-stmt". There no is separate package for those symbols (should
;; there be?)



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
(eval-when (:compile :load :eval) ;; ??
  (sys:define-declaration
      pydecl (&rest property-pairs) nil :declare
      (lambda (declaration env)
	(values :declare
		(cons 'pydecl
		      (nconc (cdr declaration)
			     (sys:declaration-information 'pydecl env)))))))

;; Macrology to easy working with PYDECL context data.

#+allegro
(defun get-pydecl (var env)
  (cdr (assoc var (sys:declaration-information 'pydecl e) :test #'eq)))

#+allegro
(defmacro with-pydecl (pairs &body body)
  `(locally (declare (pydecl ,@pairs))
     ,@body))


(defmacro module-stmt (items)
  
  ;; A module is translated into one lambda that creates and returns a
  ;; module object. Executing the lambda, i.e. importing the module,
  ;; might have all kinds of side effects.
  
  (let ((gv (module-global-vars items)))
    
    `(lambda ()
       (let* ((+module-scope+ (make-module-namespace))
	      (+module+       (make-module :namespace module-scope))
	      (+module-global-var-names+ (make-array ,(length gv) :initial-contents ',gv))
	      (+module-global-var-vals+  (make-array ,(length gv) :initial-element :unbound)))
	 
	 ;; pre-set global variable names corresponding to built-ins
	 (loop 
	     for glob-var-name across +module-global-var-vals+
	     for i from 0
	     when (builtin-name-p glob-var-name)
	     do (setf (aref +module-global-var-vals+ i) (builtin-name-value glob-var-name)))

	 (with-pydecl ((:module-global-var-names ,(make-array (length gv) :initial-contents gv))
		       (:module-lexical-visible-vars ())
		       (:inside-function nil))
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
	(loop with lex-vars = (get-pydecl :module-lexical-visible-vars e)
	    for os-name in outer-scope-names
	    if (member os-name lex-vars) collect os-name into closed-overs
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
			       
			       (block :function-body
				 (with-pydecl ((:function-variables (:global ',global-names)
								    (:closed-over ',closed-overs)
								    (:local ',local-names))
					       (:inside-function t))
				   ,suite)))))))
	   (assign-expr .func. (,name)))))))

(defmacro assign-expr (val targets &environment e)
  ;; XXX check order of evaluation
  (let* ((in-func              (get-pydecl :inside-function e))
	 (func-vars            (and in-func (get-pydecl :function-variables e)))
	 (func-var-locals      (and func-vars (cdr (assoc :local func-vars))))
	 (func-var-closed-over (and func-vars (cdr (assoc :closed-over func-vars)))))
    
    `(let ((val ,val))
       ,@(loop for tg in targets collect
	       (ecase (car tg)
		 
		 (identifier
		  (let ((name (second tg)))
		    (if (or (member name func-var-locals) (member name func-var-closed-over))
			`(setq ,name val)
		      (let ((ix (position name (get-pydecl :module-global-var-names))))
			(assert ix () "Variable ~A is global (?), but no ~
                                            entry in :module-global-var-names ~A ?!"
				name (get-pydecl :module-global-var-names))
			`(setf (svref +module-global-var-vals+ ,ix) val)))))
		 
		 (subscription-expr
		  (destructuring-bind (primary subs) (cdr tg)
		    ;; TODO: if prim is known/guessed to be dict/list/tuple, inline
		    `(setf (py-subscription ,prim ,subs) val)))
		 
		 (attributeref-expr
		  (destructuring-bind (primary attr) (cdr subscription-expr)
		    `(setf (py-attributeref ,primary ,attr) val))))))))
			
(defmacro classdef-stmt ..)

(defmacro identifier-expr (name &environment e)
  (let* ((in-func              (get-pydecl :inside-function e))
	 (func-vars            (and in-func (get-pydecl :function-variables e)))
	 (func-var-locals      (and func-vars (cdr (assoc :local func-vars))))
	 (func-var-closed-over (and func-vars (cdr (assoc :closed-over func-vars)))))
    
    (cond ((or (member name func-var-locals) (member name func-var-closed-over))
	   name)
	  
	  ((member name func-var-globals)
	   (let ((ix (position name (get-pydecl :module-global-var-names))))
	     (assert ix () "Variable ~A is global (?), but no ~
                            entry in :module-global-var-names ~A ?!"
		     name (get-pydecl :module-global-var-names))
	     `(svref +module-global-var-vals+ ,ix))))))
  

(defmacro augassign-expr (op place val)
  (when (eq (car place) 'tuple)
    (py-raise 'SyntaxError ;; perhaps check in parser
	      "Augmented assignment to multiple places not possible (got: ~A)"
	      `(,place ,op ,val)))

  (assert (member (car place) '(subscription-expr attributeref-expr identifier-expr)))

  `(let* ,(when (member (car place) '(attributeref-expr subscription-expr))
	    '((place-obj-1 ,(second place))
	      (place-obj-2 ,(third place))))
     
     (let* ((ev-val ,val)
	    (place-val-now ,(ecase (car place)
			      (identifier-expr
			       ,place)
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
	       collect `((py-val->lisp-bool ,cond) ,body))
	 ,@(when else-clause
	     `((t ,else-clause)))))

(defmacro for-in-stmt (target source suite else-suite)
  ;; potential special cases:
  ;;  - <dict>.{items,keys,values}()
  ;;  - constant list/tuple/string
  `(tagbody
     (let* ((.take-else. t)
	    (.f. (lambda (.x.)
		  (setf .take-else. nil)
		  (assign-expr .x. (,target))
		  (tagbody 
		    (locally (declare (pydecl (:inside-loop t)))
		      ,suite)
		   :continue))))
       (declare (dynamic-extent .f.))
       (map-over-py-object .f. ,source))
     
     ,(when else-suite `(when .take-else. ,else-suite))
    :break))

(defmacro while-stmt (test suite else-suite)
  `(tagbody
     (loop
	 with .take-else. = t
	 while (py-val->lisp-bool ,test)
	 do (setf .take-else. nil)
	    (tagbody
	      (locally (declare (pydecl (:inside-loop t)))
		,suite)
	     :continue)
	 finally (when .take-else.
		   ,else-suite))
    :break))

(defmacro break-stmt (&environment e)
  (if (get-pydecl :inside-loop e)
      `(go :break)
    (py-raise 'SyntaxError "BREAK was found outside loop")))

(defmacro continue-stmt ()
  (if (get-pydecl :inside-loop e)
      `(go :continue)
    (py-raise 'SyntaxError "CONTINUE was found outside loop")))

(defmacro return-stmt (val)
  (if (get-pydecl :inside-function e)
      `(return-from :function-body ,val)
    (py-raise 'SyntaxError "RETURN found outside function")))

(defmacro yield-stmt ..)
(defmacro try-except-stmt ..)
(defmacro try-finally-stmt ..)
(defmacro print-stmt ..)

(defmacro global-stmt (names)
  ;; GLOBAL statements are already determined and used at the moment a
  ;; FUNCDEF-STMT is handled.
  (unless (get-pydecl :inside-function e)
    (warn "Bogus `global' statement: not inside a function")))

(defmacro del-stmt (item)
  (ecase (car item)
    (tuple `(progn ,@(loop for x in (second item)
			 collect `(del-stmt ,x))))
    (subscription-expr `(py-del-attr ,@(cdr item)))
    (identifier-expr (let* ((name (second item))
			    (bi-name-p (builtin-name-p name))
			    (bi-name-mod-ix (when bi-name
					      (position name
							(get-pydecl :module-global-var-names))))
			    (mod-name-p (member name (get-pydecl :module-global-var-names))))
		       
		       (cond ((and (not (get-pydecl :inside-function))
				   mod-name-p)
			      `(setf (aref +module-global-var-vals+ ,bi-name-mod-ix)
				 ,(if bi-name-p
				      (builtin-name-value name)
				    nil)))
			     
			     (XXX todo))))
    (attributeref-expr `(py-del-attr ,@(cdr item)))))
		    
(defmacro exec-stmt ..)
(defmacro assert-stmt ..)

(defmacro binary-expr (op left right)
  `(let ((.left. ,left)
	 (.right. ,right))
     ,(let ((py-@ (todo op)))
	(if (member op '(+ - * /))
	    `(if (and (numberp .left.)
		      (numberp .right.))
		 (,op .left. .right.)
	       (,py-@ .left. .right.))
	  (,py-@ .left. .right.)))))

       
(defmacro binary-lazy-expr (op left right)
  (ecase op
    (or `(let ((.left. ,left))
	   (if (py-val->lisp-bool .left.)
	       .left
	     (let ((.right. ,right))
	       (if (py-val->lisp-bool .right.)
		   .right.
		 *py-false*)))))
    
    (and `(let ((.left. ,left))
	    (if (py-val->lisp-bool .left.)
		,right
	      .left.)))))
       
(defmacro unary-expr (op item)
  (ecase op
    (+ `(let ((.item. ,item))
	  (if (numberp .item.)
	      .item.
	    (py-+ .item.))))
    (- `(let ((.item. ,item))
	  (if (numberp .item.)
	      (- .item.)
	    (py-- .item.))))
    (not `(todo))))
	   
(defmacro comparison-expr (cmp left right)
  `(let ((.left. ,left)
	 (.right. ,right))
     (if (and (numberp .left.) (numberp .right.))
	 (,cmp .left. .right)
       ,(let ((py-@ (todo cmp)))
	  `(,py-@ .left. .right.)))))
	
(defmacro tuple-expr (items)
  `(make-tuple ,items)) 
    
(defmacro list-expr (items)
  `(make-list ,items))

(defmacro list-compr-expr ..)
(defmacro dict-expr ..)

(defmacro backticks-expr (item)
  (py-repr ,item))
   
(defmacro lambda-expr ..)
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