(in-package :python)

;;; Python compiler

;; Translates a Python module AST into a Lisp function.
;; 
;; Each node in the s-expression returned by the
;; parse-python-{file,string} corresponds to a macro defined below
;; that generates the corresponding Lisp code.
;; 
;; Note that each such AST node has a name ending in "-expr" or
;; "-stmt". There no is separate package for those symbols.
;; 
;; In the macro expansions, lexical variables that keep context state
;; have a name like +NAME+.
;; 
;; The Python compiler uses its own kind of declaration to keep state
;; in generated code; this declaration is named "pydecl".

(sys:define-declaration
    pydecl (&rest property-pairs) nil :declare
    (lambda (declaration env)
      (values :declare
	      (cons 'pydecl
		    (nconc (cdr declaration)
			   (sys:declaration-information 'pydecl env))))))

(defun get-pydecl (var env)
  (let ((res (second (assoc var (sys:declaration-information 'pydecl env) :test #'eq))))
    #+(or)(warn "get-pydecl ~A -> ~A" var res)
    res))

(defmacro with-pydecl (pairs &body body)
    `(locally (declare (pydecl ,@pairs))
       ,@body))

(defmacro with-gensyms (list &body body)
  `(let ,(loop for x in list
	     collect `(,x (make-symbol ,(symbol-name x))))
     ,@body))


;; Compiler debugging and optimization options.

(defvar *comp-opt*
    (copy-tree '((:include-line-numbers t) ;; XXX todo: insert line numbers in AST
		 (:inline-number-math nil)
		 (:inline-fixnum-math nil))))

(defun comp-opt-p (x)
  (assert (member x *comp-opt* :key #'car) () "Unknown Python compiler option: ~S" x)
  (cdr (assoc x *comp-opt*)))

(defun (setf comp-opt) (val x)
  (assert (member x *comp-opt* :key #'car) () "Unknown Python compiler option: ~S" x)
  (setf (cdr (assoc x *comp-opt*)) val))

;; The AST node macros

(defmacro assert-stmt (&rest args)
  (declare (ignore args))
  (break "todo: assert-stmt"))

(defmacro assign-stmt (value targets &environment e)
  (let ((context (get-pydecl :context e)))
    (with-gensyms (val)
      
      (flet ((assign-one (tg)
	       (ecase (car tg)
		 
		 ((attributeref-expr subscription-expr)  `(setf ,tg ,val))
		 
		 (identifier-expr
		  (let ((name (second tg)))
		    
		    (flet ((module-set ()
			     (let ((ix (position name (get-pydecl :mod-globals-names e))))
			       (if ix
				   `(setf (svref +mod-globals-values+ ,ix) ,val)
				 `(setf (gethash ',name +mod-dyn-globals+) ,val))))
			   
			   (local-set ()
			     `(setf ,name ,value))
			   
			   (class-set ()
			       `(setf (gethash ',name +cls-namespace+) ,val)))
		    
		      (ecase context
			
			(:module    (if (member name (get-pydecl :lexically-visible-vars e))
					(local-set)               ;; in exec-stmt
				      (module-set)))
			
			(:function  (if (or (member name (get-pydecl :funcdef-scope-globals e))
					    (not (member name (get-pydecl :lexically-visible-vars e))))
					(module-set)
				      (local-set)))
			
			(:class     (if (member name (get-pydecl :classdef-scope-globals e))
					(module-set)
				      (class-set))))))))))
	
	`(let ((,val ,value))
	   ,@(mapcar #'assign-one targets))))))

(defmacro attributeref-expr (item attr)
  `(py-attr ,item ,attr))

(defmacro augassign-stmt (op place val)
  (multiple-value-bind (py-@= py-@) (lookup-inplace-op-func op)
    (ecase (car place)
    
      (tuple-expr (py-raise 'SyntaxError
			    "Augmented assignment to multiple places not possible (got: ~S)"
			    `(,place ,op ,val)))
    
      ((attributeref-expr subscription-expr)
     
       `(let* ((primary       ,(second place))
	       (sub           ,(third place))
	       (ev-val        ,val)
	       (place-val-now (,(car place) place-obj-1 place-obj-2)))
	
	  (or (funcall ,py-@= place-val-now ev-val) ;; returns true iff __i@@@__ found
	      (let ((new-val (funcall ,py-@ place-val-now ev-val)))
		(assign-stmt new-val (,(car place) primary sub))))))

      (identifier-expr
     
       `(let* ((ev-val ,val)
	       (place-val-now ,place))
	
	  (or (funcall ,py-@= place-val-now ev-val) ;; returns true iff __i@@@__ found
	      (let ((new-val (funcall ,py-@ place-val-now ev-val)))
		(assign-stmt new-val (,place)))))))))

(defmacro backticks-expr (item)
  `(py-repr ,item))

(defun get-op-func-todo (op)
  (declare (ignore op)) ;; XXX
  :get-op-func-todo)

(defmacro binary-expr (op left right)
  (let ((py-@ (get-binary-op-func op)))
    `(funcall ,py-@ ,left ,right)))

(defmacro binary-lazy-expr (op left right)
  (ecase op
    (or `(let ((left ,left))
	   (if (py-val->lisp-bool left)
	       left
	     (let ((right ,right))
	       (if (py-val->lisp-bool right)
		   right
		 *the-false*)))))
    
    (and `(let ((left ,left))
	    (if (py-val->lisp-bool left)
		,right
	      left)))))

(defmacro break-stmt (&environment e)
  (if (get-pydecl :inside-loop e)
      `(go :break)
    (py-raise 'SyntaxError "BREAK was found outside loop")))


(defmacro call-expr (primary (&whole all-args pos-args kwd-args *-arg **-arg)
		     &environment e)
  
  ;; For complete Python semantics, we check for every call if the
  ;; function being called is one of the built-in functions EVAL,
  ;; LOCALS or GLOBALS, because they access the variable scope of the
  ;; _caller_ (which is ugly).
  ;; 
  ;; At the module level, globals() and locals() are equivalent.
  
  (let ((context (get-pydecl :context e)))
    
    `(let ((prim ,primary))
     
       (cond ((eq prim :the-locals-function-TODO)
	      (if (and ,(not (or pos-args kwd-args))
		       ,(or (null *-arg)  `(null (py-iterate->lisp-list ,*-arg)))
		       ,(or (null **-arg) `(null (py-mapping->lisp-list ,**-arg))))
		  ,(if (eq context :module)
		       `(.globals.)
		     `(.locals.))
		(py-raise 'TypeError
			  "locals() must be called without args (got: ~A)" ',all-args)))

	     ((eq prim :the-globals-function-TODO)
	      (if (and ,(not (or pos-args kwd-args))
		       ,(or (null *-arg)  `(null (py-iterate->lisp-list ,*-arg)))
		       ,(or (null **-arg) `(null (py-mapping->lisp-list ,**-arg))))
		  (.globals.)
		(py-raise 'TypeError
			  "globals() must be called without args (got: ~A)" ',all-args)))

	     ((eq prim :the-eval-function-TODO)
	      (let ((args (nconc (list ,@pos-args)
				 ,(when *-arg `(py-iterate->lisp-list ,*-arg)))))
		(if (and ,(null kwd-args)
			 ,(or (null **-arg)
			      `(null (py-mapping->lisp-list ,**-arg)))
			 (<= 1 (length args) 3))
		  
		    (py-eval-todo ,(first pos-args)
				  (or ,(second pos-args) (.globals.))
				  (or ,(third  pos-args) ,(if (eq context :module)
							      `(.globals.)
							    `(.locals.))))
		
		  (py-raise 'TypeError
			    "eval() must be called with 1 to 3 pos args (got: ~A)" ',all-args))))
	   
	     (t (py-call prim
			 ,@pos-args 
			 ,@(loop for ((i-e key) val) in kwd-args
			       do (assert (eq i-e 'identifier-expr))
				  ;; XXX tuples?
				
			       collect (intern (symbol-name key) :keyword)
			       collect val)
		       
			 ;; Because the * and ** arg may contain a huge
			 ;; number of items, larger then max num of
			 ;; lambda args, supply them using special :*
			 ;; and :** keywords.
		       
			 ,@(when *-arg  `((:* ,*-arg)))
			 ,@(when **-arg `((:** ,**-arg)))))))))

(defmacro classdef-stmt (name inheritance suite)
  ;; todo: define .locals. containing class vars
  
  `(let ((+cls-namespace+ (make-hash-table :test #'eq)))
     (with-pydecl ((:context :class)
		   (:classdef-scope-globals ',(classdef-globals suite)))
       ,suite)
     (make-py-class :name ',(second name)
		    :namespace +cls-namespace+
		    :supers ,inheritance)))

(defmacro comparison-expr (cmp left right)
  `(let ((left ,left)
	 (right ,right))
     (if (and (numberp left) (numberp right))
	 (,cmp left right)
       ,(let ((py-@ :todo-get-cmp-func))
	  `(,py-@ left right)))))

(defmacro continue-stmt (&environment e)
  (if (get-pydecl :inside-loop e)
      `(go :continue)
    (py-raise 'SyntaxError "CONTINUE was found outside loop")))

(defmacro del-stmt (item &environment e)
  (ecase (car item)
    
    (tuple
     `(progn ,@(loop for x in (second item) collect `(del-stmt ,x))))
    
    (subscription-expr
     `(py-del-subs ,@(cdr item))) ;; XXX maybe inline dict case
    
    (identifier-expr
     (let* ((name (second item))
	    (context (get-pydecl :context e)))
       
       (flet ((module-del ()
		;; reset module-level vars with built-in names to their built-in value
		(let ((ix (position name (get-pydecl :mod-globals-names e))))
		  (if ix
		      `(let ((old-val (svref +mod-globals-values+ ,ix)))
			 (if (eq old-val :unbound)
			     (py-raise 'NameError "Cannot delete variable ~A: ~
                                                   it is unbound [static global]" ',name)
			   (setf (svref +mod-globals-values+ ,ix) 
			     ,(if (builtin-name-p name)
				  (builtin-name-value name)
				:unbound))))
		    `(or (remhash ,name +mod-dyn-globals+)
			 (py-raise 'NameError "Cannot delete variable ~A: ~
                                               it is unbound [dyn global]" ',name)))))
	      
	      (local-del () `(if (eq ,name :unbound)
				 (py-raise 'NameError "Cannot delete variable ~A: ~
                                                       it is unbound [local]" ',name)
			       (setf ,name :unbound)))
	      
	      (class-del () `(or (remhash ,name +cls-namespace+)
				 (py-raise 'NameError "Cannot delete variable ~A: ~
                                                       it is unbound [dyn class]" ',name))))
	 
	 (ecase context
	 
	   (:module   (if (member name (get-pydecl :lexically-visible-vars e))
			  (local-del)
			(module-del)))
	   
	   (:function (if (or (member name (get-pydecl :func-globals e))
			      (not (member name (get-pydecl :lexically-visible-vars e))))
			  (module-del)
			(local-del)))
	   
	   (:class    (if (member name (get-pydecl :class-globals e))
			  (module-del)
			(class-del)))))))
    
    (attributeref-expr `(py-del-attr ,@(cdr item)))))

(defmacro dict-expr (alist)
  `(make-dict ,alist))


(defmacro exec-stmt (code globals locals &environment e)
  ;; XXX TODO: also allow code object etc as CODE
  
  ;; An EXEC-STMT is translated into a function, that is compiled and
  ;; then funcalled. Compiling is needed because otherwise the
  ;; environment with pydecl it not passed properly.
  ;; 
  ;; XXX todo: understand how exactly environments work
  ;; w.r.t. compilation.
  
  (let ((context (get-pydecl :context e)))
    
    (with-gensyms (exec-helper-func)
      
      `(let ((ast (parse-python-string ,code))
	     (locals-ht  (convert-to-hash-table ,(or locals (if (eq context :module)
								`(.globals.)
							      `(.locals.)))))
	     (globals-ht (convert-to-hash-table ,(or globals `(.globals.)))))
	 
	 (let ((,exec-helper-func
		(compile nil `(lambda ()
				(with-module-context (#() #() ,globals-ht)
				  
				  ;; Every key-val pair in the locals dict is translated into
				  ;; an assignment expression.
				  
				  (progn ,@(loop for k being the hash-key in locals-ht
					       using (hash-value val)
					       for k-sym = (typecase k
							     (string (intern k #.*package*))
							     (symbol k)
							     (t (error "EXEC-STMT: the dict with locals ~
                                                                 must have keys or symbols as key ~
                                                                 (got: ~A, as ~A)" k (type-of k))))
					       collect `(assign-stmt ,val ((identifier-expr ,k-sym)))))
				  
				  ,ast)))))
	   
	   (funcall ,exec-helper-func))))))
	 

(defmacro for-in-stmt (target source suite else-suite)
  ;; potential special cases:
  ;;  - <dict>.{items,keys,values}()
  ;;  - constant list/tuple/string
  (with-gensyms (f x)
    `(tagbody
       (let* ((,f (lambda (,x)
		    (assign-stmt ,x (,target))
		    (tagbody 
		      (with-pydecl ((:inside-loop t))
			,suite)
		     :continue))))
	 (declare (dynamic-extent ,f))
	 (map-over-py-object ,f ,source))
       ,@(when else-suite `(,else-suite))
      :break)))


(defmacro funcdef-stmt (decorators
			name (&whole formal-args pos-args key-args *-arg **-arg)
			suite)
  
  (assert (or (eq name :lambda) 
	      (and (listp name) (eq (car name) 'identifier-expr))))
  
  ;; Replace "def f( (x,y), z):  .." by "def f( _tmp , z):  (x,y) = _tmp; ..".
  ;; Shadows original POS-ARGS.
  
  (let ((all-pos-arg-names (loop with todo = pos-args and res = ()
			       while todo
			       do (let ((x (pop todo)))
				    (etypecase x
				      (symbol (push x res))
				      (list   (setf todo (nconc todo x)))))
			       finally (return res))))
	
    (multiple-value-bind (formal-pos-args pos-arg-destruct-form)
	(loop with params = () and destructs = ()
	    for pa in pos-args
	    do (ecase (car pa)
		 (tuple-expr (let ((tmp-var `(identifier
					      ,(make-symbol (format nil "~A" (cdr pa))))))
			       (push tmp-var params)
			       (push `(assign-stmt ,tmp-var (,pa)) destructs)))
		 (identifier-expr (push pa params)))
	    finally (return (values (nreverse params)
				    (when destructs
				      `(progn ,@(nreverse destructs))))))
      
      (multiple-value-bind (func-explicit-globals func-locals)
	  (funcdef-globals-and-locals (cons all-pos-arg-names (cdr formal-args)) suite)
    
	;; When a method is defined in a class namespace, the default
	;; argument values are evaluated at function definition time
	;; in the class namespace. Macro PY-ARG-FUNCTION ensures this.
	
	`(let ((func-lambda
		(py-arg-function ,(second name)
		 (,formal-pos-args ,key-args ,*-arg ,**-arg)
		 
		 (let ,(loop for loc in func-locals collect `(,loc :unbound))
		   
		   ,@(when pos-arg-destruct-form
		       `(,pos-arg-destruct-form))
		   
		   (block :function-body
		     
		     (flet ((.locals. ()
			      ,(when func-locals
				 `(make-py-dict
				   (delete-if (lambda (x) (eq (cdr x) :unbound))
					      (mapcar #'cons ',func-locals
						      (list ,@func-locals)))))))
			    
		       (with-pydecl ((:funcdef-scope-globals ',func-explicit-globals)
				     (:context :function)
				     (:inside-function t)
				     (:lexically-visible-vars
				      (nconc (list ,@func-locals)
					     (get-pydecl :lexically-visible-vars e))))
			 ,suite)))))))
	   
	   ,(if (eq name :lambda)
		
		`func-lambda
	      
	      (with-gensyms (func)
		`(let ((,func (make-py-function ',(second name) func-lambda)))
		   
		   ,(let ((art-deco (loop with res = func
					for deco in (nreverse decorators)
					do (setf res `(call-expr ,deco ((,res) () nil nil)))
					finally (return res))))
		      
		      `(assign-stmt ,art-deco (,name)))))))))))


(defmacro generator-expr (item for-in/if-clauses)

  ;; XXX should this take place before these AST macros run? it
  ;; introduces a new funcdef-stmt...

  (multiple-value-bind (gen-maker-lambda-one-arg initial-source)
      (rewrite-generator-expr-ast `(generator-expr ,item ,for-in/if-clauses))
    `(funcall ,gen-maker-lambda-one-arg ,initial-source)))
       
(defmacro global-stmt (names &environment e)
  ;; GLOBAL statements are already determined and used at the moment a
  ;; FUNCDEF-STMT is handled.
  ;; XXX global in class def scope?
  (declare (ignore names))
  (unless (get-pydecl :inside-function e)
    (warn "Bogus `global' statement found at top-level (not inside a function)")))

(defmacro identifier-expr (name &environment e)
  
  ;; The identifier is used for its value; it is not an assignent
  ;; target (as the latter case is handled by ASSIGN-STMT).
  (assert (symbolp name))
  
  (flet ((module-lookup ()
	   (let ((ix (position name (get-pydecl :mod-globals-names e))))
	     (if ix
		 `(let ((val (svref +mod-globals-values+ ,ix)))
		    (when (eq val :unbound) (py-raise 'NameError "Variable ~A is unbound [glob]" ',name))
		    val)
	       `(or (gethash ',name +mod-dyn-globals+)
		    (py-raise 'NameError "No variable with name ~A [dyn-glob]" ',name))))))
    
    (ecase (get-pydecl :context e)
       
      (:module   (if (member name (get-pydecl :lexically-visible-vars e)) ;; in exec-stmt
		     name
		   (module-lookup)))
	       
      (:function (if (member name (get-pydecl :lexically-visible-vars e))
		     name
		   (module-lookup)))
	       
      (:class    `(or (gethash ',name +cls-namespace+)
		      ,(if (member name (get-pydecl :lexically-visible-vars e))
			   name
			 (module-lookup)))))))

(defmacro if-stmt (if-clauses else-clause)
  `(cond ,@(loop for (cond body) in if-clauses
	       collect `((py-val->lisp-bool ,cond) ,body))
	 ,@(when else-clause
	     `((t ,else-clause)))))

(defmacro import-stmt (&rest args)
  (declare (ignore args))
  (break "todo: import-stmt"))

(defmacro import-from-stmt (&rest args)
  (declare (ignore args))
  (break "todo: import-from-stmt"))

(defmacro lambda-expr (args expr)
  ;; XXX maybe the resulting LAMBDA results in more code than
  ;; necessary for the just one expression it contains.
  
  `(funcdef-stmt nil :lambda ,args (suite-stmt (,expr))))
  

(defmacro listcompr-expr (item for-in/if-clauses)
  (with-gensyms (vec)
    `(let ((,vec (make-array 0 :adjustable t :fill-pointer 0)))
       ,(loop
	    with res = `(vector-push-extend ,item ,vec)
	    for clause in (reverse for-in/if-clauses)
	    do (setf res (ecase (car clause)
			   (for-in `(for-in-stmt ,(second clause) ,(third clause) ,res nil))
			   (if     `(if-stmt (,(second clause) ,res) nil))))
	  finally (return res))
     (make-py-list ,vec))))

(defmacro list-expr (items)
  `(make-py-list-unevaled-list ,items))

;; Used by expansions of `module-stmt', `exec-stmt', and by function `eval'.

(defmacro with-this-module-context ((module) &body body)
  (check-type module py-module)
  (with-slots (globals-names globals-values dyn-globals) module
    
    `(with-module-context (,globals-names ,globals-values ,dyn-globals)
       ,@body)))

(defmacro with-module-context ((glob-names glob-values dyn-glob
				&key locals-ht set-builtins create-return-mod)
			       &body body)
  (check-type glob-names vector)
  (check-type dyn-glob hash-table)
  (when locals-ht (check-type locals-ht hash-table))
  
  (with-gensyms (mod)
    
    `(let* ((+mod-globals-names+  ,glob-names)
	    (+mod-globals-values+ ,glob-values)
	    (+mod-dyn-globals+    ,dyn-glob)
	    ,@(when create-return-mod
		`((,mod (make-module :globals-names  ,glob-names
				     :globals-values ,glob-values
				     :dyn-globals    ,dyn-glob)))))
       
       ,@(when set-builtins
	   (loop for name across glob-names and i from 0
	       when (builtin-name-p name)
	       collect `(setf (svref +mod-globals-values+ ,i)
			  ,(builtin-name-value name)) into setfs
	       finally (when setfs
			 `((progn ,@setfs)))))

       (flet ((.globals. () (module-make-globals-dict
			       ;; Updating this dict really modifies the globals.
			       +mod-globals-names+ +mod-globals-values+ +mod-dyn-globals+)))
	 
	 #'.globals. ;; remove 'unused' warning
	 
	 (with-pydecl
	     ((:mod-globals-names      ,glob-names)
	      (:context                :module)
	      (:mod-futures            :todo-parse-module-ast-future-imports))
	   
	   (let ,(when locals-ht
		   (loop for name being the hash-key in locals-ht
		       using (hash-value val)
		       collect `(,name ,val)))
	     ,@body)))
       
       ,@(when create-return-mod
	   `(,mod)))))

(defmacro module-stmt (suite) ;; &environment e)
  
  ;; A module is translated into a lambda that creates and returns a
  ;; module object. Executing the lambda will create a module object
  ;; and register it, after which other modules can access it.
  ;; 
  ;; Functions, classes and variables inside the module are available
  ;; as attributes of the module object.
  ;; 
  ;; If we are inside an EXEC-STMT, PYDECL assumptions
  ;; :exec-mod-locals-ht and :exec-mod-globals-ht are assumed declared
  ;; (hash-tables containing local and global scope).
  
  (let* ((ast-globals (module-stmt-globals suite)))
    
    `(with-module-context (,(make-array (length ast-globals) :initial-contents ast-globals)
			   (make-array ,(length ast-globals) :initial-element :unbound) ;; not eval now
			   ,(make-hash-table :test #'eq)
			   :locals-ht nil
			   :set-builtins t
			   :create-return-mod t)
       ,suite)))

#+(or) ;; old
(defmacro module-stmt (suite &environment e)
  
  ;; A module is translated into a lambda that creates and returns a
  ;; module object. Executing the lambda will create a module object
  ;; and register it, after which other modules can access it.
  ;; 
  ;; Functions, classes and variables inside the module are available
  ;; as attributes of the module object.
  ;; 
  ;; If we are inside an EXEC-STMT, PYDECL assumptions
  ;; :exec-mod-locals-ht and :exec-mod-globals-ht are assumed declared
  ;; (hash-tables containing local and global scope).
  
  (let* ((ast-globals  (module-stmt-globals suite))
	 (exec-globals (get-pydecl :exec-mod-globals-ht e))
	 (exec-locals  (get-pydecl :exec-mod-locals-ht  e))
	 (gv           (union ast-globals exec-globals)))
    
    (with-gensyms (mod)
      
      ;;`(excl:named-function module-stmt-lambda
      ;;   (lambda ()
      `(let* ((+mod-globals-names+    (make-array ,(length gv) :initial-contents ',gv))
	      (+mod-globals-values+   (make-array ,(length gv) :initial-element :unbound))
	      (+mod-dyn-globals+      nil) ;; hash-table (symbol -> val) of dynamically added vars 
	      (,mod                   (make-module :globals-names  +mod-globals-names+
						   :globals-values +mod-globals-values+
						   :dyn-globals    +mod-dyn-globals+)))
       
	 ;; Set values of module-level variables with names
	 ;; corresponding to built-in names (like `len')
       
	 ,@(when gv
	     `((progn ,@(loop for glob-var-name in gv and i from 0
			    for val = (cond (exec-globals
					     (gethash glob-var-name exec-globals))
					    ((builtin-name-p glob-var-name)
					     (builtin-name-value glob-var-name))
					    (t
					     :unbound))
			    unless (eq val :unbound)
			    collect `(setf (svref +mod-globals-values+ ,i) ,val)))))

	 ;; Same context as +...+ vars above
	 (with-pydecl
	     ((:mod-globals-names    ,(make-array (length gv) :initial-contents gv))
	      (:lexically-visible-vars ,(when exec-locals
					  (loop for k being the hash-key in exec-locals
					      collect k)))
	      (:context :module)
	      (:mod-futures :todo-parse-module-ast-future-imports))
	   
	   (flet ((.globals. () (module-stmt-make-globals-dict
				 ;; Updating this dict really modifies the globals.
				 +mod-globals-names+ +mod-globals-values+ +mod-dyn-globals+)))
	  
	     #+(or) ;; allegro does not handle these yet
	     (declare (ignorable (function .globals.)))
	  
	     ,(if exec-locals
	       
		  `(let ,(loop for k being the hash-key in exec-locals using (hash-value v)
			     collect `(,k ,v))
		     ,suite)
	     
		suite)))
	   
	 ;; XXX if executing module failed, where to catch error?
	 ,mod))))

(defmacro pass-stmt ()
  nil)

(defmacro print-stmt (dest items comma?)
  ;; XXX todo
  (declare (ignore dest))
  `(format t "~{~A~^ ~}~@[~%~]" (list ,@items) ,(not comma?)))

(defmacro return-stmt (val &environment e)
  (if (get-pydecl :inside-function e)
      `(return-from :function-body ,val)
    (py-raise 'SyntaxError "RETURN found outside function")))

(defmacro slice-expr (start stop step)
  (declare (special *None*))
  `(make-slice ,(or start *None*) ,(or stop *None*) ,(or step *None*)))

(defmacro subscription-expr (item subs)
  `(py-subs ,item ,subs))

(defmacro suite-stmt (stmts)
  (if (null (cdr stmts))
      (car stmts)
    `(progn ,@stmts)))

(defmacro raise-stmt (exc var tb)
  `(py-raise ,exc ,var ,tb))


(defmacro try-except-stmt (suite except-clauses else-suite)
  
  ;; The Exception class in a clause is evaluated only after an
  ;; exception is thrown. Can't use handler-case for that reason.
  
  (flet ((handler->cond-clause (except-clause)
	   (destructuring-bind (exc var handler-suite) except-clause
	     (cond ((null exc)
		    `(t (progn ,handler-suite
			       (return-from :try-except-stmt nil))))
		   
		   ((eq (car exc) 'tuple-expr)
		    `((some ,@(loop for cls in (second exc)
				  collect `(typep exc ,cls)))
		      (progn ,@(when var `((assign-stmt exc (,var))))
			     ,handler-suite
			     (return-from :try-except-stmt nil))))
				
		   (t
		    `((typep exc ,exc)
		      (progn ,@(when var `((assign-stmt exc (,var))))
			     ,handler-suite
			     (return-from :try-except-stmt nil))))))))
    
    (let ((handler-form `(lambda (exc) 
			   (cond ,@(mapcar #'handler->cond-clause except-clauses)))))
      
      `(block :try-except-stmt
	 (tagbody
	   (handler-bind ((Exception ,handler-form))
	     (progn ,suite
		    ,@(when else-suite `((go :else)))))
	   
	   ,@(when else-suite
	       `(:else ,else-suite)))))))


(defmacro try-finally-stmt (try-suite finally-suite)
  `(unwind-protect 
     
       (multiple-value-bind (val exc) (ignore-errors ,try-suite)
	 (when (and (null val)
		    (typep exc 'condition)
		    (not (typep exc 'Exception)))
	   (break "Try/finally: in the TRY block Lisp condition ~S occured" exc)))
     
     ,finally-suite))


(defmacro tuple-expr (items)
  `(make-tuple-unevaled-list ,items))

(defmacro unary-expr (op item)
  (let ((py-op-func (get-unary-op-func op)))
    (assert py-op-func)
    `(,py-op-func ,item)))

(defmacro while-stmt (test suite else-suite)
  (with-gensyms (take-else)
    `(tagbody
       (loop
	   with ,take-else = t
	   while (py-val->lisp-bool ,test)
	   do (when ,take-else
		(setf ,take-else nil))
	      (tagbody
		(with-pydecl ((:inside-loop t))
		  ,suite)
	       :continue)
	   finally (when (and ,take-else ,(and else-suite t))
		     ,else-suite))
      :break)))

(defmacro yield-stmt (val)
  (declare (ignore val))
  (break "YIELD found by compiler; generator AST should be rewritten by now."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Helper functions for the compiler, that preprocess or analyze AST.

#+(or)
(defun rewrite-name-binding-statements (ast)
  "Rewrite FUNCDEF-STMT and CLASSDEF-STMT so they become (ASSIGN-STMT ...).
Returns the new AST."
  (walk-py-ast
   ast
   (lambda (x &key target value)
     (ecase (car x)
       
       (funcdef-stmt (destructuring-bind
			 (decorators fname args suite) (cdr form)
		       (declare (ignore args suite))
		       
		       `(assign-stmt (loop with res = `(funcdef-expr ,@(cdr form))
					 for deco in (nreverse decorators)
					 do (setf res `(call-expr ,deco ((,res) () nil nil)))
					 finally (return res))
				     ((identifier-expr ,fname)))))
       
       (classdef-stmt (destructuring-bind
			  (cname inheritance suite) (cdr form)
			xxxx))))
   :walk-lists-only t))


(defun ast-vars (ast &key params locals declared-globals outer-scope)
  ;; Returns: LOCALS, DECLARED-GLOBALS, OUTER-SCOPE
  ;; which are lists of symbols denoting variable names.
  
  (labels ((recurse (ast)
	     (with-py-ast-nodes ((form &key value target) t ast)
	       (case (car form)
		 
		 (classdef-stmt (destructuring-bind
				    ((identifier cname) inheritance suite) (cdr form)
				  (declare (ignore suite))

				  (assert (eq identifier 'identifier-expr))

				  ;; The class name is always a local.
				  (when (member cname declared-globals)
				    (error "SyntaxError: class name ~A may not be ~
                                            declared `global'" cname))
				  
				  (pushnew cname locals)
				  
				  (loop for x in inheritance do (recurse x)))
				(values nil t))
		 
		 (funcdef-stmt  (destructuring-bind (decorators fname args suite) (cdr form)
				  (declare (ignore suite))
				  (assert (eq (car fname) 'identifier-expr))
				  
				  (when (member fname declared-globals)
				    (error "SyntaxError: inner function name ~A declared global"
					   fname))
				  
				  (pushnew (second fname) locals)
				
				  (loop for deco in decorators do (recurse deco))
				  (loop for (nil def-val) in (second args)
				      do (recurse def-val)))
				(values nil t))
		 
		 (global-stmt (dolist (name (second form))
				(cond ((member name params)
				       (error "SyntaxError: function param ~A declared `global'"
					      name))
				      
				      ((or (member name locals) (member name outer-scope))
				       (error "SyntaxError: variable ~A used before being ~
                                          declared `global'" name))
				      
				      (t (pushnew name declared-globals))))
			      (values nil t))
	     
		 
	       
		 (lambda-expr (values nil t)) ;; skip
		 
		 (identifier-expr (let ((name (second form)))
				    
				    (when value
				      (unless (or (member name params)
						  (member name locals)
						  (member name declared-globals))
					(pushnew name outer-scope)))
				    
				    (when target
				      (when (member name outer-scope)
					(error "SyntaxError: local variable ~A referenced before ~
                                                assignment" name))
				      (unless (or (member name params)
						  (member name locals)
						  (member name declared-globals))
					(pushnew name locals)))
				    (values nil t)))
		 
		 (t form)))))
    
    (recurse ast)
    (values locals declared-globals outer-scope)))


  
(defun funcdef-globals-and-locals (args suite)
  "Given FUNCDEF ARGS and SUITE (or EXPR), return DECLARED-GLOBALS, LOCALS."
  
  (destructuring-bind (linearized-pos-args kwd-args *-arg **-arg) args
    
    (assert (not (some #'listp linearized-pos-args)))
    
    (let ((params (let ((x (append linearized-pos-args (mapcar #'first kwd-args))))
		    (when *-arg  (push *-arg  x))
		    (when **-arg (push **-arg x))
		    x)))
      
      (multiple-value-bind
	  (locals declared-globals outer-scopes) (ast-vars suite :params params)
	(declare (ignore outer-scopes))
	(values declared-globals locals)))))

(defun classdef-globals (suite)
  (multiple-value-bind
      (locals declared-globals outer-scope) (ast-vars suite)
    (declare (ignore locals outer-scope))
    declared-globals))
  

(defun module-stmt-globals (suite)
  (multiple-value-bind
      (locals declared-globals outer-scope) (ast-vars suite)
    #+(or)(warn "module vars: ~A" `(:l ,locals :dg ,declared-globals :os ,outer-scope))
    (union (union locals declared-globals) outer-scope))) ;; !?

(defun module-make-globals-dict (names-vec values-vec dyn-globals-ht)
  (make-py-dict ;; todo: proxy
   (nconc (loop for name across names-vec and val across values-vec
	      unless (eq val :unbound) collect (cons name val))
	  (loop for k being the hash-key in dyn-globals-ht using (hash-value v)
	      collect (cons k v)))))

(defun make-py-dict (list)
  (loop with ht = (make-hash-table :test #'eq)
      for (k . v) in list do (setf (gethash k ht) v)
      finally (return ht)))

(defgeneric convert-to-hash-table (x)
  (:method ((x list))       (make-py-dict x))
  (:method ((x hash-table)) x))

(defmacro py-arg-function (name (pos-args key-args *-arg **-arg) &body body)
  ;; Non-consing argument parsing! (except when *-arg or **-arg present)
  ;; 
  ;; XXX todo: the generated code can be cleaned up a bit when there
  ;; are no arguments (currently zero-length vectors are created).
  
  (assert (symbolp name))
  
  (let* ((num-pos-args (length pos-args))
	 (num-key-args (length key-args))
	 (num-pos-key-args  (+ num-pos-args num-key-args))
	 
	 
	 (pos-key-arg-names (append pos-args (mapcar #'first key-args)))
	 (key-arg-default-asts (mapcar #'second key-args))
	 (arg-name-vec (when (> num-pos-key-args 0)
			 (make-array num-pos-key-args :initial-contents pos-key-arg-names)))
	 
	 (arg-kwname-vec (when (> num-pos-key-args 0)
			   (make-array
			    num-pos-key-args
			    :initial-contents (loop for x across arg-name-vec
						  collect (intern x #.(find-package :keyword))))))
	 (key-arg-defaults (unless (= num-key-args 0)
			     (make-array num-key-args))))
    
    `(locally (declare (optimize (speed 3) (safety 1) (debug 0)))
       (let (,@(when key-args
		 `((key-arg-default-values ,key-arg-defaults))))
	 
	 ;; Evaluate default argument values outside the lambda, at
	 ;; function definition time.
	 ,@(when key-args
	     `((progn ,@(loop for i from 0 below num-key-args
			    collect `(setf (svref key-arg-default-values ,i)
				       ,(nth i key-arg-default-asts))))))

	 (excl:named-function ,name
	   (lambda (&rest %args)
	     (declare (dynamic-extent %args))
	   
	     ;; args = (pos_1 pos_2 ... pos_p ; key_1 val_1 ... key_k val_k)
	     ;; where key_i is a (regular, not :keyword) symbol
	   
	     ;; As the first step, the pos_i args are assigned to pos-args,
	     ;; and if there are more pos_i args then pos-args, then the
	     ;; remaining ones are assigned to the key-args.
	   
	     (let* (,@(when (> num-pos-key-args 0) `((arg-val-vec (make-array ,num-pos-key-args))))
		    ,@(when (> num-pos-key-args 0) `((num-filled-by-pos-args 0)))
		    ,@(when **-arg `((for-** ())))
		    ,@(when *-arg `((for-* ()))))
	     	     
	       ,@(when (> num-pos-key-args 0)
		   `((declare (dynamic-extent arg-val-vec)
			      (type (integer 0 ,num-pos-key-args) num-filled-by-pos-args))))
	     
	       ;; Spread supplied positional args over pos-args and *-arg
	     
	       ,@(when (> num-pos-key-args 0)
		   `((loop 
			 until (or (= num-filled-by-pos-args ,(if *-arg num-pos-args num-pos-key-args))
				   (symbolp (car %args))) ;; the empty list NIL is a symbol, too
			 do (setf (svref arg-val-vec num-filled-by-pos-args) (pop %args))
			    (incf num-filled-by-pos-args))))

	       ;; Collect remaining pos-arg in *-arg, if present 
	     
	       (unless (symbolp (car %args))
		 ,(if *-arg
		      `(loop until (symbolp (car %args))
			   do (push (pop %args) for-*))
		    `(break "Too many pos args")))
	     
	       (assert (symbolp (car %args)))
	     
	       ;; All remaining arguments are keyword arguments;
	       ;; they have to be matched to the remaining pos and
	       ;; key args by name.
	       ;; 
	     
	       (when %args
		 (loop
		     for key = (pop %args)
		     for val = (pop %args)
		     while key
		     do
		       (when (member key '(:* :**))
			 (break "* and ** args in a call not handled yet, sorry"))
		     
		       ,(cond ((> num-pos-key-args 0)
			       `(loop for i fixnum from num-filled-by-pos-args below ,num-pos-key-args
				    when (or (eq (svref ,arg-name-vec i) key)
					     (eq (svref ,arg-kwname-vec i) key))
				    do (setf (svref arg-val-vec i) val)
				       (return)
				    finally 
				      ,(if **-arg
					   `(push (cons key val) for-**)
					 `(break "Got unknown keyword arg and no **-arg: ~A ~A"
						 key val))))
			      (**-arg
			       ;; this reconsing is necessary as **-arg might not have dynamic extent
			       `(push (cons key val) for-**))
			    
			      (t `(break "Got unknown keyword arg and no **-arg: ~A ~A"
					 key val)))))
	     
	       ;; Ensure all positional arguments covered
	       ,@(when (> num-pos-args 0)
		   `((loop for i fixnum from num-filled-by-pos-args below ,num-pos-args
			 unless (svref arg-val-vec i)
			 do (break "Positional arg ~A has no value" (svref ,arg-name-vec i)))))
	     
	       ;; Use default values for missing keyword arguments (if any)
	       ,@(when key-args
		   `((loop for i fixnum from ,num-pos-args below ,num-pos-key-args
			 unless (svref arg-val-vec i)
			 do (setf (svref arg-val-vec i)
			      (svref key-arg-default-values (- i ,num-pos-args))))))
	     
	       ;; Initialize local variables
	       (let (,@(loop for p in pos-key-arg-names and i from 0
			   collect `(,p (svref arg-val-vec ,i)))  ;; XXX p = (identifier ..) ?
		     ,@(when  *-arg `((,*-arg (nreverse for-*))))
		     ,@(when **-arg `((,**-arg for-**))))
	       
		 ,@body))))))))

#+(or)
(defun bar ()
  (let ((f (py-arg-function foo ((a b c) ((d 42) (e 100)) nil nil)
				 (setf e (+ a b c))
				 (setf b (+ d e)))))
    (disassemble f)))

#+(or)
(defun foo ()
  (declare (optimize (speed 3)(safety 0) (debug 0)))
  (let ((f (py-arg-function foo ((a b c) ((d 42) (e 100)) arg kw)
			    (values d e arg kw))))
    ;;    (dotimes (i 1000000)
    (funcall f 1 2 3 4 5 6 'q 42 'd 1)))

#+(or)
(defun foo ()
  #+(or)(declare (optimize (speed 3)(safety 0) (debug 0)))
  (let ((f (py-arg-function foo
			    ((a b c) ((d 42) (e 100)) nil nil)
			    (setf e (+ a b c))
			    (setf b (+ d e)))))
    (values
     (funcall f 1 2 3 'e 10)
     (funcall f 1 2 3 4 5)
     (funcall f 1 2 3 'd 23))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pythonic (copied)

(defun py-raise (exc-type string &rest format-args)
  "Raise a Python exception with given format string"
  (if (find-class exc-type)
      (error exc-type :args (apply #'format nil string format-args))
    (apply #'error string format-args)))


(defclass SyntaxError (condition)
  ((args :initarg :args :documentation "Exception arguments (as tuple)")))

(defmethod print-object ((x SyntaxError) stream)
  (format stream "~A" (class-name (class-of x)))
  (when (slot-boundp x 'args)
    (format stream ": ~A" (slot-value x 'args))))


(defun lookup-inplace-op-func (op)
  (break "lookup-inplace-op-func: ~A" op))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#+(or)
(defmacro with-py-error-handlers (&body body)
  `(handler-bind
       ((division-by-zero
	 (lambda (c)
	   (declare (ignore c))
	   (py-raise 'ZeroDivisionError "Division or modulo by zero")))
	
	#+allegro
	(excl:synchronous-operating-system-signal
	 (lambda (c)
	   (when (string= (simple-condition-format-control c)
			  #+(or)(slot-value c 'excl::format-control)
			  "~1@<Stack overflow (signal 1000)~:@>")
	     (py-raise 'RuntimeError "Stack overflow"))))
	
	#+allegro
	(excl:interrupt-signal
	 (lambda (c)
	   (let ((fa (simple-condition-format-arguments c)))
	     (when (string= (second fa) "Keyboard interrupt")
	       (py-raise 'KeyboardInterrupt "Keyboard interrupt")))))
	
	;; more?
	)
     ,@body))

(defun builtin-name-p (x)
  (member x '(len range)))

(defun builtin-name-value (x)
  (ecase x
    (len :the-len-builtin)
    (range :the-range-builtin)))

#+(or)
(defun make-module (&rest args)
  :make-module-result)



(defun testw ()

  ;; 'len' is a shadowed built-in. Before the first assignment, and
  ;; after deleting it, that variable has its built-in value.
  (let* ((ast (parse-python-string "def f(): return 42"))
	 (me (macroexpand ast)))
    (prin1 me)
    (let ((f (compile nil `(lambda () ,ast))))
      (format t "f: ~S~%" f)
      ;;(format t "funcall result: ~S" (funcall f))
      nil)))

;; XXX: accept ; as delimiter of statements in grammar


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Optimization

#+(or) ;; inline number/fixnum additions arithmetic
(define-compiler-macro py-+ (&whole whole x y)
  (cond ((comp-opt-p :inline-number-math)
	 `(let ((left ,x) (right ,y))
	    (if (and (numberp left) (numberp right))
		(+ left right)
	      (py-+ left right))))
	
	((comp-opt-p :inline-fixnum-math)
	`(if (and (fixnump left) (fixnump right))
	     (+ left right)
	   (py-+ left right)))
	
	(t whole)))

#+(or) ;; methods on py-+
(defmethod py-+ ((x number) (y number))
  (+ x y))

#+(or)
(defmethod py-+ ((x fixnum) (y fixnum))
  (+ x y))

#+(or) ;; optimize membership test on sequences
(defmethod py-in (item (seq sequence))
  (py-bool (member item seq :test #'py-==)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

#+(or)
(compile nil
	 (lambda ()
	   (declare (optimize (speed 3) (safety 1) (debug 0)))
	   #.(parse-python-string "
def f():
  print 3, 4
f()
")))