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
	     collect `(,x ',(make-symbol (symbol-name x))))
     ,@body))



;; The ASSERT-STMT uses the value of *__debug__* to determine whether
;; or not to include the assertion code. Currently there is not way to
;; set it to False.

(defvar *__debug__* t)


;; Compiler debugging and optimization options.
;; >> not used yet

(defvar *comp-opt*
    (copy-tree '((:include-line-numbers t) ;; XXX todo: insert line numbers in AST?
		 (:inline-number-math nil)
		 (:inline-fixnum-math nil)))) 

(defun comp-opt-p (x)
  (assert (member x *comp-opt* :key #'car) () "Unknown Python compiler option: ~S" x)
  (cdr (assoc x *comp-opt*)))

(defun (setf comp-opt) (val x)
  (assert (member x *comp-opt* :key #'car) () "Unknown Python compiler option: ~S" x)
  (setf (cdr (assoc x *comp-opt*)) val))

;; The AST node macros

(defmacro assert-stmt (test raise-arg)
  (when *__debug__*
    `(with-simple-restart (:continue "Ignore the assertion failure")
       (unless (py-val->lisp-bool ,test)
	 (py-raise 'AssertionError ,(or raise-arg 
					`(format nil "Failing test: ~A"
						 (with-output-to-string (s)
						   (py-pprint s ',test)))))))))
  
(defmacro assign-stmt (value targets &environment e)
  (let ((context (get-pydecl :context e)))
    (with-gensyms (val)
      
      (flet ((assign-one (tg)
	       (ecase (car tg)
		 
		 ((attributeref-expr subscription-expr)  `(setf ,tg ,val))
		 
		 (identifier-expr
		  (let* ((name (second tg)))
		    (assert (not (eq name '|...|)))
		    
		    (flet ((module-set ()
			     (let ((ix (position name (get-pydecl :mod-globals-names e))))
			       (if ix
				   `(setf (svref +mod-globals-values+ ,ix) ,val)
				 `(setf (gethash ',name +mod-dyn-globals+) ,val))))
			   
			   (local-set () `(setf ,name ,val))
			
			   (class-set () `(setf (gethash ',name +cls-namespace+) ,val)))
		    
		      (ecase context
		      
			(:module    (if (member name (get-pydecl :lexically-visible-vars e))
					(local-set)
				      (module-set)))
		      
			(:function  (if (or (member name (get-pydecl :funcdef-scope-globals e))
					    (not (member name (get-pydecl :lexically-visible-vars e))))
					(module-set)
				      (local-set)))
			
			;; Inside a classdef, do not look at :lexically visible vars
			(:class     (if (member name (get-pydecl :classdef-scope-globals e))
					(module-set)      
				      (class-set))))))))))
	
	`(let ((,val ,value))
	   ,@(mapcar #'assign-one targets))))))

(defmacro attributeref-expr (item attr)
  `(py-attr ,item ',(second attr)))

(defmacro augassign-stmt (op place val)
  (let ((py-@= (get-binary-iop-func op))
	(py-@  (get-binary-op-from-iop-func op)))
	
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
     
       (cond ((eq prim (load-time-value #'pybf:locals))
	      (if (and ,(not (or pos-args kwd-args))
		       ,(or (null *-arg)  `(null (py-iterate->lisp-list ,*-arg)))
		       ,(or (null **-arg) `(null (py-mapping->lisp-list ,**-arg))))
		  ,(if (eq context :module)
		       `(.globals.)
		     `(.locals.))
		(py-raise 'TypeError
			  "locals() must be called without args (got: ~A)" ',all-args)))

	     ((eq prim (load-time-value #'pybf:globals))
	      (if (and ,(not (or pos-args kwd-args))
		       ,(or (null *-arg)  `(null (py-iterate->lisp-list ,*-arg)))
		       ,(or (null **-arg) `(null (py-mapping->lisp-list ,**-arg))))
		  (.globals.)
		(py-raise 'TypeError
			  "globals() must be called without args (got: ~A)" ',all-args)))

	     ((eq prim (load-time-value #'pybf:eval))
	      (let ((args (nconc (list ,@pos-args)
				 ,(when *-arg `(py-iterate->lisp-list ,*-arg)))))
		(if (and ,(null kwd-args)
			 ,(or (null **-arg)
			      `(null (py-mapping->lisp-list ,**-arg)))
			 (<= 1 (length args) 3))
		    
		    (funcall (load-time-value #'pybf:eval) ,(first pos-args)
			     ,(or (second pos-args)
				  `(.globals.))
			     ,(or (third  pos-args)
				  (if (eq context :module)
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
		       
			 ,@(when *-arg  `(:* ,*-arg))
			 ,@(when **-arg `(:** ,**-arg))))))))

(defmacro classdef-stmt (name inheritance suite)
  ;; todo: define .locals. containing class vars
  (assert (eq (car name) 'identifier-expr))
  (assert (eq (car inheritance) 'tuple-expr))
  
  (with-gensyms (cls)
    `(let ((+cls-namespace+ (make-hash-table :test #'eq)))
       
       (with-pydecl ((:context :class)
		     (:classdef-scope-globals ',(classdef-globals suite)))
	 ,suite)
       
       (let ((,cls (make-py-class :name ',(second name)
				  :namespace +cls-namespace+
				  :supers (list ,@(second inheritance)))))
	 (assign-stmt ,cls (,name))))))

(defmacro comparison-expr (cmp left right)
  (let ((py-@ (get-binary-comparison-func cmp)))
    `(funcall ,py-@ ,left ,right)))

(defmacro continue-stmt (&environment e)
  (if (get-pydecl :inside-loop e)
      `(go :continue)
    (py-raise 'SyntaxError "CONTINUE was found outside loop")))

(defmacro del-stmt (item &environment e)
  (ecase (car item)
    
    (tuple-expr
     `(progn ,@(loop for x in (second item) collect `(del-stmt ,x))))
    
    (subscription-expr
     `(py-del-subs ,@(cdr item))) ;; XXX maybe inline dict case
    
    (attributeref-expr `(py-del-attr ,@(cdr item)))
    
    (identifier-expr
     (let* ((name (second item))
	    (context (get-pydecl :context e))

	    (module-del
	     ;; reset module-level vars with built-in names to their built-in value
	     (let ((ix (position name (get-pydecl :mod-globals-names e))))
	       (if ix
		   `(let ((old-val (svref +mod-globals-values+ ,ix)))
		      (if (eq old-val :unbound)
			  (py-raise 'NameError "Cannot delete variable '~A': ~
                                                it is unbound [static global]" ',name)
			(setf (svref +mod-globals-values+ ,ix) 
			  ,(if (builtin-name-p name)
			       (builtin-name-value name)
			     :unbound))))
		 `(or (remhash ,name +mod-dyn-globals+)
		      (py-raise 'NameError "Cannot delete variable '~A': ~
                                            it is unbound [dyn global]" ',name)))))
	    (local-del `(if (eq ,name :unbound)
			    (py-raise 'NameError "Cannot delete variable '~A': ~
                                                  it is unbound [local]" ',name)
			  (setf ,name :unbound)))
	      
	    (class-del `(or (remhash ,name +cls-namespace+)
			    (py-raise 'NameError "Cannot delete variable '~A': ~
                                                  it is unbound [dyn class]" ',name))))
       (ecase context
	 
	 (:module   (if (member name (get-pydecl :lexically-visible-vars e))
			local-del
		      module-del))
	   
	 (:function (if (or (member name (get-pydecl :func-globals e))
			    (not (member name (get-pydecl :lexically-visible-vars e))))
			module-del
		      local-del))
	   
	 (:class    (if (member name (get-pydecl :class-globals e))
			module-del
		      class-del)))))))

(defmacro dict-expr (alist)
  `(make-dict-unevaled-list ,alist))


(defmacro exec-stmt (code globals locals &environment e)
  ;; TODO:
  ;;   - allow code object etc as CODE
  ;;   - when code is a constant string, parse it already at compile time etc
  
  ;; An EXEC-STMT is translated into a function, that is compiled and
  ;; then funcalled. Compiling is needed because otherwise the
  ;; environment with pydecl it not passed properly.
  ;; 
  ;; XXX todo: understand how exactly environments work
  ;; w.r.t. compilation.
  
  (let ((context (get-pydecl :context e)))
    
    (with-gensyms (exec-helper-func)
      
      `(let* ((ast (parse-python-string ,code))
	      (real-ast (destructuring-bind (module-stmt suite) ast
			  (assert (eq module-stmt 'module-stmt))
			  (assert (eq (car suite) 'suite-stmt))
			  suite))
	      (locals-ht  (convert-to-namespace-ht ,(or locals (if (eq context :module)
								 `(.globals.)
							       `(.locals.)))))
	      (globals-ht (convert-to-namespace-ht ,(or globals `(.globals.))))
	      (loc-kv-pairs (loop for k being the hash-key in locals-ht
				using (hash-value val)
				for k-sym = (typecase k
					      (string (intern k #.*package*))
					      (symbol k)
					      (t (error "EXEC-STMT: the dict with locals ~
                                                         must have keys or symbols as key ~
                                                         (got: ~A, as ~A)" k (type-of k))))
				collect `(,k-sym ,val)))
	      (lambda-body
	       `(with-module-context (#() #() ,globals-ht)
		  
		  ;; this assumes that there are not :lexically-visible-vars set yet
		  (with-pydecl ((:lexically-visible-vars ,(mapcar #'car loc-kv-pairs)))
		    ,real-ast))))
	 
	 (warn "EXEC-STMT: lambda-body: ~A" lambda-body)
			  
	 (let ((,exec-helper-func (compile nil `(lambda () ,lambda-body))))
	   
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
		      (go :continue) ;; prevent warning about unused tag
		     :continue))))
	 (declare (dynamic-extent ,f))
	 (map-over-py-object ,f ,source))
       ,@(when else-suite `(,else-suite))
       
       (go :break) ;; prevent warning
      :break)))

(defmacro funcdef-stmt (decorators
			fname (&whole formal-args pos-args key-args *-arg **-arg)
			suite
			&environment e)

  ;; The resulting function is returned.
  ;; 
  ;; If FNAME is a keyword symbol (like :lambda), then an anonymous
  ;; function (like from LAMBDA-EXPR) is created. The function is thus
  ;; not bound to a name. Decorators are not allowed then.
  
  (cond ((keywordp fname)
	 (assert (null decorators)))
	
	((and (listp fname) (eq (car fname) 'identifier-expr))
	 (setf fname (second fname)))
	
	(t (break :unexpected)))
  
  (flet ((list-of-all-pos-arg-names (declared-pos-args)
	   (loop with todo = declared-pos-args and res = ()
	       while todo
	       do (let ((x (pop todo)))
		    (ecase (first x)
		      (identifier-expr (push (second x) res))
		      (tuple-expr      (setf todo (nconc todo (second x))))))
	       finally (return res)))
	 
	 ;; Replace "def f( (x,y), z):  .." 
	 ;; by "def f( _tmp , z):  (x,y) = _tmp; ..".

	 (lambda-args-and-destruct-form (funcdef-pos-args)
	   (loop with lambda-pos-args = () and destructs = ()
	       for pa in funcdef-pos-args
	       do (ecase (car pa)
		    (tuple-expr      (let ((tmp-var (make-symbol (format nil "~A" (cdr pa)))))
				       (push tmp-var lambda-pos-args)
				       (push `(assign-stmt ,tmp-var ((identifier-expr ,pa)))
					     destructs)))
		    (identifier-expr (push (second pa) lambda-pos-args)))
	       finally (return (values (nreverse lambda-pos-args)
				       (when destructs
					 `(progn ,@(nreverse destructs))))))))
    
    ;; new:
    (let ((all-pos-arg-names (list-of-all-pos-arg-names pos-args)))
      
      (multiple-value-bind
	  (lambda-pos-args pos-arg-destruct-form) (lambda-args-and-destruct-form pos-args)
	
	(multiple-value-bind
	    (func-explicit-globals func-locals) (funcdef-globals-and-locals
						 (cons all-pos-arg-names (cdr formal-args))
						 suite)
	  
	  (let* ((all-locals-and-arg-names (append all-pos-arg-names
						   (mapcar #'first key-args)
						   (when *-arg (list *-arg))
						   (when **-arg (list *-arg))
						   func-locals))
		 
		 (func-lambda
		  `(py-arg-function
		    ,fname
		    (,lambda-pos-args ,key-args ,*-arg ,**-arg)
		    
		    (let ,(loop for loc in func-locals collect `(,loc :unbound))
		      
		      ,@(when pos-arg-destruct-form
			  `(,pos-arg-destruct-form))
		      
		      (block :function-body
			
			(flet ((.locals. ()  ;; A lambda (from a lamda--expr) has locals() too!
				 (make-py-dict
				  (delete :unbound
					  (mapcar #'cons ',all-locals-and-arg-names
						  (list ,@all-locals-and-arg-names))
					  :key #'cdr))))
			  
			  (with-pydecl ((:funcdef-scope-globals ',func-explicit-globals)
					(:context :function)
					(:inside-function t)
					(:lexically-visible-vars
					 ,(nconc (copy-list func-locals)
						 (copy-list all-pos-arg-names)
						 (when *-arg (list *-arg))
						 (when **-arg (list **-arg))
						 (get-pydecl :lexically-visible-vars e))))
			    
			    ,(if (generator-ast-p suite)
				 
				 `(return-stmt ,(rewrite-generator-funcdef-suite fname suite))
			       
			       suite))))))))
	    
	    (if (keywordp fname)
		
		func-lambda
	      
	      (with-gensyms (func)
		(let ((art-deco (loop with res = func
				    for deco in (reverse decorators)
				    do (setf res `(call-expr ,deco ((,res) () nil nil)))
				    finally (return res))))
		  
		  `(let ((,func (make-py-function ',fname func-lambda)))
		     (assign-stmt ,art-deco ((identifier-expr ,fname)))
		     (identifier-expr ,fname)))))))))))


(defmacro generator-expr (&whole whole item for-in/if-clauses)
  (declare (ignore item for-in/if-clauses))
  (rewrite-generator-expr-ast whole))
       
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
  
  (when (eq name '|...|)
    (setf name 'Ellipsis))
  
  (flet ((module-lookup ()
	   (let ((ix (position name (get-pydecl :mod-globals-names e))))
	     (if ix
		 `(let ((val (svref +mod-globals-values+ ,ix)))
		    (when (eq val :unbound) (py-raise 'NameError
						      "Variable '~A' is unbound [glob]" ',name))
		    val)
	       `(or (gethash ',name +mod-dyn-globals+)
		    ,(if (builtin-name-p name)
			 (builtin-name-value name)
		       `(py-raise 'NameError "No variable with name '~A' [dyn-glob]" ',name)))))))
    
    (ecase (get-pydecl :context e)
      
      ((:module :function) (if (member name (get-pydecl :lexically-visible-vars e)) ;; in exec-stmt
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

(defmacro import-stmt (args)
  `(progn ,@(loop for x in args
		collect (if (eq (first x) 'not-as)
			    `(py-import ',(second (second x))
					+mod-globals-names+
					+mod-globals-values+
					+mod-dyn-globals+)
			  `(warn "unsupported import: ~A" ',x)))))
  
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
				&key set-builtins create-return-mod)
			       &body body)
  (check-type glob-names vector)
  (check-type dyn-glob hash-table)
  
  `(let* ((+mod-globals-names+  ,glob-names)
	  (+mod-globals-values+ ,glob-values)
	  (+mod-dyn-globals+    ,dyn-glob))
	
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
	   
	 ,@body))
       
     ,@(when create-return-mod
	 `((make-module :globals-names  ,glob-names
			:globals-values ,glob-values
			:dyn-globals    ,dyn-glob)))))


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
  ;; XXX todo: use methods `write' of `dest' etc
  (with-gensyms (x.str)
    `(progn ,(when dest
	       `(warn "ignoring 'dest' arg of PRINT stmt (got: ~A)" ',dest))
	    
	    ,@(loop for x in items
		  collect `(let ((,x.str (py-str-string ,x)))
			     (format t "~A " ,x.str)))
	    
	    ,(unless comma?
	       `(write-char #\Newline t))
	    nil)))

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
  (with-gensyms (the-exc the-var)
    
    `(let ((,the-exc ,exc)
	   (,the-var ,var))
       (declare (ignorable ,the-var))
       
       ,@(when tb `((warn "Traceback arg to RAISE ignored")))
       
       ;; ERROR cannot deal with _classes_ as first condition argument;
       ;; it must be an _instance_ or condition type _name_.
       
       ,(if var
	    `(etypecase ,the-exc
	       (class (error (make-instance ,the-exc :args ,the-var)))
	       (error (progn (warn "RAISE: ignored arg, as exc was already an ~
                                        instance, not a class")
			     (error ,the-exc))))
	  
	  `(etypecase ,the-exc
	     (class    (error (make-instance ,the-exc)))
	     (error    ,the-exc))))))

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
	       `(:else
		 ,else-suite)))))))


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
    `(funcall ,py-op-func ,item)))

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
		
		(go :continue) ;; prevent warning unused tag
	       :continue)
	   finally (when (and ,take-else ,(and else-suite t))
		     ,else-suite))
       
       (go :break) ;; prevent warning
      :break)))

(defmacro yield-stmt (val)
  (declare (ignore val))
  (error "YIELD found outside function"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Helper functions for the compiler

(defun builtin-name-p (x)
  (or (find-symbol (string x) (load-time-value (find-package :python-builtin-functions)))
      (find-symbol (string x) (load-time-value (find-package :python-builtin-types)))
      (find-symbol (string x) (load-time-value (find-package :python-builtin-values)))))

(defun builtin-name-value (x)
  (let ((sym (builtin-name-p x)))
    (assert sym)
    (let ((pkg (symbol-package sym)))
      (cond ((eq pkg (load-time-value (find-package :python-builtin-functions)))
	     (symbol-function sym))
	    ((eq pkg (load-time-value (find-package :python-builtin-types)))
	     (symbol-value sym))
	    ((eq pkg (load-time-value (find-package :python-builtin-values)))
	     (symbol-value sym))))))

(defun ast-vars (ast &key params locals declared-globals outer-scope)
  ;; Returns: LOCALS, DECLARED-GLOBALS, OUTER-SCOPE
  ;; which are lists of symbols denoting variable names.
  
  (labels
      ((recurse (ast)
	 (with-py-ast ((form &key value target) ast)
	   (case (car form)
	     
	     (classdef-stmt (destructuring-bind
				((identifier cname) inheritance suite) (cdr form)
			      (declare (ignore suite))
			      
			      (assert (eq identifier 'identifier-expr))
			      (assert (eq (car inheritance) 'tuple-expr))
			      
			      (if (member cname declared-globals)
				  (error "SyntaxError: class name ~A may not be ~
                                          declared `global'" cname)
				(pushnew cname locals))
			      
			      (loop for x in (second inheritance) do (recurse x)))
			    (values nil t))
	     
	     (funcdef-stmt  (destructuring-bind (decorators (identifier-expr fname) args suite)
				(cdr form)
			      (declare (ignore suite))
			      (assert (eq identifier-expr 'identifier-expr))
			      
			      (if (member fname declared-globals)
				  (error "SyntaxError: inner function name ~A ~
                                          may not be declared global" fname)
				(pushnew fname locals))
			      
			      (loop for deco in decorators do (recurse deco))
			      
			      (loop for (nil def-val) in (second args)
				  do (recurse def-val)))
			    (values nil t))

	     (generator-expr ;; generator expressions XXX
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
	     
	     (identifier-expr #+(or)(break "ast-vars: identifier-expr ~A" (second form)) 
			      (let ((name (second form)))
				(unless (eq name '|...|)
				  
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
				      (pushnew name locals))))
				
				(values nil t)))
	     
	     (t form)))))
    
    (recurse ast)
    (values locals declared-globals outer-scope)))

(defun funcdef-globals-and-locals (args suite)
  "Given FUNCDEF ARGS and SUITE (or EXPR), return DECLARED-GLOBALS, LOCALS.
   Does _not_ return ARGS (e.g. as part of LOCALS)."
  
  (destructuring-bind (linearized-pos-arg-names kwd-args *-arg **-arg) args
    
    (assert (not (some #'listp linearized-pos-arg-names)))
    
    (let ((params (let ((x (append linearized-pos-arg-names (mapcar #'first kwd-args))))
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

(defgeneric convert-to-namespace-ht (x)
  (:method ((x hash-table)) (loop with d = (make-hash-table :test #'eq) ;; XXX sometimes not needed
				for k being the hash-key in x using (hash-value v)
				for k-sym = (typecase k
					      (string (intern k #.*package*))
					      (symbol k)
					      (t (error "Not a valid namespace hash-table,
                                                         as key is neither string or symbol: ~A ~A"
							k (type-of k))))
				do (setf (gethash k-sym d) v)
				finally (return d)))
  (:method ((x t))          (let ((x2 (deproxy x)))
				 (when (eq x x2) (error "invalid namespace: ~A" x))
				 (convert-to-namespace-ht x2))))

(defmacro py-arg-function (name (pos-args key-args *-arg **-arg) &body body)
  
  ;; Non-consing argument parsing! (except when *-arg or **-arg
  ;; present)
  ;; 
  ;; POS-ARGS: list of symbols
  ;; KEY-ARGS: list of (key-symbol default-val) pairs
  ;; *-ARG, **-ARG: a symbol or NIL 
  ;; 
  ;; XXX todo: the generated code can be cleaned up a bit when there
  ;; are no arguments (currently zero-length vectors are created).
  
  #+(or)(break "py-arg-function: args=~A ~A ~A ~A" pos-args key-args *-arg **-arg)
  
  (assert (symbolp name))
  
  (let* ((num-pos-args (length pos-args))
	 (num-key-args (length key-args))
	 (num-pos-key-args  (+ num-pos-args num-key-args))
	 
	 
	 (pos-key-arg-names (nconc (copy-list pos-args)
				   (mapcar #'first key-args)))
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
    
    `(locally (declare (optimize (safety 3) (debug 3)))
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
		    `(error "Too many pos args")))
	     
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
			 (error "* and ** args in a call not handled yet, sorry"))
		     
		       ,(cond ((> num-pos-key-args 0)
			       `(loop for i fixnum from num-filled-by-pos-args below ,num-pos-key-args
				    when (or (eq (svref ,arg-name-vec i) key)
					     (eq (svref ,arg-kwname-vec i) key))
				    do (setf (svref arg-val-vec i) val)
				       (return)
				    finally 
				      ,(if **-arg
					   `(push (cons key val) for-**)
					 `(error "Got unknown keyword arg and no **-arg: ~A ~A"
						 key val))))
			      (**-arg
			       ;; this reconsing is necessary as **-arg might not have dynamic extent
			       `(push (cons key val) for-**))
			    
			      (t `(error "Got unknown keyword arg and no **-arg: ~A ~A"
					 key val)))))
	     
	       ;; Ensure all positional arguments covered
	       ,@(when (> num-pos-args 0)
		   `((loop for i fixnum from num-filled-by-pos-args below ,num-pos-args
			 unless (svref arg-val-vec i)
			 do (error "Positional arg ~A has no value" (svref ,arg-name-vec i)))))
	     
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

#+(or) ;; todo? make function code shorter by using a general function to parse args
(defun parse-py-func-args (args arg-val-vec
			   num-pos-args num-key-args
			   pos-key-arg-names
			   arg-name-vec arg-kmname-vec
			   key-arg-default-values)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Generator rewriting

(defun generator-ast-p (ast)
  "Is AST a function definition for a generator?"
  
  (assert (not (eq (car ast) 'module-stmt)) ()
    "GENERATOR-AST-P called with a MODULE ast.")
  
  (catch 'is-generator
    (walk-py-ast ast 
		 (lambda (x &key value target)
		   (declare (ignore value target))
		   (case (car x)
		     
		     (yield-stmt (throw 'is-generator t))
		     
		     ;; don't look for 'yield' in inner functions and classes
		     ((classdef-stmt funcdef-stmt) (values nil t))
		     
		     (t x))))
    nil))

(defun rewrite-generator-funcdef-suite (fname suite)
  ;; Returns the function body
  (assert (symbolp fname))
  (assert (eq (car suite) 'suite-stmt))
  (assert (generator-ast-p suite))

  (let ((yield-counter 0)
	(other-counter 0)
	(vars ()))
    
    (flet ((new-tag (kind) (if (eq kind :yield)
			       (incf yield-counter)
			     (make-symbol (format nil "~A~A" kind (incf other-counter))))))
      
      (labels
	  ((walk (form stack)
	     (walk-py-ast
	      form
	      (lambda (form &rest context)
		(declare (ignore context))
		(case (first form)
		  
		  (yield-stmt
		   (let ((tag (new-tag :yield)))
		     (values `(:split (setf .state. ,tag)
				      (return-from :function-body ,(second form)) 
				      ,tag)
			     t)))
		  
		  (while-stmt
		   (destructuring-bind (test suite else-suite) (cdr form)
		     (let ((repeat-tag (new-tag :repeat))
			   (else-tag   (new-tag :else))
			   (after-tag  (new-tag :end+break-target)))
		       (values `(:split
				 (unless (py-val->lisp-bool ,test)
				   (go ,else-tag))

				 ,repeat-tag
				 (:split
				  ,(walk suite
					 (cons (cons repeat-tag after-tag)
					       stack)))
				 (if (py-val->lisp-bool ,test)
				     (go ,repeat-tag)
				   (go ,after-tag))
				 
				 ,else-tag
				 ,@(when else-suite
				     `((:split ,(walk else-suite stack))))
				 
				 ,after-tag)
			       t))))
		  
		  (if-stmt
		   (if (not (generator-ast-p form))
		       (values form t)
		     (destructuring-bind (clauses else-suite) (cdr form)
		       (loop
			   with else-tag = (new-tag :else) and after-tag = (new-tag :after)
									   
			   for (expr suite) in clauses
			   for then-tag = (new-tag :then)
					  
			   collect `((py-val->lisp-bool ,expr) (go ,then-tag)) into tests
			   collect `(:split ,then-tag
					    (:split ,(walk suite stack))
					    (go ,after-tag)) into suites
			   finally
			     (return
			       (values `(:split (cond ,@tests
						      (t (go ,else-tag)))
						(:split ,@suites)
						,else-tag
						,@(when else-suite
						    `((:split ,(walk else-suite stack))))
						,after-tag)
				       t))))))
		  
		  (for-in-stmt
		   (destructuring-bind (target source suite else-suite) (cdr form)
		     (let* ((repeat-tag (new-tag :repeat))
			    (else-tag   (new-tag :else))
			    (end-tag    (new-tag :end+break-target))
			    (continue-tag (new-tag :continue-target))
			    (generator  (new-tag :generator))
			    (loop-var   (new-tag :loop-var))
			    (stack2     (cons (cons continue-tag end-tag)
					      stack)))
		       (push loop-var vars)
		       (push generator vars)
		       
		       (values
			`(:split 
			  (setf ,generator (get-py-iterate-fun ,source)
				,loop-var  (funcall ,generator))
			  (unless ,loop-var (go ,else-tag))
			  
			  ,repeat-tag
			  (assign-stmt ,loop-var (,target))
			  (:split ,(walk suite stack2))
			  
			  ,continue-tag
			  (setf ,loop-var (funcall ,generator))
			  (if ,loop-var (go ,repeat-tag) (go ,end-tag))
			  
			  ,else-tag
			  ,@(when else-suite
			      `((:split ,(walk else-suite stack2))))
			  
			  ,end-tag
			  (setf ,loop-var nil
				,generator nil))
			t))))
		  
		  (break-stmt
		   (unless stack (error "BREAK outside loop"))
		   (values `(go ,(cdr (car stack)))
			   t))
		    
		  (continue-stmt
		   (unless stack (error "CONTINUE outside loop"))
		   (values `(go ,(car (car stack)))
			   t))
		    
		  (return-stmt
		   (when (second form)
		     (error "SyntaxError: Inside generator, RETURN statement may not have ~
                             an argument (got: ~S)" form))
		    
		   ;; from now on, we will always return to this state
		   (values `(generator-finished)
			   t))

		  (suite-stmt
		   (if (not (generator-ast-p form))
		       (values form t)
		     (values `(:split ,@(loop for stmt in (second form)
					    collect (walk stmt stack)))
			     t)))
		    
		  (try-except-stmt
		   (if (not (generator-ast-p form))
		       (values form t)
		     
		     ;; Three possibilities:
		     ;;  1. YIELD-STMT or RETURN-STMT in TRY-SUITE 
		     ;;  2. YIELD-STMT or RETURN-STMT in some EXCEPT-CLAUSES
		     ;;  3. YIELD-STMT or RETURN-STMT in ELSE-SUITE
		     ;; 
		     ;; We rewrite it such that all cases are covered,
		     ;; so maybe there is more rewritten than strictly
		     ;; needed.
		     
		     (destructuring-bind (try-suite except-clauses else-suite) (cdr form)
		       (loop
			   with try-tag = (new-tag :yield)
			   with else-tag = (new-tag :else)
			   with after-tag = (new-tag :after)
			   with gen-maker = '#:helper-gen-maker and gen = '#:helper-gen
				      				      
			   initially (push gen vars)
			     
			   for (exc var suite) in except-clauses
			   for tag = (new-tag :exc-suite)
				     
			   collect `(,exc ,var (go ,tag)) into jumps
			   nconc `(,tag ,(walk suite stack) (go ,after-tag)) into exc-bodies
									      
			   finally
			     (return
			       (values
				`(:split
				  (setf ,gen (py-iterate->lisp-fun
					      (funcall ,(suite->generator gen-maker try-suite))))
				  (setf .state. ,try-tag)
				  
				  ;; yield all values returned by helpder function .gen.
				  ,try-tag
				  (try-except-stmt
				   (let ((val (funcall ,gen)))
				     (case val
				       (:explicit-return (generator-finished))
				       (:implicit-return (go ,else-tag))
				       (t (return-from :function-body val))))
				   
				   ,@jumps)
				  
				  ,@exc-bodies
				  
				  ,else-tag
				  ,@(when else-suite
				      `((:split ,(walk else-suite stack))))
				  
				  ,after-tag
				  (setf ,gen nil))
				t))))))
		  
		  (try-finally
		   (destructuring-bind (try-suite finally-suite) (cdr form)
		     (when (generator-ast-p try-suite)
		       (error "SyntaxError: YIELD is not allowed in the TRY suite of ~
                               a TRY/FINALLY statement (got: ~S)" form))
		     
		     (if (not (generator-ast-p finally-suite))
			 (values form t)

		       (let ((fin-catched-exp '#:fin-catched-exc))
			 
			 (pushnew fin-catched-exp vars)
			 (values
			  `(:split
			    (multiple-value-bind (val cond)
				(ignore-errors ,try-suite ;; no need to walk
					       (values))
			      (setf ,fin-catched-exp cond))
			    
			    ,(walk finally-suite stack)
			    
			    (when ,fin-catched-exp
			      (error ,fin-catched-exp)))
			  
			  t)))))
		  
		  (t (values form
			     t)))))))

	(let ((walked-as-list (multiple-value-list (apply-splits (walk suite ()))))
	      (final-tag -1))
	  
	  `(let ((.state. 0)
		 ,@(nreverse vars))
	     
	     (make-iterator-from-function 
	      :name '(:iterator-from-function ,fname)
	      :func
	      (excl:named-function (:iterator-from-function ,fname)
		(lambda ()
		  ;; This is the function that will repeatedly be
		  ;; called to return the values
		  
		  (macrolet ((generator-finished ()
			       '(progn (setf .state. ,final-tag)
				 (go ,final-tag))))
		    
		    (block :function-body
		      (tagbody
			(case .state.
			  ,@(loop for i from -1 to yield-counter
				collect `(,i (go ,i))))
		       0
			,@walked-as-list

			(generator-finished)
			
			,final-tag
			(py-raise 'StopIteration "The generator has finished.")))))))))))))

(defun suite->generator (fname suite)
  ;; Lisp generator function that returns one of:
  ;;  VAL              -- value explicitly `yield'-ed
  ;;  :implicit-return -- no more statements in the function body
  ;;  :explicit-return -- explicit return from function
  
  (assert (eq (car suite) 'suite-stmt))
  (assert (generator-ast-p suite))
  
  `(funcdef-stmt
    nil (identifier-expr ,fname) (nil nil nil nil)
    (suite-stmt
     ,@(mapcar (lambda (x)
		 (walk-py-ast x
			      (lambda (form &rest context)
				(declare (ignore context))
				(case (car form)
				     
				  ((funcdef-stmt classdef-stmt)
				   (values form t))
				     
				  (return-stmt 
				   (when (second form)
				     (error "SyntaxError: Inside generator, RETURN ~
                                             statement may not have an argument ~
                                             (got: ~S)" form))
				   (values `(return-from :function-body :explicit-return)
					   t))
				     
				  (t form)))))
	       (second suite))
     (return-from :function-body :implicit-return))))



(defun rewrite-generator-expr-ast (ast)
  ;; rewrite:  (x*y for x in bar if y)
  ;; into:     def f(src):  for x in src:  if y:  yield x*y
  ;;           f(bar)
  ;; values: (FUNCDEF ...)  bar
  (assert (eq (car ast) 'generator-expr))
  (destructuring-bind (item for-in/if-clauses) (cdr ast)

    (let ((first-for (pop for-in/if-clauses))
	  (first-source '#:first-source))
      
      (assert (eq (car first-for) 'for-in))
      
      (let ((stuff (loop with res = `(yield-stmt ,item)
		       for clause in (reverse for-in/if-clauses)
		       do (setf res
			    (ecase (car clause)
			      (for-in `(for-in-stmt
					,(second clause) ,(third clause) ,res nil))
			      (if     `(if-stmt ((,(second clause) ,res)) nil))))
		       finally (return res))))
	
	`(call-expr 
	  (funcdef-stmt nil (identifier-expr :generator-expr-helper-func)
			(((identifier-expr ,first-source)) nil nil nil)
			(suite-stmt
			 ((for-in-stmt ,(second first-for) (identifier-expr ,first-source)
				       ,stuff nil))))
	  
	  ((,(third first-for)) nil nil nil))))))

(defun apply-splits (form)
  (cond ((atom form)
	 (values form))
	
	((eq (car form) :split)
	 (values-list (loop for elm in (cdr form)
			  append (multiple-value-list (apply-splits elm)))))
	
	(t (loop for elm in form
	       append (multiple-value-list (apply-splits elm))))))


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
	   #.(parse-python-string "
def f():
  print 3, 4
f()
")))