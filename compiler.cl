(in-package :python)

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

(defmacro with-fresh (list &body body)
  `(let ,(loop for x in list
	     collect `(,x (make-symbol ,(symbol-name x))))
     ,@body))


;; The AST node macros

(defmacro assert-stmt (&rest args)
  (declare (ignore args))
  (break "todo: assert-stmt"))

(defmacro assign-expr (value targets &environment e)
  
  #+(or) ;; debug
  (trace get-pydecl)
  
  (let ((context (get-pydecl :context e)))
    (with-fresh (val)
      
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

(defmacro augassign-expr (op place val)
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
		(assign-expr new-val (,(car place) primary sub))))))

      (identifier-expr
     
       `(let* ((ev-val ,val)
	       (place-val-now ,place))
	
	  (or (funcall ,py-@= place-val-now ev-val) ;; returns true iff __i@@@__ found
	      (let ((new-val (funcall ,py-@ place-val-now ev-val)))
		(assign-expr new-val (,place)))))))))

(defmacro backticks-expr (item)
  `(py-repr ,item))

(defun get-op-func-todo (op)
  (declare (ignore op)) ;; XXX
  :get-op-func-todo)

(defmacro binary-expr (op left right)
  `(let ((left ,left)
	 (right ,right))
     
     ,(let ((py-@ (get-op-func-todo op)))
	(if (member op '(+ - * /))
	    
	    `(if (and (numberp left) (numberp right))
		 (,op left right)
	       (,py-@ left right))
	  
	  `(,py-@ left right)))))

(defmacro binary-lazy-expr (op left right)
  (ecase op
    (or `(let ((left ,left))
	   (if (py-val->lisp-bool left)
	       left
	     (let ((right ,right))
	       (if (py-val->lisp-bool right)
		   right
		 *py-false*)))))
    
    (and `(let ((left ,left))
	    (if (py-val->lisp-bool left)
		,right
	      left)))))

(defmacro break-stmt (&environment e)
  (if (get-pydecl :inside-loop e)
      `(go :break)
    (py-raise 'SyntaxError "BREAK was found outside loop")))


(defmacro call-expr (primary (&whole all-args pos-args kwd-args *-arg **-arg))
  
  ;; For complete Python semantics, we check for every call if the
  ;; function being called is one of the built-in functions EVAL,
  ;; LOCALS or GLOBALS, because they access the variable scope of the
  ;; _caller_ (which is ugly).

  `(let ((prim ,primary))
     
     (cond ((eq prim :the-locals-function-TODO)
	    (if (and ,(not (or pos-args kwd-args))
		     ,(or (null *-arg)  `(null (py-iterate->lisp-list ,*-arg)))
		     ,(or (null **-arg) `(null (py-mapping->lisp-list ,**-arg))))
		(.locals.)
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
		  
		  (py-eval ,(first pos-args)
			   (or ,(second pos-args) (.globals.))
			   (or ,(third  pos-args) (.locals.)))
		
		(py-raise 'TypeError
			  "eval() must be called with 1 to 3 pos args (got: ~A)" ',all-args))))
	   
	   (t (py-call prim
		       ,@pos-args 
		       ,@(loop for ((i-e key) val) in kwd-args
			     do (assert (eq i-e 'identifier-expr))
				;; XXX tuples?
				
			     collect (intern (symbol-name key) :keyword)
			     collect val)
		       ,@(when *-arg `((:* ,*-arg)))
		       ,@(when **-arg `((:** ,**-arg))))))))

(defmacro classdef-stmt (name inheritance suite)
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


(defmacro exec-stmt (code globals locals)
  ;; XXX TODO: also allow code object etc as CODE
  
  `(let ((ast (let ((ast (parse-python-string ,code)))
		(assert (eq (car ast) 'module-stmt))
		ast))
	 (locals-ht  (py-convert-to-hashtable ,(or locals  `(.locals.))))
	 (globals-ht (py-convert-to-hashtable ,(or globals `(.globals.)))))
     
     (with-pydecl ((:exec-mod-globals-ht globals-ht)
		   (:exec-mod-locals-ht  locals-ht))

       (eval ast))))
	 

(defmacro for-in-stmt (target source suite else-suite)
  ;; potential special cases:
  ;;  - <dict>.{items,keys,values}()
  ;;  - constant list/tuple/string
  (with-fresh (f x)
    `(tagbody
       (let* ((,f (lambda (,x)
		    (assign-expr ,x (,target))
		    (tagbody 
		      (locally (declare (pydecl (:inside-loop t)))
			,suite)
		     :continue))))
	 (declare (dynamic-extent ,f))
	 (map-over-py-object ,f ,source))
       ,else-suite
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
			       (push `(assign-expr ,tmp-var (,pa)) destructs)))
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
		(py-arg-function
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
	      
	      (with-fresh (func)
		`(let ((,func (make-py-function :name ',(second name)
						:function func-lambda)))
		   
		   ,(let ((art-deco (loop with res = func
					for deco in (nreverse decorators)
					do (setf res `(call-expr ,deco ((,res) () nil nil)))
					finally (return res))))
		      
		      `(assign-expr ,art-deco (,name)))))))))))
      

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
  ;; target (as the latter case is handled by ASSIGN-EXPR).
  (assert (symbolp name))
  
  (flet ((module-lookup ()
	   (let ((ix (position name (get-pydecl :mod-globals-names e))))
	     (if ix
		 `(svref +mod-globals-values+ ,ix)
	       `(or (gethash ,name +mod-dyn-globals+)
		    (py-raise 'NameError "No variable with name ~A" ',name))))))
    
    (ecase (get-pydecl :context e)
       
      (:module     (if (member name (get-pydecl :lexically-visible-vars e)) ;; in exec-stmt
		       name
		     (module-lookup)))
	       
      (:function   (if (member name (get-pydecl :lexically-visible-vars e))
		       name
		     (module-lookup)))
	       
      (:class      `(or (gethash ',name +cls-namespace+)
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
  (with-fresh (vec)
    `(let ((,vec (make-array 0 :adjustable t :fill-pointer 0)))
       ,(loop
	    with res = `(vector-push-extend ,item ,vec)
	    for clause in (reverse for-in/if-clauses)
	    do (setf res (ecase (car clause)
			   (list-for-in `(for-in-stmt ,(second clause) ,(third clause) ,res nil))
			   (list-if     `(if-stmt (,(second clause) ,res) nil))))
	  finally (return res))
     (make-py-list ,vec))))

(defmacro list-expr (items)
  `(make-list ,items))


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
    
    ;;`(excl:named-function module-stmt-lambda
    ;;   (lambda ()
    `(let* ((+mod-globals-names+    (make-array ,(length gv) :initial-contents ',gv))
	    (+mod-globals-values+   (make-array ,(length gv) :initial-element :unbound))
	    (+mod-dyn-globals+      nil) ;; hash-table (symbol -> val) of dynamically added vars 
	    (+mod+                  (make-module :globals-names  +mod-globals-names+
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
	   (:context :module))
	   
	(flet ((.globals. () (module-stmt-make-globals-dict
			      +mod-globals-names+ +mod-globals-values+ +mod-dyn-globals+)))
	  
	  ,(if exec-locals
	       
	       `(let ,(loop for k being the hash-key in exec-locals using (hash-value v)
			  collect `(,k ,v))
		  ,suite)
	     
	     suite)))
	   
      ;; XXX if executing module failed, where to catch error?
      +mod+)))

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
		      (progn ,@(when var `((assign-expr exc (,var))))
			     ,handler-suite
			     (return-from :try-except-stmt nil))))
				
		   (t
		    `((typep exc ,exc)
		      (progn ,@(when var `((assign-expr exc (,var))))
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
  `(make-tuple ,items))

(defmacro unary-expr (op item)
  (ecase op
    (+ `(let ((item ,item))
	  (if (numberp item)
	      item
	    (py-+ item))))
    
    (- `(let ((item ,item))
	  (if (numberp item)
	      (- item)
	    (py-- item))))
    
    (not `(todo))))

(defmacro while-stmt (test suite else-suite)
  (with-fresh (take-else)
    `(tagbody
       (loop
	   with ,take-else = t
	   while (py-val->lisp-bool ,test)
	   do (when ,take-else
		(setf ,take-else nil))
	      (tagbody
		(locally (declare (pydecl (:inside-loop t)))
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
  "Rewrite FUNCDEF-STMT and CLASSDEF-STMT so they become (ASSIGN-EXPR ...).
Returns the new AST."
  (walk-py-ast
   ast
   (lambda (x &key target value)
     (ecase (car x)
       
       (funcdef-stmt (destructuring-bind
			 (decorators fname args suite) (cdr form)
		       (declare (ignore args suite))
		       
		       `(assign-expr (loop with res = `(funcdef-expr ,@(cdr form))
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

(defun module-stmt-make-globals-dict (names-vec values-vec dyn-globals-ht)
  (make-py-dict
   (nconc (loop for name across names-vec and val across values-vec
	      unless (eq val :unbound) collect (cons name val))
	  (loop for k being the hash-key in dyn-globals-ht using (hash-value v)
	      collect (cons k v)))))

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
