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

(eval-when (:compile :load :eval) ;; ??
  (sys:define-declaration
      pydecl (&rest property-pairs) nil :declare
      (lambda (declaration env)
	(values :declare
		(cons 'pydecl
		      (nconc (cdr declaration)
			     (sys:declaration-information 'pydecl env)))))))

;; pydecl abbreviations

(defun get-pydecl (var env)
  (cdr (assoc var (sys:declaration-information 'pydecl env) :test #'eq)))

(defmacro with-pydecl (pairs &body body)
  `(locally (declare (pydecl ,@pairs))
     ,@body))

(with-fresh ((val val))
  (list val))

(defmacro with-fresh (list &body body)
  `(let ,(loop for x in list
	     collect `(,x (make-symbol ,(symbol-name x))))
     ,@body))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A macro for every AST node

(defmacro assert-stmt (break "todo: assert"))


(defmacro assign-expr (value targets &environment e)
  (with-fresh (val)
    
    (flet ((assign-one (tg)
	     (let ((context (get-pydecl :context e))
		   (kind (car tg)))
	       (ecase kind
		 
		 (identifier-expr
		  (let* ((name (second tg))
			 (module-set (let ((ix (gethash name (get-pydecl :mod-globals-names-ht e))))
				       (if ix
					   `(setf (svref +mod-globals-values+ ,ix) ,val)
					 `(setf (gethash ,name +mod-dyn-globals+) ,val))))
			 (local-set `(setf ,name ,value))
			 (class-set `(setf (gethash ,name +cls-namespace+) ,val)))
		    
		    (ecase context
		      
		      (:module    (if (member name (get-pydecl :lexically-visible-vars e))
				      local-set               ;; in exec-stmt
				    module-set))
		      
		      (:function  (if (or (member name (get-pydecl :func-globals e))
					  (not (member name (get-pydecl :lexically-visible-vars e))))
				      module-set
				    local-set))
		    
		      (:class     (if (member name (get-pydecl :class-globals e))
				      module-set
				    class-set)))))
	       
		 (attributeref-expr
		  `(setf ,tg ,val))
	       
		 (subscription-expr
		  `(setf ,tg ,val))))))
    	   
      `(let ((,val ,value))
	 ,@(loop for tg in targets
	       collect (assign-one tg))))))

(defmacro attributeref-expr (item attr)
  `(py-attr ,item ,attr))

(defmacro augassign-expr (op place val)
  (when (eq (car place) 'tuple)
    (py-raise 'SyntaxError
	      "Augmented assignment to multiple places not possible (got: ~S)"
	      `(,place ,op ,val)))

  (assert (member (car place) '(subscription-expr attributeref-expr identifier-expr)))

  `(let* ,(when (member (car place) '(attributeref-expr subscription-expr))
	    `((place-obj-1 ,(second place))
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

(defmacro backticks-expr (item)
  `(py-repr ,item))

(defmacro binary-expr (op left right)
  `(let ((left ,left)
	 (right ,right))
     ,(let ((py-@ (get-op-func-todo op)))
	(if (member op '(+ - * /))
	    `(if (and (numberp left) (numberp right))
		 (,op left right)
	       (,py-@ left right))
	  (,py-@ left right)))))

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

(defmacro call-expr (primary (&rest orig-args pos-args kwd-args *-arg **-arg) &environment e)
  
  ;; For complete Python semantics, we check for every call if the
  ;; function being called is one of the built-in functions EVAL,
  ;; LOCALS or GLOBALS, because they access the variable scope of the
  ;; _caller_ (which is ugly).

  `(block :call-expr
     (let ((prim ,primary))
       
       ;; This clause is the common case. Is it indeed most efficient
       ;; to have this as first test?
       
       (unless (member prim '(:the-locals-function-TODO 
			      :the-globals-function-TODO
			      :the-eval-function-TODO)
		       :test #'eq)
	 (return-from call-expr (py-call prim ,orig-args)))
       
       (return-from :call-expr
	 (cond ((eq prim :the-locals-function-TODO)
		(if (and ,(not (or pos-args kwd-args))
			 ,(or (null *-arg)  `(null (py-iterate->lisp-list ,*-arg)))
			 ,(or (null **-arg) `(null (py-mapping->lisp-list ,**-arg))))
		    (.locals.)
		  (py-raise 'TypeError
			    "locals() must be called without args (got: ~A ~A ~A ~A)"
			    ,pos-args ,kwd-args ,*-arg ,**-arg)))

	       ((eq prim :the-globals-function-TODO)
		(if (and ,(not (or pos-args kwd-args))
			 ,(or (null *-arg)  `(null (py-iterate->lisp-list ,*-arg)))
			 ,(or (null **-arg) `(null (py-mapping->lisp-list ,**-arg))))
		    (.globals.)
		  (py-raise 'TypeError
			    "globals() must be called without args (got: ~A ~A ~A ~A)"
			    ,pos-args ,kwd-args ,*-arg ,**-arg)))
	     

	       ((eq prim :the-eval-function-TODO)
		(let ((args (nconc (list ,@pos-args)
				   ,(when *-arg `(py-iterate->lisp-list ,*-arg)))))
		  (if (and ,(null kwd-args)
			   ,(or (null **-arg)
				`(null (py-mapping->lisp-list ,**-arg)))
			   (<= 1 (length args) 3))
		    
		      `(py-eval ,(first pos-args)
				(or (second pos-args) (.globals.))
				(or (third  pos-args) (.locals.)))
		  
		    (py-raise 'TypeError
			      "eval() must be called with 1 to 3 pos args (got: ~A ~A ~A ~A)"
			      ,pos-args ,kwd-args ,*-arg ,**-arg))))
	     
	       (t (break :unexpected)))))))


(defmacro classdef-stmt (name inheritance suite)
  `(let ((+cls-namespace+ (make-hash-table :test #'eq)))
     (with-pydecl ((:context       :class)
		   (:class-globals ,(classdef-get-globals suite)))
       ,suite)
     (make-py-class :name ,(second name)
		    :namespace +cls-namespace+
		    :supers ,inheritance)))

(defmacro comparison-expr (cmp left right)
  `(let ((left ,left)
	 (right ,right))
     (if (and (numberp left) (numberp right))
	 (,cmp left right)
       ,(let ((py-@ :todo-get-cmp-func))
	  `(,py-@ left right)))))

(defmacro continue-stmt ()
  (if (get-pydecl :inside-loop e)
      `(go :continue)
    (py-raise 'SyntaxError "CONTINUE was found outside loop")))

(defmacro del-stmt (item &environment e)
  (ecase (car item)
    
    (tuple
     `(progn ,@(loop for x in (second item) collect `(del-stmt ,x))))
    
    (subscription-expr
     `(py-del-subs ,@(cdr item))) ;; maybe inline dict case
    
    (identifier-expr
     ;; reset built-in names to their built-in value
     (let* ((name (second item))
	    (context (get-pydecl :context e))
	    (module-del (let ((ix (gethash name (get-pydecl :mod-globals-names-ht e))))
			  (if ix
			      `(let ((old-val (svref +mod-globals-values+ ,ix)))
				 (if (eq old-val :unbound)
				     (py-raise 'NameError "Cannot delete variable ~A: ~
                                                           it is unbound [static global]" name)
				   `(setf (svref +mod-globals-values+ ,ix) 
				      `(if (builtin-name-p name)
					   ,(builtin-name-value name)
					 :unbound))))
			    `(or (remhash ,name +mod-dyn-globals+)
				 (py-raise 'NameError "Cannot delete variable ~A: ~
                                                       it is unbound [dyn global]" name)))))
	    
	    (local-del `(if (eq ,name :unbound)
			    (py-raise 'NameError "Cannot delete variable ~A: ~
                                                  it is unbound [local]" name)
			  (setf ,name :unbound)))
	    
	    (class-del `(or (remhash ,name +cls-namespace+)
			    (py-raise 'NameError "Cannot delete variable ~A: ~
                                                  it is unbound [dyn class]" name))))
       
       (ecase context
	 
	 (:module    (if (member name (get-pydecl :lexically-visible-vars e))
			 local-del
		       module-del))
	 
	 (:function  (if (or (member name (get-pydecl :func-globals e))
			     (not (member name (get-pydecl :lexically-visible-vars e))))
			 module-del
		       local-del))
	 
	 (:class     (if (member name (get-pydecl :class-globals e))
			 module-del
		       class-del)))))
    
    (attributeref-expr `(py-del-attr ,@(cdr item)))))

(defmacro dict-expr (alist)
  `(make-dict ,alist))


(defmacro exec-stmt (code globals locals)
  ;; XXX TODO: also allow code object etc as CODE
  
  `(let ((ast (let ((ast (parse-python-string code)))
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
			suite
			&environment e)
  
  (assert (or (eq name :lambda)
	      (eq (car name) 'identifier)))
  
  ;; Replace "def f( (x,y), z):  .." by "def f( _tmp , z):  (x,y) = _tmp; ..".
  ;; This shadows POS-ARGS from the lambda list above.
  
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
				    `(progn ,@(nreverse destructs)))))
      
      (multiple-value-bind (func-explicit-globals func-locals)
	  (funcdef-globals-and-locals (cons all-pos-arg-names (cdr formal-args)) suite)
    
	;; When a method is defined in a class namespace, the default
	;; argument values are evaluated at function definition time
	;; in the class namespace. Macro PY-ARG-FUNCTION ensures this.
	
	`(let ((func-lambda
		(py-arg-function
		 (,formal-pos-args ,key-args ,*-arg ,**-arg)
		 
		 (let ,(loop for loc in func-locals collect `(,loc :unbound))
		   ,pos-arg-destruct-form
		   
		   (block :function-body
		     
		     (flet ((.locals. ()
			      (loop
				  for loc-name in ',func-locals
				  for loc-val in (list ,@func-locals)
				  unless (eq loc-val :unbound)
				  collect (cons loc-name loc-val) into res
				  finally (return (make-py-dict res)))))
		       
		       (with-pydecl ((:func-explicit-globals ',func-explicit-globals)
				     (:context :function)
				     (:lexically-visible-vars
				      (nconc (list ,@func-locals)
					     (get-pydecl :lexically-visible-vars e))))
			 
			 ,suite)))))))
	   
	   
	   ,(if (eq name :lambda)
		
		`func-lambda
	      
	      (with-fresh (func)
		`(let ((,func (make-py-function :name ',(second name)
						:function func-lambda)))
		   
		   ,(let ((art-deco (loop with res = ,func
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
       
(defmacro global-stmt (names)
  ;; GLOBAL statements are already determined and used at the moment a
  ;; FUNCDEF-STMT is handled.
  (unless (get-pydecl :inside-function e)
    (warn "Bogus `global' statement found at top-level (not inside a function)")))

(defmacro identifier-expr (name &environment e)
  
  ;; The identifier is used for its value; it is not an assignent
  ;; target (as the latter case is handled by ASSIGN-EXPR).
  (assert (symbolp name))
  
  (let ((module-lookup (let ((ix (gethash name (get-pydecl :mod-globals-names-ht e))))
			 (if ix
			     `(svref +mod-globals-values+ ix)
			   `(or (gethash ,name +mod-dyn-globals+)
				(py-raise 'NameError "No variable with name ~A" ,name))))))
    (ecase (get-pydecl :context e)
       
      (:module     (if (member name (get-pydecl :lexically-visible-vars e)) ;; in exec-stmt
		       name
		     module-lookup))
	       
      (:function   (if (member name (get-pydecl :lexically-visible-vars e))
		       name
		     module-lookup))
	       
      (:class      `(or (gethash ,name +cls-namespace+)
			,(if (member name (get-pydecl :lexically-visible-vars e))
			     name
			   module-lookup))))))

(defmacro if-stmt (if-clauses else-clause)
  `(cond ,@(loop for (cond body) in if-clauses
	       collect `((py-val->lisp-bool ,cond) ,body))
	 ,@(when else-clause
	     `((t ,else-clause)))))

(defmacro import-stmt ..)

(defmacro import-from-stmt ..)

(defmacro lambda-expr (args expr)
  ;; XXX maybe the resulting LAMBDA results in more code than
  ;; necessary for just one expression.
  `(funcdef-stmt nil :lambda ,args (suite-stmt ((return-stmt ,expr)))))

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


(defmacro module-stmt (items &environment e)
  
  ;; A module is translated into one lambda that creates and returns a
  ;; module object. Executing the lambda will create a module object
  ;; and register it (so other modules can reach it).
  ;; 
  ;; Functions, classes and variables inside the module are available
  ;; as attributes of the module object.
  ;; 
  ;; pydecl: :exec-mod-locals-ht and :exec-mod-globals-ht are
  ;;         hash-tables containing global and local scopes (if inside
  ;;         exec-stmt)
  
  (let* ((ast-globals  (module-items-global-vars items))
	 (exec-globals (get-pydecl :exec-mod-globals-ht e))
	 (exec-locals  (get-pydecl :exec-mod-locals-ht  e))
	 (gv           (union ast-globals exec-globals)))
    
    `(lambda ()
       (let* ((+mod-globals-names+    (make-array ,(length gv) :initial-contents ',gv))
	      (+mod-globals-names-ht+ (let ((ht (make-hash-table :test #'eq)))
					(loop for name in gv and for i from 0
					    do (setf (gethash name ht) i)
					       (assert (eq (aref +mod-globals-names+ i) name)))
					ht))
	      (+mod-globals-values+   (make-array ,(length gv) :initial-element :unbound))
	      (+mod-dyn-globals+ nil) ;; hash-table (symbol -> val) dynamically added mod vars 
	      (+mod+                  (make-module
				       :global-var-names  +mod-globals-names+
				       :global-var-values +mod-globals-values+
				       :namespace module-scope)))
	 

	 ;; Set values of module-level variables with names
	 ;; corresponding to built-in names (like `len')
	 
	 (loop for glob-var-name across +module-global-var-values+ and for i from 0
	     do (setf (aref +module-global-var-values+ i)
		  (or (and exec-globals
			   (gethash glob-var-name exec-globals))
		      (and (builtin-name-p glob-var-name)
			   (builtin-name-value glob-var-name)))))


	 (with-pydecl ((:mod-globals-names ,(make-array (length gv) :initial-contents gv))
		       (:mod-globals-names-ht ,(let ((ht (make-hash-table :test #'eq)))
						 (loop for name in gv and for i from 0
						     do (setf (gethash name ht) i)
							(assert (eq (aref +mod-globals-names+ i) name)))
						 ht))
		       (:lexically-visible-vars ,(and exec-locals
						      (loop for k being the hash-key in exec-locals
							  collect k)))
		       (:context :module))
	   
	   (flet ((.globals. ()
		    (loop
			for glob-name across +mod-globals-names+
			for glob-val  across +mod-globals-values+
			unless (eq glob-val :unbound)
			collect (cons glob-name glob-val) into list
			finally (maphash (lambda (k v) (push (cons k v) list))
					 +mod-dyn-globals+)
				(return (make-py-dict list)))))
	     
	     (let ,(when exec-locals
		     (loop for k being the hash-key in exec-locals using (hash-value v)
			 collect `(,k ,v)))
	       
	       ,@items)))
	   
	 ;; XXX if executing module failed, where to catch error?
	 +mod+))))

(defmacro print-stmt ..)

(defmacro return-stmt (val)
  (if (get-pydecl :inside-function e)
      `(return-from :function-body ,val)
    (py-raise 'SyntaxError "RETURN found outside function")))

(defmacro slice-expr (start stop step)
  `(make-slice ,(or start *None*) ,(or stop *None*) ,(or step *None*)))

(defmacro subscription-expr (item subs)
  `(py-subs ,item ,subs))

(defmacro suite-stmt (stmts)
  `(progn ,@stmts))

(defmacro raise-stmt ..)

(defmacro try-except-stmt (suite except-clauses else-suite)
  ;; Note that the Exception class that an 'except' clause catches, is
  ;; evaluated after an exception is thrown, not earlier; so long as
  ;; there is no exception thrown, it is not evaluated.
  ;; 
  ;; That is also the reason why we can't use handler-case: we don't
  ;; want to unwind for exceptions that are not catched by an
  ;; `except:' clause. Instead, we want to get into the debugger to
  ;; analyze and perhaps resume executions.

  (let ((handler-form 
	 `(lambda (exc)
	    (cond ,@(loop
			for (exc var handler-suite) in except-clauses
			for sublist on except-clauses
				      
			collect
			  (cond ((null exc)
				 (when (cdr sublist)
				   (warn "A bare `except:' shadows next `except' clauses"))
				 `(t (progn ,handler-suite
					    (return-from try-except-stmt nil))))
				
				((eq (car exc) 'tuple-expr)
				 `((some ,@(loop for cls in (second exc)
					       collect `(typep exc ,cls)))
				   (progn ,@(when var `((assign-expr exc (,var))))
					  ,handler-suite
					  (return-from try-except-stmt nil))))
				
				(t
				 `((typep exc ,exc)
				   (progn ,@(when var `((assign-expr exc (,var))))
					  ,handler-suite
					  (return-from try-except-stmt nil))))))))))
    `(tagbody
       
       (handler-bind ((Exception ,handler-form))
	 (progn ,suite
		,@(when else-suite `((go :else)))))
       
       ,@(when else-suite
	   `(:else ,else-suite)))))

(defmacro try-finally-stmt (try-suite finally-suite)
  ;; not IGNORE-ERRORS, as we want to catch Lisp errors
  `(unwind-protect 
       (multiple-value-bind (val exc)
	   (ignore-errors ,try-suite)
	 
	 (when (and (null val)
		    (typep exc 'condition)
		    (not (typep exc 'Exception)))
	   (break "Try/finally: in the TRY block, Lisp condition ~S occured" exc)))
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
	   do (setf ,take-else nil)
	      (tagbody
		(locally (declare (pydecl (:inside-loop t)))
		  ,suite)
	       :continue)
	   finally (when (and ,take-else ,(not (null else-suite)))
		     ,else-suite))
      :break)))

(defmacro yield-stmt (val)
  (break "YIELD found by compiler"))


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
	     (with-sub-ast ((form &key value target) t ast (:walk-lists-only t))
	       (case (car form)
		 
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

	     
		 (global-stmt (dolist (name (second form))
				(cond ((member name params)
				       (error "SyntaxError: function param ~A declared `global'"
					      name))
				      
				      ((or (member name locals) (member name outer-scope))
				       (error "SyntaxError: variable ~A used before being ~
                                          declared `global'" name))
				      
				      (t (pushnew name declared-globals))))
			      (values nil t))
	     
	     
		 (funcdef-stmt  (destructuring-bind (decorators fname args suite) (cdr form)
				  (declare (ignore args suite))

				  (assert (eq (car fname) 'identifier))
				  
				  (when (member fname declared-globals)
				    (error "SyntaxError: inner function name ~A declared global"
					   fname))
				  
				  (pushnew fname locals)
				
				  (loop for deco in decorators do (recurse deco))
				  (loop for (name def-val) in (second args)
				      do (recurse def-val)))
				(values nil t))
		 
		 
		 (classdef-stmt (destructuring-bind
				    ((identifier cname) inheritance suite) (cdr form)
				  (assert (eq identifier 'identifier))
				  
				  ;; The class name is always a local.
				  (when (member cname globals)
				    (error "SyntaxError: class name ~A may not be ~
                                            declared `global'" name))
				  
				  (pushnew cname locals)
				  
				  (loop for inh in in heritance do (recurse inh)))
				(values nil t))
	       
		 (lambda-expr (values nil t)) ;; skip
		 
		 (t form)))))
    
    (recurse ast)
    (values locals declared-globals outer-scope)))


  
(defun funcdef-globals-and-locals (args suite)
  "Given FUNCDEF ARGS and SUITE (or EXPR), return DECLARED-GLOBALS, LOCALS."
  
  (destructuring-bind (linearized-pos-args kwd-args *-arg **-arg) args
    
    (assert (not (some #'listp linearized-pos-args)))
    
    (let ((params (let ((x (append linearized-pos-args (mapcar #'first kwd-arg))))
		    (when *-arg  (push *-arg  x))
		    (when **-arg (push **-arg x))
		    x)))
      
      (multiple-value-bind
	  (locals declared-globals outer-scopes) (ast-vars suite :params params)
	
	(values declared-globals locals)))))


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