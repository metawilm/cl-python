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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A macro for every AST node

(defmacro assert-stmt (break "todo: assert"))

(defmacro assign-expr (val targets &environment e)
  
  (flet ((assign-one (tg)
	   (let ((context (get-pydecl :context e))
		 (kind (car tg)))
	     (ecase kind
	       
	       (identifier-expr
		(let ((module-set
		       (let ((ix (gethash name (get-pydecl :mod-globals-names-ht e))))
			 (if ix
			     `(setf (svref +mod-globals-values+ ,ix) .val.)
			   `(setf (gethash ,name +mod-dyn-globals+) .val.))))
		      (local-set `(setf ,name .val.))
		      (class-set `(setf (gethash ,name +cls-namespace+) .val.)))
		  
		  (ecase context
		    (:module    module-set)
		    (:function  (if (or (member name (get-pydecl :func-globals e))
					(not (member name (get-pydecl :lexically-visible-vars e))))
				    module-set
				  local-set))
		    (:class     (if (member name (get-pydecl :class-globals e))
				    module-set
				  class-set)))))
	       
	       (attributeref-expr
		`(setf ,tg .val.))
	       
	       (subscription-expr
		`(setf ,tg .val.))))))
    	   
    `(let ((.val. ,val))
       ,@(loop for tg in targets
	     collect (assign-one tg)))))

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

(defmacro call-expr (primary args &environment e)
  ;; todo: rework *-arg and **-arg
  
  ;; For complete Python semantics, we should check in every call if
  ;; the function being called is fbuilt-in function EVAL, LOCALS or
  ;; GLOBALS, because they possibly access the variable scope of the
  ;; caller.
  ;; 
  ;; However, as there is no need for calling such a function by an
  ;; alias, for efficiency we'll only check for these special cases if
  ;; the thing being called has name EVAL, LOCALS or GLOBALS.
  
  (if (and (listp primary)
	   (eq (first primary) 'identifier)
	   (member (second primary) '(locals globals eval)))
      
      (ecase (second primary)
	
	(locals `(let ((primary ,primary))
		   (if (eq primary :the-locals-function-TODO)
		       (cond (,(or (first args) (second args))
			      (error "LOCALS() should be called without ~
                                      args (got: ~S)" args))
			     (,(some #'identity args)
			      (warn "Sorry, args *POS and **KW for LOCALS() ~
                                     not allowed (todo) (got: ~S)" args))
			     (t (.locals.)))
		     (py-call primary ,args))))
	
	(globals `(let ((primary ,primary))
		    (if (eq primary :the-globals-function-TODO)
			(cond (,(or (first args) (second args))
			       (error "GLOBALS() should be called without ~
                                       args (got: ~S)" args))
			      (,(some #'identity args)
			       (warn "Sorry, args *POS and **KW for GLOBALS() ~
                                      not allowed (todo) (got: ~S)" args))
			      (t (.globals.)))
		      (py-call primary ,args))))
	
	(eval `(let ((primary ,primary))
		 (if (eq primary :the-eval-function-TODO)
		     (cond (,(and (not (<= 1 (length (first args)) 3))
				  (not (third args)))
			    (error "EVAL() should be called with 1 ~
                                    to 3 positional args (got: ~S)" args))
			   (,(and (<= (length (first args)) 3)
				  (or (third args) (fourth args)))
			    (error "Sorry, args *POS and **KW for EVAL() ~
                                    not allowed (todo) (got: ~S)" args))
			   (t (let ((pos-args (first args)))
				`(py-eval ,(first pos-args)
					  ,(or (second pos-args) `(.globals.))
					  ,(or (third pos-args)  `(.locals.))))))
		   (py-call primary ,args)))))
    
    `(py-call ,primary ,args)))


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
	 (:module    module-del)
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
  `(py-exec ,code ,(or globals `(.globals.)) ,(or locals  `(.locals.))))

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
     
     `(when (and .take-else. ,(not (null else-suite)))
	,else-suite)
    :break))

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
	(loop with lex-vars = (get-pydecl :lexically-visible-vars e)
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
			     
			     (let ,(loop for loc in locals collect `(,loc :unbound)
			       ,destruct-statement
			       
			       (block :function-body
				 
				 ;; The `locals' function causes all local variables to be
				 ;; lexically visible all the time.
				 (flet ((.locals. ()
					  (loop
					      for loc-name in ',local-names
					      for loc-val in (list ,@local-names)
					      unless (eq loc-val :unbound)
					      collect (cons loc-name loc-val) into list
					      finally (return (make-py-dict list)))))
				   
				   (with-pydecl ((:func-globals ',global-names)
						 (:context :function)
						 (:lexically-visible-vars
						  (nconc ,local-names
							 (get-pydecl :lexically-visible-vars e))))
				     
				     ,suite)))))))))
	   
	   ,(unless (eq (second name) :lambda)
	      `(assign-expr .func. (,name)))
	   
	   .func.)))))


(defmacro generator-expr (item for-in/if-clauses)
  (multiple-value-bind (gen-maker-lambda-one-arg initial-source)
      (rewrite-generator-expr-ast `(generator-expr ,item ,for-in/if-clauses))
    `(funcall ,gen-maker-lambda-one-arg ,initial-source)))
       
(defmacro global-stmt (names)
  ;; GLOBAL statements are already determined and used at the moment a
  ;; FUNCDEF-STMT is handled.
  (unless (get-pydecl :inside-function e)
    (warn "Bogus `global' statement found at top-level (not inside a function)")))

(defmacro identifier-expr (name &environment e)
  (assert (symbolp name))
  (let ((module-lookup (let ((ix (gethash name (get-pydecl :mod-globals-names-ht e))))
			 (if ix
			     `(svref +mod-globals-values+ ix)
			   `(or (gethash ,name +mod-dyn-globals+)
				(py-raise 'NameError "No variable with name ~A" ,name))))))
    
    (ecase (get-pydecl :context e)
      (:module     module-lookup)
      (:function   (if (member name (get-pydecl :lexically-visible-vars e))
		       name
		     module-lookup))
      (:class      `(or (gethash ,name +cls-namespace+)
			,module-lookup)))))

(defmacro if-stmt (if-clauses else-clause)
  `(cond ,@(loop for (cond body) in if-clauses
	       collect `((py-val->lisp-bool ,cond) ,body))
	 ,@(when else-clause
	     `((t ,else-clause)))))

(defmacro import-stmt ..)

(defmacro import-from-stmt ..)

(defmacro lambda-expr (args expr)
  ;; XXX represented by regular python functions for now
  `(funcdef-stmt nil (identifier :lambda) ,args (suite-stmt ((return-stmt ,expr)))))

(defmacro listcompr-expr (item for-in/if-clauses)
  `(let ((.vec. (make-array 0 :adjustable t :fill-pointer 0)))
     ,(loop
	  with res = `(vector-push-extend ,item .vec.)
	  for clause in (reverse for-in/if-clauses)
	  do (setf res (ecase (car clause)
			 (list-for-in `(for-in-stmt ,(second clause) ,(third clause) ,res nil))
			 (list-if     `(if-stmt (,(second clause) ,res) nil))))
	  finally (return res))
     (make-py-list .vec.)))

(defmacro list-expr (items)
  `(make-list ,items))

(defmacro module-stmt (items)
  
  ;; A module is translated into one lambda that creates and returns a
  ;; module object. Executing the lambda will create a module object
  ;; and register it (so other modules can reach it).
  ;; 
  ;; Functions, classes and variables inside the module are available
  ;; as attributes of the module object.
  
  
  (let ((gv (module-global-vars items)))
    
    `(lambda ()
       (let* ((+mod-globals-names+    (make-array ,(length gv) :initial-contents ',gv))
	      (+mod-globals-names-ht+ (let ((ht (make-hash-table :test #'eq)))
					(loop for n in gv and for i from 0
					    do (setf (gethash n ht) i)
					       (assert (eq (aref +mod-globals-names+ i) n)))
					ht))
	      (+mod-globals-values+ (make-array ,(length gv) :initial-element :unbound))
	      (+mod-dyn-globals+ nil) ;; hash-table (symbol -> val) dynamically added attr 
	      (+mod+                   (make-module
					   :global-var-names  +mod-globals-names+
					   :global-var-values +mod-globals-values+
					   :namespace module-scope)))
	 
	 ;; Set values of module-level variables with names
	 ;; corresponding to built-in names (like `len')
	 (loop 
	     for glob-var-name across +module-global-var-values+ and for i from 0
	     when (builtin-name-p glob-var-name)
	     do (setf (aref +module-global-var-values+ i) 
		  (builtin-name-value glob-var-name)))

	 (with-pydecl ((:module-global-var-names ,(make-array (length gv) :initial-contents gv))
		       (:mod-globals-names-ht ,(let ((ht (make-hash-table :test #'eq)))
						 (loop for n in gv and for i from 0
						     do (setf (gethash n ht) i)
							(assert (eq (aref +mod-globals-names+ i) n)))
						 ht))
		       (:lexically-visible-vars ())
		       (:context :module))
	   
	   (flet ((.globals. ()
		    (loop
			for glob-name across +mod-globals-names+
			for glob-val across +mod-globals-values+
			unless (eq glob-val :unbound)
			collect (cons glob-name glob-val) into list
			finally (maphash (lambda (k v) (push (cons k v) list))
					 +mod-dyn-globals+)
				(return (make-py-dict list)))))
	     ,@items
	   
	    ;; XXX if executing module failed, where to catch error?
	   +module+))))))

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
    (+ `(let ((.item. ,item))
	  (if (numberp .item.)
	      .item.
	    (py-+ .item.))))
    (- `(let ((.item. ,item))
	  (if (numberp .item.)
	      (- .item.)
	    (py-- .item.))))
    (not `(todo))))

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
	 finally (when (and .take-else. ,(not (null else-suite)))
		   ,else-suite))
    :break))

(defmacro yield-stmt (val)
  (break "YIELD found by compiler"))


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