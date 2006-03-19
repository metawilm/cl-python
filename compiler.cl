(in-package :python)

(declaim (optimize (debug 3)))

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
 


;; The Python compiler uses its own kind of declaration to keep state
;; in generated code. This declaration is named "pydecl".
;; 
;; Currently, the following "pydecl" declaration keys are used:
;;  
;;  :mod-globals-names          : vector of variable names at the module level
;;  :mod-futures                : the features imported form the __future__ module
;;
;;  :context                    : innermost context, one of (:class :module :function)
;;  :context-stack              : list of class and function names, innermost first
;;  
;;  :lexically-declared-globals : list of variable names declared global in an outer scope
;;                                (which makes them also global in inner scopes)
;;  :lexically-visible-vars     : list of variable names that can be closed over
;; 
;;  :inside-loop-p              : T iff inside WHILE of FOR  (to check BREAK, CONTINUE)
;;  :inside-function-p          : T iff inside FUNCDEF       (to check RETURN)

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
  ;; Actually, the new symbols are uninterned fresh symbols with
  ;; the same name, not gensyms.
  `(let ,(loop for x in list
	     collect `(,x ',(make-symbol (symbol-name x))))
     ,@body))



;; The ASSERT-STMT uses the value of *__debug__* to determine whether
;; or not to include the assertion code.
;; 
;; XXX Currently there is not way to set *__debug__* to False.

(defvar *__debug__* t)


;; Modules are kept in a dictionary `sys.modules', mapping from string
;; to module object. For now, we keep a symbol->module hash table.

(defvar *py-modules* (make-hash-table :test #'eq))
    

;; The name of the module now being compiled.

(defvar *current-module-name* "__main__")


;; Compiler debugging and optimization options.
;; XXX not used yet

(defvar *comp-opt*
    (copy-tree '((:include-line-numbers t) ;; XXX include line numbers in AST? not sure its useful
		 (:inline-number-math t)
		 (:inline-fixnum-math nil)))) 

(defun comp-opt-p (x)
  (assert (member x *comp-opt* :key #'car) () "Unknown Python compiler option: ~S" x)
  (second (assoc x *comp-opt*)))

(defun (setf comp-opt) (val x)
  (assert (member x *comp-opt* :key #'car) () "Unknown Python compiler option: ~S" x)
  (setf (second (assoc x *comp-opt*)) val))


(defmacro fast (&body body)
  `(locally (declare (optimize (safety 3) (debug 3)))
     ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;  The macros corresponding to AST nodes

(defmacro assert-stmt (test raise-arg)
  (when *__debug__*
    `(with-simple-restart (:continue "Ignore the assertion failure")
       (unless (py-val->lisp-bool ,test)
	 (py-raise 'AssertionError ,(or raise-arg 
					`(format nil "Failing test: ~A"
						 (with-output-to-string (s)
						   (py-pprint s ',test)))))))))

(defun assign-stmt-list-vals (iterable num-targets)
  (let ((val-list (py-iterate->lisp-list iterable)))
    (unless (= (length val-list) num-targets)
      (py-raise 'ValueError
		"Assignment to several vars: wanted ~A values, but got ~A"
		num-targets (length val-list)))
    val-list))
    
(defmacro assign-stmt (value targets &environment e)

  (when (and (listp value) (member (car value) '(tuple-expr list-expr))
	     (= (length targets) 1) (member (caar targets) '(tuple-expr list-expr))
	     (= (length (second value)) (length (second (car targets)))))

    ;; Shortcut the case "a,b,.. = 1,2,.." where left and right same
    ;; number of items. Note that all RHS values are evaluated before
    ;; assignment to LHS places takes place.
    
    (let* ((value-items (second value))
	   (tg-items    (second (car targets)))
	   (temp-items  (loop for i from 0 below (length value-items)
			    collect (gensym "assign-val"))))
      
      (return-from assign-stmt
	`(let ,(loop for te in temp-items for va in value-items
		   collect `(,te ,va))
	   ,@(loop for te in temp-items for tg in tg-items
		 collect `(assign-stmt ,te (,tg)))))))
  
  
  (let ((context (get-pydecl :context e)))
    (with-gensyms (val)
      
      (flet ((assign-one (tg)
	       (ecase (car tg)
		 
		 (attributeref-expr 
		  (destructuring-bind (item attr) (cdr tg)
		    `(setf-py-attr ,item ',(second attr) ,val)))
		 
		 (subscription-expr
		  (destructuring-bind (item subs) (cdr tg)
		    `(setf-py-subs ,item ,subs ,val)))
		 		  
		 ((list-expr tuple-expr)
		  (let* ((targets (second tg))
			 (num-targets (length targets)))
		    ;; for now KISS
		    `(let ((val-list (assign-stmt-list-vals ,val ,num-targets)))
		       ,@(loop for target in targets
			     collect `(assign-stmt (pop val-list) (,target))))))
		 
		 (identifier-expr
		  (let* ((name (second tg)))
		    (assert (not (eq name '|...|)))
		    
		    (flet ((module-set ()
			     (let ((ix (position name (get-pydecl :mod-globals-names e))))
			       (if ix
				   `(setf (svref +mod-globals-values+ ,ix) ,val)
				 `(setf (gethash ',name +mod-dyn-globals+) ,val))))
			   
			   (local-set () `(setf ,name ,val))
			
			   (class-set () `(setf 
					      (gethash ,(symbol-name name) +cls-namespace+)
					    ,val)))
		    
		      (ecase context
		      
			(:module    (module-set))
		      
			(:function  (if (or (member name (get-pydecl :lexically-declared-globals e))
					    (not (member name (get-pydecl :lexically-visible-vars e))))
					(module-set)
				      (local-set)))
			
			;; Inside a classdef, do not look at lexically visible vars
			(:class     (if (member name (get-pydecl :lexically-declared-globals e))
					(module-set)      
				      (class-set))))))))))
	
	`(let ((,val ,value))
	   ,@(mapcar #'assign-one targets))))))

(defmacro attributeref-expr (item attr)
  `(py-attr ,item ',(second attr)))

(defmacro augassign-stmt (op place val)
  (let ((py-@= (get-binary-iop-func-name op))
	(py-@  (get-binary-op-func-name-from-iop op)))

    ;; py-@= returns t iff __i@@@__ found
    
    ;; Using (function py-+) syntax to enable compiler macros:
    ;; 
    ;; inserting func directly as (funcall ,(function py-+) ..) will
    ;; skip comp macros.

    (ecase (car place)
    
      (tuple-expr (py-raise 'SyntaxError
			    "Augmented assignment to multiple places not possible (got: ~S)"
			    `(,place ,op ,val)))

      (attributeref-expr (destructuring-bind (item attr) (cdr place)
			   `(let* ((ev-prim ,item)
				   (ev-val  ,val)
				   (place-val-now (attributeref-expr ev-prim ,attr)))
			      
			      (or (funcall (function ,py-@=) place-val-now ev-val)
				  (let ((new-val (funcall (function ,py-@)
							  place-val-now ev-val)))
				    (assign-stmt new-val
						 ((attributeref-expr ev-prim ,attr))))))))
			 
      (subscription-expr (destructuring-bind (item sub) (cdr place)
			   `(let* ((ev-prim ,item)
				   (ev-sub  ,sub)
				   (ev-val  ,val)
				   (place-val-now (subscription-expr ev-prim ev-sub)))
			  
			      (or (funcall (function ,py-@=) place-val-now ev-val)
				  (let ((new-val (funcall (function ,py-@)
							  place-val-now ev-val)))
				    (assign-stmt new-val
						 ((subscription-expr ev-prim ev-sub))))))))
      
      (identifier-expr `(let* ((ev-val        ,val)
			       (place-val-now ,place))
			  
			  (or (funcall (function ,py-@=) place-val-now ev-val)
			      (let ((new-val (funcall (function ,py-@)
						      place-val-now ev-val)))
				(assign-stmt new-val (,place)))))))))

(defmacro backticks-expr (item)
  `(py-repr ,item))

(defmacro binary-expr (op left right)
  (let ((py-@ (get-binary-op-func-name op)))
    `(funcall (function ,py-@) ,left ,right)))

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
  (if (get-pydecl :inside-loop-p e)
      `(go :break)
    (py-raise 'SyntaxError "BREAK was found outside loop")))


(defmacro call-expr (primary (pos-args kwd-args *-arg **-arg))
  
  ;; XXX todo: check that key args are after pos args (or in parser?)
  
  ;; For complete Python semantics, we should check for every call if
  ;; the function being called is one of the built-in functions EVAL,
  ;; LOCALS or GLOBALS, because they access the variable scope of the
  ;; _caller_ (which is ugly).
  ;; 
  ;; At the module level, globals() and locals() are equivalent.

  (let ((kw-args (loop for ((i-e key) val) in kwd-args
		     do (assert (eq i-e 'identifier-expr))
		     collect (intern (symbol-name key) :keyword)
		     collect val)))
    
    (if (and (listp primary)
	     (eq (car primary) 'attributeref-expr)
	     (null (or kwd-args *-arg **-arg)))
	
	(destructuring-bind (obj (identifier-expr attr)) (cdr primary)
	  (assert (eq identifier-expr 'identifier-expr))
	  `(py-attr-call ,obj ,attr ,@pos-args))
	
      `(let* ((prim ,primary))
	 
	 
	 (when (call-expr-special-p prim)
	   (setf prim (call-expr-special prim (.locals.) (.globals.))))
	 
	 ,(cond ((or kw-args **-arg)
		 `(call-expr-pos+*+kw+** prim (list ,@pos-args) ,*-arg (list ,@kw-args) ,**-arg))
		
		((and pos-args *-arg)
		 `(call-expr-pos+* prim (list ,@pos-args) ,*-arg))
		
		(*-arg
		 `(call-expr-* prim ,*-arg))
		
		(t
		 `(py-call prim ,@pos-args)))))))

(defun call-expr-pos+*+kw+** (prim pos-args *-arg kw-args **-arg)
  (apply #'py-call prim
	 (nconc pos-args
		(when *-arg (py-iterate->lisp-list *-arg))
		kw-args
		(when **-arg (py-**-mapping->lisp-arg-list **-arg)))))

(defun call-expr-pos+* (prim pos-args *-arg)
  (apply #'py-call
	 prim
	 (nconc pos-args (py-iterate->lisp-list *-arg))))

(defun call-expr-* (prim *-args)
  (apply #'py-call prim (py-iterate->lisp-list *-args)))

(defun call-expr-special (func locals-dict globals-dict)
  (lambda (&rest args)
    
    (cond ((and args
		(member func (load-time-value (list #'pybf:locals #'pybf:globals))))
	   (py-raise 'TypeError
		     "Function ~A must be called without args (got: ~A)" func args))
	  
	  ((eq func (load-time-value #'pybf:locals)) 
	   locals-dict)
	  
	  ((eq func (load-time-value #'pybf:globals))
	   globals-dict)
	  
	  ((eq func (load-time-value #'pybf:eval))
	   (if (or (some #'keywordp args)
		   (not (<= 1 (length args) 3)))
	     
	       (py-raise 'TypeError
			 "Function `eval' must be called with 1 to 3 pos args (got: ~A)" args)
	     
	     (pybf:eval (first args)
			(or (second args) globals-dict)
			(or (third args)  locals-dict)))))))

(defun call-expr-special-p (prim)
  (or (eq prim (load-time-value #'pybf:locals))
      (eq prim (load-time-value #'pybf:globals))
      (eq prim (load-time-value #'pybf:eval))))

(defmacro py-attr-call (prim attr &rest pos-args)
  ;; A method call with only positional args: <prim>.<attr>(p1, p2, .., pi)
  (let ((len (length pos-args)))

    ;; Inline calls to commonly called methods.
    ;; 
    ;; XXX This optimization should be considered for all methods on
    ;; built-in types, by looking for bi-classes that have a method
    ;; (or attribute) <ATTR>.
    
    (multiple-value-bind (test outcome)
	(cond ((and (eq attr 'isspace) (= len 0))
	       (values `(stringp prim) `(py-string.isspace prim)))
	      
	      ((and (eq attr 'append) (= len 1))
	       (values `(vectorp prim) `(py-list.append prim ,@pos-args)))
	      
	      ((and (eq attr 'isalpha) (= len 0))
	       (values `(stringp prim) `(py-string.isalpha prim)))
	      
	      ((and (eq attr 'isalnum) (= len 0))
	       (values `(stringp prim) `(py-string.isalnum prim)))
	      
	      ((and (eq attr 'isdigit) (= len 0))
	       (values `(stringp prim) `(py-string.isdigit prim)))
	      
	      ((and (eq attr 'keys) (= len 0))
	       (values `(hash-table-p prim) `(py-dict.keys prim)))
	      
	      ((and (eq attr 'sort) (= len 0))
	       (values `(vectorp prim) `(py-list.sort prim)))
	      
	      ((and (eq attr 'next) (= len 0))
	       (values `(typep prim ,(find-class 'py-func-iterator))
		       `(py-func-iterator.next prim))))
      
      `(block :call-expr-block
	 (let* ((prim ,prim))
	   
	   ,(when test `(when ,test (return-from :call-expr-block ,outcome)))
	   
	   (let ((prim-attr (py-attr prim ',attr)))
	     
	     (when (call-expr-special-p prim-attr)
	       (setf prim-attr (call-expr-special prim-attr (.locals.) (.globals.))))
	     
	     (py-call prim-attr ,@pos-args)))))))
  


(defmacro classdef-stmt (name inheritance suite &environment e)
  ;; todo: define .locals. containing class vars
  (assert (eq (car name) 'identifier-expr))
  (assert (eq (car inheritance) 'tuple-expr))
  
  (multiple-value-bind (globals locals) (classdef-stmt-suite-globals-locals
					 suite
					 (get-pydecl :lexically-declared-globals e))
    (declare (ignore locals))
    (let* ((cname (second name))
	   (new-context-stack (cons cname (get-pydecl :context-stack e)))
	   (context-cname (intern (format nil "~{~A~^.~}" (reverse new-context-stack))
				  #.*package*)))
      
      (with-gensyms (cls)
	`(let ((+cls-namespace+ (make-dict)))
	   
	   ;; First, run the statements in the body of the class
	   ;; definition. This will fill +cls-namespace+ with the
	   ;; class attributes and methods.
	   
	   (with-pydecl ((:context :class)
			 (:context-stack ,new-context-stack)
			 (:lexically-declared-globals ,(append 
							(get-pydecl :lexically-declared-globals e)
							globals)))

	     ;; Note that the local class variables are not locally visible
	     ;; i.e. they don't extend ":lexically-visible-vars"
	   
	     ,suite)
	   
	   ;; Second, now that +cls-namespace+ is filled, make the
	   ;; class with that as namespace.
	   
	   (let ((,cls (make-py-class :name ',cname
				      :context-name ',context-cname
				      :namespace +cls-namespace+
				      :supers (list ,@(second inheritance))
				      :cls-metaclass (gethash "__metaclass__" +cls-namespace+)
				      :mod-metaclass
				      ,(let ((ix (position '__metaclass__
							   (get-pydecl :mod-globals-names e))))
					 (if ix
					     `(svref +mod-globals-values+ ,ix)
					   `(gethash '__metaclass__ +mod-dyn-globals+))))))
	     (assign-stmt ,cls (,name))))))))

(defmacro comparison-expr (cmp left right)
  (let ((py-@ (get-binary-comparison-func-name cmp)))
    `(funcall (function ,py-@) ,left ,right)))

(defmacro continue-stmt (&environment e)
  (if (get-pydecl :inside-loop-p e)
      `(go :continue)
    (py-raise 'SyntaxError "CONTINUE was found outside loop")))

(defmacro del-stmt (item &environment e)
  (ecase (car item)
    
    (tuple-expr
     `(progn ,@(loop for x in (second item) collect `(del-stmt ,x))))
    
    (subscription-expr
     `(py-del-subs ,@(cdr item))) ;; XXX maybe inline dict case
    
    (attributeref-expr
     (destructuring-bind (object (id-ex attr-name)) (cdr item)
       (assert (eq id-ex 'identifier-expr))
       `(py-del-attr ,object ',attr-name)))
    
    (identifier-expr
     (let* ((name (second item)))
       
       (flet ((module-del ()
		;; reset module-level vars with built-in names to their built-in value
		(let ((ix (position name (get-pydecl :mod-globals-names e))))
		  `(delete-identifier-at-module-level
		    ',name ,ix ,(when (builtin-name-p name)
				  `(builtin-name-value ',name)))))
	      
	      (local-del ()
		`(progn (check-del-bound ',name ,name)
			(setf ,name nil)))
	      
	      (class-del ()
		`(class-del ,(symbol-name name) +cls-namespace+)))
	 
	 (ecase (get-pydecl :context e)
	 
	   (:module   (module-del))
	   
	   (:function (if (or (member name (get-pydecl :lexically-declared-globals e))
			      (not (member name (get-pydecl :lexically-visible-vars e))))
			  (module-del)
			(local-del)))
	   
	   (:class    (if (member name (get-pydecl :class-globals e))
			  (module-del)
			(class-del)))))))))

(defmacro dict-expr (alist)
  `(make-dict-unevaled-list ,alist))


(defmacro exec-stmt (code globals locals &environment e)
  ;; TODO:
  ;;   - allow code object etc as CODE
  ;;   - when code is a constant string, parse it already at compile time etc
  
  ;; An EXEC-STMT is translated into a Python suite containing a
  ;; function definition and a subsequent call of the function.
  
  (let ((context (get-pydecl :context e)))
    
    `(let* ((ast (parse-python-string ,code))
	    (ast-suite (destructuring-bind (module-stmt suite) ast
			 (assert (eq module-stmt 'module-stmt))
			 (assert (eq (car suite) 'suite-stmt))
			 suite))
	      
	    (locals-ht  (convert-to-namespace-ht 
			 ,(or locals
			      (if (eq context :module) `(.globals.) `(.locals.)))))
	      
	    (globals-ht (convert-to-namespace-ht ,(or globals `(.globals.))))
	      
	    (loc-kv-pairs (loop for k being the hash-key in locals-ht
			      using (hash-value val)
			      for k-sym = (py-string->symbol k)
			      collect `(,k-sym ,val)))
	      
	    (lambda-body `(with-module-context (#() #() ,globals-ht)
			    (suite-stmt

			     ;; Create helper function
			     ((funcdef-stmt  
			       nil (identifier-expr exec-stmt-helper-func)
			       (nil nil nil nil)
			       (suite-stmt 

				;; set local variables
				,(loop for (k v) in loc-kv-pairs
				     collect `(assign-stmt ,v ((identifier-expr ,k))))

				;; execute suite
				,ast-suite))
			      
			      ;; Call helper function
			      (call-expr (identifier-expr exec-stmt-helper-func)
					 (nil nil nil nil)))))))
	      
       (warn "EXEC-STMT: lambda-body: ~A" lambda-body)
	 
       (funcall (compile nil `(lambda () ,lambda-body))))))

(defmacro for-in-stmt (target source suite else-suite)
  ;; potential special cases:
  ;;  - <dict>.{items,keys,values}()
  ;;  - constant list/tuple/string
  (with-gensyms (f x)
    `(tagbody
       (let* ((,f (lambda (,x)
		    (assign-stmt ,x (,target))
		    (tagbody 
		      (with-pydecl ((:inside-loop-p t))
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
  ;;
  ;; You can rely on the whole function body being included in
  ;;  (block :function-body ...).
  
  (cond ((keywordp fname)
	 (assert (null decorators)))
	
	((and (listp fname) (eq (car fname) 'identifier-expr))
	 (setf fname (second fname)))
	
	(t (break :unexpected)))
  
  (flet ((lambda-args-and-destruct-form (funcdef-pos-args)
	   ;; Replace "def f( (x,y), z):  .." 
	   ;; by "def f( _tmp , z):  (x,y) = _tmp; ..".
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
    
    (let ((all-arg-names (apply #'funcdef-list-all-arg-names formal-args)))
      
      (multiple-value-bind
	  (lambda-pos-args pos-arg-destruct-form) (lambda-args-and-destruct-form pos-args)
	
	(multiple-value-bind
	    (func-explicit-globals func-locals) (funcdef-stmt-suite-globals-locals
						 suite
						 all-arg-names
						 (get-pydecl :lexically-declared-globals e))
	  
	  (let* ((all-locals-and-arg-names (append all-arg-names func-locals))
		 (new-context-stack (cons fname (get-pydecl :context-stack e)))
		 (context-fname (intern (format nil "~{~A~^.~}" (reverse new-context-stack))
				  #.*package*))
		 
		 (func-lambda
		  `(py-arg-function
		    ,context-fname
		    (,lambda-pos-args 
		     ,(loop for ((nil name) val) in key-args collect `(,name ,val))
		     ,(when *-arg  (second *-arg))
		     ,(when **-arg (second **-arg)))
		    
		    (let ,func-locals
		      
		      ,@(when pos-arg-destruct-form
			  `(,pos-arg-destruct-form))
		      
		      (block :function-body
			
			(flet
			    ,(when (contains-call-p suite)
			       `((.locals. () ;; lambdas and gen-exprs have 'locals()' too
					   (make-locals-dict 
					    ',all-locals-and-arg-names
					    (list ,@all-locals-and-arg-names)))))
			  
			  (with-pydecl ((:lexically-declared-globals  ;; funcdef globals are also valid
					 ,(append func-explicit-globals  ;; for inner functions
						  (get-pydecl :lexically-declared-globals e)))
					
					(:context :function)
					(:inside-function-p t)
					(:lexically-visible-vars
					 ,(append all-locals-and-arg-names
						  (get-pydecl :lexically-visible-vars e)))
					(:context-stack ,new-context-stack))
			    
			    ,(if (generator-ast-p suite)
				 
				 `(return-stmt
				   ,(rewrite-generator-funcdef-suite context-fname suite))
			       
			       `(progn ,suite
				       *the-none*)))))))))
	    
	    (if (keywordp fname)
		
		func-lambda
	      
	      (with-gensyms (undecorated-func)
		(let ((art-deco (loop with res = undecorated-func
				    for deco in (reverse decorators)
				    do (setf res `(call-expr ,deco ((,res) () nil nil)))
				    finally (return res))))
		  
		  `(let ((,undecorated-func (make-py-function :name ',fname
							      :context-name ',context-fname
							      :lambda ,func-lambda)))
		     
		     (assign-stmt ,art-deco ((identifier-expr ,fname)))
		     
		     ;;;;;;;;;
		     ;; 
		     ;; Ugly special-casing:
		     ;;  class C:
		     ;;   def __new__(..):    <-- the __new__ method inside a class
		     ;;      ...                  automatically becomes a 'static-method'
		     ;; 
		     ;; as if this were followed by:
		     ;; 
		     ;;   __new__ = staticmethod(__new__)
		     ;; 
		     ;; XXX check whether this works correctly when user does same explicitly
		     ;; 
		     ,@(when (and (eq (get-pydecl :context e) :class)
				  (eq fname '__new__))
			 
			 `((assign-stmt (call-expr pybt:staticmethod
						   (((identifier-expr ,fname)) nil nil nil))
					((identifier-expr ,fname)))))
		     ;;
		     ;;;;;;;;;
		     
		     (identifier-expr ,fname)))))))))))




(defmacro generator-expr (&whole whole item for-in/if-clauses)
  (declare (ignore item for-in/if-clauses))
  (rewrite-generator-expr-ast whole))
       
(defmacro global-stmt (names &environment e)
  ;; GLOBAL statements are already determined and used at the moment a
  ;; FUNCDEF-STMT is handled.
  ;; XXX global in class def scope?
  (declare (ignore names))
  (unless (get-pydecl :inside-function-p e)
    (warn "Bogus `global' statement found at top-level (not inside a function)")))

(defmacro identifier-expr (name &environment e)
  
  ;; The identifier is used for its value; it is not an assignent
  ;; target (as the latter case is handled by ASSIGN-STMT).
  
  (assert (symbolp name))
  
  (flet ((module-lookup ()
	   (let ((ix (position name (get-pydecl :mod-globals-names e))))
	     `(identifier-expr-module-lookup ',name
					     ,ix
					     ,(when (builtin-name-p name)
						`(builtin-name-value ',name)))))
	 
	 (local-lookup ()
	   `(or ,name
		(check-local-bound ',name ,name))))
    
    (ecase (get-pydecl :context e)

      (:function (if (or (member name (get-pydecl :lexically-declared-globals e))
			 (not (member name (get-pydecl :lexically-visible-vars e))))
		     (module-lookup)
		   (local-lookup)))
		 
      (:module   (module-lookup))
      
      (:class    `(or (gethash ',(symbol-name name) +cls-namespace+)
		      ,(if (member name (get-pydecl :lexically-visible-vars e))
			   (local-lookup)
			 (module-lookup)))))))

(defmacro if-stmt (if-clauses else-clause)
  `(cond ,@(loop for (cond body) in if-clauses
	       collect `((py-val->lisp-bool ,cond) ,body))
	 ,@(when else-clause
	     `((t ,else-clause)))))

(defmacro import-stmt (args)
  `(progn ,@(loop for x in args
		collect (if (eq (first x) 'not-as)
			    
			    (let ((mod-name (second (second x))))
			      `(let ((module-obj (py-import ',mod-name)))
				 (assign-stmt module-obj ((identifier-expr ,mod-name)))))
			
			  `(warn "XXX unsupported import: ~A" ',x)))))
  
(defmacro import-from-stmt (source-name items)
  (if (eq (car source-name) 'identifier-expr)
      
      ;; import names from 1 module: "from x import ..."
      `(let ((module ,source-name))
	 ,@(loop for item in items
	       collect (ecase (car item)
			 (not-as  ;; "from mod import name"
			  (let ((name (second item)))
			    (assert (eq (car name) 'identifier-expr))
			    ;; name = mod.name
			    `(assign-stmt (attributeref-expr module ,name)
					  (,name))))
			 (as ;; "from mod import name-in-mod as name-here"
			  (destructuring-bind 
			      (name-in-mod name-here) (cdr item)
			    (assert (eq (car name-in-mod) 'identifier-expr))
			    (assert (eq (car name-here)   'identifier-expr))
			    ;; name-here = mod.name-in-mod
			    `(assign-stmt (attributeref-expr module ,name-in-mod)
					  (,name-here)))))))
    
    `(error "TODO: dotted import, like:  from x.y import ...")))

(defmacro lambda-expr (args expr)
  ;; XXX Maybe treating lambda as a funcdef-stmt results in way more
  ;; code than necessary for the just one expression it contains.
  
  `(funcdef-stmt nil :lambda ,args (suite-stmt ((return-stmt ,expr)))))
  

(defmacro listcompr-expr (item for-in/if-clauses)
  (with-gensyms (list)
    `(let ((,list ()))
       ,(loop
	    with res = `(push ,item ,list)
	    for clause in (reverse for-in/if-clauses)
	    do (setf res (ecase (car clause)
			   (for-in `(for-in-stmt ,(second clause) ,(third clause) ,res nil))
			   (if     `(if-stmt (,(second clause) ,res) nil))))
	    finally (return res))
       (make-py-list-from-list (nreverse ,list)))))


(defmacro list-expr (items)
  `(make-py-list-unevaled-list ,items))

;; Used by expansions of `module-stmt', `exec-stmt', and by function `eval'.

(defmacro with-this-module-context ((module) &body body)
  (check-type module py-module)
  (with-slots (globals-names globals-values dyn-globals) module
    
    `(with-module-context (,globals-names ,globals-values ,dyn-globals)
       ,@body)))

(defmacro with-module-context ((glob-names glob-values dyn-glob
				&key set-builtins create-return-mod module-name)
			       &body body)
  (check-type glob-names vector)
  (check-type dyn-glob hash-table)
  
  `(let* ((+mod-globals-names+  ,glob-names)
	  (+mod-globals-values+ ,glob-values)
	  (+mod-dyn-globals+    ,dyn-glob)
	  (+mod+ ,(when create-return-mod
		    `(make-module :globals-names  +mod-globals-names+
				  :globals-values +mod-globals-values+
				  :dyn-globals    +mod-dyn-globals+
				  :name ,module-name))))
     (declare (ignorable +mod+))
	      
     ,@(when set-builtins
	 (loop for name across glob-names and i from 0
	     if (builtin-name-p name)
	     collect `(setf (svref +mod-globals-values+ ,i)
			(builtin-name-value ',name)) into setfs
							 
	     else if (eq name '__name__)
	     collect `(setf (svref +mod-globals-values+ ,i) ,(or module-name "__main__"))
	     into setfs
		     
	     else if (eq name '__debug__)
	     collect `(setf (svref +mod-globals-values+ ,i) 1)
	     into setfs
	       
	     finally (when setfs
		       (return `((progn ,@setfs))))))

     (flet ((.globals. () (module-make-globals-dict
			   ;; Updating this dict really modifies the globals.
			   +mod-globals-names+ +mod-globals-values+ +mod-dyn-globals+))
	    
	    (identifier-expr-module-lookup (name ix builtin-value)
	      (excl::fast
	       (if ix
		   (or (svref +mod-globals-values+ ix)
		       (py-raise 'NameError "Variable '~A' is unbound [glob]" name))
		 
		 (or (gethash name +mod-dyn-globals+)
		     builtin-value
		     (py-raise 'NameError
			       "Variable '~A' is unbound [dyn-glob]" name)))))
	    
	    (delete-identifier-at-module-level (name ix builtin-value)
	      (excl::fast
	       (or (if ix
		       (prog1 (svref +mod-globals-values+ ix)
			 (setf (svref +mod-globals-values+ ix) builtin-value))
		     (prog1 (remhash name +mod-dyn-globals+)
		       (when builtin-value
			 (setf (gethash name +mod-dyn-globals+) builtin-value))))
		   (py-raise 'NameError "Cannot delete variable '~A': it is unbound [global]" name))))
	    
	    (check-del-bound (name val)
	      (excl::fast
	       (unless val 
		 (py-raise 'NameError "Cannot delete variable '~A': it is unbound [local]" name))))
	    
	    (check-local-bound (name val)
	      (excl::fast
	       (or val (py-raise 'NameError "Local variable ~A is unbound" name))))
	    
	    (class-del (name cls-namespace)
	      (excl::fast
	       (unless (py-del-subs cls-namespace name)
		 (py-raise 'NameError "Cannot delete variable '~A': it is unbound [class]" name)))))

       
       ;; remove 'unused' warnings
       #'.globals.
       #'identifier-expr-module-lookup

       ;; "locals()" at the module level is equivalent to "globals()"
       
       (macrolet ((.locals. () `(.globals.)))
	 
	 (with-pydecl
	     ((:mod-globals-names  ,glob-names)
	      (:context            :module)
	      (:mod-futures        :todo-parse-module-ast-future-imports))
	   
	   ,@body)))
     
     ,@(when create-return-mod
	 `(+mod+))))



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
  
  (let* ((ast-globals (module-stmt-suite-globals suite)))
    
    `(with-module-context (,(make-array (length ast-globals) :initial-contents ast-globals)
			   (make-array ,(length ast-globals) :initial-element nil) ;; not eval now
			   ,(make-hash-table :test #'eq)
			   :set-builtins t
			   :create-return-mod t
			   :module-name ,*current-module-name*)
       ,suite)))

(defmacro pass-stmt ()
  nil)

(defmacro print-stmt (dest items comma?)
  ;; XXX todo: use methods `write' of `dest' etc
  `(py-print ,dest (list ,@items) ,comma?))

(defmacro return-stmt (val &environment e)
  (if (get-pydecl :inside-function-p e)
      `(return-from :function-body ,(or val *the-none*))
    (py-raise 'SyntaxError "RETURN found outside function")))

(defmacro slice-expr (start stop step)
  `(make-slice ,start ,stop ,step))

(defmacro subscription-expr (item subs)
  `(py-subs ,item ,subs))

(defmacro suite-stmt (stmts)
  (if (null (cdr stmts))
      (car stmts)
    `(progn ,@stmts)))

(defvar *last-raised-exception* nil)

(defun raise-stmt-1 (exc var tb)
  (when tb (warn "Traceback arg to RAISE ignored"))
  
  ;; ERROR does not support _classes_ as first condition argument; it
  ;; must be an _instance_ or condition type _name_.
  (flet ((do-error (e)
	   (setf *last-raised-exception* e)
	   (error e)))
    
    (cond ((stringp (deproxy exc))
	   (break "String exceptions are not supported (got: ~S)" (deproxy exc))
	   (py-raise 'TypeError
		     "String exceptions are not supported (got: ~S)" (deproxy exc)))
	    
	  ((and exc var)
	   (etypecase exc
	     (class  (do-error (make-instance exc :args var)))
	     (error  (progn (warn "RAISE: ignored arg, as exc was already an instance, not a class")
			    (do-error exc)))))
	  (exc
	   (etypecase exc
	     (class    (do-error (make-instance exc)))
	     (error    (do-error exc))))
	  
	  (t
	   (if *last-raised-exception*
	       (error *last-raised-exception*)
	     (py-raise 'ValueError "There is not exception to re-raise (got bare `raise')."))))))
    
(defmacro raise-stmt (exc var tb)
  (when (stringp exc)
    (warn "Encountered unsupported string exception ('raise ~A')" exc))
  `(raise-stmt-1 ,exc ,var ,tb))


(defmacro try-except-stmt (suite except-clauses else-suite)

  ;; The Exception class in a clause is evaluated only after an
  ;; exception is thrown.
  
  (with-gensyms (the-exc)
      
    (flet ((handler->cond-clause (except-clause)
	
	     (destructuring-bind (exc var handler-suite) except-clause
	       (cond ((null exc)
		      `(t (progn ,handler-suite
				 (return-from :try-except-stmt nil))))
		   
		     ((eq (car exc) 'tuple-expr)
		      `((some ,@(loop for cls in (second exc)
				    collect `(typep ,the-exc ,cls)))
			(progn ,@(when var `((assign-stmt ,the-exc (,var))))
			       ,handler-suite
			       (return-from :try-except-stmt nil))))
				
		     (t
		      `((progn (assert (typep ,exc 'class) ()
				 "try/except: except clause should select a class (got: ~A)"
				 ,exc)
			       (typep ,the-exc ,exc))
			(progn ,@(when var `((assign-stmt ,the-exc (,var))))
			       ,handler-suite
			       (return-from :try-except-stmt nil))))))))
    
      (let ((handler-form `(lambda (,the-exc)
			     (declare (ignorable ,the-exc))
			     (cond ,@(mapcar #'handler->cond-clause except-clauses)))))
      
	`(block :try-except-stmt
	   (tagbody
	     (handler-bind ((Exception ,handler-form))
	       
	       (progn (with-py-errors ,suite)
		      ,@(when else-suite `((go :else)))))
	     
	     ,@(when else-suite
		 `(:else ,else-suite))))))))


(defmacro try-finally-stmt (try-suite finally-suite)
  `(unwind-protect
       ,try-suite
     ,finally-suite))

(defmacro tuple-expr (items)
  `(make-tuple-unevaled-list ,items))

(defmacro unary-expr (op item)
  (let ((py-op-func (get-unary-op-func-name op)))
    (assert py-op-func)
    `(funcall (function ,py-op-func) ,item)))

(defmacro while-stmt (test suite else-suite)
  (with-gensyms (take-else)
    `(tagbody
       (loop
	   with ,take-else = t
	   while (py-val->lisp-bool ,test)
	   do (when ,take-else
		(setf ,take-else nil))
	      (tagbody
		(with-pydecl ((:inside-loop-p t))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Helper functions for the compiler
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Detecting names and values of built-ins

(defun builtin-name-p (x)
  (or (find-symbol (string x) (load-time-value (find-package :python-builtin-functions)))
      (find-symbol (string x) (load-time-value (find-package :python-builtin-types)))
      (find-symbol (string x) (load-time-value (find-package :python-builtin-values)))
      (find-symbol (string x) (load-time-value (find-package :python-builtin-clpy)))))

(defun builtin-name-value (x)
  (let ((sym (builtin-name-p x)))
    (assert sym)
    (let ((pkg (symbol-package sym)))
      (cond ((eq pkg (load-time-value (find-package :python-builtin-functions)))
	     (symbol-function sym))
	    ((eq pkg (load-time-value (find-package :python-builtin-types)))
	     (symbol-value sym))
	    ((eq pkg (load-time-value (find-package :python-builtin-values)))
	     (symbol-value sym))
	    ((eq pkg (load-time-value (find-package :python-builtin-clpy)))
	     (symbol-function sym))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Support for introspection: locals() and globals()

(defun make-locals-dict (name-list value-list)
  (make-dict-from-symbol-alist
   (delete nil (mapcar #'cons name-list value-list) :key #'cdr)))

(defun module-make-globals-dict (names-vec values-vec dyn-globals-ht)
  (warn "todo: module-make-globals-dict as proxy")
  (make-dict-from-symbol-alist
   (nconc (loop for name across names-vec and val across values-vec
	      unless (null val) collect (cons name val))
	  (loop for k being the hash-key in dyn-globals-ht using (hash-value v)
	      collect (cons k v)))))

(defgeneric convert-to-namespace-ht (x)

  ;; Convert a Python dict to a namespace, by replacing all string
  ;; keys by corresponding symbols.
  
  (:method ((x hash-table))
	   (loop with d = (make-hash-table :test #'eq) ;; XXX sometimes not needed
	       for k being the hash-key in x using (hash-value v)
	       for k-sym = (typecase k
			     (string (intern k #.*package*))
			     (symbol k)
			     (t (error "Not a valid namespace hash-table,
                                        as key is neither string or symbol: ~A ~A"
				       k (type-of k))))
	       do (setf (gethash k-sym d) v)
	       finally (return d)))
  
  (:method ((x t))
	   (let ((x2 (deproxy x)))
	     (when (eq x x2) (error "invalid namespace: ~A" x))
	     (convert-to-namespace-ht x2))))

(defun py-**-mapping->lisp-arg-list (**-arg)
  ;; Return list: ( |key1| <val1> |key2| <val2> ... )
  (let* ((items-meth (or (recursive-class-lookup-and-bind **-arg 'items)
			 (py-raise 'TypeError
				   "The ** arg in call must be mapping, ~
                                   supporting 'items' (got: ~S)" **-arg)))
	 (items-list (py-iterate->lisp-list (py-call items-meth))))
    
    (loop with res = ()
	for k-v in items-list
	do (let ((k-and-v (py-iterate->lisp-list k-v)))
	     (unless (= (length k-and-v) 2)
	       (py-raise 'TypeError
			 "The ** arg must be list of 2-element tuples (got: ~S)"
			 k-v))
	     (destructuring-bind (k v) k-and-v
	       (push (py-string-val->symbol k) res)
	       (push v res)))
	finally (return res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Detecting the globals and locals of modules, functions and classes

(defun module-stmt-suite-globals (suite)
  "A list of the global variables of the module."

  ;; We make use of the fact that every global variable must be _set_
  ;; sometime: at the toplevel of the module, or in a function or
  ;; classdef.
  ;; 
  ;; The first way, at toplevel, can be detected by looking for the
  ;; variables used at the top level. The latter two (func/class) can
  ;; be detected by looking for the required `global' declaration.
  ;; 
  ;; However, the resulting list of names is an _underestimate_ of the
  ;; total list of global variables in the module, as more can be
  ;; created dynamically from outside the module by another module,
  ;; and also by code in an "exec" stmt in this module.
  
  (declare (optimize (debug 3)))
  (assert (eq (car suite) 'suite-stmt))
  
  (let ((globals ()))
    
    ;; Variables assigned/looked up at module level
    
    (with-py-ast  (form suite)
      (case (car form)

	((classdef-stmt) 
	 ;; name of this class, but don't recurse
	 (destructuring-bind
	     ((identifier-expr cname) inheritance csuite)  (cdr form)
	   (declare (ignore inheritance csuite))
	   (assert (eq identifier-expr 'identifier-expr))
	   (pushnew cname globals))
	 (values nil t))

	(funcdef-stmt
	 ;; name of this function, but don't recurse
	 (destructuring-bind (decorators (identifier-expr fname) args fsuite) (cdr form)
	   (declare (ignore decorators fsuite args))
	   (assert (eq identifier-expr 'identifier-expr))
	   (pushnew fname globals))
	 (values nil t))
	
	(identifier-expr (let ((name (second form)))
			   (pushnew name globals))
			 (values nil t))
	
	(t form)))
    
    ;; Variables explicitly declared `global'

    (with-py-ast (form suite)
      (case (car form)

	(global-stmt (dolist (name (second form))
		       (pushnew name globals))
		     (values nil t))
	
	(t form)))
    
    ;; Every module has some special names predefined
    (dolist (n '(__name__ __debug__))
      (pushnew n globals))
    
    globals))


(defun funcdef-stmt-suite-globals-locals (suite params enclosing-declared-globals)
  "Lists with the locals and globals of the function."
  
  ;; The local variables of a function are those variables that are
  ;; set inside the function.
  
  (declare (optimize (debug 3)))
  (assert (eq (car suite) 'suite-stmt))
  
  (let ((locals ())
	(declared-globals ()))
    
    (with-py-ast ((form &key value target) suite :value t)

      ;; :value, to be sure that is suite contains one expr
      ;; (LAMBDA-EXPR) it is regarded as value
            
      (declare (ignore value))
      
      (case (car form)

	((classdef-stmt funcdef-stmt)  (check-class/func-not-global
					form
					declared-globals enclosing-declared-globals)
				       (values nil t))
	
	(identifier-expr (let ((name (second form)))
			   (when (and target
				      (not (member* name params declared-globals)))
			     (pushnew name locals)))
			 (values nil t))
	
	(global-stmt     (dolist (name (second form))
			   (cond ((member name params)
				  (py-raise 'SyntaxError
					    "Function param `~A' may not be declared ~
                                             `global'" name))
			       
				 ((member name locals)
				  (py-raise 'SyntaxError
					    "The `global' declaration of variable ~
                                             `~A' must be lexically before it is ~
                                             first used." name))
				 
				 (t (pushnew name declared-globals))))
			 (values nil t))
	(t form)))
    
    (values declared-globals locals)))


(defun classdef-stmt-suite-globals-locals (suite enclosing-declared-globals)
  "Lists with the locals and globals of the class."
  
  ;; The local variables of a class are those variables that are set
  ;; inside the class' suite.
  
  (funcdef-stmt-suite-globals-locals suite () enclosing-declared-globals))


(defun check-class/func-not-global (ast &rest globals-lists)
  (ecase (car ast)
    
    (classdef-stmt
     (destructuring-bind
	 ((identifier cname) inheritance csuite)  (cdr ast)
       (declare (ignore inheritance csuite))
       (assert (eq identifier 'identifier-expr))
       (when (apply #'member* cname globals-lists)
	 (py-raise 'SyntaxError
		   "A class name may not be declared `global' (class: '~A')." cname))))
    
    (funcdef-stmt
     (destructuring-bind (decorators (identifier-expr fname) args suite) (cdr ast)
       (declare (ignore decorators suite args))
       (assert (eq identifier-expr 'identifier-expr))
       (when (apply #'member* fname globals-lists)
	 (py-raise 'SyntaxError
		   "SyntaxError: inner function name may not be declared global ~
                    (function: '~A', at ~A)." fname))))))


(defun member* (item &rest lists)
  (dolist (list lists)
    (when (member item list)
      (return-from member* t)))
  nil)

(defun funcdef-list-all-arg-names (pos-args key-args *-arg **-arg)
  (let ((pos-arg-names (loop with todo = pos-args and res = ()
			   while todo
			   do (let ((x (pop todo)))
				(ecase (first x)
				  (identifier-expr (push (second x) res))
				  (tuple-expr      (setf todo (nconc todo (second x))))))
			   finally (return res)))
	 
	(key-arg-names (loop for ((identifier-expr name) nil) in key-args
			   do (assert (eq identifier-expr 'identifier-expr))
			   collect name)))
    
    (nconc pos-arg-names
	   key-arg-names
	   (when *-arg  (list (second *-arg)))
	   (when **-arg (list (second **-arg))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function argument handling

(defun only-pos-args (args)
  "Returns NIL if not only pos args;
Non-negative integer denoting the number of args otherwise."
  (loop with num = 0
      for a in args
      if (symbolp a) return nil ;; regular Python values are never symbols,
      else do (incf num)   ;; so a symbol indicates a key-value, * or ** argument 
      finally (return num)))


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
	 (some-args-p (or pos-args key-args *-arg **-arg))
	 (pos-key-arg-names (nconc (copy-list pos-args) (mapcar #'first key-args)))
	 (key-arg-default-asts (mapcar #'second key-args))
	 (arg-name-vec (make-array num-pos-key-args :initial-contents pos-key-arg-names))
	 
	 (arg-kwname-vec (make-array
			  num-pos-key-args
			  :initial-contents (loop for x across arg-name-vec
						collect (intern x #.(find-package :keyword)))))
    
	 (fa (make-fa :num-pos-args     num-pos-args
		      :num-key-args     num-key-args
		      :num-pos-key-args num-pos-key-args
		      :pos-key-arg-names (make-array (length pos-key-arg-names)
						     :initial-contents pos-key-arg-names)
		      :key-arg-default-vals :py-will-be-filled-at-load-time
		      :arg-name-vec     arg-name-vec
		      :arg-kwname-vec   arg-kwname-vec
		      :*-arg            *-arg
		      :**-arg           **-arg)))
    
    `(progn
       (setf (fa-key-arg-default-vals ,fa)
	 (make-array ,num-key-args :initial-contents (list ,@key-arg-default-asts)))
       
       (excl:named-function ,name
	 
	 (lambda (&rest %args)
	   (declare (dynamic-extent %args)
		    (optimize (safety 3) (debug 3)))
	   
	   (let (,@pos-key-arg-names ,@(when *-arg `(,*-arg)) ,@(when **-arg `(,**-arg))
		 ,@(when (and some-args-p (not *-arg) (not **-arg))
		     `((only-pos-args (only-pos-args %args)))))
	     
	     ;; There are two ways to parse the argument list:
	     ;;    
	     ;; - The pop way, which quickly assigns the variables a
	     ;;   local name (only usable when there are only
	     ;;   positional arguments supplied, and the number of
	     ;;   them is correct);
	     ;;   
	     ;; - The array way, where a temporary array is created
	     ;;   and a arg-parse function is called (used everywhere
	     ;;   else).
	    
	     ,(let ((the-array-way
		     
		     `(let ((arg-val-vec (make-array ,(+ num-pos-key-args
							 (if (or *-arg **-arg) 1 0)
							 (if **-arg 1 0)) :initial-element nil)))
			(declare (dynamic-extent arg-val-vec))
			(parse-py-func-args %args arg-val-vec ,fa)
			
			,@(loop for p in pos-key-arg-names and i from 0
			      collect `(setf ,p (svref arg-val-vec ,i)))
			
			,@(when  *-arg
			    `((setf  ,*-arg (svref arg-val-vec ,num-pos-key-args))))
			    
			,@(when **-arg
			    `((setf ,**-arg (svref arg-val-vec ,(1+ num-pos-key-args)))))))
		    
		    (the-pop-way
		     `(progn ,@(loop for p in pos-key-arg-names collect `(setf ,p (pop %args))))))
		
		(cond ((or *-arg **-arg)  the-array-way)
		      (some-args-p        `(if (or (null only-pos-args)
						   (/= only-pos-args ,num-pos-key-args))
					       ,the-array-way
					     ,the-pop-way))
		      (t `(when %args (error "too many args")))))
	     
	     (locally #+(or)(declare (optimize (safety 3) (debug 3)))
	       ,@body)))))))



(defstruct (func-args (:type vector) (:conc-name fa-) (:constructor make-fa))
  (num-pos-args         :type fixnum :read-only t)
  (num-key-args         :type fixnum :read-only t)
  (num-pos-key-args     :type fixnum :read-only t)
  (pos-key-arg-names    :type vector :read-only t)
  (key-arg-default-vals :type vector :read-only nil) ;; filled at load time
  (arg-name-vec         :type vector :read-only t)
  (arg-kwname-vec       :type vector :read-only t)
  (*-arg                :type symbol :read-only t)
  (**-arg               :type symbol :read-only t))
  

(defun parse-py-func-args (%args arg-val-vec fa)
  (declare (dynamic-extent %args)
	   (optimize (safety 3) (debug 3)))

  ;; %ARGS: the (&rest) list containing pos and ":key val" arguments
  ;; ARG-VAL-VEC: (dynamic extent) vector to store final argument values in
  ;;              => the penultimate item will get *-arg value (if any)
  ;;                 the last item **-arg value (if any)
  ;;                 so ARG-VAL-VEC must be larger than just num-pos-and-key-args! 
  ;; FA: func-args struct
  ;; Returns nothing
  ;; 
  ;; XXX make it raise Python exceptions on errors
  
  (declare (optimize (safety 3) (debug 3))
	   (type list %args))
  
  (let ((num-filled-by-pos-args 0)
	(for-*  ())
	(for-** ()))
    
    (declare (type (integer 0 #.most-positive-fixnum) num-filled-by-pos-args))
    
    ;; Match standard pos-args and *-arg
    
    (loop
	with max-to-fill-with-pos =
	  (the fixnum (if (fa-*-arg fa) (fa-num-pos-args fa) (fa-num-pos-key-args fa)))
	until (or (= num-filled-by-pos-args max-to-fill-with-pos)
		  (symbolp (car %args))) ;; the empty list NIL is a symbol, too
	      
	do (setf (svref arg-val-vec num-filled-by-pos-args) (pop %args))
	   (incf num-filled-by-pos-args)
	   
	finally
	  (unless (symbolp (car %args))
	    (if (fa-*-arg fa)
		
		(setf for-*
		  ;; Reconsing because %args might be dynamic-extent.
		  (loop until (symbolp (car %args)) collect (pop %args)))

	      (error "Too many pos args"))))
    
    ;; All remaining arguments are keyword arguments;
    ;; they have to be matched to the remaining pos and
    ;; key args by name.
    
    (loop
	for key = (pop %args) and val = (pop %args) 
	while key do

	  ;; key is either a symbol |foo| or keyword symbol |:foo|
	  
	  (cond ((> (the fixnum (fa-num-pos-key-args fa)) 0)
		 (loop 
		     with name-vec = (fa-arg-name-vec fa)
		     with kwname-vec = (fa-arg-kwname-vec fa)
				       
		     for i fixnum from num-filled-by-pos-args below 
		       (the fixnum (fa-num-pos-key-args fa))
		       
		     when (or (eq (svref name-vec   i) key) 
			      (eq (svref kwname-vec i) key))
			  
		     do (when (svref arg-val-vec i)
			  (error "Got multiple values (at least once via `key=arg' ~%
                                  for parameter `~A'" (svref name-vec i)))
			(setf (svref arg-val-vec i) val)
			(return)
			
		     finally 
		       (if (fa-**-arg fa)
			   (push (cons key val) for-**)
			 (error "Got unknown keyword arg and no **-arg: ~A ~A"
				key val))))
		
		((fa-**-arg fa)
		 (push (cons key val) for-**))
		
		(t (error "Got unknown keyword arg and no **-arg: ~A ~A" key val))))
    
    ;; Ensure all positional arguments covered
    (loop for i fixnum from num-filled-by-pos-args below (the fixnum
							   (fa-num-pos-args fa))
	unless (svref arg-val-vec i)
	do (error "Positional arg ~A has no value" (svref (fa-arg-name-vec fa) i)))
    
    ;; Use default values for missing keyword arguments
    (loop for i fixnum from (fa-num-pos-args fa) below (the fixnum
							 (fa-num-pos-key-args fa))
	unless (svref arg-val-vec i)
	do (setf (svref arg-val-vec i)
	     (svref (fa-key-arg-default-vals fa) (- i (fa-num-pos-args fa)))))

    (when (fa-*-arg fa)
      (setf (svref arg-val-vec (fa-num-pos-key-args fa))
	(make-tuple-from-list for-*)))

    (when (fa-**-arg fa)
      (setf (svref arg-val-vec (1+ (the fixnum (fa-num-pos-key-args fa))))
	(make-dict-from-symbol-alist for-**))))
  
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exceptions: convert Lisp conditions to Python exceptions

(defparameter *max-py-error-level* 100000) ;; should be >= 1000 later on (for b1.py)
(defvar *with-py-error-level* 0)

(defun check-max-with-py-error-level ()
  (when (> *with-py-error-level* *max-py-error-level*)
    (py-raise 'RuntimeError "Stack overflow (~A)" *max-py-error-level*)))

(defmacro with-py-errors (&body body)
  `(let ((*with-py-error-level* (1+ *with-py-error-level*)))
     (check-max-with-py-error-level)
     
     ;; Using handler-bind, so uncatched errors are shown in precisely
     ;; the context where they occur.
     
     (handler-bind
	 
	 ((division-by-zero (lambda (c) 
			      (declare (ignore c))
			      (py-raise 'ZeroDivisionError
					"Division or modulo by zero")))
	  
	  (storage-condition (lambda (c)
			       (declare (ignore c))
			       (py-raise-runtime-error)))
	  
	  (excl:synchronous-operating-system-signal
	   (lambda (c)
	     (if (string= (simple-condition-format-control c)
			  "~1@<Stack overflow (signal 1000)~:@>")
		 (py-raise 'RuntimeError "Stack overflow")
	       (py-raise 'RuntimeError "Synchronous OS signal: ~A" c))))
	  
	  (excl:interrupt-signal
	   (lambda (c)
	     (let ((args (simple-condition-format-arguments c)))
	       (when (string= (cadr args) "Keyboard interrupt")
		 (py-raise 'KeyboardInterrupt "Keyboard interrupt")))))
       
	  #+(or)
	  (error (lambda (c)
		   (warn "with-py-handlers passed on error: ~A" c))))
       
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;  Generator rewriting

(defun generator-ast-p (ast)
  "Is AST a function definition for a generator?"
  
  ;; Note that LAMBDA-EXPR can't contain (yield) statements
  
  (assert (not (eq (car ast) 'module-stmt)) ()
    "GENERATOR-AST-P called with a MODULE ast.")
  
  (with-py-ast (form ast)
    (case (car form)
      (yield-stmt                   (return-from generator-ast-p t))
      ((classdef-stmt funcdef-stmt) (values nil t))
      (t                            form)))
  
  nil)


(defun contains-call-p (ast)
  (with-py-ast (form ast)
    (case (car form)
      (call-expr                                (return-from contains-call-p t))
      ((classdef-stmt funcdef-stmt lambda-expr) (values nil t))
      (t                                        form)))
  nil)
	

(defun rewrite-generator-funcdef-suite (fname suite)
  ;; Returns the function body
  (assert (symbolp fname))
  (assert (eq (car suite) 'suite-stmt) ()
    "CAR of SUITE must be SUITE-STMT, but got: ~S" (car suite))
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
		  
		  (break-stmt
		   (unless stack (break "BREAK outside loop"))
		   (values `(go ,(cdr (car stack)))
			   t))
		    
		  (continue-stmt
		   (unless stack (break "CONTINUE outside loop"))
		   (values `(go ,(car (car stack)))
			   t))
		  
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
			  
			  (go ,continue-tag) ;; prevent warnings
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
		  
		  (if-stmt
		   
		   ;; Rewriting of the IF-STMT used to be conditional on:
		   ;; 
		   ;;   (generator-ast-p form)
		   ;; 
		   ;; but it turns out that we always need to rewrite,
		   ;; because of, for example:
		   ;; 
		   ;;  def f():
		   ;;    while test:
		   ;;      yield 1
		   ;;        if foo:
		   ;;        continue
		   ;; 
		   ;; where the 'continue' must be rewritten
		   ;; correspondingly to the rewritten 'while'.
		   
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
				     t)))))
		    
		  (return-stmt
		   (when (second form)
		     (break "SyntaxError: Inside generator, RETURN statement may not have ~
                             an argument (got: ~S)" form))
		    
		   ;; From now on, we will always return to this state
		   (values `(generator-finished)
			   t))

		  (suite-stmt
		   (values `(:split ,@(loop for stmt in (second form)
					  collect (walk stmt stack)))
			   t))

		  
		  (try-except-stmt

		   ;; Three possibilities:
		   ;;  1. YIELD-STMT or RETURN-STMT in TRY-SUITE 
		   ;;  2. YIELD-STMT or RETURN-STMT in some EXCEPT-CLAUSES
		   ;;  3. YIELD-STMT or RETURN-STMT in ELSE-SUITE
		   ;; 
		   ;; We rewrite it once completely, such that all
		   ;; cases are covered. Maybe there is more rewriting
		   ;; going on than needed, but it doesn't hurt.
		   
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
				(setf ,gen (get-py-iterate-fun
					    (funcall
					     ,(suite->generator gen-maker try-suite))))
				(setf .state. ,try-tag)
				
				;; yield all values returned by helper function .gen.
				,try-tag
				(try-except-stmt
				 
				 (let ((val (funcall ,gen))) ;; try-suite
				   (case val
				     (:explicit-return (generator-finished))
				     (:implicit-return (go ,else-tag))
				     (t (return-from :function-body val))))
				 
				 ,jumps ;; handlers
				 
				 nil) ;; else-suite
				
				,@exc-bodies
				
				,else-tag
				,@(when else-suite
				    `((:split ,(walk else-suite stack))))
				
				,after-tag
				(setf ,gen nil))
			      t)))))
		  
		  (try-finally
		   (destructuring-bind (try-suite finally-suite) (cdr form)
		     (when (generator-ast-p try-suite)
		       (break "SyntaxError: YIELD is not allowed in the TRY suite of ~
                               a TRY/FINALLY statement (got: ~S)" form))
		     
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
			
			t))))
		  
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
		  
		  (yield-stmt
		   (let ((tag (new-tag :yield)))
		     (values `(:split (setf .state. ,tag)
				      (return-from :function-body ,(second form)) 
				      ,tag)
			     t)))
		  
		  (t (values form
			     t))))
	      :build-result t)))

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
			(raise-StopIteration)
			#+(or)(py-raise 'StopIteration "The generator has finished.")))))))))))))


(defun suite->generator (fname suite)
  (flet ((suite-walker (form &rest context)
	   (declare (ignore context))
	   (case (car form)
	     
	     ((funcdef-stmt classdef-stmt) (values form t))
	     
	     (return-stmt (when (second form)
			    (error "SyntaxError: Inside generator, RETURN ~
                                    statement may not have an argument ~
                                    (got: ~S)" form))
			  
			  (values `(return-from :function-body :explicit-return)
				  t))
	     
	     (t form))))
	     
    `(excl:named-function (:suite->generator ,fname)
       (lambda ()
	 ,(rewrite-generator-funcdef-suite
	   fname
	   `(suite-stmt (,(walk-py-ast suite #'suite-walker :build-result t)
			 (return-from :function-body :implicit-return))))))))

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
      
      (let ((iteration-stuff (loop with res = `(yield-stmt ,item)
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
				       ,iteration-stuff nil))))
	  
	  ((,(third first-for)) nil nil nil))))))

(defun apply-splits (form)
  (cond ((atom form)
	 (values form))
	
	((eq (car form) :split)
	 (values-list (loop for elm in (cdr form)
			  append (multiple-value-list (apply-splits elm)))))
	
	(t (loop for elm in form
	       append (multiple-value-list (apply-splits elm))))))






;; `global' in a class def leaks into the methods within:
;; 
;; def f():
;;   x = 'fl'
;;   class C:
;;     global x
;;     y = x
;;     def m(self):
;;       return x
;;   print C().m()
;;
;; x = 'gl'
;;
;; f()
;; => prints 'fl'


;; When a function defines x as global, for inner
;; functions it's a global too:
;; ---
;; a = 'global'
;; 
;; def f():
;;   a = 'af'
;;   def g():
;;     global a
;;     def h():
;;       print a
;;     return h
;;   return g
;; ---
;; f()()() -> prints 'global', not 'af'

