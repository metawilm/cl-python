;; -*- package: clpython; readtable: py-ast-user-readtable -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython)
(in-syntax *ast-user-readtable*)

(declaim (optimize (debug 3)))

;;; Python compiler

;; Translates a Python module AST into a Lisp function.
;; 
;; Each node in the s-expression returned by the
;; parse-python-{file,string} corresponds to a macro defined below
;; that generates the corresponding Lisp code.
;; 
;; Each such AST node has a name ending in "-expr" or "-stmt", they are
;; in the :clpython.ast.node package.
;; 
;; In the macro expansions, lexical variables that keep context state
;; have a name like +NAME+.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Declarations
;;
;; The Python compiler uses its own set of declaration to keep state
;; in generated code. This declaration is named "pydecl".
;; The declarations are in the form of (:key ...val...) pairs.
;; 
;; Currently, the following "pydecl" declaration keys are used:
;;  
;;  :mod-globals-names          : vector of variable names at the module level
;;  :mod-futures                : the features imported from the __future__ module
;;
;;  :context                    : innermost context, one of (:class :module :function)
;;  :context-stack              : list of class and function names, innermost first;
;;                                starts with :lambda if in anonymous function
;;  
;;  :lexically-declared-globals : list of variable names declared global in an outer scope
;;                                (which makes them also global in inner scopes)
;;  :lexically-visible-vars     : list of variable names that can be closed over
;; 
;;  :inside-loop-p              : T iff inside WHILE of FOR  (to check BREAK, CONTINUE)
;;  :inside-function-p          : T iff inside FUNCDEF       (to check RETURN)
;;
;;  :safe-lex-visible-vars      : list of all variables guaranteed to be lexically visible and bound;
;;                                subset of :LEXICALLY-VISIBLE-VARS.
;;
;;  :inside-setf-py-attr        : T if insides a (setf (py-attr ..) ..) form (to work around Allegro CL
;;                                issues w.r.t. compiler macros and setf forms.

(sys:define-declaration
    pydecl (&rest property-pairs) nil :declare
    (lambda (declaration env)
      (values :declare
	      (cons 'pydecl
		    (nconc (copy-list (cdr declaration))
			   (sys:declaration-information 'pydecl env))))))

(defun get-pydecl (var env)
  (let ((res (second (assoc var (sys:declaration-information 'pydecl env) :test #'eq))))
    (when (and (eq var :context) (null res))
      (warn "Environment info for var ~S is missing; assuming now in global scope." var)
      (setf res :module))
    res))

(defmacro with-pydecl (pairs &body body)
  `(locally (declare (pydecl ,@pairs))
     ,@body))

;; Allegro CL 8.0, during file compilation, sometimes calls CONSTANTP without
;; environment. However, in case of operations on Python variables, the
;; environment is needed to look up what kind of variable we're dealing with.
;; 
;; As a workaround, in those situations the variable macroexpands into a
;; reference to *the-great-unknown*, about which nothing is known --
;; least of all, whether it is a constant...

(defvar *the-great-unknown*)

(defmacro with-gensyms (list &body body)
  `(let ,(loop for x in list
	     collect `(,x (gensym ,(symbol-name x))))
     ,@body))


;;; Compiler optimization and debugging options.

(defvar *allow-indirect-special-call* nil
  "Whether `eval', `locals' and `globals' can be called indirectly, like `x = locals; x()'
When true, call expressions result in more code. It is rare for Python code to require this.")

(defvar *mangle-private-variables-in-class* nil
  "In class definitions, replace __foo by _CLASSNAME__foo, like CPython does")

(defvar *warn-unused-function-vars* t
  "Controls insertion of IGNORABLE declaration around function variables.")

(defmacro with-complete-python-semantics (&body body)
  `(let ((*allow-indirect-special-call* t)
	 (*mangle-private-variables*    t))
     ,@body))

;; Various settings

(defvar *__debug__* t
  "The ASSERT-STMT uses the value of *__debug__* to determine whether
or not to include the assertion code.
 
XXX Currently there is not way to set *__debug__* to False.")

(defvar *current-module-name* "__main__"
  "The name of the module now being compiled; module.__name__ is set to it.")

(defvar *current-module-path* ""
  "The path of the Python file being compiled; saved in module's `filepath' slot.")


(defmacro fast (&body body)
  `(locally (declare (optimize (speed 3) (safety 0)))
     ,@body))

(defconstant +standard-module-globals+ '({__name__} {__debug__})
  "Names of global variables automatically created for every module")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;  The macros corresponding to AST nodes

(defun assert-stmt-1 (test test-ast raise-arg)
  (with-simple-restart (:continue "Ignore the assertion failure")
    (unless (py-val->lisp-bool test)
      (py-raise '{AssertionError} (or raise-arg 
				    (format nil "Failing test: ~A"
					    (with-output-to-string (s)
					      (py-pprint test-ast s))))))))
  
(defmacro [assert-stmt] (test raise-arg)
  (when *__debug__*
    `(assert-stmt-1 ,test ',test ,raise-arg)))

(defun assign-stmt-list-vals (iterable num-targets)
  (let ((val-list (py-iterate->lisp-list iterable)))
    (unless (= (length val-list) num-targets)
      (py-raise '{ValueError}
		"Assignment to several vars: wanted ~A values, but got ~A"
		num-targets (length val-list)))
    val-list))

(defun assign-stmt-get-bound-vars (ass-stmt)
  (destructuring-bind (assign-statement value targets) ass-stmt
    (declare (ignore value))
    (assert (eq assign-statement '[assign-stmt]))
    (let* ((todo targets)
	   (res  ()))
      (loop for x = (pop todo)
	  while x do
	    (ecase (first x)
	      ([attributeref-expr] )
	      ([subscription-expr] )
	      ([identifier-expr]          (push (second x) res))
	      (([list-expr] [tuple-expr]) (setf todo (append todo (second x))))))
      res)))

(defmacro [assign-stmt] (value targets)
  (with-gensyms (assign-val)
    `(let ((,assign-val ,value))
       ,@(loop for tg in targets collect `(setf ,tg ,assign-val)))))

(define-compiler-macro [assign-stmt] (&whole whole value targets &environment e)
  (declare (ignore e))
  (if (and (listp value) (member (car value) '([tuple-expr] [list-expr]))
	   (= (length targets) 1) (member (caar targets) '([tuple-expr] [list-expr]))
	   (= (length (second value)) (length (second (car targets)))))
      
      ;; Shortcut the case "a,b,.. = 1,2,.." where left and right same
      ;; number of items. This saves creation of a tuple for RHS.
      ;; 
      ;; Note that all RHS values are evaluated before assignment to
      ;; LHS places takes place.
      `(psetf ,@(mapcan #'list (second (car targets)) (second value)))
    whole))

(defmacro [attributeref-expr] (item attr)
  (assert (eq (car attr) '[identifier-expr]))
  `(py-attr ,item ',(second attr)))

(define-setf-expander [attributeref-expr] (item attr)
  (assert (eq (car attr) '[identifier-expr]))
  (with-gensyms (prim store)
    (values `(,prim) ;; temps
	    (list item) ;; values
	    `(,store) ;; stores
	    `(with-pydecl ((:inside-setf-py-attr t)) ;; store-form
	       (setf (py-attr ,prim ',(second attr)) ,store))
	    `(py-attr ,prim ',(second attr)) ;; read-form
	    `(with-pydecl ((:inside-setf-py-attr t)) ;; del-form
	       (setf (py-attr ,prim ',(second attr)) nil)))))
    

(defmacro [augassign-stmt] (&whole whole op place val &environment env)
  (case (car place)
    
    (([attributeref-expr] [subscription-expr] [identifier-expr])
     
     (let ((py-@= (get-binary-iop-func-name op))
	   (py-@  (get-binary-op-func-name-from-iop op)))
       (multiple-value-bind (vars vals stores writer reader)
	   (get-setf-expansion place env)
	 (assert (null (cdr stores)))
	 (with-gensyms (place-val-now op-val)
	   `(let* (,@(mapcar #'list vars vals)
		   (,op-val ,val)
		   (,place-val-now ,reader))

	      ;; The @= functions are not defined on numbers and strings.
	      ;; Check for fixnum inline.
	      (or (unless (excl::fixnump ,place-val-now)
		    ;; py-@= returns t iff __i@@@__ found
		    (,py-@= ,place-val-now ,op-val))
		  (let ((,(car stores) (,py-@ ,place-val-now ,op-val)))
		    ,writer)))))))

    (t (py-raise '{SyntaxError} "Invalid augmented assignment: ~A"
		 (py-pprint whole nil)))))

(defmacro [backticks-expr] (item)
  `(py-repr ,item))

(defmacro [binary-expr] (op left right)
  `(,(get-binary-op-func-name op) ,left ,right))

(defmacro [binary-lazy-expr] (op left right)
  (ecase op
    ([or] `(let ((.left ,left))
	     (if (py-val->lisp-bool .left)
		 .left
	       (let ((.right ,right))
		 (if (py-val->lisp-bool .right)
		     .right
		   *the-false*)))))
    
    ([and] `(let ((.left ,left))
	      (if (py-val->lisp-bool .left)
		  ,right
		.left)))))

(defmacro [break-stmt] (&environment e)
  (if (get-pydecl :inside-loop-p e)
      `(go :break)
    (py-raise '{SyntaxError} "BREAK was found outside loop")))

(defvar *special-calls* '({locals} {globals} {eval}))

(defmacro [call-expr] (&whole whole primary all-args &environment e)
  ;; For complete Python semantics, we should check for every call if
  ;; the function being called is one of the built-in functions EVAL,
  ;; LOCALS or GLOBALS, because they access the variable scope of the
  ;; caller.
  ;; 
  ;; As a compromise, by default we only check in case the name is
  ;; literally used, so "x = locals()" will work, while
  ;; "y = locals; y()" will not.
  ;;
  ;; But when *allow-indirect-special-call* is true, all calls
  ;; are checked regardless the primitive's name
  (destructuring-bind (pos-args kwd-args *-arg **-arg)
      all-args
    
    (labels ((%there-are-args ()
	       (cond ((or pos-args kwd-args) `t)
		     ((and *-arg **-arg)     `(or (py-iterate->lisp-list ,*-arg)
						  (py-iterate->lisp-list ,**-arg)))
		     (*-arg                  `(py-iterate->lisp-list ,*-arg))
		     (**-arg                 `(py-iterate->lisp-list ,**-arg))
		     (t                      `nil)))
	     (%pos-args ()
	       `(nconc (list ,@pos-args) ,(when *-arg `(py-iterate->lisp-list ,*-arg))))
	     (%there-are-key-args ()
	       (cond (kwd-args  `t)
		     (**-arg    `(py-iterate->lisp-list ,**-arg))
		     (t         `nil)))
	     (%locals-dict ()
	       (if (get-pydecl :inside-function-p e)
		   `(.locals.)
		  `(create-module-globals-dict)))
	     (%globals-dict ()
	       `(create-module-globals-dict))
	     (%do-maybe-special-call (prim which)
	       `(cond ,@(when (member '{locals} which)
			  `(((eq ,prim (function {locals}))
			     (call-expr-locals ,(%locals-dict) ,(%there-are-args)))))
		      ,@(when (member '{globals} which)
			  `(((eq ,prim (function {globals}))
			     (call-expr-globals ,(%globals-dict) ,(%there-are-args)))))
		      ,@(when (member '{eval} which)
			  `(((eq ,prim (function {eval}))
			     (call-expr-eval ,(%locals-dict) ,(%globals-dict)
					     ,(%pos-args) ,(%there-are-key-args)))))
		      (t (call-expr-1 ,prim ,@(cddr whole))))))
      
      (if (or *allow-indirect-special-call*
	      (and (listp primary)
		   (eq (first primary) '[identifier-expr])
		   (member (second primary) *special-calls*)))
	  `(let* ((.prim ,primary))
	     ,(%do-maybe-special-call '.prim *special-calls*))
	`(call-expr-1 ,@(cdr whole))))))

(defun call-expr-locals (locals-dict args-p)
  (when args-p
    (py-raise '{TypeError} "Built-in function `locals' does not take args."))
  locals-dict)

(defun call-expr-globals (globals-dict args-p)
  (when args-p
    (py-raise '{TypeError} "Built-in function `globals' does not take args."))
  globals-dict)

(defmacro call-expr-1 (primary (pos-args kwd-args *-arg **-arg))
  (let ((kw-args (loop for ((i-e key) val) in kwd-args
		     do (assert (eq i-e '[identifier-expr]))
		     collect (intern (symbol-name key) :keyword)
		     collect val)))
    (cond
     ((or kw-args **-arg)  `(call-expr-pos+*+kw+** ,primary 
						   (list ,@pos-args) ,*-arg
						   (list ,@kw-args) ,**-arg))
     ((and pos-args *-arg) `(call-expr-pos+* ,primary (list ,@pos-args) ,*-arg))
     (*-arg                `(call-expr-* ,primary ,*-arg))
     (t                    `(py-call ,primary ,@pos-args)))))

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

(define-compiler-macro [call-expr] (&whole whole primary (pos-args kwd-args *-arg **-arg))
  (cond ((and (listp primary) 
	      (eq (car primary) '[attributeref-expr])
	      (null (or kwd-args *-arg **-arg)))
	 ;; Optimize x.y( ...), saving allocation of bound method
	 (destructuring-bind (obj (identifier-expr attr)) (cdr primary)
	   (assert (eq identifier-expr '[identifier-expr]))
	   `(py-attr-call ,obj ,attr ,@pos-args)))
	
	((and (listp primary)
	      (eq (first primary) '[call-expr])
	      (equal (second primary) '([identifier-expr] {getattr}))
	      (not (or kwd-args *-arg **-arg))
	      (destructuring-bind (p k s ss)
		  (third primary)
		(and (= 2 (length p))
		     (not (or k s ss)))))
	 ;; Optimize "getattr(x,y)(...)" where getattr(x,y) is a function.
	 ;; This saves allocation of bound method
	 
	 ;; As primary is IDENTIFIER-EXPR, accessing it is side effect-free.
	 `(if (eq ,(second primary) (symbol-function '{getattr}))
	      
	      ,(destructuring-bind ((obj attr) k s ss)
		   (third primary)
		 (declare (ignore k s ss))
		 `(multiple-value-bind (.a .b .c)
		      (getattr-nobind ,obj ,attr nil)
		    (if (eq .a :class-attr)
			(funcall .b .c ,@pos-args)
		      (py-call .a ,@pos-args))))

	    (py-call ,primary ,@pos-args)))
	
	;; XXX todo: Optimize obj.__get__(...)
	(t whole)))

(defmacro py-attr-call (prim attr &rest args)
  ;; A method call with only positional args: <prim>.<attr>(p1, p2, .., pi)
  (if (inlineable-method-p attr args)
      (let ((prim-var (if (multi-eval-safe prim)
			  prim
			(with-gensyms (evaled-prim)
			  evaled-prim))))
	
	(multiple-value-bind (test outcome)
	    (inlineable-method-code prim-var attr args)
	  `(let ,(unless (eq prim-var prim)
		   `((,prim-var ,prim)))
	     (if ,test
		 ,outcome
	       (py-call (py-attr ,prim-var ',attr) ,@args)))))
  
    `(py-call (py-attr ,prim ',attr) ,@args)))
	    

(defmacro [classdef-stmt] (name inheritance suite &environment e)
  ;; todo: define .locals. containing class vars
  (assert (eq (car name) '[identifier-expr]))
  (assert (eq (car inheritance) '[tuple-expr]))
  
  (multiple-value-bind (all-class-locals new-locals class-cumul-declared-globals)
      (classdef-stmt-suite-globals-locals suite (get-pydecl :lexically-declared-globals e))
    (assert (equal new-locals all-class-locals))
    (let* ((cname             (second name))
	   (new-context-stack (cons cname (get-pydecl :context-stack e)))
	   (context-cname     (ensure-user-symbol 
			       (format nil "~{~A~^.~}" (reverse new-context-stack)))))

      (with-gensyms (cls)
	`(let ((new-cls-dict 
		
		;; Need a nested LET, as +cls-namespace+ may not be set when the ASSIGN-STMT
		;; below is executed, as otherwise nested classes don't work.
		(let ((+cls-namespace+ (make-dict)))
		  
		  ;; First, run the statements in the body of the class
		  ;; definition. This will fill +cls-namespace+ with the
		  ;; class attributes and methods.
		  
		  (with-pydecl ((:context :class)
				(:context-stack ,new-context-stack)
				(:lexically-declared-globals
				 ,class-cumul-declared-globals))
		    
		    ;; Note that the local class variables are not locally visible
		    ;; i.e. they don't extend ":lexically-visible-vars"
		    
		    ,(if *mangle-private-variables-in-class*
			(mangle-suite-private-variables cname suite)
		       suite))
		  
		  +cls-namespace+)))
	   
	   ;; Second, now that +cls-namespace+ is filled, make the
	   ;; class with that as namespace.
	   (let ((,cls (make-py-class :name ',cname
				      :context-name ',context-cname
				      :namespace new-cls-dict
				      :supers (list ,@(second inheritance))
				      :cls-metaclass (py-dict-getitem new-cls-dict "__metaclass__")
				      :mod-metaclass
				      ,(let ((ix (position '{__metaclass__}
							   (get-pydecl :mod-globals-names e))))
					 (if ix
					     `(svref +mod-static-globals-values+ ,ix)
					   `(gethash '{__metaclass__} +mod-dyn-globals+))))))

	     ;; See comment for record-source-file at funcdef-stmt
	     (excl:record-source-file ',context-cname :type :type)
	     ,(let ((upcase-sym (ensure-user-symbol (string-upcase context-cname))))
		`(excl:record-source-file ',upcase-sym :type :type))
	     
	     ([assign-stmt] ,cls (,name))))))))

(defun mangle-suite-private-variables (cname suite)
  "Rename all attributes `__foo' to `_CNAME__foo'."
  (declare (ignore cname suite))
  (error "todo"))

(defmacro [comparison-expr] (cmp left right)
  (let ((py-@ (get-binary-comparison-func-name cmp)))
    `(funcall (function ,py-@) ,left ,right)))

(defmacro [continue-stmt] (&environment e)
  (if (get-pydecl :inside-loop-p e)
      `(go :continue)
    (py-raise '{SyntaxError} "CONTINUE was found outside loop")))

(defmacro [del-stmt] (item &environment e)
  (multiple-value-bind (temps values stores store-form read-form del-form)
      (get-setf-expansion item e)
    (declare (ignore stores store-form read-form))
    (assert del-form () "No DEL form for: ~A" item)
    `(let ,(mapcar #'list temps values)
       ,del-form)))

(defmacro [dict-expr] (alist)
  `(make-dict-unevaled-list ,alist))

(defvar *exec-stmt-result-handler* nil)

(defmacro [exec-stmt] (code globals locals &key (allowed-stmts t) &environment e)
  ;; TODO:
  ;;   - allow code object etc as CODE
  ;;   - when code is a constant string, parse it already at compile time etc
  ;;
  ;; An EXEC-STMT is translated into a Python suite containing a
  ;; function definition and a subsequent call of the function.
  ;;
  ;; ALLOWED-STMTS: if T:      allow all statements
  ;;                   a list: allow only those statements
  ;;                   NIL:    allow no statements
  ;;  (not evaluated)
  
  (let ((context (if locals
		     "context-not-needed"
		   (get-pydecl :context e))))
    
    `(let ((ast (parse-python-string ,code)))

       (when (ast-contains-stmt-p ast :allowed-stmts ,allowed-stmts)
	 (py-raise '{TypeError}
		   "No statements allowed in Python code string (got: ~S)" ,code))

       ;; Some statements are valid in a function, but no in an EXEC.
       ;; We catch those here.
       (with-py-ast (form ast :into-nested-namespaces nil)
	 (case (car form)
	   ([return-stmt] (py-raise '{TypeError}
				    "RETURN statement found outside function (in EXEC)."))
	   (t form)))
       
       (let* ((ast-suite (destructuring-bind (module-stmt suite) ast
			   (assert (eq module-stmt '[module-stmt]))
			   (assert (eq (car suite) '[suite-stmt]))
			   suite))
	      (locals-ht  (convert-to-namespace-ht 
			   ,(or locals
				(if (eq context :module) `(create-module-globals-dict) `(.locals.)))))
	      (globals-ht (convert-to-namespace-ht ,(or globals `(create-module-globals-dict))))
	      (loc-kv-pairs (loop for k being the hash-key in (py-dict-hash-table locals-ht)
				using (hash-value val)
				for k-sym = (py-string->symbol k)
				collect (list k-sym val)))
	      (lambda-body `(with-module-context (#() #() (py-dict-hash-table ,globals-ht) :create-mod t)
			      ([suite-stmt]

			       ;; Create helper function
			       (([funcdef-stmt]
				 nil ([identifier-expr] exec-stmt-helper-func)
				 (nil nil nil nil)
				 ([suite-stmt]
				  ((block helper
				     ([suite-stmt]
				      
				      ;; set local variables
				      (,@(loop for (k v) in loc-kv-pairs
					     collect `([assign-stmt] ,v (([identifier-expr] ,k))))
					 
					 ;; execute suite
					 (let ((res ,ast-suite))
					   (when *exec-stmt-result-handler*
					     (funcall *exec-stmt-result-handler* res))
					   (return-from helper res))))))))
				
				;; Call helper function
				([call-expr] ([identifier-expr] exec-stmt-helper-func)
					     (nil nil nil nil)))))))
		       
	 #+(or)(warn "EXEC-STMT: lambda-body: ~A" lambda-body)
	 (let ((func (let ((.parser::*walk-warn-unknown-form* nil))
		       (compile nil `(lambda ()
				       (locally (declare (optimize (debug 3)))
					 ,lambda-body))))))
	   (funcall func))))))

(defun call-expr-eval (locals-dict globals-dict pos-args key-args-p)
  ;; Uses exec-stmt, therefore below it.
  (when (or key-args-p 
	    (not pos-args)
	    (> (length pos-args) 3))
    (py-raise '{TypeError} "Built-in function `eval' takes from 1 to three positional args."))
  (let* ((string (pop pos-args))
	 (glob-d (or (pop pos-args) globals-dict))
	 (loc-d  (or (pop pos-args) locals-dict)))
    
    ;; Make it an EXEC stmt, but be sure to save the result.
    (let* ((res nil)
	   (*exec-stmt-result-handler* (lambda (val) (setf res val))))
      (declare (special *exec-stmt-result-handler*))
      ([exec-stmt] string glob-d loc-d :allowed-stmts '([module-stmt] [suite-stmt]))
      res)))

(defmacro [for-in-stmt] (target source suite else-suite)
  (with-gensyms (f x)
    `(tagbody
       (let* ((,f (lambda (,x)
		    ([assign-stmt] ,x (,target))
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

(defun lambda-args-and-destruct-form (funcdef-pos-args)
  ;; Replace "def f( (x,y), z):  .." 
  ;; by "def f( |(x,y)|, z):  x, y = |(x,y)|; ..".
  (let (nested-vars)
    (labels ((sym-tuple-name (tup)
	       ;; Convert tuple with identifiers to symbol:  (a,(b,c)) -> |(a,(b,c))|
	       ;; Returns the symbol and a list with the "included" symbols (here: a, b and c)
	       (assert (and (listp tup) (eq (first tup) '[tuple-expr])))
	       (labels ((rec (x)
			  (ecase (car x)
			    ([tuple-expr] (format nil "(~{~A~^, ~})"
						  (loop for v in (second x) collect (rec v))))
			    ([identifier-expr] (push (second x) nested-vars)
					       (symbol-name (second x))))))
		 (ensure-user-symbol (rec tup)))))
      (let (lambda-pos-args destructs normal-pos-args)
	(dolist (pa funcdef-pos-args)
	  (ecase (car pa)
	    ([identifier-expr] (let ((name (second pa)))
				 (push name lambda-pos-args)
				 (push name normal-pos-args)))
	    ([tuple-expr] (let ((tuple-var (sym-tuple-name pa)))
			    (push tuple-var lambda-pos-args)
			    (push `([assign-stmt] ,tuple-var (,pa)) destructs)))))
	(values (nreverse lambda-pos-args)
		(when destructs
		  `(progn ,@(nreverse destructs)))
		normal-pos-args
		(nreverse nested-vars))))))

(defun funcdef-globals-locals (suite locals globals)
  "Returns two lists, containing LOCALS and GLOBALS of function. Locals are the
variables assigned to within the function body. Both share tail structure with
input arguments."
  (declare (optimize (debug 3)))
  (assert (eq (car suite) '[suite-stmt]))
  (let (new-locals)
    (with-py-ast ((form &key value target) suite :value t)
      ;; Use :VALUE T, so the one expression for lambda suites is handled correctly.
      (declare (ignore value))
      (case (car form)

	(([classdef-stmt] [funcdef-stmt])
	 (multiple-value-bind (name kind)
	     (ecase (pop form)
	       ([classdef-stmt] (destructuring-bind ((identifier cname) inheritance csuite)
				    form
				  (declare (ignore inheritance csuite))
				  (assert (eq identifier '[identifier-expr]))
				  (values cname "class")))
	       ([funcdef-stmt]  (destructuring-bind (decorators (identifier-expr fname) args suite)
				    form
				  (declare (ignore decorators suite args))
				  (assert (eq identifier-expr '[identifier-expr]))
				  (values fname "function"))))
	   (when (member name globals)
	     (py-raise '{SyntaxError}
		       "The ~A name `~A' may not be declared `global'." kind name)))
	 (values nil t))
	
	([identifier-expr]
	 (let ((name (second form)))
	   (when (and target 
		      (not (member name locals))
		      (not (member name new-locals)))
	     (push name locals)
	     (push name new-locals)))
	 (values nil t))
	
	([global-stmt]
	 (let ((erroneous (intersection (second form) locals :test 'eq)))
	   (when erroneous
	     ;; CPython gives SyntaxWarning, and seems to internally move the `global'
	     ;; declaration before the first use. Let us signal an error; it's easy
	     ;; for the user to fix this.
	     (py-raise '{SyntaxError}
		       "Variable(s) ~{`~A'~^, ~} may not be declared `global'." erroneous)))
	 (setf globals (nconc (second form) globals))
	 (values nil t))
	
	(t form)))
  
    (values locals new-locals globals)))

(defmacro [funcdef-stmt] (decorators
			  fname (pos-args key-args *-arg **-arg)
			  suite
			  &environment e)
  ;; The resulting function is returned.
  ;; 
  ;; If FNAME is a keyword symbol (like :lambda), then an anonymous
  ;; function (like from LAMBDA-EXPR) is created. The function is thus
  ;; not bound to a name. Decorators are not allowed then.
  ;;
  ;; You can rely on the whole function body being included in
  ;;  (block function-body ...).
  
  (cond ((keywordp fname)
	 (assert (null decorators)))
	((and (listp fname) (eq (car fname) '[identifier-expr]))
	 (setf fname (second fname)))
	((break :unexpected) ))
  
  (multiple-value-bind (lambda-pos-args tuples-destruct-form normal-pos-args destruct-nested-vars)
      (lambda-args-and-destruct-form pos-args)
        
    (let ((nontuple-arg-names (append normal-pos-args destruct-nested-vars)))
      (loop for ((identifier-expr name) nil) in key-args
	  do (assert (eq identifier-expr '[identifier-expr]))
	     (push name nontuple-arg-names))
      (when *-arg (push (second *-arg) nontuple-arg-names))
      (when **-arg (push (second **-arg) nontuple-arg-names))

      (multiple-value-bind (all-nontuple-func-locals new-locals func-cumul-declared-globals)
	  (funcdef-globals-locals suite
				  nontuple-arg-names
				  (get-pydecl :lexically-declared-globals e))
	
	(let* ((new-context-stack (cons fname (get-pydecl :context-stack e))) ;; fname can be :lambda
	       (context-fname     (ensure-user-symbol
				   (format nil "~{~A~^.~}" (reverse new-context-stack))))
	       (body-decls       `((:lexically-declared-globals ,func-cumul-declared-globals)
				   (:context :function)
				   (:context-stack ,new-context-stack)
				   (:inside-function-p t)
				   (:lexically-visible-vars
				    ,(append all-nontuple-func-locals
					     (get-pydecl :lexically-visible-vars e)))
				   (:safe-lex-visible-vars
				    (nset-difference
				     (append nontuple-arg-names
					     (get-pydecl :safe-lex-visible-vars e))
				     (ast-deleted-variables suite)))))
	       (func-lambda
		`(py-arg-function
		  ,context-fname
		  (,lambda-pos-args ;; list of symbols
		   ,(loop for ((nil name) val) in key-args collect `(,name ,val))
		   ,(when *-arg  (second *-arg))
		   ,(when **-arg (second **-arg)))
		  
		  (let (,@destruct-nested-vars
			,@new-locals)
		    
		    ,@(unless *warn-unused-function-vars*
			`((declare (ignorable ,@nontuple-arg-names ,@new-locals))))
		    
		    (block function-body
		      (flet
			  (,@(when (funcdef-should-save-locals-p suite)
			       `((.locals. () 
					   ;; lambdas and gen-exprs have 'locals()' too
					   (make-locals-dict 
					    ',all-nontuple-func-locals
					    (list ,@all-nontuple-func-locals))))))
			
			(with-pydecl ,body-decls
			  ,tuples-destruct-form
			  ,(if (generator-ast-p suite)
			       `([return-stmt] ,(rewrite-generator-funcdef-suite
						 context-fname suite))
			     `(progn ,suite
				     *the-none*)))))))))
	  
	  (when (keywordp fname)
	    (return-from [funcdef-stmt] func-lambda))
	  
	  (with-gensyms (undecorated-func)
	    (let ((art-deco undecorated-func))
	      (dolist (x (reverse decorators))
		(setf art-deco `([call-expr] ,x ((,art-deco) () nil nil))))
	      
	      `(let ((,undecorated-func (make-py-function :name ',fname
							  :context-name ',context-fname
							  :lambda ,func-lambda)))
		 
		 ([assign-stmt] ,art-deco (([identifier-expr] ,fname)))
		 
		 ;; Ugly special case:
		 ;;  class C:
		 ;;   def __new__(..):    <-- the __new__ method inside a class
		 ;;      ...                  automatically becomes a 'static-method'
		 ;; XXX check whether this works correctly when user does same explicitly
		 ,@(when (and (eq (get-pydecl :context e) :class)
			      (eq fname '{__new__}))
		     `(([assign-stmt] 
			([call-expr] ([identifier-expr] {staticmethod})
				     ((([identifier-expr] ,fname)) nil nil nil))
			(([identifier-expr] ,fname)))))
		 
		 ;; Make source location known to Allegro, using "fi:lisp-find-definition".
		 ;; Also record upper case version, apparently otherwise lower
		 ;; case names must be |escaped|.
		 (excl:record-source-file ',context-fname :type :operator)
		 ,(let ((upcase-sym (ensure-user-symbol (string-upcase context-fname))))
		    `(excl:record-source-file ',upcase-sym :type :operator))
		 
		 ;; return the function
		 ([identifier-expr] ,fname)))))))))


(defmacro [generator-expr] (&whole whole item for-in/if-clauses)
  (declare (ignore item for-in/if-clauses))
  (rewrite-generator-expr-ast whole))
       
(defmacro [global-stmt] (names &environment e)
  ;; GLOBAL statements are already determined and used at the moment a
  ;; FUNCDEF-STMT is handled.
  (declare (ignore names))
  (unless (get-pydecl :inside-function-p e)
    (warn "Bogus `global' statement found at top-level.")))

(define-setf-expander [identifier-expr] (name &environment e)
  ;; As looking up identifiers is side-effect free, the valuable
  ;; functionality here is the "store form" (fourth value).
  ;; As a bonus the "delete form" is given (sixth value). 
  (let ((glob-ix (position name (get-pydecl :mod-globals-names e))))
    (assert (not (eq name '{...})))
    (with-gensyms (val)

      ;; 1) Store form
      (symbol-macrolet ((module-set
			    (if glob-ix
				`(setf (svref +mod-static-globals-values+ ,glob-ix) ,val)
			      `(setf (gethash ',name +mod-dyn-globals+) ,val)))
			(local-set `(setf ,name ,val))
			(class-set `(setf 
					(this-dict-get +cls-namespace+ ,(symbol-name name))
				      ,val)))
	(let ((store-form
	       (ecase (get-pydecl :context e)
		 (:module    module-set)
		 (:function  (if (or (member name (get-pydecl :lexically-declared-globals e))
				     (not (member name (get-pydecl :lexically-visible-vars e))))
				 module-set
			       local-set))
		 (:class     (if (member name (get-pydecl :lexically-declared-globals e))
				 module-set 
			       class-set))
		 ((nil)      `*the-great-unknown*))))
	  ;; 2) Del form
	  (symbol-macrolet ((module-del
				`(delete-identifier-at-module-level ',name ,glob-ix +mod+))
			    (local-del
				(if (member name (get-pydecl :safe-lex-visible-vars e))
				    `(load-time-value
				      (error "Bug: DEL for lexically safe variable `~A'" name))
				  `(progn (unless ,name
					    (unbound-variable-error ',name))
					  (setf ,name ,(when (builtin-value name)
							 `(builtin-value ',name))))))	      
			    (class-del `(unless (py-del-subs +cls-namespace+ ,name)
					  (unbound-variable-error ',name))))
	    (let ((del-form 
		   (ecase (get-pydecl :context e)
		     (:module   module-del)
		     (:function (if (or (member name (get-pydecl :lexically-declared-globals e))
					(not (member name (get-pydecl :lexically-visible-vars e))))
				    module-del
				  local-del))
		     (:class    (if (member name (get-pydecl :class-globals e))
				    module-del
				  class-del))
		     ((nil)     #+(or)(break "Environment info is missing")
				`*the-great-unknown*))))
	      (values
	       () ;; temps
	       () ;; values
	       (list val) ;; stores
	       store-form
	       `([identifier-expr] ,name) ;; name is literal symbol, thus no side-effect
	       del-form)))))))) ;; bonus

(defmacro [identifier-expr] (name &environment e)
  ;; The identifier is used for its value; it is not an assignent
  ;; target (as the latter case is handled by ASSIGN-STMT).
  (assert (symbolp name) () "Identifier name should be a symbol: ~S" name)
  
  (flet ((module-lookup ()
	   (let ((ix (position name (get-pydecl :mod-globals-names e))))
	     (if ix
		 `(or (fast (svref +mod-static-globals-values+ ,ix))
		      (unbound-variable-error ',name t))
	       `(identifier-expr-module-lookup-dyn ',name +mod-dyn-globals+))))
	 
	 (local-lookup ()
	   (if (member name (get-pydecl :safe-lex-visible-vars e))
	       (progn #+(or)(warn "safe: ~A" name)
		      name)
	     `(or ,name
		  (unbound-variable-error ',name t)))))
    
    (ecase (get-pydecl :context e)
      
      (:function (if (or (member name (get-pydecl :lexically-declared-globals e))
			 (not (member name (get-pydecl :lexically-visible-vars e))))
		     (module-lookup)
		   (local-lookup)))
		 
      (:module   (module-lookup))
      
      (:class    `(or (this-dict-get +cls-namespace+ ',(symbol-name name))
		      ,(if (member name (get-pydecl :lexically-visible-vars e))
			   (local-lookup)
			 (module-lookup))))
      
      ((nil)     (break)
		 `*the-great-unknown*))))

(defmacro [if-stmt] (if-clauses else-clause)
  `(cond ,@(loop for (cond body) in if-clauses
	       collect `((py-val->lisp-bool ,cond) ,body))
	 ,@(when else-clause
	     `((t ,else-clause)))))

(defmacro [import-stmt] (items)
  `(values ,@(loop for (mod-name-as-list bind-name) in items append
		   (loop for m in mod-name-as-list
		       for res = (list m) then (append res (list m)) collect
			 `(let ((module-obj (py-import ',res)))
			    (declare (ignorable module-obj))
			    ,(cond ((= (length res) 1)
				    (if (= 1 (length mod-name-as-list))
					`([assign-stmt] module-obj
							(([identifier-expr] ,(or bind-name
										 (car mod-name-as-list)))))
				      `([assign-stmt] module-obj (([identifier-expr] ,(car mod-name-as-list))))))
				   ((and bind-name (= (length res) (length mod-name-as-list)))
				    `([assign-stmt] module-obj (([identifier-expr] ,bind-name)))))
			    ,(when (equalp res mod-name-as-list)
			       `module-obj))))))

(defmacro [import-from-stmt] (mod-name-as-list items)
  `(let ((mod-obj (py-import ',mod-name-as-list)))
     ,@(if (eq items '[*])

	  `((let ((src-items (py-module-get-items mod-obj :import-* t)))
	     (loop for (k . v) in src-items
		 do (py-module-set-kv +mod+ k v))))
	
	 (loop for (item bind-name) in items
	     collect `([assign-stmt] ([attributeref-expr] mod-obj ([identifier-expr] ,item))
				     (([identifier-expr] ,(or bind-name item))))))))
       
(defmacro [lambda-expr] (args expr)
  ;; XXX Maybe treating lambda as a funcdef-stmt results in way more
  ;; code than necessary for the just one expression it contains.
  
  `([funcdef-stmt] nil :lambda ,args ([suite-stmt] (([return-stmt] ,expr)))))
  
(defmacro [listcompr-expr] (item for-in/if-clauses)
  (with-gensyms (list)
    `(let ((,list ()))
       ,(loop
	    with res = `(push ,item ,list)
	    for clause in (reverse for-in/if-clauses)
	    do (setf res (ecase (car clause)
			   ([for-in-clause] `([for-in-stmt] ,(second clause) ,(third clause) ,res nil))
			   ([if-clause]     `([if-stmt] (,(second clause) ,res) nil))))
	    finally (return res))
       (make-py-list-from-list (nreverse ,list)))))

(defmacro [list-expr] (items)
  `(make-py-list-unevaled-list ,items))

(define-setf-expander [list-expr] (items &environment e)
  (get-setf-expansion `(list/tuple-expr ,items) e))

(defmacro with-this-module-context ((module) &body body)
  ;; Used by REPL
  (check-type module py-module)
  (with-slots (globals-names globals-values dyn-globals) module
    `(with-module-context (,globals-names ,globals-values ,dyn-globals :existing-mod ,module)
       ,@body)))

(defparameter *module-hook* nil)

(defmacro with-module-context ((glob-names glob-values dyn-glob
				&key set-builtins call-hook create-mod existing-mod
				     module-name module-path)
			       &body body)
  (check-type glob-names vector)
  ;;(check-type dyn-glob hash-table)
  (assert (or create-mod existing-mod))
  (assert (not (and create-mod existing-mod)))
  `(progn (in-package :clpython)
	  
	  (let* ((*habitat* (or *habitat* (make-habitat :search-paths '("."))))
		 (+mod-static-globals-names+  ,glob-names)
		 (+mod-static-globals-values+ ,glob-values)
		 (+mod-static-globals-builtin-values+
		  (make-array ,(length glob-names)
			      :initial-contents (mapcar 'builtin-value ',(coerce glob-names 'list))))
		 (+mod-dyn-globals+ ,dyn-glob)
		 (+mod+ ,(if create-mod
			     
			     `(make-module :globals-names  +mod-static-globals-names+
					   :globals-values +mod-static-globals-values+
					   :dyn-globals    +mod-dyn-globals+
					   :name ,module-name
					   :path ,module-path)
			   existing-mod)))

	    (declare (ignorable +mod-static-globals-names+
				+mod-static-globals-values+
				+mod-static-globals-builtin-values+
				+mod-dyn-globals+
				+mod+))
	    
	    (progn ;; Initialize global value arrays
	      ,@(when set-builtins
		  `((replace +mod-static-globals-values+ +mod-static-globals-builtin-values+)))
	      
	      ,@(loop with res
		    for (k v) in `(({__name__}  ,(or module-name "__main__"))
				   ({__debug__}  1))
		    unless (and set-builtins (builtin-value k))
		    do (let ((ix (position k glob-names)))
			 (when ix
			   (push `(setf (svref +mod-static-globals-values+ ,ix) ,v) res)))
		    finally (return res))
	      
	      #+(or) ;; debug
	      (loop for n across +mod-static-globals-names+
		  for v across +mod-static-globals-values+
		  do (format t "~A: ~A~%" n v)))
	      
	    ,@(when call-hook
		`((when *module-hook*
		    (funcall *module-hook* +mod+))))
	    
	    (with-pydecl
		((:mod-globals-names  ,glob-names)
		 (:context            :module)
		 (:mod-futures        :todo-parse-module-ast-future-imports))
	      
	      ,@body))))

(defmacro create-module-globals-dict ()
  ;; Updating this dict really modifies the globals.
  `(module-make-globals-dict +mod+
			     +mod-static-globals-names+ +mod-static-globals-values+ +mod-dyn-globals+))

(defun unbound-variable-error (name &optional resumable)
  (declare (special *py-signal-conditions*))
  (if resumable
      (restart-case
	  (py-raise '{NameError} "Variable '~A' is unbound" name)
	(cl:use-value (val)
	    :report (lambda (stream)
		      (format stream "Enter a value to use for '~A'" name))
	    :interactive (lambda () 
			   (format t "Enter new value for '~A': " name)
			   (multiple-value-list (eval (read))))
	  (return-from unbound-variable-error val)))
    (py-raise '{NameError} "Variable '~A' is unbound" name)))

(defun identifier-expr-module-lookup-dyn (name +mod-dyn-globals+)
  (or (gethash name +mod-dyn-globals+)
      (builtin-value name)
      (unbound-variable-error name t)))

(defun delete-identifier-at-module-level (name ix +mod+)
  ;; Reset module-level vars with built-in names to their built-in value
  (with-slots (globals-names globals-values dyn-globals) +mod+
    (let ((biv (builtin-value name))  ;; maybe NIL
	  (old-val (if ix
		       (svref globals-values ix) 
		     (remhash name dyn-globals))))
      (unless old-val
	(unbound-variable-error name))
      (cond (ix  (setf (svref globals-values ix) biv))
	    (biv (setf (gethash name dyn-globals) biv))
	    (t   (remhash name dyn-globals))))))

(defmacro [module-stmt] (suite) ;; &environment e)
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
			   (make-hash-table :test #'eq)
			   :set-builtins t
			   :create-mod t
			   :call-hook t
			   :module-name *current-module-name* ;; load time
			   :module-path ,*current-module-path*) ;; compile time
       ,suite)))

(defmacro [pass-stmt] ()
  nil)

(defmacro [print-stmt] (dest items comma?)
  ;; XXX todo: use methods `write' of `dest' etc
  `(py-print ,dest (list ,@items) ,comma?))

(defmacro [return-stmt] (val &environment e)
  (if (get-pydecl :inside-function-p e)
      `(return-from function-body ,(or val `(load-time-value *the-none*)))
    (py-raise '{SyntaxError} "RETURN found outside function")))

(defmacro [slice-expr] (start stop step)
  `(make-slice ,start ,stop ,step))

(defmacro [subscription-expr] (item subs)
  `(py-subs ,item ,subs))

(define-setf-expander [subscription-expr] (item subs &environment e)
  (declare (ignore e))
  (with-gensyms (it su store)
    (values `(,it ,su) ;; temps
	    `(,item ,subs) ;; values
	    `(,store) ;; stores
	    `(setf (py-subs ,it ,su) ,store) ;; store-form
	    `(py-subs ,it ,su) ;; read-form
	    `(setf (py-subs ,it ,su) nil)))) ;; del-form

(defmacro [suite-stmt] (stmts)
  (if (null (cdr stmts))
      (car stmts)
    `(progn ,@stmts)))

(define-compiler-macro [suite-stmt] (&whole whole stmts &environment e)
  ;; Insert declarations in the suite body indicating the lexical variables that
  ;; are certainly bound. The compiler will skip checks for bound-ness.
  ;; 
  ;; KISS: if there is a del-stmt somewhere in the suite, skip this optimization.
  ;; This should be more nuanced.
  
  (cond ((and (some (lambda (s)
		      (and (listp s) (eq (car s) '[assign-stmt])))
		    (butlast stmts))
	      (not (ast-deleted-variables whole)))
	 
	 ;; Collect the stmts before the assignment, and those after
	 
	 (multiple-value-bind (before-stmts ass-stmt after-stmts)
	     (loop for sublist on stmts
		 for s = (car sublist)
		 until (and (listp s) (eq (car s) '[assign-stmt]))
		 collect s into before
		 finally (return (values before s (cdr sublist))))
	   #+(or)(warn "bef: ~A  ass: ~A  after: ~A" before-stmts ass-stmt after-stmts)
	   (assert ass-stmt)
	   `(progn ,@(when before-stmts
		       `(([suite-stmt] ,before-stmts))) ;; recursive, but doesn't contain assign-stmt
		   ,ass-stmt
		   ,@(when after-stmts
		       (let ((bound-vars (assign-stmt-get-bound-vars ass-stmt)))
			 `((with-pydecl ((:safe-lex-visible-vars
					  ,(union (set-difference 
						   bound-vars
						   (get-pydecl :lexically-declared-globals e))
						  (get-pydecl :safe-lex-visible-vars e))))
			     ([suite-stmt] ,after-stmts)))))))) ;; recursive, but 1 assign-stmt less
	
	(t whole)))

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
	   (py-raise '{TypeError}
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
	     (py-raise '{ValueError} "There is not exception to re-raise (got bare `raise')."))))))

(defmacro [raise-stmt] (exc var tb)
  (when (stringp exc)
    (warn "Raising string exceptions not supported (got: 'raise ~S')" exc))
  `(raise-stmt-1 ,exc ,var ,tb))

(defparameter *try-except-current-handled-exception* nil)

(defmacro [try-except-stmt] (suite except-clauses else-suite)
  ;; The Exception class in a clause is evaluated only after an
  ;; exception is thrown.
  (with-gensyms (the-exc)
    (flet ((handler->cond-clause (except-clause)
	
	     (destructuring-bind (exc var handler-suite) except-clause

	       ;; Every handler should store the exception, so it can be returned
	       ;; in sys.exc_info().
	       (setq handler-suite
		 `(progn (setf *try-except-current-handled-exception* ,the-exc)
			 ,handler-suite))
	       
	       (cond ((null exc)
		      `(t (progn ,handler-suite
				 (return-from try-except-stmt nil))))
		   
		     ((eq (car exc) '[tuple-expr])
		      `((some ,@(loop for cls in (second exc)
				    collect `(typep ,the-exc ,cls)))
			(progn ,@(when var `(([assign-stmt] ,the-exc (,var))))
			       ,handler-suite
			       (return-from try-except-stmt nil))))
				
		     (t
		      `((progn (assert (typep ,exc 'class) ()
				 "try/except: except clause should select a class (got: ~A)"
				 ,exc)
			       (typep ,the-exc ,exc))
			(progn ,@(when var `(([assign-stmt] ,the-exc (,var))))
			       ,handler-suite
			       (return-from try-except-stmt nil))))))))
    
      (let ((handler-form `(lambda (,the-exc)
			     (declare (ignorable ,the-exc))
			     (cond ,@(mapcar #'handler->cond-clause except-clauses)))))
      
	`(block try-except-stmt
	   (tagbody
	     (handler-bind (({Exception} ,handler-form))
	       
	       (progn (with-py-errors ,suite)
		      ,@(when else-suite `((go :else)))))
	     
	     ,@(when else-suite
		 `(:else ,else-suite))))))))


(defmacro [try-finally-stmt] (try-suite finally-suite)
  `(unwind-protect
       ,try-suite
     ,finally-suite))

(defmacro [tuple-expr] (items)
  `(make-tuple-unevaled-list ,items))

(define-setf-expander [tuple-expr] (items &environment e)
  (get-setf-expansion `(list/tuple-expr ,items) e))

(define-setf-expander list/tuple-expr (items &environment e)
  (with-gensyms (store val-list)
    (values () ;; temps
	    () ;; values
	    (list store)
	    
	    `(let ((,val-list (assign-stmt-list-vals ,store ,(length items))))
	       ,@(mapcar (lambda (it)
			   (multiple-value-bind (temps values stores store-form)
			       (get-setf-expansion it e)
			     (assert (null (cdr stores)))
			     `(let* (,@(mapcar #'list temps values)
				     (,(car stores) (pop ,val-list)))
				,store-form)))
			 items)
	       ,store)
	    
	    'setf-tuple-read-form-unused
	    `(progn ,@(loop for it in items collect `([del-stmt] ,it))))))
  
(defmacro [unary-expr] (op item)
  (let ((py-op-func (get-unary-op-func-name op)))
    (assert py-op-func)
    `(funcall (function ,py-op-func) ,item)))

(defmacro [while-stmt] (test suite else-suite)
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

(defmacro [yield-stmt] (val)
  (declare (ignore val))
  (error "YIELD found outside function"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions for the compiler
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ast-contains-stmt-p (ast &key allowed-stmts)
  (when (eq allowed-stmts t)
    (return-from ast-contains-stmt-p nil))
  (labels ((is-stmt-sym (s)
	     (let ((s.name (symbol-name s)))
	       (cond ((<= (length s.name) 5) nil)
		     ((string-equal (subseq s.name (- (length s.name) 5)) "-stmt") t)
		     (t nil))))
	   
	   (test (ast)
	     (typecase ast
	       (list (loop for x in ast when (test x) return t finally (return nil)))
	       (symbol (unless (member ast allowed-stmts :test #'eq)
			 (when (is-stmt-sym ast)
			   (return-from test t))))
	       (t    nil))))
    (test ast)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Detecting whether we need to use gensyms in order to evaluate a form just
;;; once.

(defun multi-eval-safe (form)
  ;; Can FORM be evaluated multiple times or would that cause side effects?
  ;; Only variable lookup is considered safe.
  (cond ((and (listp form)
	      (= (length form) 2)
	      (eq (car form) '[identifier-expr]))
	 t)
	
	((listp form)
	 nil)
	
	(t t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Detecting names and values of built-ins

(defun builtin-name-p (x)
  (find-symbol (string x) (load-time-value (find-package :clpython.user.builtin))))

(defun builtin-value (x)
  (let ((sym (builtin-name-p x)))
    (or (and (boundp sym) (symbol-value sym))
	(and (fboundp sym) (symbol-function sym))
	(find-class sym nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Inlining of method calls on built-in objects

(defparameter *inlineable-methods* (make-hash-table :test #'eq))

(defun register-inlineable-methods ()
  (clrhash *inlineable-methods*)
  (loop for item in
	'(({isalpha} 0 stringp      py-string.isalpha)
	  ({isalnum} 0 stringp      py-string.isalnum)
	  ({isdigit} 0 stringp      py-string.isdigit)
	  ({islower} 0 stringp      py-string.islower)
	  ({isspace} 0 stringp      py-string.isspace)
	  ({join}    0 stringp      py-string.join   )
	  ({lower}   0 stringp      py-string.lower  )
	  ({strip}   0 stringp      py-string.strip  )
	  ({upper}   0 stringp      py-string.upper  )
	  	  
	  ({keys}    0 py-dict-p    py-dict.keys     )
	  ({items}   0 py-dict-p    py-dict.items    )
	  ({values}  0 py-dict-p    py-dict.values   )
	  	  
	  ({next}    0 py-func-iterator-p py-func-iterator.next)
	  
	  ({read}       (0 . 1) filep    py-file.read      )
	  ({readline}   (0 . 1) filep    py-file.readline  )
	  ({readlines}  (0 . 1) filep    py-file.readlines )
	  ({xreadlines}  0      filep    py-file.xreadlines)
	  ({write}       1      filep    py-file.write  )
	  
	  ({append}      1      vectorp  py-list.append )
	  ({sort}        0      vectorp  py-list.sort   )
	  ({pop}        (0 . 1) vectorp  py-list.pop    ))
	
      do (when (gethash (car item) *inlineable-methods*)
	   (warn "Replacing existing entry in *inlineable-methods* for attr ~A:~% ~A => ~A"
		 (car item) (gethash (car item) *inlineable-methods*) (cdr item)))
	 (setf (gethash (car item) *inlineable-methods*) (cdr item))))

(register-inlineable-methods)

(defun inlineable-method-p (attr args)
  (let ((item (gethash attr *inlineable-methods*)))
    (when item
      (destructuring-bind (req-args check func) 
	  item
	(declare (ignore check func))
	(etypecase req-args
	  (integer (= (length args) req-args))
	  (cons    (= (car req-args) (length args) (cdr req-args))))))))

(defun inlineable-method-code (prim attr args)
  (let ((item (gethash attr *inlineable-methods*)))
    (assert item)
    
    (destructuring-bind (req-args check func) 
	item
      (assert (etypecase req-args
		(integer (= (length args) req-args))
		(cons    (<= (car req-args) (length args) (cdr req-args)))))
      
      (let ((check-code
	     (ecase check
	       ((stringp vectorp) `(,check ,prim))
	       
	       (filep             `(eq (class-of ,prim)
				       (load-time-value (find-class 'py-func-iterator))))
	       
	       (py-dict-p         `(eq (class-of ,prim)
				       (load-time-value (find-class 'py-dict))))
	       
	       (py-func-iterator-p `(eq (class-of ,prim) 
					(load-time-value (find-class 'py-func-iterator))))))
	    
	    (run-code `(,func ,prim ,@args)))
	(values check-code run-code)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Support for introspection: locals() and globals()

(defun make-locals-dict (name-list value-list)
  (make-dict-from-symbol-alist
   (delete nil (mapcar #'cons name-list value-list) :key #'cdr)))

(defun module-make-globals-dict (mod names-vec values-vec dyn-globals-ht)
  (let ((d (make-dict-from-symbol-alist
	    (nconc (loop for name across names-vec and val across values-vec
		       unless (null val) collect (cons name val))
		   (loop for k being the hash-key in dyn-globals-ht using (hash-value v)
		       collect (cons k v))))))
    (change-class d 'py-dict-moduledictproxy :module mod)
    d))

(defgeneric convert-to-namespace-ht (x)
  ;; Convert a Python dict to a namespace, by replacing all string
  ;; keys by corresponding symbols.
  (:method ((x py-dict))
	   (let ((new (make-class-dict)))
	     (loop for k being the hash-key in (py-dict-hash-table x) using (hash-value v)
		 if (typep k '(or string symbol))
		 do (py-dict-setitem new k v)
		 else do (py-raise
			  '{TypeError}
			  "Cannot use ~A as namespace dict, because non-string key present: ~A"
			  x k)
		 finally (return new)))))

(defun py-**-mapping->lisp-arg-list (**-arg)
  ;; Return list: ( :|key1| <val1> :|key2| <val2> ... )
  ;; 
  ;; XXX CPython checks that ** args are unique (also w.r.t. k=v args supplied before it).
  ;;     We catch errors while the called function parses its args.
  (let* ((items-meth (or (recursive-class-lookup-and-bind **-arg '{items})
			 (py-raise '{TypeError}
				   "The ** arg in call must be mapping, ~
                                   supporting 'items' (got: ~S)" **-arg)))
	 (items-list (py-iterate->lisp-list (py-call items-meth))))
    
    (loop with res = ()
	for k-v in items-list
	do (let ((k-and-v (py-iterate->lisp-list k-v)))
	     (unless (= (length k-and-v) 2)
	       (py-raise '{TypeError}
			 "The ** arg must be list of 2-element tuples (got: ~S)"
			 k-v))
	     (destructuring-bind (k v) k-and-v
	       (push v res)
	       (push (intern (py-val->string k) :keyword)
		     res)))
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
  ;; However, the resulting list of names is a subset (underestimate)
  ;; of the total list of global variables in the module, as more can be
  ;; created dynamically from outside the module by another module,
  ;; and also by code in an "exec" stmt in this module.
  
  (declare (optimize (debug 3)))
  (assert (eq (car suite) '[suite-stmt]))
  
  (let ((globals ()))
    
    ;; Variables assigned/looked up at module level
    
    (with-py-ast (form suite)
      (case (car form)

	(([classdef-stmt]) 
	 ;; name of this class, but don't recurse
	 (destructuring-bind
	     ((identifier-expr cname) inheritance csuite)  (cdr form)
	   (declare (ignore inheritance csuite))
	   (assert (eq identifier-expr '[identifier-expr]))
	   (pushnew cname globals))
	 (values nil t))

	([funcdef-stmt]
	 ;; name of this function, but don't recurse
	 (destructuring-bind (decorators (identifier-expr fname) args fsuite) (cdr form)
	   (declare (ignore decorators fsuite args))
	   (assert (eq identifier-expr '[identifier-expr]))
	   (pushnew fname globals))
	 (values nil t))
	
	([identifier-expr] (let ((name (second form)))
			     (pushnew name globals))
			   (values nil t))
	
	(t form)))
    
    ;; Variables explicitly declared `global'

    (with-py-ast (form suite)
      (case (car form)

	([global-stmt] (dolist (name (second form))
			 (pushnew name globals))
		       (values nil t))
	
	(t form)))
    
    ;; Every module has some special names predefined
    (dolist (n '({__name__} {__debug__}))
      (pushnew n globals))
    
    globals))

(defun classdef-stmt-suite-globals-locals (suite enclosing-declared-globals)
  "Lists with the locals and globals of the class."
  ;; The local variables of a class are those variables that are set
  ;; inside the class' suite.
  (funcdef-globals-locals suite () enclosing-declared-globals))

(defun member* (item &rest lists)
  (dolist (list lists)
    (when (member item list)
      (return-from member* t)))
  nil)

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


(defun raise-invalid-key-arg-error (key allowed-keys)
  (py-raise '{TypeError} "Invalid key argument supplied to function: ~S (allowed keys: ~S)"
	    key allowed-keys))

(defun raise-double-arg-supplied-error (key)
  (py-raise '{TypeError} "Duplicate value for function argument named ~A" key))

(defun raise-too-many-pos-args-error ()
  (py-raise '{TypeError} "Too many positional arguments supplied to function"))

(defun raise-too-few-args-error ()
  (py-raise '{TypeError} "Too few arguments supplied to function"))

(defun raise-invalid-func-args-error ()
  (py-raise '{TypeError}
	    "Function got incorrect number of arguments, or unknown keyword arguments"))

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
						collect (intern x :keyword))))
    
	 (fa (make-fa :func-name        name
		      :num-pos-args     num-pos-args
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


(defun py-arg-function-1-kw-test (got-kw want-kw)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cond ((eq got-kw want-kw))
	((keywordp got-kw)   (raise-invalid-key-arg-error got-kw want-kw))
	(t                   (raise-too-many-pos-args-error))))

(defun py-arg-function-2-kw-test (a1 a2 a3 a4 kw1 kw2)
  (declare (optimize (speed 3) (safety 0) (debug 3)))
  ;; A3 is not NIL, or A1 is a symbol
  (cond (a4
	 ;; either:  (a1=kw1) = a2, (a3=kw2) = a4
	 ;;     or:  (a1=kw2) = a2, (a3=kw1) = a4
	 (cond ((and (eq a1 kw1) (eq a3 kw2))
		(values a2 a4))
	       
	       ((and (eq a1 kw2) (eq a3 kw1))
		(values a4 a2))
		  
	       (t (raise-invalid-func-args-error))))
	
	(a3 ;; must be  a1, (a2=kw2) = a3
	 (cond ((eq a2 kw2)
		(values a1 a3))
	       
	       (t (raise-invalid-func-args-error))))
	
	;; a1 is a symbol
	(t (raise-too-few-args-error))))
		  

(define-compiler-macro py-arg-function (&whole whole name (pos-args key-args *-arg **-arg) &body body)
  ;; Special code for "simple" cases, where function only has a few positional args
  
  (or (when (not (or key-args *-arg **-arg))
	(case (length pos-args)
	  
	  (0 ;; No args
	   `(excl:named-function ,name
	      (lambda ()
		,@body)))
      
	  (1 ;; One pos arg
	   (let* ((pa (car pos-args))
		  (ka (intern (symbol-name pa) :keyword))
		  (e  (gensym "e")))
	     
	     `(excl:named-function ,name
		(lambda (,pa &optional ,e)
		  (declare (optimize (speed 3) (safety 1) (debug 0)))
		  (when ,e
		    (py-arg-function-1-kw-test ,pa ,ka)
		    (setf ,pa ,e))
		  
		  ,@body))))
	  
	  (2 ;; Two pos args
	   (destructuring-bind (pa pb)
	       pos-args
	     (let ((ka (intern (symbol-name pa) :keyword))
		   (kb (intern (symbol-name pb) :keyword))
		   (e1 (gensym "e1"))
		   (e2 (gensym "e2")))
	       
	       `(excl:named-function ,name
		  (lambda (,pa ,pb &optional ,e1 ,e2)
		    
		    (when (or ,e1
			      (symbolp ,pa))
		      (multiple-value-setq 
			  (,pa ,pb) 
			(py-arg-function-2-kw-test ,pa ,pb ,e1 ,e2 ,ka ,kb)))
		    
		    ,@body)))))))
      
      ;; Not only pos args, or more than two pos args
      (return-from py-arg-function whole)))



(defstruct (func-args (:type vector) (:conc-name fa-) (:constructor make-fa))
  (num-pos-args         :type fixnum :read-only t)
  (num-key-args         :type fixnum :read-only t)
  (num-pos-key-args     :type fixnum :read-only t)
  (pos-key-arg-names    :type vector :read-only t)
  (key-arg-default-vals :type vector :read-only nil) ;; filled at load time
  (arg-name-vec         :type vector :read-only t)
  (arg-kwname-vec       :type vector :read-only t)
  (*-arg                :type symbol :read-only t)
  (**-arg               :type symbol :read-only t)
  (func-name            :type symbol :read-onle t))
  

(defun parse-py-func-args (%args arg-val-vec fa)
  (declare (dynamic-extent %args)
	   (optimize (speed 3) (safety 1) (debug 0)))

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
	(for-*     ())
	(for-**    ())
	(orig-%args %args))
    
    (declare (type (integer 0 #.most-positive-fixnum) num-filled-by-pos-args))
    
    ;; Match standard pos-args and *-arg
    
    (loop
	with max-to-fill-with-pos =
	  (the fixnum (if (fa-*-arg fa) (fa-num-pos-args fa) (fa-num-pos-key-args fa)))
	until (or (= num-filled-by-pos-args max-to-fill-with-pos)
		  (symbolp (car %args))) ;; the empty list NIL is a symbol, too
	      
	do (setf (svref arg-val-vec num-filled-by-pos-args) (fast (pop %args)))
	   (incf num-filled-by-pos-args)
	   
	finally
	  (unless (symbolp (car %args))
	    (if (fa-*-arg fa)
		
		(setf for-*
		  ;; Reconsing because %args might be dynamic-extent.
		  (loop until (symbolp (car %args)) collect (fast (pop %args))))

	      (py-raise '{TypeError}
			"Function ~A got too many positional args (got ~A, wanted at most ~A)"
			(fa-func-name fa) (length orig-%args) (fa-num-pos-args fa)))))
    
    ;; All remaining arguments are keyword arguments;
    ;; they have to be matched to the remaining pos and
    ;; key args by name.
    
    (loop
	for key = (fast (pop %args)) and val = (fast (pop %args))
	while key do

	  ;; key is either a symbol |foo| or keyword symbol |:foo|
	  
	  (cond ((> (the fixnum (fa-num-pos-key-args fa)) 0)
		 (loop 
		     with name-vec = (fa-arg-name-vec fa)
		     with kwname-vec = (fa-arg-kwname-vec fa)
				       
		     for i fixnum from num-filled-by-pos-args below 
		       (the fixnum (fa-num-pos-key-args fa))
		       
		     when (or (eq (svref kwname-vec i) key))
			  
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
    (loop for i fixnum from num-filled-by-pos-args below (the fixnum (fa-num-pos-args fa))
	unless (svref arg-val-vec i)
	do (error "Positional arg ~A has no value" (svref (fa-arg-name-vec fa) i)))
    
    ;; Use default values for missing keyword arguments
    (loop for i fixnum from (fa-num-pos-args fa) below (the fixnum
							 (fa-num-pos-key-args fa))
	unless (svref arg-val-vec i)
	do (setf (svref arg-val-vec i)
	     (svref (fa-key-arg-default-vals fa) (the fixnum (- i (the fixnum (fa-num-pos-args fa)))))))

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

(defparameter *max-py-error-level* 1000) ;; max number of nested try/except; for b1.py
(defvar *with-py-error-level* 0)

(defun check-max-with-py-error-level ()
  (fast
   (when (> (the fixnum *with-py-error-level*) (the fixnum *max-py-error-level*))
     (py-raise '{RuntimeError} "Stack overflow (~A)" *max-py-error-level*))))

(defmacro with-py-errors (&body body)
  `(let ((*with-py-error-level* (fast (1+ (the fixnum *with-py-error-level*)))))
     (check-max-with-py-error-level)
     
     ;; Using handler-bind, so uncatched errors are shown in precisely
     ;; the context where they occur.
     
     (handler-bind
	 
	 ((division-by-zero (lambda (c) 
			      (declare (ignore c))
			      (py-raise '{ZeroDivisionError}
					"Division or modulo by zero")))
	  
	  (storage-condition (lambda (c)
			       (declare (ignore c))
			       (py-raise-runtime-error)))
	  
	  (excl:synchronous-operating-system-signal
	   (lambda (c)
	     (if (string= (simple-condition-format-control c)
			  "~1@<Stack overflow (signal 1000)~:@>")
		 (py-raise '{RuntimeError} "Stack overflow")
	       (py-raise '{RuntimeError} "Synchronous OS signal: ~A" c))))
	  
	  (excl:interrupt-signal
	   (lambda (c)
	     (let ((args (simple-condition-format-arguments c)))
	       (when (string= (cadr args) "Keyboard interrupt")
		 (py-raise '{KeyboardInterrupt} "Keyboard interrupt")))))
       
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
  
  (assert (not (eq (car ast) '[module-stmt])) ()
    "GENERATOR-AST-P called with a MODULE ast.")
  
  (with-py-ast (form ast)
    (case (car form)
      ([yield-stmt]                     (return-from generator-ast-p t))
      (([classdef-stmt] [funcdef-stmt]) (values nil t))
      (t                                form)))
  
  nil)

(defun ast-deleted-variables (ast)
  "Is there a DEL statement in the AST? If so, returns a list of all
symbol names which are deleted. (Some compiler optimizations are possible
in the absence of DEL statements, as then variables can be guaranteed to
be bound."
  (let (deleted-names)
    (with-py-ast ((form &key value target) ast)
      (case (car form)
	([identifier-expr] (when (eq target +delete-target+)
			     (assert (not value))
			     (push (second form) deleted-names))
			   form)
	(t form)))
    deleted-names))
  

(defun funcdef-should-save-locals-p (ast)
  (or (func-ast-contains-locals-call ast)
      (func-ast-contains-globals-call ast)
      (func-ast-contains-exec ast)))
       
(defun func-ast-contains-locals-call (ast)
  (with-py-ast (form ast)
    (case (car form)
      ([call-expr] 
       (destructuring-bind (primary (pos-args kwd-args *-arg **-arg))
	   (cdr form)
	 (when (and (listp primary)
		    (eq (first primary) '[identifier-expr])
		    (eq (second primary) '{locals})
		    (null (or pos-args kwd-args *-arg **-arg)))
	   (return-from func-ast-contains-locals-call t)))
       form)
      
      (([classdef-stmt] [funcdef-stmt] [lambda-expr])
       (values nil t))
      
      (t
       form)))
  nil)

(defun func-ast-contains-globals-call (ast)
  (with-py-ast (form ast)
    (case (car form)
      ([call-expr] 
       (destructuring-bind (primary (pos-args kwd-args *-arg **-arg))
	   (cdr form)
	 (when (and (listp primary)
		    (eq (first primary) '[identifier-expr])
		    (eq (second primary) '{globals})
		    (null (or pos-args kwd-args *-arg **-arg)))
	   (return-from func-ast-contains-globals-call t)))
       form)
      
      (([classdef-stmt] [funcdef-stmt] [lambda-expr])
       (values nil t))
      
      (t
       form)))
  nil)

(defun func-ast-contains-exec (ast)
  (with-py-ast (form ast)
    (case (car form)
      ([exec-stmt]
       (return-from func-ast-contains-exec t))
      
      (([classdef-stmt] [funcdef-stmt] [lambda-expr])
       (values nil t))
      
      (t
       form)))
  nil)

(defun rewrite-generator-funcdef-suite (fname suite)
  ;; Returns the function body
  (assert (symbolp fname))
  (assert (eq (car suite) '[suite-stmt]) ()
    "CAR of SUITE must be SUITE-STMT, but got: ~S" (car suite))
  (assert (generator-ast-p suite))

  (let ((yield-counter 0)
	(other-counter 0)
	(vars ()))
    
    (flet ((new-tag (kind) (if (eq kind :yield)
			       (incf yield-counter)
			     (make-symbol (format nil "~A~A" kind (incf other-counter)))))
	   (add-suites (list)
	     ;; group multiple non-symbols s as (suite-stmt s1 s2 ...)
	     (let ((res      ())
		   (non-tags ()))
	       
	       (dolist (x list)
		 (if (listp x)
		     (push x non-tags)
		   (progn
		     (when non-tags
		       (push `([suite-stmt] ,(nreverse non-tags)) res)
		       (setf non-tags nil))
		     (push x res))))
	       
	       (when non-tags
		 (push `([suite-stmt] ,(nreverse non-tags)) res))
	       
	       (nreverse res))))
      
      (labels
	  ((walk (form stack)
	     (walk-py-ast
	      form
	      (lambda (form &rest context)
		(declare (ignore context))
		(case (first form)
		  
		  ([break-stmt]
		   (unless stack (break "BREAK outside loop"))
		   (values `(go ,(cdr (car stack)))
			   t))
		    
		  ([continue-stmt]
		   (unless stack (break "CONTINUE outside loop"))
		   (values `(go ,(car (car stack)))
			   t))
		  
		  ([for-in-stmt]
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
			  #+(or)(warn "type of for-in in gen: ~A" (class-of ,source))
			  (setf ,generator (get-py-iterate-fun ,source)
				,loop-var  (funcall ,generator))
			  (unless ,loop-var (go ,else-tag))
			  
			  ,repeat-tag
			  ([assign-stmt] ,loop-var (,target))
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
		  
		  ([if-stmt]
		   
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
		   ;;      if foo:
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
		    
		  ([return-stmt]
		   (when (second form)
		     (py-raise '{SyntaxError}
			       "Inside generator, RETURN statement may not have ~
                                an argument (got: ~S)" form))
		    
		   ;; From now on, we will always return to this state
		   (values `(generator-finished)
			   t))

		  ([suite-stmt]
		   (values `(:split ,@(loop for stmt in (second form)
					  collect (walk stmt stack)))
			   t))

		  
		  ([try-except-stmt]

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
			 with gen-maker = (gensym "helper-gen-maker") and gen = (gensym "helper-gen")
									
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
				([try-except-stmt]
				 
				 (let ((val (funcall ,gen))) ;; try-suite
				   (case val
				     (:explicit-return (generator-finished))
				     (:implicit-return (go ,else-tag))
				     (t (return-from function-body val))))
				 
				 ,jumps ;; handlers
				 
				 nil) ;; else-suite
				
				,@exc-bodies
				
				,else-tag
				,@(when else-suite
				    `((:split ,(walk else-suite stack))))
				
				,after-tag
				(setf ,gen nil))
			      t)))))
		  
		  ([try-finally-stmt]
		   (destructuring-bind (try-suite finally-suite) (cdr form)
		     (when (generator-ast-p try-suite)
		       (py-raise '{SyntaxError}
				 "YIELD is not allowed in the TRY suite of ~
                                  a TRY/FINALLY statement (got: ~S)" form))
		     
		     (let ((fin-catched-exp (gensym "fin-catched-exc")))
		       
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
		  
		  ([while-stmt]
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
		  
		  ([yield-stmt]
		   (let ((tag (new-tag :yield)))
		     (values `(:split (setf .state. ,tag)
				      (return-from function-body ,(second form)) 
				      ,tag)
			     t)))
		  
		  (t (values form
			     t))))
	      :build-result t)))

	(let* ((walked-as-list (multiple-value-list (apply-splits (walk suite ()))))
	       
	       ;; Add SUITE-STMT, to trigger :safe-lex-visible-vars optimization
	       (walked-list-with-suites (add-suites walked-as-list))
	       
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
		    
		    (block function-body
		      (tagbody
			(case .state.
			  ,@(loop for i from -1 to yield-counter
				collect `(,i (go ,i))))
		       0
			,@walked-list-with-suites

			(generator-finished)
			
			,final-tag
			(return-from function-body nil)
			#+(or)(raise-StopIteration)))))))))))))


(defun suite->generator (fname suite)
  (flet ((suite-walker (form &rest context)
	   (declare (ignore context))
	   (case (car form)
	     
	     (([funcdef-stmt] [classdef-stmt]) (values form t))
	     
	     ([return-stmt] (when (second form)
			    (py-raise '{SyntaxError}
				      "Inside generator, RETURN statement may ~
				       not have an argument (got: ~S)" form))
			  
			  (values `(return-from function-body :explicit-return)
				  t))
	     
	     (t form))))
	     
    `(excl:named-function (:suite->generator ,fname)
       (lambda ()
	 ,(rewrite-generator-funcdef-suite
	   fname
	   `([suite-stmt] (,(walk-py-ast suite #'suite-walker :build-result t)
			   (return-from function-body :implicit-return))))))))

(defun rewrite-generator-expr-ast (ast)
  ;; rewrite:  (x*y for x in bar if y)
  ;; into:     def f(src):  for x in src:  if y:  yield x*y
  ;;           f(bar)
  ;; values: (FUNCDEF ...)  bar
  (assert (eq (car ast) '[generator-expr]))
  (destructuring-bind (item for-in/if-clauses) (cdr ast)

    (let ((first-for (pop for-in/if-clauses))
	  (first-source (gensym "first-source")))
      
      (assert (eq (car first-for) '[for-in-clause]))
      
      (let ((iteration-stuff (loop with res = `([yield-stmt] ,item)
				 for clause in (reverse for-in/if-clauses)
				 do (setf res
				      (ecase (car clause)
					([for-in-clause] `([for-in-stmt]
							   ,(second clause) ,(third clause) ,res nil))
					([if-clause]     `([if-stmt] ((,(second clause) ,res)) nil))))
				 finally (return res))))
	
	`([call-expr] 
	  ([funcdef-stmt] nil ([identifier-expr] :generator-expr-helper-func)
			  ((([identifier-expr] ,first-source)) nil nil nil)
			  ([suite-stmt]
			   (([for-in-stmt] ,(second first-for) ([identifier-expr] ,first-source)
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
