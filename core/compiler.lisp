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

;;; Language Semantics

(defvar *allow-indirect-special-call* nil
  "Whether `eval', `locals' and `globals' can be called indirectly, like:
 x = locals; x()
If true, the compiler must generate additional code for every call,
and execution will be slower. It is very rare for Python code to do
such indirect special calls.")
;; This is similar to the Javscript restriction on `eval' (ECMA 262, §15.1.2.1)

(defvar *mangle-private-variables-in-class* nil
  "In class definitions, replace __foo by _CLASSNAME__foo, like CPython does")

(defmacro with-complete-python-semantics (&body body)
  `(let ((*allow-indirect-special-call* t)
	 (*mangle-private-variables*    t))
     ,@body))

(defvar *__debug__* t
  "The ASSERT-STMT uses the value of *__debug__* to determine whether
or not to include the assertion code.")

(defconstant +standard-module-globals+ '({__name__} {__debug__})
  "Names of global variables automatically created for every module")

;;; Compiler warnings

(defvar *warn-unused-function-vars* t
  "Controls insertion of IGNORABLE declaration around function variables.")

(defvar *warn-bogus-global-declarations* t
  "Signal warnings for bogus `global' declarations at toplevel.")

;;; Compiler optimizations

(defvar *inline-fixnum-arithmetic* t
  "For common arithmetic operations (+,-,*,/) the (often common) two-fixnum case is inlined")

(defvar *inline-builtin-methods* t
  "Inline method calls to common builtin methods (with a run-time check) for method calls
like .join (string.join), .sort (list.sort), etc")

(defvar *inline-getattr-call* t
  "Inline getattr(x,y).(zzz) calls, which usually saves creation of a temporary bound method.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +optimize-std+     '(optimize (speed 3) (safety 1) (debug 1)))
  (defconstant +optimize-fast+    '(optimize (speed 3) (safety 1)))
  (defconstant +optimize-fastest+ '(optimize (speed 3) (safety 0) (debug 0))))

(defmacro fast (&body body)
  `(locally (declare ,+optimize-fastest+)
     ,@body))

;;; `Exex' statement handling

(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar *exec-early-parse-constant-string* t
  "Whether a constant string argument to the `exec' statement is parsed at compile time already.")
)

(defvar *exec-stmt-compile-before-run* t
  "The code in the `exec' statement is translated into a function, that is
then (optionally) compiled and called. Should it be compiled before running?")

(defvar *exec-stmt-result-handler* nil)

;;; Line number handling

(defvar *include-line-number-hook-calls* nil
  "Include calls to *runtime-line-number-hook* in generated code?")

(defvar *runtime-line-number-hook* nil
  "Function to call at run time, when arrived on new line number")

(defvar *compile-line-number-hook* nil
  "Function to call at compile time, when a line number token is encountered.
Only has effect when *include-line-number-hook-calls* is true.")

(defmacro with-line-numbers ((&key compile-hook runtime-hook) &body body)
  ;; You have to set *runtime-line-number-hook* yourself.
  `(let ((*include-line-number-hook-calls* t)
	 (.parser::*include-line-numbers* t)
	 ,@(when runtime-hook `((*runtime-line-number-hook* ,runtime-hook)))
	 ,@(when compile-hook `((*compile-line-number-hook* ,compile-hook))))
     ,@body))

;;; Compiler Progress Messages

(defvar *compiler-verbose* nil
  "Whether to print Python compiler messages, e.g. inlining decisions")

(defun compiler-message (&rest args)
  (when *compiler-verbose*
    (format t "CLPython compiler: ")
    (apply #'format t args)))

;;; Compiler State

(defconstant +__main__-module-name+ "__main__")

(defvar *current-module-name* +__main__-module-name+
  "The name of the module now being compiled; module.__name__ is set to it.")

(defvar *current-module-path* ""
  "The path of the Python file being compiled; saved in module's `filepath' slot.")


;;; Gensym handling

(defmacro with-gensyms (list &body body)
  `(let ,(loop for x in list
	     collect `(,x (gensym ,(symbol-name x))))
     ,@body))

(defun multi-eval-safe (form)
  ;; Can FORM be evaluated multiple times safely?
  ;; (Always yielding the same result, without side effects)
  (cond ((match-p form '([identifier-expr] ?_))
         ;; variable lookup
         t)
        ((atom form)
         t)
        (t
         nil)))

;;; Python code templates

(defmacro def-py-macro (name params &key code gensyms)
  "CODE (a string) forms the template. All uppercase identifiers in CODE equal to
a parameter name are replaced by that PARAM's value. The identifiers given in
GENSYMS are made gensym'd Lisp vars."
  (check-type name symbol)
  (check-type params list)
  (check-type gensyms list)
  `(defmacro ,name ,params
     (let* ((param-repl (list ,@(loop for p in params
                                    collect `(cons ',([make-identifier-expr] :name (intern (string-upcase p) :clpython.user))
                                                   ,p))))
            (gensym-repl (list ,@(loop for g in gensyms
                                     do (check-type g string)
                                     collect `(cons ',([make-identifier-expr] :name (intern g :clpython.user))
                                                    (gensym ,g)))))
            (ast (clpython.parser::parse-with-replacements ,code (nconc param-repl gensym-repl) 
                                                           :warn-unused nil
                                                           :parse-options '(:incl-module nil))))
       `(let ,(mapcar #'cdr gensym-repl)
          ,ast))))
                   
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
  ;; The decision whether or not to execute `assert' statements
  ;; is taken at compile-time.
  (when *__debug__*
    `(assert-stmt-1 ,test ',test ,raise-arg)))

(defun assign-stmt-list-vals (iterable num-targets)
  (let ((val-list (py-iterate->lisp-list iterable)))
    (unless (= (length val-list) num-targets)
      (py-raise '{ValueError}
		"Assignment to several vars: wanted ~A values, but got ~A"
		num-targets (length val-list)))
    val-list))

(defun target-list-get-bound-vars (tg)
  (loop with todo and res
      for x = tg then (pop todo)
      while x
      do (when (listp x) ;; Usually true, but not in [with-stmt] expansion.
           (ecase (first x)
             ([attributeref-expr] )
             ([subscription-expr] )
             ([identifier-expr]          (push (second x) res))
             (([list-expr] [tuple-expr]) (setf todo (nconc todo (second x))))))
      finally (return res)))
             
(defun assign-stmt-get-bound-vars (ass-stmt)
  ;; Valid for ASSIGN-STMT targets and DEL-STMT target.
  (with-matching (ass-stmt ([assign-stmt] ?value ?targets))
    (declare (ignore ?value))
    (mapcan #'target-list-get-bound-vars ?targets)))

(defmacro [assign-stmt] (value targets)
  (with-gensyms (assign-val)
    `(let ((,assign-val ,value))
       ,@(loop for tg in targets collect `(setf ,tg ,assign-val)))))

(define-compiler-macro [assign-stmt] (&whole whole value targets)
  ;; Shortcut the case "a,b,.. = 1,2,.." where left and right same number of
  ;; items. This saves creation of a tuple for RHS.
  (if (and (or (match-p value '([tuple-expr] ?items))
               (match-p value '([list-expr] ?items)))
           (or (match-p targets '(([tuple-expr] ?items)))
               (match-p targets '(([list-expr] ?items))))
           (= (length (second value)) (length (second (car targets)))))
      
      ;; Note that by using RHS, we force values to be evaluated before targets.
      `(psetf ,@(mapcan #'list (second (car targets)) (second value)))
    
    whole))

(defmacro [attributeref-expr] (item attr)
  (with-matching (attr ([identifier-expr] ?name))
    `(py-attr ,item ',?name)))

(define-setf-expander [attributeref-expr] (item attr)
  (with-matching (attr ([identifier-expr] ?name))
    (with-gensyms (prim store)
      (values `(,prim)    ;; temps
              `(,item) ;; values
              `(,store)   ;; stores
              `(with-pydecl ((:inside-setf-py-attr t)) ;; store-form
                 (setf (py-attr ,prim ',?name) ,store))
              `(py-attr ,prim ',?name)                 ;; read-form
              `(with-pydecl ((:inside-setf-py-attr t)) ;; del-form
                 (setf (py-attr ,prim ',?name) nil))))))

(defmacro [augassign-stmt] (&whole whole op place val &environment env)
  (unless (listp place)
    (py-raise '{SyntaxError}
              "Augmented assignment to a literal is not possible (got: \"~A ~A ..\")."
              place op)) 
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
      `(go .break)
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
		   (progn `(.locals.))
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
      
      (let ((specials-to-check (if *allow-indirect-special-call*
                                   *special-calls*
                                 (with-perhaps-matching (primary ([identifier-expr] ?name))
                                   (intersection (list ?name) *special-calls*)))))
        (if specials-to-check
	    `(let* ((.prim ,primary))
	       ,(%do-maybe-special-call '.prim specials-to-check))
	  `(call-expr-1 ,@(cdr whole)))))))

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

(define-compiler-macro [call-expr] (&whole whole  primary args)
  (declare (ignore primary args))

  ;; The transformations below inline common cases, but
  ;; there are still run-time checks to verify whether
  ;; the inline case should be taken.

  ;; Optimize calls of the form OBJ.ATTR(POS-ARGS..)
  ;; where ATTR is usually a built-in method,
  ;; so "x.sort()" gets inlined call to `py-list.sort'.
  (when *inline-builtin-methods*
    (with-perhaps-matching (whole
                            ([call-expr] ([attributeref-expr] ?obj ([identifier-expr] ?attr-name))
                                         (?pos-args () nil nil)))
      (when (inlineable-method-p ?attr-name (length ?pos-args))
        (compiler-message "Inlining call to builtin method `~A'." ?attr-name)
        (return-from [call-expr]
          (inlined-method-code ?obj ?attr-name ?pos-args)))))
          
  ;; Optimize "getattr(OBJ, ATTR)(POSARGS...)", to save allocation of bound method.
  (when *inline-getattr-call*
    (with-perhaps-matching (whole ([call-expr]
                                   ([call-expr] ?id-getattr ((?obj ?attr) () nil nil))
                                   (?pos-args () nil nil)))
      (with-perhaps-matching (?id-getattr ([identifier-expr] {getattr}))
        (assert (multi-eval-safe ?id-getattr))
        (compiler-message "Optimizing \"getattr(x,y)(...)\" call, skipping bound method.")
        (return-from [call-expr]
          `(if (eq ,?id-getattr (symbol-function '{getattr}))
               (multiple-value-bind (.a .b .c)
                   (getattr-nobind ,?obj ,?attr nil)
                 (if (eq .a :class-attr)
                     (funcall .b .c ,@?pos-args)
                   (py-call .a ,@?pos-args)))
             (py-call ,?id-getattr ,@?pos-args))))))
  
  ;; XXX todo: Optimize obj.__get__(...)
  whole)

(defmacro [classdef-stmt] (name inheritance suite &environment e)
  ;; todo: define .locals. containing class vars
  (multiple-value-bind (all-class-locals new-locals class-cumul-declared-globals)
      (classdef-stmt-suite-globals-locals suite (get-pydecl :lexically-declared-globals e))
    (assert (equal new-locals all-class-locals))
    (let* ((cname             (with-matching (name ([identifier-expr] ?name))
                                ?name))
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
		  
                  ;; Note that the local class variables are not locally visible
                  ;; i.e. they don't extend ":lexically-visible-vars".
                                    
		  (with-pydecl ((:context :class)
				(:context-stack ,new-context-stack)
				(:lexically-declared-globals
				 ,class-cumul-declared-globals))
		    		    
		    ,(if *mangle-private-variables-in-class*
			(mangle-suite-private-variables cname suite)
		       suite))
		  
		  +cls-namespace+)))
	   
	   ;; Second, now that +cls-namespace+ is filled, make the
	   ;; class with that as namespace.
	   (let ((,cls (make-py-class
                        :name ',cname
                        :context-name ',context-cname
                        :namespace new-cls-dict
                        :supers ,(with-matching (inheritance ([tuple-expr] ?supers))
                                   `(list ,@?supers))
                        :cls-metaclass (sub/dict-get new-cls-dict "__metaclass__")
                        :mod-metaclass
                        ,(let ((ix (position '{__metaclass__}
                                             (get-pydecl :mod-globals-names e))))
                           (if ix
                               `(svref +mod-static-globals-values+ ,ix)
                             `(gethash '{__metaclass__} +mod-dyn-globals+))))))
             
             (record-source-file-loc ',context-cname :type)
             ([assign-stmt] ,cls (,name))))))))

(defun mangle-suite-private-variables (cname suite)
  "Rename all attributes `__foo' to `_CNAME__foo'."
  (declare (ignore cname suite))
  (error "todo"))

(defmacro [clpython-stmt] (&key line-no)
  ;; XXX The module name should also be a param.
  (when *include-line-number-hook-calls*
    (when *compile-line-number-hook*
      (funcall *compile-line-number-hook* line-no))
    `(let ((hook *runtime-line-number-hook*))
       (when hook (funcall hook ,line-no)))))

(defmacro [comparison-expr] (cmp left right)
  (let ((py-@ (get-binary-comparison-func-name cmp)))
    `(,py-@ ,left ,right)))

(defmacro [continue-stmt] (&environment e)
  (if (get-pydecl :inside-loop-p e)
      `(go .continue)
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

(defmacro [exec-stmt] (code-string globals locals &key (allowed-stmts t) &environment e)
  ;; TODO:
  ;;   - allow code object etc as CODE
  ;;
  ;; An EXEC-STMT is translated into a Python suite containing a
  ;; function definition and a subsequent call of the function.
  ;;
  ;; ALLOWED-STMTS: if T:      allow all statements
  ;;                   a list: allow only those statements
  ;;                   NIL:    allow no statements
  ;;  (not evaluated)

  `(multiple-value-bind (glo loc)
       ,(cond ((and globals locals) `(values ,globals ,locals))
              (globals              `(let ((.x ,globals))
                                       (values .x .x))) ;; globals also used for locals
              (t                    `(let ((.g (create-module-globals-dict)))
                                       (values .g
                                               ,(if (eq (get-pydecl :context e) :module)
                                                    `.g
                                                  `(.locals.))))))
     (exec-stmt-check-namespaces glo loc)
     ;; ensure dicts reflect current namespace
     (dolist (x (list glo loc))
       (when (typep x 'py-dict-moduledictproxy)
         (funcall (mdp-updater x))))
     (exec-stmt-string ,code-string glo loc ',allowed-stmts)))

(defun exec-stmt-check-namespaces (globals locals)
  (check-type globals py-dict) ;; todo: support any mapping for globals, locals
  (check-type locals py-dict)
  (flet ((check-is-namespace-dict (d)
           ;; Ensure dict has only string keys.
           (check-type d py-dict)
           (dikt-map (py-dict-dikt d)
                     (lambda (k v)
                       (declare (ignore v))
                       (unless (typep k '(or string symbol))
                         (py-raise
                          '{TypeError}
                          "Cannot use ~A as namespace dict for `exec', due to non-string key: ~A."
                          d k))))))
    (check-is-namespace-dict globals)
    (unless (eq globals locals)
      (check-is-namespace-dict locals))))

(defun exec-stmt-string (code-string globals locals allowed-stmts)
  (check-type code-string string)
  (let ((ast (parse code-string)))
    (exec-stmt-check-ast code-string ast allowed-stmts)
    (exec-stmt-ast ast globals locals)))

(define-compiler-macro exec-stmt-string (&whole whole code-string globals locals allowed-stmts)
  (assert (and (listp allowed-stmts)
               (eq (car allowed-stmts) 'quote)))
  ;; Move compilation of string to compile-time. Warn if string contains errors.
  (labels ((warn-static-error (error)
             (let ((nice-code (if (> (length code-string) 40)
                                  (concatenate 'string (subseq code-string 0 40) "...")
                                code-string)))
               (warn "Invalid argument for `exec': parsing ~S will raise [~A]."
                     nice-code error))))
    (when (and *exec-early-parse-constant-string*
               (stringp code-string))
      (multiple-value-bind (ast error)
          (ignore-errors (parse code-string))
        (if error
            (warn-static-error error)
          (multiple-value-bind (ok error)
              (ignore-errors (exec-stmt-check-ast code-string ast (second allowed-stmts)))
            (declare (ignore ok))
            (if error
                (warn-static-error error)
              (return-from exec-stmt-string
                `(exec-stmt-ast ',ast ,globals ,locals)))))))
    whole))

(defun exec-stmt-check-ast (string ast allowed-stmts)
  (whereas ((s (ast-contains-stmt-p ast :allowed-stmts allowed-stmts)))
    (py-raise '{TypeError}
              "Statements are not allowed in this Python code string (found `~A' in \"~A\")." s string))
  
  (with-py-ast (form ast :into-nested-namespaces nil)
    (case (car form)
      ([return-stmt] (py-raise '{TypeError}
                               "`return' statement found outside function (in `exec')."))
      (t form)))
  
  t)

(defconstant exec-stmt-helper-func-name 'exec-stmt-helper-func)

(defun exec-stmt-ast (ast globals locals)
  (let ((f `(with-module-context (#() #() (let ((ht (make-py-hash-table)))
                                            (dict-map ,globals (lambda (k v) (setf (gethash k ht) v)))
                                            ht)
                                  :create-mod t)
              ,([make-suite-stmt*]

                ;; Define helper function
                ([make-funcdef-stmt]
                 :fname ([make-identifier-expr*] exec-stmt-helper-func-name)
                 :args '(() () nil nil)
                 :suite
                 ([make-suite-stmt*]
                  `(block helper
                     ,([make-suite-stmt*]
                       
                       `([suite-stmt]
                         ;; Set local variables
                         ,(let (res)
                            (dict-map locals
                                      (lambda (k v)
                                        (assert (not (null v)) () 
                                          "Local var `~A' (passed as EXEC local) is NIL." k)
                                        (push ([make-assign-stmt*]
                                               :value `',v
                                               :target ([make-identifier-expr*]
                                                        (py-string->symbol k)))
                                              res)))
                            res))
                         
                       `(progn ;; Execute exec suite, save result
                          (let ((res ,(with-matching (ast ([module-stmt] ?suite))
                                        (assert (match-p ?suite '([suite-stmt] ?stmts)))
                                        ?suite)))
                            (when *exec-stmt-result-handler*
                              (funcall *exec-stmt-result-handler* res))
                            (return-from helper res)))))))
                
                ;; Call helper function
                ([make-call-expr] :primary ([make-identifier-expr*] 'exec-stmt-helper-func)
                                  :all-args '(() () nil nil)))

              ;; Update `globals'
              (loop for (k . v) in (py-module-get-items +mod+)
                  unless (eq k exec-stmt-helper-func-name)
                  do (setf (py-subs ,globals k) v)))))
    
    #+(or)(warn "EXEC-STMT: lambda-body: ~A" f)
    
    (setf f `(lambda ()
               (locally (declare (optimize (debug 3)))
                 ;; When this is compiled, the environment object is NIL.
                 (with-pydecl ((:function-must-save-locals t))
                   ,f))))
    
    (when *exec-stmt-compile-before-run*
      (setf f (compile nil f)))
    
    (handler-bind ((error (lambda (c)
                            ;; Only print header line if condition not handled in outer scope.
                            (signal c)
                            (format t "[Error occured inside an `exec' statement:]"))))
      (block run-exec-body
        (restart-case
            (funcall f)
          (return-from-exec ()
              :report "Abort evaluation of the `exec' statement, but continue execution."
            (warn "Evaluation of `exec' body was aborted.")
            (return-from run-exec-body)))))))

;;; `Call' expression

(defun call-expr-eval (locals-dict globals-dict pos-args key-args-p)
  "Handle call to `Eval' at runtime."
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
      ([exec-stmt] string glob-d loc-d :allowed-stmts ([module-stmt] [suite-stmt]))
      res)))

(defmacro [for-in-stmt] (target source suite else-suite)
  (with-gensyms (f x)
    `(tagbody
       (let* ((,f (lambda (,x)
		    ([assign-stmt] ,x (,target))
		    (tagbody 
		      (with-pydecl ((:inside-loop-p t))
			,suite)
		      (go .continue) ;; prevent warning about unused tag
		     .continue))))
	 (declare (dynamic-extent ,f))
	 (map-over-py-object ,f ,source))
       ,@(when else-suite `(,else-suite))
       
       (go .break) ;; prevent warning
      .break)))

(defun lambda-args-and-destruct-form (f-pos-args f-key-args)
  ;; Replace "def f( (x,y), z):  .." 
  ;; by "def f( |(x,y)|, z):  x, y = |(x,y)|; ..".
  (let (nested-vars)
    (labels ((sym-tuple-name (tup)
	       ;; Convert tuple with identifiers to symbol:  (a,(b,c)) -> |(a,(b,c))|
	       ;; Returns the symbol and a list with the "included" symbols (here: a, b and c)
	       (assert (match-p tup '([tuple-expr] ?items)))
	       (labels ((rec (x)
			  (ecase (car x)
			    ([tuple-expr] (format nil "(~{~A~^,~})"
						  (loop for v in (second x) collect (rec v))))
			    ([identifier-expr] (push (second x) nested-vars)
					       (symbol-name (second x))))))
		 (ensure-user-symbol (rec tup))))
             (analyze-args (args)
               (let (new-arglist normal-args destructs)
                 (dolist (arg args)
                   (ecase (car arg)
                     ([identifier-expr] (let ((name (second arg)))
                                          (push name new-arglist)
                                          (push name normal-args)))
                     ([tuple-expr] (let ((tuple-var (sym-tuple-name arg)))
                                     (push tuple-var new-arglist)
                                     (push `([assign-stmt] ,tuple-var (,arg)) destructs)))))
                 (values (nreverse new-arglist)
                         (nreverse normal-args)
                         (nreverse destructs)))))
      
      (multiple-value-bind (lambda-pos-args normal-pos-args pos-destructs ) 
          (analyze-args f-pos-args)
        (multiple-value-bind (lambda-key-args normal-key-args key-destructs)
            (analyze-args (mapcar #'car f-key-args))
          (values lambda-pos-args ;; LAMBDA args
                  lambda-key-args ;;  are in the same order as original lists
                  (when (or pos-destructs key-destructs)
                    `(progn ,@(nconc pos-destructs key-destructs)))
                  (nconc normal-pos-args normal-key-args)
                  (nreverse nested-vars)))))))

(defun funcdef-globals-locals (suite locals globals)
  "Returns three lists: LOCALS, NEW-LOCALS, GLOBALS.
LOCALS are the variables assigned to within the function body.
LOCALS shares share tail structure with input arg locals."
  (declare (optimize (debug 3)))
  (assert (match-p suite '([suite-stmt] ?items)))
  (let (new-locals)
    (with-py-ast ((form &key value target) suite :value t)
      ;; Use :VALUE T, so the one expression for lambda suites is handled correctly.
      (declare (ignore value))
      (case (car form)

	(([classdef-stmt] [funcdef-stmt])
	 (multiple-value-bind (name kind)
	     (ecase (pop form)
	       ([classdef-stmt]
                (with-matching (form (([identifier-expr] ?cname) ?inhericante ?csuite))
                  (values ?cname "class")))
	       ([funcdef-stmt]
                (with-matching (form (?decorators ([identifier-expr] ?fname) ?fargs ?fsuite))
                  (values ?fname "function"))))
	   (when (member name globals)
	     (py-raise '{SyntaxError}
		       "The ~A name `~A' may not be declared `global'." kind name))
           (unless (or (member name locals)
                       (member name new-locals))
             (push name locals)
             (push name new-locals)))
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
         (with-matching ((second form) ([tuple-expr] ?identifiers))
           (dolist (x ?identifiers)
             (assert (match-p x '([identifier-expr] ?_))))
           (let* ((sym-list (mapcar #'second ?identifiers))
                  (erroneous (intersection sym-list locals :test 'eq)))
             (when erroneous
               ;; CPython gives SyntaxWarning, and seems to internally move the `global'
               ;; declaration before the first use. Let us signal an error; it's easy
               ;; for the user to fix this.
               (py-raise '{SyntaxError}
                         "This `global' declaration for variable `~A' is not allowed: ~
                          declaration must be given before first use in function body."
                         (car erroneous)))
             (setf globals (nconc sym-list globals)))
           (values nil t)))

        ([del-stmt]
         ;; Local variables are determined by looking at assignments.
         ;; Deletions play no role, so don't walk into them.
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
	((with-matching (fname ([identifier-expr] ?name))
           (setf fname ?name)
           t))
	((break :unexpected) ))
  
  (multiple-value-bind (lambda-pos-args lambda-key-args tuples-destruct-form
                        normal-pos-key-args destruct-nested-vars)
      (lambda-args-and-destruct-form pos-args key-args)
    
    (let ((nontuple-arg-names (nconc normal-pos-key-args destruct-nested-vars)))
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
                                    ,(let ((sum (append all-nontuple-func-locals
                                                        (get-pydecl :lexically-visible-vars e))))
                                       ;; def f(x):
                                       ;;   def g(y):
                                       ;;     <Here G is locally visibe because it is a /local variable/
                                       ;;      in F. In general the name of a function is not visible
                                       ;;      in its body.>
                                       ;; 
                                       ;; See also the testcases for the :LEXICALLY-VISIBLE-VARS declaration.
                                       (when (eq (get-pydecl :context e) :function)
                                         (assert (get-pydecl :inside-function-p e))
                                         (pushnew fname sum))
                                       sum))
				   (:safe-lex-visible-vars
				    ,(nset-difference
				      (append nontuple-arg-names
					      (get-pydecl :safe-lex-visible-vars e))
				      (ast-deleted-variables suite)))))
	       (func-lambda
		`(py-arg-function
                  ,context-fname
		  (,lambda-pos-args
		   ,(loop for lambda-key-arg in lambda-key-args
                        for ((nil nil) key-default-arg) in key-args
                        collect `(,lambda-key-arg ,key-default-arg))
		   ,(when *-arg  (second *-arg))
		   ,(when **-arg (second **-arg)))
		  
		  (let (,@destruct-nested-vars
			,@new-locals)
		    
		    ,@(unless *warn-unused-function-vars*
			`((declare (ignorable ,@nontuple-arg-names ,@new-locals))))
		    
		    (block function-body
		      (flet
			  (,@(when (funcdef-should-save-locals-p suite e)
			       `((.locals. () 
					   ;; lambdas and gen-exprs have 'locals()' too
					   (make-locals-dict 
					    ',all-nontuple-func-locals
					    (list ,@all-nontuple-func-locals))))))
			,@(when (funcdef-should-save-locals-p suite e)
                            `((declare (ignorable #'.locals.))))
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
		 
		 (record-source-file-loc ',context-fname :operator)
                 ;; return the function
		 ([identifier-expr] ,fname)))))))))


(defmacro [generator-expr] (&whole whole item for-in/if-clauses)
  (declare (ignore item for-in/if-clauses))
  (rewrite-generator-expr-ast whole))
       
(defmacro [global-stmt] (names &environment e)
  ;; GLOBAL statements are already determined and used at the moment a
  ;; FUNCDEF-STMT is handled.
  (declare (ignore names))
  (when (and *warn-bogus-global-declarations*
             (not (get-pydecl :inside-function-p e)))
    (warn "Bogus `global' statement found at top-level.")))

(define-setf-expander [identifier-expr] (name &environment e)
  ;; As looking up identifiers is side-effect free, the valuable
  ;; functionality here is the "store form" (fourth value).
  ;; As a bonus the "delete form" is given (sixth value).
  (let ((glob-ix (position name (get-pydecl :mod-globals-names e))))
    (with-gensyms (val)

      ;; 1) Store form
      (symbol-macrolet ((module-set
			    (if glob-ix
				`(setf (svref +mod-static-globals-values+ ,glob-ix) ,val)
			      `(setf (gethash ',name +mod-dyn-globals+) ,val)))
			(local-set `(setf ,name ,val))
			(class-set `(sub/dict-set +cls-namespace+ ',name ,val)))
	(let ((store-form
	       (ecase (get-pydecl :context e)
		 (:module    module-set)
		 (:function  (if (or (member name (get-pydecl :lexically-declared-globals e))
				     (not (member name (get-pydecl :lexically-visible-vars e))))
				 module-set
			       local-set))
		 (:class     (if (member name (get-pydecl :lexically-declared-globals e))
				 module-set 
			       class-set)))))
	  ;; 2) Del form
	  (symbol-macrolet ((module-del
				`(delete-identifier-at-module-level ',name ,glob-ix +mod+))
			    (local-del
                                `(progn (unless ,name
                                          (unbound-variable-error ',name nil))
                                        (setf ,name ,(when (builtin-value name) `(builtin-value ',name)))))
			    (class-del `(unless (py-del-subs +cls-namespace+ ,name)
					  (unbound-variable-error ',name nil))))
	    (let ((del-form 
		   (ecase (get-pydecl :context e)
		     (:module   module-del)
		     (:function (if (or (member name (get-pydecl :lexically-declared-globals e))
					(not (member name (get-pydecl :lexically-visible-vars e))))
				    module-del
				  local-del))
		     (:class    (if (member name (get-pydecl :class-globals e))
				    module-del
				  class-del)))))
	      (values
	       () ;; temps
	       () ;; values
	       (list val) ;; stores
	       store-form
	       `([identifier-expr] ,name) ;; name is literal symbol, thus no side-effect
	       del-form)))))))) ;; bonus

(defmacro [identifier-expr] (name &environment e)
  ;; The identifier is used for its value; it is not an assignent
  ;; target (as the latter case is handled by the setf expander for ID..-EXPR.
  (check-type name symbol)
  (flet ((module-lookup ()
	   (let ((ix (position name (get-pydecl :mod-globals-names e))))
	     (if ix
		 `(or (fast (svref +mod-static-globals-values+ ,ix))
		      (unbound-variable-error ',name))
	       `(identifier-expr-module-lookup-dyn ',name +mod-dyn-globals+))))
	 
	 (local-lookup ()
	   (if (member name (get-pydecl :safe-lex-visible-vars e))
	       (progn #+(or)(warn "safe: ~A" name)
		      name)
	     `(or ,name
		  (unbound-variable-error ',name)))))
    
    (ecase (get-pydecl :context e)
      
      (:function (if (or (member name (get-pydecl :lexically-declared-globals e))
			 (not (member name (get-pydecl :lexically-visible-vars e))))
		     (module-lookup)
		   (local-lookup)))
		 
      (:module   (module-lookup))
      
      (:class    `(or (sub/dict-get +cls-namespace+ ',(symbol-name name))
		      ,(if (member name (get-pydecl :lexically-visible-vars e))
			   (local-lookup)
			 (module-lookup)))))))

(defmacro [if-expr] (condition then else)
  `(if (py-val->lisp-bool ,condition) ,then ,else))
       
(defmacro [if-stmt] (if-clauses else-clause)
  `(cond ,@(loop for (cond body) in if-clauses
	       collect `((py-val->lisp-bool ,cond) ,body))
	 ,@(when else-clause
	     `((t ,else-clause)))))

(defmacro [import-stmt] (items)
  `(values ,@(loop for (mod-name-as-list bind-name) in items nconc
		   (loop for m in mod-name-as-list
                       for toplevel = t then nil ;; Ensure topleve module is imported relative to +mod+
		       for res = (list m) then (nconc res (list m)) collect
			 `(let ((module-obj (py-import ',res ,@(when toplevel `(:within-mod +mod+)))))
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
  `(let ((m (py-import '(,(car mod-name-as-list)) :within-mod +mod+)))
     (declare (ignorable m)) ;; Ensure topleve module is imported relative to +mod+
     (whereas ((mod-obj ,(if (= (length mod-name-as-list) 1)
                             `m
                           `(py-import ',mod-name-as-list))))
       ,@(cond ((eq items '[*])
                `((let ((src-items (py-module-get-items mod-obj :import-* t)))
                    (loop for (k . v) in src-items
                        do (py-module-set-kv +mod+ k v)))))
               (t (loop for (item bind-name) in items
                      collect `([assign-stmt] ([attributeref-expr] mod-obj ([identifier-expr] ,item))
                                              (([identifier-expr] ,(or bind-name item))))))))))
       
(defmacro [lambda-expr] (args expr)
  ;; XXX Treating lambda as a funcdef-stmt results in way more
  ;; code than necessary for the just one expression it contains.
  `([funcdef-stmt] nil :lambda ,args ([suite-stmt] (([return-stmt] ,expr)))))
  
(defmacro [listcompr-expr] (item for-in/if-clauses)
  (with-gensyms (list)
    `(let ((,list ()))
       ,(loop
	    with res = `(push ,item ,list)
	    for clause in (reverse for-in/if-clauses)
	    do (setf res (ecase (car clause)
			   ([for-in-clause] ([make-for-in-stmt] :target (second clause) :source (third clause)
                                                                :suite res :else-suite nil))
			   ([if-clause]     ([make-if-stmt] :if-clauses `((,(second clause) ,res))
                                                            :else-clause nil))))
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

  `(let* ((clpython::*habitat* (or clpython::*habitat*
                                   (make-habitat :search-paths '("."))))
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
     (declare (special clpython::*habitat*))
     (declare (ignorable +mod-static-globals-names+
                         +mod-static-globals-values+
                         +mod-static-globals-builtin-values+
                         +mod-dyn-globals+
                         +mod+)
              (optimize (debug 3))) ;; WB
     
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
       
       
       (with-py-errors (:name (python-module ,(or module-name '__main__)))
         ,@body))))

(defmacro create-module-globals-dict ()
  ;; Updating this dict really modifies the globals.
  `(module-make-globals-dict +mod+
			     +mod-static-globals-names+ +mod-static-globals-values+ +mod-dyn-globals+))

(defun unbound-variable-error (name &optional (expect-value t))
  (declare (special *py-signal-conditions*))
  (if expect-value
      (restart-case
	  (py-raise '{NameError} "Variable `~A' is unbound." name)
        (cl:use-value (val)
	    :report (lambda (stream)
		      (format stream "Enter a Lisp value to use for `~A'." name))
	    :interactive (lambda () 
			   (format t "Enter new Lisp value for `~A': " name)
			   (multiple-value-list (eval (read))))
	  (return-from unbound-variable-error val)))
    (with-simple-restart (continue "Continue as if `~A' is currently bound." name)
      (py-raise '{NameError} "Variable `~A' is unbound." name))))

(defun identifier-expr-module-lookup-dyn (name +mod-dyn-globals+)
  (or (gethash name +mod-dyn-globals+)
      (builtin-value name)
      (unbound-variable-error name)))

(defun delete-identifier-at-module-level (name ix +mod+)
  ;; Reset module-level vars with built-in names to their built-in value
  (with-slots (globals-names globals-values dyn-globals) +mod+
    (let ((biv (builtin-value name))  ;; maybe NIL
	  (old-val (if ix
		       (svref globals-values ix) 
		     (remhash name dyn-globals))))
      (unless old-val
	(unbound-variable-error name nil))
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
       (declare (optimize (debug 3)))
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
  ;; Skip checks for bound-ness, when a lexical variable is certainly bound.
  (cond ((and (some (lambda (s) (match-p s '([assign-stmt] ?value ?targets)))
		    (butlast stmts))
	      (not (ast-deleted-variables whole))) ; XXX this should be more nuanced
         ;; Collect the stmts before the assignment, and those after
         (multiple-value-bind (before-stmts ass-stmt after-stmts)
	     (loop for sublist on stmts
		 for s = (car sublist)
		 until (match-p s '([assign-stmt] ?value ?targets))
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

(defparameter *try-except-currently-handled-exception* *the-none*
  "Information about the currently handled exception. This is only not-None
inside an `except' clause.")

(defmacro [try-except-stmt] (suite except-clauses else-suite)
  ;; The Exception class in a clause is evaluated only after an
  ;; exception is thrown.
  (with-gensyms (the-exc)
    (flet ((handler->cond-clause (except-clause)
	
	     (destructuring-bind (exc var handler-suite) except-clause

	       ;; Every handler should store the exception, so it can be returned
	       ;; in sys.exc_info().
	       (setq handler-suite
		 `(progn (let ((*try-except-currently-handled-exception* ,the-exc))
                           ,handler-suite)))
	       
	       (cond ((null exc)
		      `(t (progn ,handler-suite
				 (return-from try-except-stmt nil))))
		   
		     ((and (listp exc)
                           (eq (car exc) '[tuple-expr]))
                      ;; Because the names in EXC may be any variable that is bound to an exception
                      ;; class, not possible to use `(typep ,the-exc (or ,@names))
		      `((or ,@(loop for cls in (second exc)
                                  collect `(typep ,the-exc ,cls)))
			(progn ,@(when var `(([assign-stmt] ,the-exc (,var))))
			       ,handler-suite
			       (return-from try-except-stmt nil))))
				
		     (t
		      `((progn (try-except-ensure-valid-exception-class ,exc)
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
	       
	       (progn (with-py-errors (:name try-except-function) ,suite)
		      ,@(when else-suite `((go :else)))))
	     
	     ,@(when else-suite
		 `(:else ,else-suite))))))))

(defun try-except-ensure-valid-exception-class (exc)
  (unless (and (typep exc 'class)
               (subtypep exc '{Exception}))
    (py-raise '{TypeError} "The `except' argument must be a subclass of `Exception' (got: ~A)." exc)))

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
  `(tagbody
    .continue
     (if (py-val->lisp-bool ,test)
         (go .body)
       (go .else))
     
    .body
     (with-pydecl ((:inside-loop-p t))
       ,suite)
     (go .continue)
     
    .else
     ,@(when else-suite `(,else-suite))

     (go .break) ;; prevent warning
    .break
     ))

(def-py-macro [with-stmt] (expr var block)
  :code (format nil "
#import sys as __clpy_sys ## ugly
mgr = (EXPR)
exit = mgr.__exit__  # Not calling it yet
value = mgr.__enter__()
exc = True
try:
  try: ~@[
    VAR = value ~]  # Only if 'as VAR' is present
    BLOCK
  except:
    # The exceptional case is handled here
    exc = False
    if not exit(*__clpy_sys.exc_info()):
      raise
    # The exception is swallowed if exit() returns true
finally:
  # The normal and non-local-goto cases are handled here
  if exc:
    exit(None, None, None)" var)
  :gensyms ("mgr" "exit" "value" "exc"))

(defmacro [yield-expr] (val)
  (declare (ignore val))
  (py-raise '{SyntaxError} "Statement `yield' found outside function."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helper functions for the compiler

(defun ast-contains-stmt-p (ast &key allowed-stmts)
  "Returns the forbidden statement, or NIL"
  (when (eq allowed-stmts t)
    (return-from ast-contains-stmt-p nil))
  (labels ((is-stmt-sym (s)
	     (let ((s.name (symbol-name s)))
	       (cond ((<= (length s.name) 5) nil)
		     ((string-equal (subseq s.name (- (length s.name) 5)) "-stmt") s)
		     (t nil))))
           (test (ast)
	     (typecase ast
	       (list (loop for x in ast when (test x) return it finally (return nil)))
	       (symbol (unless (member ast allowed-stmts :test #'eq)
			 (whereas ((s (is-stmt-sym ast)))
			   (return-from test s))))
	       (t    nil))))
    (test ast)))


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
	'(({isalpha}    0 stringp      py-string.isalpha)
	  ({isalnum}    0 stringp      py-string.isalnum)
	  ({isdigit}    0 stringp      py-string.isdigit)
	  ({islower}    0 stringp      py-string.islower)
	  ({isspace}    0 stringp      py-string.isspace)
	  ({join}       0 stringp      py-string.join   )
	  ({lower}      0 stringp      py-string.lower  )
	  ({strip}      0 stringp      py-string.strip  )
	  ({upper}      0 stringp      py-string.upper  )
	  	     
	  ({keys}       0 py-dict-p    py-dict.keys     )
	  ({items}      0 py-dict-p    py-dict.items    )
	  ({values}     0 py-dict-p    py-dict.values   )
	  	     
	  ({next}       0 py-func-iterator-p py-func-iterator.next)
	  
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

(defun inlineable-method-p (attr num-pos-args)
  (let ((item (gethash attr *inlineable-methods*)))
    (and item
         (destructuring-bind (req-args check inline-func) 
             item
           (declare (ignore check inline-func))
           (etypecase req-args
             (integer (= num-pos-args req-args))
             (cons    (<= (car req-args) num-pos-args (cdr req-args))))))))

(defun inlined-method-code (prim attr args)
  (assert (inlineable-method-p attr (length args)))
  (destructuring-bind (req-args check inline-func)
      (gethash attr *inlineable-methods*)
    (declare (ignore req-args))
    `(let ((.prim ,prim))
       (if ,(ecase check
              ((stringp vectorp)  `(,check .prim))
              (filep              `(eq (class-of .prim) (ltv-find-class 'py-func-iterator)))
              (py-dict-p          `(eq (class-of .prim) (ltv-find-class 'py-dict)))
              (py-func-iterator-p `(eq (class-of .prim) (ltv-find-class 'py-func-iterator))))
           (,inline-func .prim ,@args)
         (py-call (py-attr .prim ',attr) ,@args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Support for introspection: locals() and globals()

(defun make-locals-dict (name-list value-list)
  (make-dict-from-symbol-alist
   (delete nil (mapcar #'cons name-list value-list) :key #'cdr)))

(defun module-make-globals-dict (mod names-vec values-vec dyn-globals-ht)
  (flet ((make-regular-dict ()
           (make-dict-from-symbol-alist
	    (nconc (loop for name across names-vec and val across values-vec
		       unless (null val) collect (cons name val))
		   (loop for k being the hash-key in dyn-globals-ht using (hash-value v)
		       collect (cons k v))))))
    
  (let* ((d (make-regular-dict))
         (updater (lambda () 
                    (let ((new-d (make-regular-dict)))
                      (clear-dict d) ;; closes over D itself
                      (dict-map new-d (lambda (k v)
                                        (sub/dict-set d k v)))
                      d))))
    (change-class d 'py-dict-moduledictproxy :module mod :updater updater)
    d)))

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
         (with-matching ((cdr form) (([identifier-expr] ?cname) ?inhericance ?csuite))
	   (pushnew ?cname globals))
	 (values nil t))

	([funcdef-stmt]
	 ;; name of this function, but don't recurse
         (with-matching ((cdr form) (?deco ([identifier-expr] ?fname) ?args ?fsuite))
           (pushnew ?fname globals))
	 (values nil t))
	
	([identifier-expr] (let ((name (second form)))
			     (pushnew name globals))
			   (values nil t))
	
	(t form)))
    
    ;; Variables explicitly declared `global', somewhere arbitrarily deeply nested.
    (with-py-ast (form suite :into-nested-namespaces t)
      (case (car form)

	([global-stmt] (with-matching ((second form) ([tuple-expr] ?identifiers))
                         (dolist (name (mapcar #'second ?identifiers))
                           (pushnew name globals))
                         (values nil t)))
	
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

(defun raise-wrong-args-error ()
  (py-raise '{TypeError} "Wrong number of arguments, or wrong keyword, supplied to function."))

(defun raise-invalid-keyarg-error (kw)
  (py-raise '{TypeError}
	    "Function got unsupported keyword argument `~A'." kw))

(defun raise-double-keyarg-error (kw)
  (py-raise '{TypeError}
	    "Function got multiple values for keyword argument `~A'." kw))

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
  (declare #.+optimize-fast+)
  (let* ((num-pos-args (length pos-args))
	 (num-key-args (length key-args))
	 (num-pos-key-args  (+ num-pos-args num-key-args))
	 (some-args-p (or pos-args key-args *-arg **-arg))
	 (pos-key-arg-names (nconc (copy-list pos-args) (mapcar #'first key-args)))
	 (key-arg-default-asts (mapcar #'second key-args))
         (arg-kwname-vec (make-array
			  num-pos-key-args
			  :initial-contents (loop for x in pos-key-arg-names
						collect (intern x :keyword))))
         
	 (fa (make-fa :func-name        name
		      :num-pos-args     num-pos-args
		      :num-key-args     num-key-args
		      :num-pos-key-args num-pos-key-args
		      :pos-key-arg-names (make-array (length pos-key-arg-names)
						     :initial-contents pos-key-arg-names)
		      :key-arg-default-vals nil ;; If there are any key args, this will be filled at load time.
		      :arg-kwname-vec   arg-kwname-vec
		      :*-arg            *-arg
		      :**-arg           **-arg)))
    
    `(progn
       ,@(when (> num-key-args 0)
	   `((setf (fa-key-arg-default-vals ,fa)
	       (make-array ,num-key-args :initial-contents (list ,@key-arg-default-asts)))))
       
       (excl:named-function ,name
	 (lambda (&rest %args)
	   (declare (dynamic-extent %args)
		    #.+optimize-std+)
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
			    `((setf ,*-arg (svref arg-val-vec ,num-pos-key-args))))
                        
			,@(when **-arg
			    `((setf ,**-arg (svref arg-val-vec ,(1+ num-pos-key-args)))))))
		    
		    (the-pop-way
		     `(progn ,@(loop for p in pos-key-arg-names collect `(setf ,p (pop %args))))))
		
		(cond ((or *-arg **-arg)  the-array-way)
		      (some-args-p        `(if (or (null only-pos-args)
                                                   (/= (the fixnum only-pos-args) ,num-pos-key-args))
					       ,the-array-way
					     ,the-pop-way))
		      (t `(when %args (raise-wrong-args-error)))))
	     
	     (locally #+(or)(declare (optimize (safety 3) (debug 3)))
	       ,@body)))))))

(defun check-1-kw-call (got-kw nargs-mi want-kw)
  (unless (and (= (excl::ll :mi-to-fixnum nargs-mi) 2)
               (eq got-kw want-kw))
    (raise-wrong-args-error)))

(defun slow-2-kw-call (nargs-mi a1 a2 a3 a4 kw12 f)
  (let ((nargs (excl::ll :mi-to-fixnum nargs-mi)))
    (destructuring-bind (kw1 kw2) kw12
      (multiple-value-bind (pa pb)
        (cond ((and (= nargs 3) (eq a2 kw2))
               (values a1 a3))
              ((= nargs 4)
               (cond ((and (eq a1 kw1)
                           (eq a3 kw2))
                      (values (values a2 a4)))
                     ((and (eq a1 kw2)
                           (eq a3 kw1))
                      (values a4 a2))
                     (t #1=(raise-wrong-args-error))))
              (t #1#))
        (funcall f pa pb)))))

(defmacro with-nof-args-supplied-as-mi ((n) &body body)
  "Bind N to nofargs, as machine integer (not regular fixnum)"
  `(let* ((,n (excl::ll :register :nargs)))
     ,@body))

(define-compiler-macro py-arg-function (&whole whole
                                               name (pos-args key-args *-arg **-arg) &body body)
  ;; More efficient argument-parsing, for functions that take only a few positional arguments.
  ;; Allegro passes the number of supplied args in a register; the code below makes use of
  ;; that register value.
  ;; 
  ;; If BODY creates closures, then the register value will be overwritten before we have
  ;; a chance to look at it. Therefore, if we read the :nargs register, the BODY is wrapped
  ;; in FLET.
  (when (or (>= (length pos-args) 3)
            key-args *-arg **-arg)
    (return-from py-arg-function whole))
  
  (ecase (length pos-args)
    (0 `(excl:named-function ,name
          (lambda ()
            ,@body)))
    
    (1 (let* ((pa (car pos-args))
              (ka (intern (symbol-name pa) :keyword))
              (e  (gensym "e")))
         `(excl:named-function ,name
            (lambda (,pa ,e)
              (declare ,+optimize-fastest+) ;; surpress default arg checking
              (let ((f-body (excl:named-function ,name
                              (lambda (,pa)
                                (declare ,+optimize-fastest+) ;; surpress default arg checking
                                (locally (declare ,+optimize-std+) ;; but run body with safety
                                  ,@body)))))
                (declare (dynamic-extent f-body))
                (with-nof-args-supplied-as-mi (nargs-mi)
                  (unless (eq nargs-mi (excl::ll :fixnum-to-mi 1))
                    (check-1-kw-call ,pa nargs-mi ,ka)
                    (setf ,pa ,e)))
                (funcall f-body ,pa))))))
    
    (2 (destructuring-bind (pa pb)
           pos-args
         (let ((ka (intern (symbol-name pa) :keyword))
               (kb (intern (symbol-name pb) :keyword))
               (e1 (gensym "e1"))
               (e2 (gensym "e2")))
           `(excl:named-function ,name
              (lambda (,pa ,pb ,e1 ,e2)
                (declare ,+optimize-fastest+) ;; surpress default arg checking
                (let ((f-body (excl:named-function ,name
                                (lambda (,pa ,pb)
                                  (declare ,+optimize-fastest+) ;; surpress default arg checking
                                  (locally (declare ,+optimize-std+) ;; but run body with safety
                                    ,@body)))))
                  (declare (dynamic-extent f-body))
                  (with-nof-args-supplied-as-mi (nargs-mi)
                    (if (and (eq nargs-mi (excl::ll :fixnum-to-mi 2))
                             (not (symbolp ,pa)))
                        (funcall f-body ,pa ,pb)
                      (slow-2-kw-call ,pa ,pb ,e1 ,e2
                                      nargs-mi
                                      '(,ka ,kb) f-body)))))))))))

(defstruct (func-args (:type vector) (:conc-name fa-) (:constructor make-fa))
  (num-pos-args         :type fixnum :read-only t)
  (num-key-args         :type fixnum :read-only t)
  (num-pos-key-args     :type fixnum :read-only t)
  (pos-key-arg-names    :type vector :read-only t)
  (key-arg-default-vals :type vector :read-only nil) ;; filled at load time
  (arg-kwname-vec       :type vector :read-only t)
  (*-arg                :type symbol :read-only t)
  (**-arg               :type symbol :read-only t)
  (func-name            :type symbol :read-onle t))
  

(defun parse-py-func-args (%args arg-val-vec fa)
  ;; %ARGS: the (&rest) list containing pos and ":key val" arguments
  ;; ARG-VAL-VEC: (dynamic extent) vector to store final argument values in
  ;;              => the penultimate item will get *-arg value (if any)
  ;;                 the last item **-arg value (if any)
  ;;                 so ARG-VAL-VEC must be larger than just num-pos-and-key-args! 
  ;; FA: func-args struct
  ;; Returns nothing
  (declare (optimize (safety 3) (debug 3))
	   (dynamic-extent %args)
	   (type list %args))
  
  (let ((num-filled-by-pos-args 0) for-* for-**)
    (declare (type (integer 0 #.most-positive-fixnum) num-filled-by-pos-args))
    
    ;; Match standard pos-args and *-arg
    (loop
	with max-to-fill-with-pos = (the fixnum (fa-num-pos-key-args fa))
	until (or (= num-filled-by-pos-args max-to-fill-with-pos)
		  (symbolp (car %args))) ;; the empty list NIL is a symbol, too
	      
	do (setf (svref arg-val-vec num-filled-by-pos-args) (fast (pop %args)))
	   (incf num-filled-by-pos-args)
	   
	finally
	  (unless (symbolp (car %args))
	    (cond ((fa-*-arg fa)
		   (setf for-*
		     ;; Reconsing because %args might be dynamic-extent.
		     (loop until (symbolp (car %args)) collect (fast (pop %args)))))
		  (t (raise-wrong-args-error)))))
    
    ;; All remaining arguments are keyword arguments;
    ;; they have to be matched to the remaining pos and
    ;; key args by name.
    
    (loop
	for key = (fast (pop %args)) and val = (fast (pop %args))
	while key do
	  ;; `key' is a keyword symbol
	  (or (block find-key-index
		(when (> (the fixnum (fa-num-pos-key-args fa)) 0)
		  (loop with name-vec = (fa-pos-key-arg-names fa)
		      with kwname-vec = (fa-arg-kwname-vec fa)
		      for i fixnum from num-filled-by-pos-args below
			(the fixnum (fa-num-pos-key-args fa))
		      when (eq (svref kwname-vec i) key)
		      do (when (svref arg-val-vec i)
			   (raise-double-keyarg-error (svref name-vec i)))
			 (setf (svref arg-val-vec i) val)
			 (return-from find-key-index t))))
	      
	      (when (fa-**-arg fa)
		(push (cons key val) for-**)
		t)

	      (raise-invalid-keyarg-error key)))
    
    ;; Ensure all positional arguments covered
    (loop for i fixnum from num-filled-by-pos-args below (the fixnum (fa-num-pos-args fa))
	unless (svref arg-val-vec i)
	do (raise-wrong-args-error))
    
    ;; Use default values for missing keyword arguments
    (loop for i fixnum from (fa-num-pos-args fa) below (the fixnum (fa-num-pos-key-args fa))
	unless (svref arg-val-vec i)
	do (setf (svref arg-val-vec i)
	     (svref (fa-key-arg-default-vals fa)
		    (the fixnum (- i (the fixnum (fa-num-pos-args fa)))))))

    ;; Create * arg
    (when (fa-*-arg fa)
      (setf (svref arg-val-vec (fa-num-pos-key-args fa))
	(make-tuple-from-list for-*)))

    ;; Create ** arg
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

(defmacro with-py-errors ((&key (name 'with-py-errors-func)) &body body)
  (check-type name (or symbol list))
  `(let ((f (excl:named-function ,name
              (lambda () ,@body))))
     (declare (dynamic-extent f))
     (call-with-py-errors f)))

(defun call-with-py-errors (f)
  (let ((*with-py-error-level* (fast (1+ (the fixnum *with-py-error-level*)))))
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
      (funcall f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;  Generator rewriting

(defun generator-ast-p (ast)
  "Is AST a function definition for a generator?"
  
  ;; Note that LAMBDA-EXPR can't contain (yield) statements
  
  (assert (not (match-p ast '([module-stmt] ?_))) ()
    "GENERATOR-AST-P called with a MODULE ast.")
  
  (with-py-ast (form ast)
    (case (car form)
      ([yield-expr]                     (return-from generator-ast-p t))
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
  
(defun funcdef-should-save-locals-p (ast env)
  (when (or *allow-indirect-special-call*
            (get-pydecl :function-must-save-locals env))
    (return-from funcdef-should-save-locals-p t))
  
  (with-py-ast (form ast)
    (case (car form)
      ([call-expr] (destructuring-bind (primary args)
		       (cdr form)
		     (declare (ignore args))
		     ;; `locals()' or `globals()'
                     (with-perhaps-matching (primary ([identifier-expr] ?name))
                       (when (member ?name '({locals} {globals} {eval}))
                         ;; We could check for num args here already, but that is a bit hairy,
                         ;; e.g. locals(*arg) is allowed if arg evaluates to e.g. [].
                         (return-from funcdef-should-save-locals-p t)))
		     form))
      ([exec-stmt] (return-from funcdef-should-save-locals-p t))
      (t form)))
  nil)

(defun rewrite-generator-funcdef-suite (fname suite)
  ;; Returns the function body
  (assert (symbolp fname))
  (assert (match-p suite '([suite-stmt] ?_)) ()
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
				   (go ,else-tag))
				 
				 ,else-tag
				 ,@(when else-suite
				     `((:split ,(walk else-suite stack))))
				 
                                 (go ,after-tag)
				 ,after-tag)
			       t))))
		  
		  ([yield-expr]
		   (let ((tag (new-tag :yield)))
		     (values `(:split (setf .state. ,tag)
				      (return-from function-body ,(or (second form) '*the-none*))
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
			(return-from function-body nil)))))))))))))


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
      (let ((iteration-stuff (loop with res = `([yield-expr] ,item)
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
			  nconc (multiple-value-list (apply-splits elm)))))
        (t (loop for elm in form
	       nconc (multiple-value-list (apply-splits elm))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Source locations of classes and functions

(defun record-source-file-loc (context-name kind)
  ;; Make source location known to Allegro, using "fi:lisp-find-definition".
  ;; Also record upper case version, apparently otherwise lower case names must
  ;; be |escaped| in ANSI mode.
  ;; Besides in :clpython.user, the sources are also recorded as symbols in
  ;; the :clpython package. That eases the use.
  (check-type context-name symbol)
  (let ((syms (list context-name 
                    (ensure-user-symbol (string-upcase context-name))
                    (intern (string context-name) (load-time-value (find-package :clpython)))
                    (intern (string-upcase context-name) (load-time-value (find-package :clpython))))))
    
    (excl:without-redefinition-warnings
     (dolist (s syms)
       (excl:record-source-file s :type kind)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Compiler warnings, unused variables

(defvar *comp-warning-dispatch* (copy-pprint-dispatch nil))

(defgeneric pprint-compiler-warning (stream cond)
  (:method (stream (c excl:compiler-inconsistent-name-usage-warning))
           (write-string ";;; Warning: " stream)
           (let ((mod (if (string= *current-module-name* +__main__-module-name+)
                          nil
                        *current-module-name*)))
             (format stream "~Aunction `~A': unused variable `~A'."
                     (if mod (format nil "Module `~A', f" *current-module-name*) "F")
                     compiler::.function-spec.
                     (car (simple-condition-format-arguments c))))))

(set-pprint-dispatch 'excl:compiler-inconsistent-name-usage-warning 'pprint-compiler-warning 0 *comp-warning-dispatch*)

(defun call-with-better-python-style-warnings (f)
  ;; Old:  Warning: Variable clpython.user::x is never used.
  ;; New:  Warning: Variable {x} is never used.
  (let ((*print-pprint-dispatch* *comp-warning-dispatch*))
    (handler-bind ((excl:compiler-inconsistent-name-usage-warning 
                    (lambda (c)
                      (format t "~A~%" c)
                      (muffle-warning c))))
      (funcall f))))

(defmacro with-python-compiler-style-warnings (&body body)
  `(flet ((.f () ,@body))
     (declare (dynamic-extent #'.f))
     (call-with-better-python-style-warnings #'.f)))


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
