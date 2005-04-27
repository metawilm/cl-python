(in-package :python)

(defparameter *scope* nil "Current execution namespace")
(defparameter *__debug__* 1
  "CPython readonly variable `__debug__' (see EVAL-ASSERT)")
(defparameter *__future__.division* nil)

(defparameter *active-excepts* nil)

;;; Evaluation

(defun user-py-eval (ast &optional namespace)
  "Evaluate some Python code. This is the function to be called for ~
   evaluating user-supplied code in the form of an AST, and ~
   optionally a namespace."
  
  (let ((*scope* (or namespace 
		     (make-namespace :name "<new user-py-eval ns>"
				     :builtins t))))
    (namespace-bind *scope* '__name__ "__main__")
    (py-eval ast)))


(defvar *py-eval-handler-set* nil)

;; During evaluation of Python code, some Lisp errors may occur. Some
;; of them are catched and converted to the corresponding Python
;; exception.

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
	
	;; XXX more?
	)
     ,@body))
     
(defun py-eval (ast)
  "Evaluate AST. Assumes *scope* is set appropriately. Will set error ~
   handlers if not already set. Call PY-EVAL-1 when you know error ~
   handlers are set already."
  
  (if *py-eval-handler-set*
      (py-eval-1 ast)
    (with-py-error-handlers
	(let ((*py-eval-handler-set* t))
	  (py-eval-1 ast)))))


(defmethod py-eval-1 ((x python-object)) x)
(defmethod py-eval-1 ((x python-type))   x)
(defmethod py-eval-1 ((x number))        x)
(defmethod py-eval-1 ((x string))        x)
(defmethod py-eval-1 ((x function))      x)
(defmethod py-eval-1 ((x symbol))
  (unless x (warn "py-eval of NIL"))  ;; TODO check NIL as identifier
  x)  ;; string designator  XXX ?
(defmethod py-eval-1 ((x class)) x)


(defmethod py-eval-1 ((ast list))
  (case (car ast)
    (assert (funcall #'eval-assert (cdr ast)))
    (assign-expr (funcall #'eval-assign-expr (cdr ast)))
    (attributeref (funcall #'eval-attributeref (cdr ast)))
    (augassign-expr (funcall #'eval-augassign-expr (cdr ast)))
    (backticks (funcall #'eval-backticks (cdr ast)))
    (binary (funcall #'eval-binary (cdr ast)))
    (binary-lazy (funcall #'eval-binary-lazy (cdr ast)))
    (break (funcall #'eval-break (cdr ast)))
    (call (funcall #'eval-call (cdr ast)))
    (classdef (funcall #'eval-classdef (cdr ast)))
    (comparison (funcall #'eval-comparison (cdr ast)))
    (continue (funcall #'eval-continue (cdr ast)))
    (del (funcall #'eval-del (cdr ast)))
    (dict (funcall #'eval-dict (cdr ast)))
    (ellipsis (funcall #'eval-ellipsis (cdr ast)))
    (file-input (funcall #'eval-file-input (cdr ast)))
    (for-in (funcall #'eval-for-in (cdr ast)))
    (funcdef (funcall #'eval-funcdef (cdr ast)))
    (global (funcall #'eval-global (cdr ast)))
    (identifier (funcall #'eval-identifier (cdr ast)))
    (if (funcall #'eval-if (cdr ast)))
    (import (funcall #'eval-import (cdr ast)))
    (import-from (funcall #'eval-import-from (cdr ast)))
    (lambda (funcall #'eval-lambda (cdr ast)))
    (list (funcall #'eval-list (cdr ast)))
    (list-compr (funcall #'eval-list-compr (cdr ast)))
    (pass (funcall #'eval-pass (cdr ast)))
    (print (funcall #'eval-print (cdr ast)))
    (print->> (funcall #'eval-print->> (cdr ast)))
    (raise (funcall #'eval-raise (cdr ast)))
    (return (funcall #'eval-return (cdr ast)))
    (slice (funcall #'eval-slice (cdr ast)))
    (subscription (funcall #'eval-subscription (cdr ast)))
    (suite (funcall #'eval-suite (cdr ast)))
    #+(or)(testlist (funcall #'eval-testlist (cdr ast)))
    (tuple (funcall #'eval-tuple (cdr ast)))
    (try-except (funcall #'eval-try-except (cdr ast)))
    (try-finally (funcall #'eval-try-finally (cdr ast)))
    (unary (funcall #'eval-unary (cdr ast)))
    (while (funcall #'eval-while (cdr ast)))
    ;; YIELD can't occur: rewritten
    (t (error "uncatched in py-eval: ~S~%" ast))))


;; This was an attempt to speed up interpreting the AST by putting
;; functions in the AST in place of the first list term and funcalling
;; the function. It turned out this does not make a difference w.r.t speed.

#+(or)
(defmethod py-eval-1 ((ast list))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (py-eval-2 (car ast) ast))

#+(or)
(defmethod py-eval-2 ((x symbol) ast)
  (let ((f (symbol-function (intern (format nil "eval-~A" (car ast)) #.*package*))))
    (setf (car ast) f)
    (funcall f (cdr ast))))

#+(or)
(defmethod py-eval-2 ((x function) ast)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (funcall x (cdr ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun careful-py-eval (ast &optional namespace)
  "Tries to evaluate AST. When the evaluation raises an exception, ~
   NIL is returned; otherwise the value of AST is returned."
  ;; Used to evaluate compile-time constants.
  ;; TODO: are there computations that are considered constant but
  ;; that might take very long to calculate?!
  (when (and (listp ast)
	     (eq (car ast) 'file-input)
	     (= (length (second ast)) 1))
    (setq ast (first (second ast))))
  (handler-case (values (if namespace
			    (py-eval ast)
			  (user-py-eval ast)))
    (Exception () nil)
    (error (e) (progn
		 (warn "Carefully evaluating ast ~A gave a Lisp error: ~A"
		       ast e)
		 nil))
    (:no-error (val)
      val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun eval-file-input (list-ast-items)
  (cons 'file-input (mapcar #'py-eval-1 (car list-ast-items))))

#+(or)
(defun eval-testlist (items-comma?)
  (let ((items (first items-comma?))
	(comma? (second items-comma?)))
    (unless (or items comma?)
      (return-from eval-testlist (make-tuple)))
    
    (let ((make-tuple (or comma?
			  (>= (length items) 2))))
      (if make-tuple
	  (make-tuple-from-list (mapcar #'py-eval-1 items))
	(py-eval-1 (car items))))))

(defun eval-funcdef (fname-params-suite)
  "In the current namespace, FNAME becomes bound to a function object ~@
   with given formal parameters and function body."
  
  ;; Special-case: the function, when called, will return a
  ;; generator. If this is the case, treat it a bit special.
  ;; 
  ;; Note that 'yield' inside "exec" or "eval" is not allowed, so it
  ;; can be statically checked whether the function returns a
  ;; generator or not.

  (destructuring-bind (fname params suite) fname-params-suite
    
    (if (generator-ast-p suite)
	
	(let* ((params
		(multiple-value-list (parse-function-parameter-list params)))
	       (f (make-python-function-returning-generator
		   (symbol-name fname) params suite)))
	  (namespace-bind *scope* fname f))
  
      (let* ((params 
	      (multiple-value-list (parse-function-parameter-list params)))
	     (namespace (make-namespace
			 :name (format nil "ns for function ~A" fname)
			 :inside *scope*))
	     (call-rewriter (apply #'make-call-rewriter fname params))
	     (ast (add-return-None-to-suite suite))
	     (f (make-user-defined-function 
		 :name (symbol-name fname)
		 :ast ast
		 :namespace namespace
		 :params params
		 :call-rewriter call-rewriter
		 :call-handler
		 (lambda (pos-args key-args)
		   (let ((actual-args 
			  (funcall call-rewriter pos-args key-args))
			 (namespace-to-use (namespace-copy namespace)))
		     (loop for (arg . val) in actual-args
			 do (namespace-bind namespace-to-use arg val))
		     (let ((*scope* namespace-to-use))
		       (declare (special *scope*))
		       (catch 'function-block
			 (py-eval-1 ast))))))))
	
	;; Content of function is not evaluated yet; only when called.
	;; Bind function name to function object in current namespace:
	(namespace-bind *scope* fname f)))))

(defun add-return-None-to-suite (suite)
  (assert (eq (car suite) 'suite))
  (destructuring-bind
      (dummy (&rest suites)) suite
    (declare (ignore dummy))
    `(suite (,@suites (return (identifier None))))))

(defun parse-function-parameter-list (params)
  "Returns POS-PARAMS, KW-PARAMS, *-PAR, **-PAR as multiple values"

  (destructuring-bind (pos-kw-params *-par **-par)
      params
    
    ;; Keyword arguments must come after positional arguments.
    ;; The grammar doesn't restrict that, so check it here.
    
    (let ((found-kw nil))
      (dolist (x pos-kw-params)
	(cond ((consp x)
	       (setf found-kw (car x)))
	      ((and found-kw (symbolp x))
	       (py-raise 'SyntaxError
			 "Non-default argument '~A' follows default ~
                          argument '~A'" x found-kw))
	      (t ))))
    
    (loop for x in pos-kw-params
	if (consp x) 
	collect (progn (setf (cdr x) (py-eval-1 (cdr x)))
		       x)
	into kw-args
	else collect x into pos-args
	finally (return (values pos-args kw-args *-par **-par)))))


(defun eval-classdef (cname-inheritance-suite)
  (destructuring-bind ((identifier cname) inheritance suite) cname-inheritance-suite
    (assert (eq identifier 'identifier))
    
    ;; The inheritance list will be evaluated, but MAKE-PYTHON-CLASS
    ;; expects a list of *names*, therefore after each of the items is
    ;; evaluated and has a class object as value, CLASS-NAME must be
    ;; called.
  
    (let* ((ns (make-namespace :name (format nil "<ns for class ~A>" cname)
			       :inside *scope*))
	   (supers (mapcar
		    (lambda (x)
		      (let ((c (py-eval-1 x)))
			(etypecase c
			  (symbol (find-class c))
			  (class c))))
		    inheritance)))
    
      ;; Evaluate SUITE now, in the new namespace inside the class:
      ;; methods and class attributes are defined in it, plus there may
      ;; be other side effects.
      ;; 
      ;; Need to do this now, in order for __slots__ to possible be
      ;; bound, usedin the next step. (XXX Check order ok)
    
      (when suite
	(let ((*scope* ns))
	  (py-eval-1 suite)))
    
      (multiple-value-bind (slots has-slots)
	  (multiple-value-bind (val found)
	      (namespace-lookup ns '__slots__)
	    (if found
		(values (mapcar (lambda (name) (intern (typecase name
							 (string name)
							 (py-string (slot-value name 'string)))
						       #.*package*))
				(py-iterate->lisp-list val))
			t)
	      (values nil nil)))
	(declare (ignore slots has-slots))
    
	(let* (#+(or)(doc (or (namespace-lookup ns '__doc__) *None*))
	       (metaclass (or (namespace-lookup ns '__metaclass__)
			      (load-time-value (find-class 'python-type))))
	       (c (call-attribute-via-class metaclass '__new__ (list cname supers ns))))
	  (call-attribute-via-class c '__init__ (list cname supers ns)) ;; ignore result
	  
	  #+(or)(c (make-python-class :name cname :module "ModuleName" :supers supers
				      :slots slots :has-slots has-slots :namespace ns
				      :metaclass metaclass :documentation doc))
	  
	  ;; In the current namespace, the name of the class that is defined
	  ;; now becomes bound to the class object that results from
	  ;; evaluating the classdef.
	  (namespace-bind *scope* cname c)
	  
	  ;; Finally, return the class (purely for debuggin: classdef has no
	  ;; return value, as it is a stmt, not an expr.
	  c)))))

#+(or)
(defun eval-classdef (cname-inheritance-suite)
  (destructuring-bind (cname inheritance suite) cname-inheritance-suite
    
    ;; The inheritance list will be evaluated, but MAKE-PYTHON-CLASS
    ;; expects a list of *names*, therefore after each of the items is
    ;; evaluated and has a class object as value, CLASS-NAME must be
    ;; called.
  
    (let* ((ns (make-namespace :name (format nil "<ns for class ~A>" cname)
			       :inside *scope*))
	   (supers (mapcar
		    (lambda (x)
		      (let ((c (py-eval-1 x)))
			(etypecase c
			  (symbol (find-class c))
			  (class c))))
		    inheritance)))
    
      ;; Evaluate SUITE now, in the new namespace inside the class:
      ;; methods and class attributes are defined in it, plus there may
      ;; be other side effects.
      ;; 
      ;; Need to do this now, in order for __slots__ to possible be
      ;; bound, usedin the next step. (XXX Check order ok)
    
      (when suite
	(let ((*scope* ns))
	  (py-eval-1 suite)))
    
      (multiple-value-bind (slots has-slots)
	  (multiple-value-bind (val found)
	      (namespace-lookup ns '__slots__)
	    (if found
		(values (mapcar (lambda (name) (intern (typecase name
							 (string name)
							 (py-string (slot-value name 'string)))
						       #.*package*))
				(py-iterate->lisp-list val))
			t)
	      (values nil nil)))
    
	(let* ((doc (or (namespace-lookup ns '__doc__) *None*))
	       (metaclass (or (namespace-lookup ns '__metaclass__) nil))
	       (c (make-python-class :name cname :module "ModuleName" :supers supers
				     :slots slots :has-slots has-slots :namespace ns
				     :metaclass metaclass :documentation doc)))
      
	  ;; In the current namespace, the name of the class that is defined
	  ;; now becomes bound to the class object that results from
	  ;; evaluating the classdef.
	  (namespace-bind *scope* cname c)
    
	  ;; Finally, return the class (purely for debuggin: classdef has no
	  ;; return value, as it is a stmt, not an expr.
	  c)))))

(defun eval-import (items)
  (dolist (item (car items))
    (eval-import-one item)))

(defun eval-import-one (item)  ;; XXX untested
  ;; (break "import: TODO")
  
  (let* ((mod-name-ast (second item))
	 (mod-dotted-string (module-list-to-dotted-string mod-name-ast))
	 (mod-obj (py-dict-gethash *sys.modules* mod-dotted-string))
	 (first-name-symbol (if (symbolp mod-name-ast)
				mod-name-ast 
			      (car (second mod-name-ast))))
	 (bind-name (if (eq (first item) 'as)
			(third item) 
		      first-name-symbol)))
      
    (when mod-obj  ;; Module already loaded. Simply bind name.
      (namespace-bind *scope* bind-name mod-obj)
      (return-from eval-import-one nil))
    
    (when (symbolp mod-name-ast) ;; Simple case of a single module
      (let ((mod-obj (or (load-py-source-file first-name-symbol 
					      (py-iterate->lisp-list *sys.path*)
					      :module-ok t :package-ok t)
			 (py-raise 'ImportError
				   "Could not import module/package ~A" first-name-symbol))))
	(namespace-bind *scope* bind-name mod-obj))
      (return-from eval-import-one nil))
    
    
    ;; Determine how much of the modules are already imported
    ;; (for "foo.bar.baz", lookup "foo", "foo.bar", .. until
    ;; it stalls).
      
    (progn
      (assert (eq (first mod-name-ast) 'dotted))
      (let* ((packages-todo (butlast (cdr (second mod-name-ast))))
	     (first-pkg-name (pop packages-todo)))
	
	;; Is first pkg name available?
	
	(let ((first-pkg
	       (or (py-dict-gethash *sys.modules* (string first-pkg-name))
		   (load-py-source-file first-name-symbol
					(py-iterate->lisp-list *sys.path*)
					:module-ok nil :package-ok t)
		   (py-raise 'ImportError
			     "Could not import package ~A (as part of ~A)"
			     first-pkg-name mod-dotted-string))))
	  
	  (check-type first-pkg py-package)
	  	  
	  ;; Then traverse from first package to second etc
	  
	  (loop with curr-pkg = first-pkg
	      with curr-path-dots and curr-path-slashes
	      with next-pkg-name
	      with pkg-done = ()
	      while packages-todo
	      do 
		(setf next-pkg-name (pop packages-todo))
		(push next-pkg-name pkg-done)
		(setf curr-path-dots (module-list-to-dotted-string
				      `(dotted ,@(reverse pkg-done)))
		      curr-path-slashes (module-list-to-dotted-string
					 `(dotted ,@(reverse pkg-done)) "/"))
		
		(let ((pkg-obj (py-dict-gethash *sys.modules* curr-path-dots)))
		   
		  (if pkg-obj
		      
		      (progn (check-type pkg-obj py-package)
			     (setf curr-pkg pkg-obj))

		    (let* ((pkg-dir (slot-value curr-pkg 'directory))
			   (next-pkg-dir (make-pathname :directory pkg-dir
							:name next-pkg-name))
			   (__init__.py-path (make-pathname :directory next-pkg-dir
							    :name "__init__.py")))
		      (cond 
		       ((not (path-directory-p pkg-dir))
			(py-raise 'ImportError "Directory ~A disappeared" pkg-dir))
		       ((not (path-directory-p next-pkg-dir))
			(py-raise 'ImportError "Package ~A not found (directory ~A)"
				  next-pkg-name next-pkg-dir))
		       ((not (path-file-p __init__.py-path))
			(py-raise 'ImportError "Package ~A has no __init__.py at ~A"
				  next-pkg-name __init__.py-path)))
		       
		      (let ((mod-name-ast (parse-python-string (read-file __init__.py-path)))
			    (ns (make-namespace :builtins t)))
			 
			(loop for (k . v) in
			      `((__path__ ,(make-py-list-from-list curr-path-slashes))
				(__file__ ,(concatenate 'string
					     curr-path-dots "/__init__.py"))
				(__name__ ,(string curr-pkg)))
			    do (namespace-bind ns k v))
		   
			(let ((*scope* ns))
			  (py-eval-1 mod-name-ast))
			 
			(setf curr-pkg
			  (make-py-package
			   :namespace ns
			   :init-file (truename __init__.py-path)
			   :directory (truename next-pkg-dir) ))))))))))))

;; XXX todo
(defun eval-import-from (mod-items)
  ;; (break "import: TODO")
  ;; "from a.b.c import d"  -->  (import-from (dotted a b c) ((as d d)))
  ;; "from a import b"      -->  (import-from a ((as b b)))
  (destructuring-bind (mod items) mod-items
    
    ;; First, evaluate the source from which the items should come.
  
    (let* ((mod-string (module-list-to-dotted-string mod))
	   (mod-obj (py-dict-gethash *sys.modules* mod-string)))
    ;;; XXXX todo....
      (cond (mod-obj (let ((ns (slot-value mod-obj 'namespace)))
		       (loop for (as? name-in-mod bind-name) in items
			   do (assert (eq as? 'as))
			      (namespace-bind *scope* bind-name
					      (or (namespace-lookup ns name-in-mod)
						  (py-raise 'NameError
							    "Module ~A has no attribute ~A"
							    mod-obj name-in-mod))))))
    
	    ((not mod-obj)
	     (let ((mod-obj (or (load-py-source-file mod-string
						     (py-iterate->lisp-list *sys.path*))
				(py-raise 'ImportError "No module named ~A" mod-string))))
	
	       (cond ((eq items '*)
		      (multiple-value-bind (all-names found)
			  (internal-get-attribute mod-obj '__all__)
			(if found
		     
			    ;; First attempt: the __all__ attribute of the module.
		     
			    (dolist (name (py-iterate->lisp-list all-names))
			      (namespace-bind *scope* name 
					      (or (internal-get-attribute mod-obj name)
						  (py-raise 'AttributeError
							    "Module ~A has no attribute ~A ~@
                                                  (mentioned in __all__)"
							    mod-obj name))))
		   
			  ;; Fall-back: import all names from the module dict
			  ;; that don't start with underscore.
		   
			  (loop for (name . val) in (dict->alist (package/module-dict mod-obj))
			      when (char/= (aref 0 (string name)) #\_) 
			      do (namespace-bind *scope* name val)))))
	      
		     (t
		      (loop for (as? name-in-mod name-here) in items
			  do (assert (eq as? 'as))
			     (let ((obj (or (internal-get-attribute mod-obj name-in-mod)
					    (py-raise 'ImportError "Module ~A has no attribute ~A"
						      mod-string name-in-mod))))
			       (namespace-bind *scope* name-here obj)))))))))))

(defun eval-assign-expr (items)
  (destructuring-bind (val targets) items
    (let ((ev-val (py-eval-1 val)))
      (unless ev-val
	(error "PY-EVAL returned NIL for value to be assigned: ~A" val))
      (dolist (tar targets)
	(eval-assign-one (eval-one-assignment-target tar) ev-val)
	#+(or)(eval-real-assign-expr tar ev-val)))))

(defun eval-augassign-expr (target-operator-expr)
  (destructuring-bind (operator target expr) target-operator-expr
    
    ;; e()[ f() ] += g()  is evaluated as follows:
    ;; 
    ;;   e() -> m  \__in eval-one-assignment-target
    ;;   f() -> p  /
    ;; 
    ;;   g() -> q
    ;;   m[p] -> r
    ;; try  r.__iadd__(q)
    ;; if no __iadd__method found, try:  x[p] = r + q
    
    (cond ((not (listp target))
	   (py-raise 'SyntaxError "Augmented assignment can't have atom as target (got: ~A)" target))
	  
	  ((eq (car target) 'tuple)
	   (py-raise 'SyntaxError
		     "Augmented assign to multiple places not possible (target AST: ~A)" target))
	  
	  ((not (member (car target) '(identifier subscription attributeref)))
	   (py-raise 'SyntaxError
		     "Augmented assign to forbidden place (maybe TODO) (target AST: ~A)" target))
	  
	  (t (let* ((ev-target (eval-one-assignment-target target))
		    (ev-expr (py-eval-1 expr))
		    (target-value-now (py-eval-1 ev-target))
		    (op-funcs (cdr (assoc operator *math-inplace-op-assoc*)))
		    (py-@= (car op-funcs))
		    (py-@ (cdr op-funcs)))
	       
	       (assert (and py-@= py-@))
	       ;; try __iadd__ first, otherwise __add__ + store
	       (unless (funcall py-@= target-value-now ev-expr) ;; returns true iff __iadd__ found
		 (let ((value-to-store (funcall py-@ target-value-now ev-expr)))
		   (eval-assign-one ev-target value-to-store)))))))
  *None*)

       
(defun eval-assign-one (target val)
  "TARGET is evaluated as far as possible, but it still contains ~@
   identifier, subscription, attributeref. VAL is the fully evaluated ~@
   value to be assigned."
  
  (when (not (listp target))
    (py-raise 'SyntaxError "A literal is not a valid assignment target (got: ~A)" target))

  (ecase (car target)
    
    (identifier    ;; x = 5  (identifier x)
     (namespace-bind *scope* (second target) val))
	 
    (attributeref  ;; x.y = 5  (attributeref <primary> (identifier attr-name))
     (destructuring-bind (primary (identifier attr-name))
	 (cdr target)
       (assert (eq identifier 'identifier))
       (internal-set-attribute primary attr-name val)))
    
    (subscription  ;; x[y] = 5  (subscription <primary> <subs>)
     (destructuring-bind (primary subs)
	 (cdr target)
       (multiple-value-bind (res setitem-found)
	   (call-attribute-via-class primary '__setitem__ (list subs val))
	 (declare (ignore res))
	 (unless setitem-found
	   (py-raise 'TypeError "Can't set item on object ~A" primary)))))
    
    ((tuple list)   ;; [a,b] = (c,d) = 3,4  (list (<A> <B> ...))
     (let* ((targets (second target))
	    (val-vec (py-iterate-n-values val (length targets))))
       (loop for val across val-vec
	   for tar in targets
	   do (eval-assign-one tar val))))))


(defun eval-assignment-targets (targets)
  ;; x     ->  `(identifier x)
  ;; x[y]  ->  `(subscription ,x ,y)
  ;; x.y   ->  `(attributeref ,x y)
  ;; [x,y] ->  `(list x y)  -- x,y recursive
  (mapcar #'eval-one-assignment-target targets))

(defun eval-one-assignment-target (tg)
  
  (when (atom tg)
    (py-raise 'SyntaxError
	      "Cannot assign to a literal (got target: ~A)" tg))
  
  (ecase (car tg)
    ;; (IDENTIFIER a) : remains the same
    (identifier tg)
  
    ;; (TUPLE (A B)) and (LIST (A B)) both go to (LIST (<A> <B>))
    ((tuple list) `(,(car tg) ,(mapcar #'eval-one-assignment-target (second tg))))
    
    ;; foo.x => (attributeref (identifier foo) (identifier x))
    ;; 
    ;; The primary is evaluated; the attribute identifier
    ;; is just a name and can't be further evaluated.
    (attributeref `(attributeref ,(py-eval-1 (second tg)) ,(third tg)))
    
    ;; x[1,2,3]
    ;; => (subscription (identifier x) ((1 2 3) nil))) nil)
    ;;
    ;; Both primary and expr-list are evaluated; result is
    ;;  (subscription <object> <item-or-tuple>)
    (subscription 
     (destructuring-bind (primary sub)
	 (cdr tg)
       `(subscription ,(py-eval-1 primary)
		      ,(py-eval-1 sub)))) ;; check -- tuple etc
      
    #+(or) ;; needed?
    (t
     (warn "EVAL-ASSIGNMENT-TARGETS: assuming ~A is an expression ~
            to be evaluated." tg)
     (py-eval-1 tg))))

(defun eval-del (item)
  (setf item (car item))
  (let ((place2 (eval-one-assignment-target (second item))))
    (ecase (car place2)
      
      (identifier   (or (namespace-delete *scope* (second place2))
			(py-raise 'ValueError "Can't delete variable ~A: it's not bound"
				  (second place2))))
      
      (subscription (let ((primary (second place2)))
		      (multiple-value-bind (res delitem-found)
			  (call-attribute-via-class primary '__delitem__ (cddr place2))
			(declare (ignore res))
			(unless delitem-found
			  (py-raise 'TypeError "Can't delete items from ~A (no __delitem__ in class)"
				    primary)))))
      
      (attributeref (warn "deletion of attribute: a bit buggy")
		    (destructuring-bind (primary (identifier att-name))
			(cdr place2)
		      (assert (and (eq identifier 'identifier)))
		      (multiple-value-bind (res delattr-found)
			  (call-attribute-via-class primary '__delattr__ (list att-name))
			(declare (ignore res))
			(unless delattr-found
			  (py-raise 'TypeError "Can't delete attribute from ~A (no __delattr__ in class)"
				    primary))))))))

(defun eval-global (varlist)
  (setf varlist (car varlist))
  (let ((ns *scope*))
    (dolist (varname varlist)
      (namespace-declare-global ns varname))))

(defun eval-try-except (suite--except-clauses--else-clause)
  ;;(declare (optimize (debug 3)))
  
  ;; Note that the Exception class that an 'except' clause catches, is
  ;; evaluated after an exception is thrown, not earlier; so long as
  ;; there is no exception thrown, it is not evaluated.
  ;; 
  ;; That is also the reason why we can't use handler-case: we don't
  ;; want to unwind for exceptions that are not catched by an
  ;; `except:' clause. Instead, we want to get into the debugger to
  ;; analyze and perhaps resume executions.
  
  (destructuring-bind
      (suite except-clauses else-clause) suite--except-clauses--else-clause
    (let* ((handler-scope *scope*)
	   (handler-func
	    (lambda (exc)
	      (loop for (cls/tuple parameter handler-form) in except-clauses
		  do (cond 
		     
		      ;; `except Something:'  where Something a class or tuple
		      ((and cls/tuple  
			    (let ((ecls/tuple (let ((*scope* handler-scope))
						(py-eval-1 cls/tuple))))
			      (typecase ecls/tuple
				(class    (typep exc ecls/tuple))
				(py-tuple (loop for cls in (tuple->lisp-list ecls/tuple)
					      when (typep exc cls)
					      do (return t)
					      finally (return nil)))
				(t (warn "Non-class as `except' specializer ~
                                          (ignored): ~S" ecls/tuple)
				   nil))))
		       (let ((*scope* handler-scope))
			 (when parameter
			   (assert (eq (first parameter) 'identifier))
			   (namespace-bind handler-scope (second parameter) exc))
			 (py-eval-1 handler-form))
		       (return-from eval-try-except nil))
				
		      ((null cls/tuple)
		       ;; a bare `except:' matches all exceptions
		       (let ((*scope* handler-scope))
			 (py-eval-1 handler-form))
		       (return-from eval-try-except nil))
		     
		      (t ;; error not catched by this exception
		       ))))))
	  
      (handler-bind 
	  ((Exception handler-func))
	
	;; The `py-error-handlers' we re already set in py-eval. Need to
	;; set them here again, because when one of the py-eval
	;; handler-bind handlers takes control, the handler above for
	;; Exception is not active anymore.
	
	(if *track-exception-stack*
	    
	    ;; XXX combine with stuff in handler-func
	    (loop with excepts = ()
		for ((cls/tuple nil) nil) in except-clauses
		do (if (null cls/tuple)
		       (push (find-class 'Exception) excepts) ;; bare `except: ...'
		     (let ((ecls/tuple (py-eval-1 cls/tuple)))
		       (etypecase ecls/tuple
			 (class (push ecls/tuple excepts))
			 (py-tuple (dolist (cls (tuple->lisp-list ecls/tuple))
				     (push cls excepts))))))
		finally (let ((*active-excepts* (cons (nreverse excepts) *active-excepts*)))
			  (with-py-error-handlers
			      (py-eval-1 suite))))
	  
	  (with-py-error-handlers
	      (py-eval-1 suite))))
      
      (assert (eq *scope* handler-scope))
      (when else-clause
	(py-eval-1 else-clause)))))

(defun eval-try-finally (suite--suite)
  (destructuring-bind (try-suite finally-suite) suite--suite
    ;; not IGNORE-ERRORS, as we don't want to catch Lisp errors here
    (handler-case (values (py-eval-1 try-suite))
      (Exception ())
      (:no-error ()))
    (py-eval-1 finally-suite)))

(defun eval-raise (first-second-third) ;; exctype value traceback)
  ;; Complicated interpretation of parameters. See Python Reference Manual, par 6.9
  (destructuring-bind (first second third) first-second-third
    (setf first  (when first (py-eval-1 first))
	  second (when second (py-eval-1 second))
	  third  (when (and third (not (eq third *None*)))
		   (py-eval-1 third)))

    (cond (third
	   ;; "If a third object is present and not None, it must be a
	   ;; traceback object (see section 3.2), and it is substituted
	   ;; instead of the current location as the place where the
	   ;; exception occurred."
	   (error "TODO: Traceback parameter to RAISE"))
	
	  ((null first)
	   ;; "If no expressions are present, raise re-raises the last
	   ;; expression that was active in the current scope. If no
	   ;; exception is active in the current scope, an exception is
	   ;; raised indicating this error." 
	   (error "TODO: reraise previous exception"))

	  ((typep first 'class)
	   #+(or)(warn "raise c")
	   ;; "If the first object is a class, it becomes the type of
	   ;; the exception.
	   ;; 
	   ;; The second object is used to determine the exception
	   ;; value: If it is an instance of the class, the instance
	   ;; becomes the exception value. If the second object is a
	   ;; tuple, it is used as the argument list for the class
	   ;; constructor; if it is None, an empty argument list is
	   ;; used, and any other object is treated as a single argument
	   ;; to the constructor. The instance so created by calling the
	   ;; constructor is used as the exception value."
	   (cond ((typep second first)
		  (error second))
	       
		 (second
		  ;; not quite correct, but will do for now... (XXX call __new__ etc)
		  (let ((cond (make-condition first :args second)))
		    (error cond)))

		 ((null second)
		  (let ((cond (make-condition first)))
		    (error cond)))
		 (t
		  (error "shouldn't come here"))))

	  ((py-string-designator-p first)
	   (let ((str (py-string-designator-val first)))
	     (py-raise 'Exception str)))
	  
	  (t
	   ;; "If the first object is an instance, the type of the
	   ;; exception is the class of the instance, the instance itself
	   ;; is the value, and the second object must be None."
	   (if (not second)
	       (error first)
	     (py-raise 'ValueError
		       "RAISE: when first arg is instance, second argument must ~@
                      be None or not supplied (got: ~A)" second))))))

(defun eval-identifier (name)
  "Look up the identifier in the active namespaces, and fall back to
   looking in the (module) object named '__builtins__."
  (let ((name (car name)))

    (or (namespace-lookup *scope* name) ;; traverses all enclosing scopes too
      
	(let ((bi (namespace-lookup *scope* '__builtins__)))
	  (when bi
	    (namespace-lookup bi name)))
      
	(py-raise 'NameError "Name ~A is not defined" name))))

(defun eval-list (data)
  (make-py-list-from-list (mapcar #'py-eval-1 (car data))))
  
(defun eval-list-compr (data)
  (let* ((expr (first data))
	 (list-for-ifs (second data))
	 (acc (make-array 0 :adjustable t :fill-pointer 0)))
    
    (labels ((process-for/ifs (for-ifs)
	       (if (null for-ifs)
		   (vector-push-extend (py-eval-1 expr) acc)
		 (let ((clause (pop for-ifs)))
		   (ecase (car clause)
		     (list-for-in (process-for-in (second clause) (third clause) for-ifs))
		     (list-if     (process-if (second clause) for-ifs))))))
	     
	     (process-for-in (exprlist source for-ifs)
	       (let ((f (lambda (x) (eval-assign-one exprlist x) (process-for/ifs for-ifs))))
		 (declare (dynamic-extent f))
		 (map-over-py-object f (py-eval-1 source))))
	     
	     (process-if (condition for-ifs)
	       (when (py-val->lisp-bool (py-eval-1 condition))
		 (process-for/ifs for-ifs))))
      
      (process-for/ifs list-for-ifs)
      (make-py-list acc))))


(defun eval-dict (data)
  ;; eval keys and values in this order: key1, val1, key2, val2, ...
  (make-dict (mapcar (lambda (kv) (cons (py-eval-1 (car kv))
					(py-eval-1 (cdr kv))))
		     (car data))))

(defun eval-backticks (obj)
  (py-repr (py-eval-1 (first obj))))

(defun eval-call (primary-args)
  (destructuring-bind (primary args) primary-args
    
    (let ((eprim (py-eval-1 primary)))
    
      ;; f(1, 2, b=3, c=4, *(1,2), **{'q':6})
      ;; corresponds with ARGS:
      ;; (1 2 (= b 3) (= c 4) (* (testlist ...)) (** (dict ...)))
    
      (when (null args)
	(return-from eval-call (py-call eprim)))
    
      (let* ((x (pop args))
	   
	     (pos-args  (loop while (eq (car x) 'pos)
			    collect (py-eval-1 (second x))
			    do (setf x (pop args))))
	   
	     (kw-args   (loop while (eq (car x) 'key)
			    collect (cons (second x) (py-eval-1 (third x)))
			    do (setf x (pop args))))
      
	     (*-arg     (when (eq (car x) '*)
			  (prog1 (py-eval-1 (second x))
			    (setf x (pop args)))))
	      
	     (**-arg    (when (eq (car x) '**)
			  (py-eval-1 (second x)))))
      
	(assert (null args))
      
	#+(or)(break "between: pos=~A, kw=~A, *=~A, **-~A" pos-args kw-args *-arg **-arg)
	  
	(when *-arg
	  (setf pos-args (nconc pos-args (py-iterate->lisp-list *-arg))))
      
	(when **-arg
	  (check-type **-arg py-dict "a dictionary (subclass) instance")
	  (loop for (key . val) in (dict->alist **-arg)
	      with key-py-obj
	      with key-symbol
	    
	      do (setf key-py-obj (convert-to-py-object key))
	       
		 (if (typep key-py-obj 'py-string)
		     (setf key-symbol (py-string->symbol key-py-obj))
		   (py-raise 'TypeError "**-argument in function call must be a dictionary with strings as keys ~@
                                       (got as key: ~S)" key-py-obj))
		 ;; XXX improve this conversion stuff if it's inefficient
	       
	      when (assoc key-symbol kw-args)
	      do (py-raise 'ValueError "Keyword ~S appeared more than once in call" key-py-obj)
	       
	      do (push (cons key-symbol val) kw-args)))
      
	(py-call eprim pos-args kw-args)))))

(defun eval-comparison (operator-left-right)
  "Does comparison, returns Python boolean"
  (destructuring-bind (operator left right) operator-left-right
    (let ((comp-fun (cdr (assoc operator *math-binary-cmp-assoc*))))
      (declare (special *math-binary-cmp-assoc*))
      (assert comp-fun)
      (lisp-val->py-bool (funcall comp-fun (py-eval-1 left) (py-eval-1 right))))))

(defun eval-unary (operator-val)
  (destructuring-bind (operator val) operator-val
    (declare (special *math-unary-op-assoc*))
    (let ((func (cdr (assoc operator *math-unary-op-assoc*))))
      (assert func)
      (funcall func (py-eval-1 val)))))

(defun eval-binary (operator-left-right)
  (declare (special *math-binary-op-assoc*)) ;; defined in mathops.cl
  (destructuring-bind (operator left right) operator-left-right
    #+(or) ;; XXX check this...
    (when (and (eq operator '/)
	       *__future__.division*)
      (setf operator '/t/))
    (let ((func (cdr (assoc operator *math-binary-op-assoc*))))
      (assert func)
      (funcall func (py-eval-1 left) (py-eval-1 right)))))

(defun eval-binary-lazy (operator-left-right)
  (destructuring-bind (operator left right) operator-left-right
    (let ((func (cdr (assoc operator *math-binary-lazy-op-assoc*))))
      (declare (special *math-binary-lazy-op-assoc*)) ;; mathops.cl
      (assert func)
      (funcall func left right))))

(defun eval-attributeref (obj-attr)
  (destructuring-bind (obj attr) obj-attr
    (assert (eq (car attr) 'identifier))
    (let ((ev-obj (py-eval-1 obj))
	  (attr-name (second attr)))
      (multiple-value-bind (val found)
	  (internal-get-attribute ev-obj attr-name)
	(if found
	    val
	  (py-raise 'AttributeError
		    "Object ~A has not attribute ~A" ev-obj attr-name))))))

(defun eval-subscription (primary-data)
  (destructuring-bind (primary data) primary-data
    (let ((ev-prim (py-eval-1 primary)))
      (multiple-value-bind (res found)
	  (call-attribute-via-class ev-prim '__getitem__ (list (py-eval-1 data)))
	(if found
	    res
	  (py-raise 'TypeError "No __getitem__ found (~A)" ev-prim))))))

(defun eval-slice (data)
  (destructuring-bind (start stop &optional step)
      data
    (make-slice (if start (py-eval-1 start) *None*)
		(if stop  (py-eval-1 stop)  *None*)
		(if step  (py-eval-1 step)  *None*))))

(defun eval-lambda (params-expr)
  (destructuring-bind (params expr) params-expr
    (let ((parsed-params (multiple-value-list (parse-function-parameter-list params))))
      #+(or)(break "lambda")
      (make-lambda-function
       :name "lambda"
       :ast expr
       :namespace (make-namespace :name "lambda namespace"
				  :inside *scope*)
       :params parsed-params
       :call-rewriter (apply #'make-call-rewriter 'lambda parsed-params)))))

;; XXX print todo: lookup sys.stdout instead of blindly using `t' as
;; stdout stream.

(defun eval-print (objs-comma?)
  "Print OBJS to STDOUT; trailing comma means no newline at the end."
  (destructuring-bind (objs comma?) objs-comma?
    (eval-print-stream t objs comma?)))

(defun eval-print->> (stream-objs-comma?)
  "Print OBJS to STREAM ~@
   If STREAM is None, then use STDOUT.
   Trailing comma means no newline at the end."
  (destructuring-bind (stream objs comma?) stream-objs-comma?
    (eval-print-stream (if (eq stream *None*) t (py-eval-1 stream))
		       objs
		       comma?)))

(defun eval-print-stream (output objs comma?)
  "The Python PRINT statement (a rough approximation)"

  (let ((do-write (if (eq output 't)
		      (lambda (str) (format t "~A" str))
		    (let* ((e-output (py-eval-1 output))
			   (write-meth
			    (or (internal-get-attribute e-output 'write)
				(py-raise 'AttributeError
					  "Can't print to object ~A: no `write' method"
					  e-output))))
		      (lambda (str)
			(py-call write-meth (list str)))))))
    (dolist (x objs)
      (let* ((ex (py-eval-1 x))
	     (str (py-str ex)))
	(funcall do-write str)
	(funcall do-write (string #\Space))))
	    
    (unless comma?
      (funcall do-write (string #\Newline)))))

(defun eval-ellipsis (data)
  (declare (ignore data))
  *Ellipsis*)

(defun eval-pass (data)
  (declare (ignore data)))

(defun eval-suite (stmts)
  "Evaluate all statements in suite; return None"
  (setf stmts (car stmts))
  (mapc #'py-eval-1 stmts))


(defun eval-for-in (target-sources-suite-else-suite)
  (destructuring-bind (target sources suite else-suite) target-sources-suite-else-suite
    (let ((take-else t)
	  (ev-targets (eval-one-assignment-target target)))
      (catch 'break
	(let ((f (lambda (x) 
		   (setf take-else nil)
		   (eval-assign-one ev-targets x)
		   (catch 'continue
		     (py-eval-1 suite)))))
	  (declare (dynamic-extent f))
	  (map-over-py-object f (py-eval-1 sources))))
      (when (and take-else else-suite)
	(py-eval-1 else-suite)))))

(defun eval-tuple (items)
  (let ((items (first items)))
    (make-tuple-from-list (mapcar #'py-eval-1 items))))

  
(defun eval-while (test-suite-else-suite)
  (destructuring-bind (test suite else-suite) test-suite-else-suite
    (let ((taken nil))
      (catch 'break
	(loop while (py-val->lisp-bool (py-eval-1 test))
	    do (catch 'continue
		 (setf taken t)
		 (py-eval-1 suite))))
      (when (and (not taken) else-suite)
	(py-eval-1 else-suite)))))

(defun eval-break (data)
  (declare (ignore data))
  (throw 'break nil))

(defun eval-continue (data)
  (declare (ignore data))
  (throw 'continue nil))

(defun eval-if (clauses--else-suite)
  (destructuring-bind (clauses else-suite) clauses--else-suite
    (loop for (expr suite) in clauses
	when (py-val->lisp-bool (py-eval-1 expr))
	do (return-from eval-if (py-eval-1 suite))
	finally
	  (when else-suite
	    (py-eval-1 else-suite)))))

(defun eval-return (val)
  (setf val (car val))
  (let ((res (if val (py-eval-1 val) *None*)))
    (throw 'function-block res)))

(defun eval-assert (test-expr)
  "Test whether assertion holds. Is only executed when __debug__ is true"
  (when (py-val->lisp-bool *__debug__*)
    (destructuring-bind (test expr) test-expr
      (unless (py-val->lisp-bool (py-eval-1 test))
	(py-raise 'AssertionError (py-eval-1 expr))))))


;; WER

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)

    ;; (file-length stream) gives number of bytes, counting
    ;; \r\n as one, although that combination is one #\Newline
    ;; so file-length gives maximum length. KISS for now.
    
    (loop with res = (make-array (or (file-length stream) 1000)
				 :element-type 'character
				 :adjustable t :fill-pointer 0)
	for c = (read-char stream nil nil)
	while c do (vector-push-extend c res)
	finally (return res))))

(defun make-module-object (module-name)
  (let* ((file-name (concatenate 'string (string module-name) ".py")))
    
    (loop
      (with-simple-restart (continue "Reload '~A' and retry import"
				     module-name)
	
	;; In CPython, when the toplevel of modules is executed, the
	;; name of the module is not yet bound to the module object
	
	(let* ((module-ns (make-namespace
			   :name (format nil "namespace for module ~A"
					 module-name)
			   :builtins t))
	       (file-contents (read-file file-name))
	       (module-ast (parse-python-string file-contents)))
	  
	  (let ((*scope* module-ns))
	    (declare (special *scope*))
	    (namespace-bind module-ns '__name__ (string module-name))
	    (namespace-bind module-ns '__file__ file-name)
	    (py-eval-1 module-ast))

	  ;; Now bind module name to the object in the enclosing namespace
	  (return-from make-module-object
	    (make-py-module :namespace module-ns)))))))

(defun path-directory-p (path)
  #+allegro
  (excl:file-directory-p path))

(defun path-file-p (path)
  (and (probe-file path)
       #+allegro
       (eq (excl.osi:stat-type (excl.osi:stat path)) :file)))


(defmethod load-py-source-file ((mod-name string) (paths list) &key (module-ok t) (package-ok t))
  (load-py-source-file (intern mod-name #.*package*) paths
		       :module-ok module-ok :package-ok package-ok))
		       
(defmethod load-py-source-file ((mod-name symbol) (paths list)
				&key (module-ok t) (package-ok t))
  
  "Load python source file with given NAME. Search will try all ~@
   PATHS (a list). If MODULE-OK or PACKAGE-OK is false, ~@
   then that kind of source file is not allowed.
   Returns the module or package, or NIL."
  
  ;; Documentation: http://www.python.org/doc/essays/packages.html
  
  (unless paths  ;; XXX to test
    (push "." paths))
  
  (dolist (path-dir paths)
    (block mapper
	
      (unless (path-directory-p path-dir)
	(warn "sys.path contains non-directory: ~A (skipped)" path-dir)
	(return-from mapper nil))
	
      ;; Create all pathnames we might use (XXX)
	
      (let* ((mod-name-str (symbol-name mod-name))
	     (mod-path (make-pathname :directory (namestring path-dir)
				      :name mod-name))
	     (mod/__init__.py-path (make-pathname :directory (namestring mod-path)
						  :name '__init__.py ))
	     (mod.py-path (make-pathname
			   :directory (namestring path-dir)
			   :name (concatenate 'string mod-name-str ".py"))))
	  
	;; Either MOD is a directory, in which case there should be a
	;; MOD/__init__.py file in it to make it a package; or there
	;; is a file named MOD.py.
	  
	(cond ((and package-ok
		    (path-directory-p  mod-path)
		    (path-file-p mod/__init__.py-path))
		
	       ;; Create a package.
	       ;; 
	       ;; Now, file __init__.py is read and evaluated. Before
	       ;; evaluation of the content is started, a magic
	       ;; variable __path__ is inserted in the file's
	       ;; namespace.

	       (loop
		 (with-simple-restart (continue "Reload '~A' and retry import" mod-name-str)

		   (let ((mod-ast (parse-python-string (read-file mod/__init__.py-path)))
			 (ns (make-namespace :builtins t)))
		   
		     (loop for (k v) in
			   `((__path__ ,(make-py-list-from-list (list mod-name-str)))
			     (__file__ ,(concatenate 'string  ;; relative to a path
					  mod-name-str "/__init__.py"))
			     (__name__ ,mod-name-str))
			 do (namespace-bind ns k v))
		   
		     (let ((*scope* ns))
		       (py-eval-1 mod-ast))
		     
		     (return-from load-py-source-file
		       (make-py-package :namespace ns
					:init-file (truename mod/__init__.py-path)
					:directory (truename mod-path) ))))))
		
	      ((and module-ok
		    (path-file-p mod.py-path))
		 
	       ;; Create a module.
	       ;; Packages have __path__ attribute ; modules don't.
	       
	       (loop
		 (with-simple-restart (continue "Reload '~A' and retry import" mod-name-str)

		   (let ((mod-ast (parse-python-string (read-file mod.py-path)))
			 (ns (make-namespace :builtins t)))
		     
		     (loop for (k v) in
			   `((__file__ ,(concatenate 'string  ;; relative to a path
					  mod-name-str ".py"))
			     (__name__ ,mod-name-str))
			 do (namespace-bind ns k v))
		     
		     (let ((*scope* ns))
		       (py-eval-1 mod-ast))
		     
		     (return-from load-py-source-file
		       (make-py-module :namespace ns
				       :module (truename mod.py-path) )))))))))))

(defun module-list-to-dotted-string (mod-ast &optional (sep "."))
  (assert (or (symbolp mod-ast)
	      (eq (car mod-ast) 'dotted)))
  (if (symbolp mod-ast)
      (symbol-name mod-ast)
    (progn (setf mod-ast (cdr mod-ast))
	   (loop with res = ""
	       for sublist on mod-ast
	       do (setf res (concatenate 'string res
					 (symbol-name (car sublist))
					 (when (cdr sublist) sep)))
	       finally (return res)))))



;; AST `item':
;;  import a         ->  (not-as a)
;;  import a as b    ->  (as a b)
;;  import a.b       ->  (not-as (dotted a b))
;;  import a.b as c  ->  (as (dotted a b) c)
;;
;;
;;  A `module' is a `xxx.py' file. A `package' is a directory with
;;  a `__init__.py' file.
;;
;;
;;  "import foo.bar.baz"
;;    
;;    Each except last name (foo and bar) must be packages, not
;;    modules. Last name (baz) must be either a module or a
;;    package.
;;    
;;    In current namespace, 'foo' gets bound to <foo> package.
;;    
;;    The value of `foo' before the import statement is irrelevant.
;;  
;;  
;;  "import foo.bar.baz as zut"
;;  
;;    Binds 'zut' to package or module <foo.bar.baz>
;;  
;;    
;;  *sys.modules* : a py-dict, mapping from dotted names to
;;                  module/package objects
;;
;;    Here, {'foo': <foo pkg>, 'foo.bar': <foo.bar pkg>, 
;;           'foo.bar.baz': <foo.bar.baz mod [or pkg]>} would be added to
;;    *sys.modules*.

