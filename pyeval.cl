(in-package :python)

;; XXX todo:
;;  support   def f((a,b), c): print b
;;            f((1,2),3) -> "2"
;;   by introducing `anonymous' variable name for first argument.
;;  http://mail.python.org/pipermail/python-dev/2003-October/038915.html

(defparameter *scope* nil)
(defparameter *py-eval-warn* nil)

#+(or)
(defun make-builtins-namespace ()
  ;; Fill a new namespace with all built-in names. Because there are
  ;; no name conflicts, the order of filling with functions and types
  ;; doesn't matter.
  (let ((ns (make-namespace :name "builtins-namespace")))
    (do-external-symbols (s 'python-builtin-functions)
      (namespace-bind ns (symbol-name s) (symbol-function s)))
    (do-external-symbols (s 'python-builtin-types)
      (if (boundp s) ;; check needed, as some symbols are TODO
	  (namespace-bind ns (symbol-name s) (symbol-value s))))
    (loop for (key . val) in `((None . ,*None*)
			       (Ellipsis . ,*Ellipsis*)
			       (NotImpemented . ,*NotImplemented*)
			       (True . ,*True*)
			       (False . ,*False*))
	do (namespace-bind ns key val))
    (loop for (name . exc) in *python-exceptions*
	do (namespace-bind ns name exc))
    ns))

(defun make-builtins-module ()
  
  ;; Fill a new namespace with all built-in names, and create a module
  ;; for it. Because there are no name conflicts, the order of filling
  ;; with functions and types doesn't matter.
  
  (let ((ns (make-namespace :name "builtins-namespace")))
    (do-external-symbols (s 'python-builtin-functions)
      (namespace-bind ns (symbol-name s) (symbol-function s)))
    (do-external-symbols (s 'python-builtin-types)
      (if (boundp s) ;; check needed, as some symbols are TODO
	  (namespace-bind ns (symbol-name s) (symbol-value s))))
    (loop for (key . val) in `((None . ,*None*)
			       (Ellipsis . ,*Ellipsis*)
			       (NotImpemented . ,*NotImplemented*)
			       (True . ,*True*)
			       (False . ,*False*))
	do (namespace-bind ns key val))
    (loop for (name . exc) in *python-exceptions*
	do (namespace-bind ns name exc))
    (make-module :name "__builtin__"
		 :namespace ns)))

#+(or)
(defparameter *builtins* (make-builtins-namespace))

(defparameter *builtins* (make-builtins-module))

;;; evaluation

(defun user-py-eval (ast &optional namespace)
  "Evaluate some Python code. This is the function to be called for ~
   evaluating user-supplied code in the form of an AST, and optionally a namespace."
  (let ((*scope* (or namespace 
		     (make-namespace :name "<new user-py-eval ns>"
				     :builtins t))))
    (py-eval ast)))

(defun py-eval (ast)
  "Evaluate AST. Assumes *scope* is set to something."

  (handler-bind 
      
      ;; XXX how to get at operands? (slot-value doesn't work)
      ((division-by-zero (lambda (c) (declare (ignore c))
				 (py-raise 'ZeroDivisionError
					   "Division or modulo by zero"))))
    (typecase ast
      (python-object (return-from py-eval ast)) ;; already evaluated
      (python-type (return-from py-eval ast))
      (number (return-from py-eval ast)) ;; Lisp objects (for efficiency)
      (string (return-from py-eval ast))
      ((and symbol
	(not (eql nil)))
       (return-from py-eval ast)) ;; string designator
      ((eql nil) (error "PY-EVAL of NIL")))
    
    (when (eql ast (find-class 'python-type))
      (return-from py-eval ast))
	
    
    (case (car ast)
      (file-input (eval-file-input (cdr ast)))
      (testlist (apply #'eval-testlist (cdr ast)))
    
      ;; special hook for inserting Lisp code directly in AST
      ;; (implicit PROGN)
      (lisp (eval `(progn ,@(cdr ast))))
	   
      ;; these bind names and create new scope:
      (module (apply #'eval-module "mod_name_here" (cdr ast)))
      (funcdef (apply #'eval-funcdef (cdr ast)))
      (class (apply #'eval-classdef (cdr ast)))
      (import (apply #'eval-import (cdr ast)))
      (assign-expr (apply #'eval-assign-expr (cdr ast)))
      (del (eval-del (second ast)))
      (try-except (apply #'eval-try-except (cdr ast)))
    
      ;; expressions:
      (identifier (eval-identifier (second ast)))
      (string (make-py-string (cdr ast)))
      (number (check-type (cdr ast) number)
	      (make-py-number (cdr ast)))
    
      (list (apply #'eval-list (cdr ast)))
      (tuple (make-tuple-from-list (mapcar #'py-eval (cdr ast))))
      (dict (eval-dict (second ast)))
      (string-conversion (apply #'eval-string-conv (cdr ast)))
    
      (call (apply #'eval-call (cdr ast)))
    
      (comparison (apply #'eval-comparison (cdr ast)))
      (unary (apply #'eval-unary (cdr ast)))
      (binary (apply #'eval-binary (cdr ast)))
    
      (attributeref (apply #'eval-attributeref (cdr ast)))
      (subscription (apply #'eval-subscription (cdr ast)))
    
      ;;(simple-slice (apply #'eval-simple-slice (cdr ast)))
      ;;(extended-slice (apply #'eval-extended-slice (cdr ast)))
      (slice (eval-slice (cdr ast)))
    
      (lambda (apply #'eval-lambda (cdr ast)))
    
      (print (apply #'eval-print (cdr ast)))
      (print>> (apply #'eval-print>> (cdr ast)))
    
      (ellipsis *Ellipsis*)

      ;; statements
      (return (eval-return (cadr ast)))
      ;; for now, SUITE returns the values of all stmts/exprs in it.
      (suite (eval-suite (second ast)))
      (for-in (apply #'eval-for-in (cdr ast)))
      (if (apply #'eval-if (cdr ast)))
      (assert (apply #'eval-assert (cdr ast)))
    
      (pass) ;; nothing
    
      (t (error "uncatched in py-eval: ~S~%" ast))
      )))


#+never
(eval-try-except-m (suite ((testlist ((identifier foo)) nil)))
		   ((except ((identifier Exc) (identifier e))
			    (suite ((testlist ((identifier bar)) nil)))))
		   nil)

(defun eval-try-except (suite except-clauses else-clause)
  (handler-bind
      ((Exception (lambda (c)
		    (loop for (nil (class (nil parameter)) handler-form) in except-clauses
			do (when (typep c (py-eval class))
			     (namespace-bind *scope* parameter c)
			     (py-eval handler-form)
			     (return-from eval-try-except nil))))))
    (py-eval suite))
  (py-eval else-clause))

#+nomore
(defun eval-try-except (suite except-clauses else-clause)
  (let ((ev `(eval-try-except-m ,suite ,except-clauses ,else-clause)))
    (eval ev)))

#+nomore
(defmacro eval-try-except-m (suite except-clauses else-clause)
  `(handler-case (py-eval ',suite)
     ,@(loop for ex-cl in except-clauses
	   collect (destructuring-bind (except (exc-name exc-param) suite)
		       ex-cl
		     (declare (ignore except))
		     (if exc-name
			 (progn (unless (eq (first exc-name) 'identifier)
				  (py-raise 'TypeError
					    "try/except: the token after 'except' ~
                                             must be a variable name (got: ~A)"
					    exc-name))
				`(,(second exc-name)
				  ,(if exc-param
				       (progn
					 (assert (and (consp exc-param)
						      (eq (car exc-param) 'identifier))
					     () "Invalid 'except' parameters")
					 `($cond$))
				     nil)
				  (namespace-bind *scope* ',(second exc-param) $cond$)
				  (py-eval ',suite)))
		       (progn (assert (not exc-param) () "should be enforced by syntax")
			      `(Exception () (py-eval ',suite))))))
     ,@(when else-clause
	 `(:no-error (&rest $args$)
		     (declare (ignore $args$))
		     (py-eval ',else-clause)))))
		    
(defun eval-assert (test expr)
  (unless (py-val->lisp-bool (py-eval test))
    ;; first evaluate expr, as that might lead to another exception
    (let ((str (py-eval expr)))
      (py-raise 'AssertionError str))))

(defun eval-del (exprlist)
  (assert (eq (car exprlist) 'exprlist))
  (dolist (place (second exprlist))
    (cond ((eq (car place) 'identifier)
	   (namespace-delete *scope* (second place)))
	  (t
	   (error "eval-del: can only do simple vars for now")))))

(defun eval-list (data)
  ;; either normal list or list comprehension
  (if (and (second data)
	   (listp (second data))
	   (listp (car (second data)))
	   (eq (car (car (second data))) 'list-for-in)) ;; hack
      
      ;; list compr.
      (let ((expr (car data))
	    (for-ifs (cadr data)))
	;;(format t "expr=~A~%for-ifs=~A~%" expr for-ifs)
	(eval-listcompr expr for-ifs))

    (make-py-list-from-list (map-into data #'py-eval data))))
    
(defun eval-testlist (items comma?)
  (unless (or items comma?)
    (py-raise 'SyntaxError
	      "Empty tuple is invalid -- use this instead: (,)"))
  (let ((make-tuple (or comma?
			(>= (length items) 2))))
    (if make-tuple
	(make-tuple-from-list (mapcar #'py-eval items))
      (py-eval (car items)))))
   
(defun eval-file-input (data)
  (cons :file-input (mapcar #'py-eval data)))

(defun eval-suite (stmts)
  "Return value of last stmt in suite"
  (mapc #'py-eval stmts))

(defun eval-identifier (name)
  "Look up the identifier in the active namespaces, and fall back to
   looking in the (module) object named '__builtins__."

  (or (namespace-lookup *scope* name) ;; traverses all enclosing scopes too
      
      (let ((bi (namespace-lookup *scope* '__builtins__)))
	(when bi
	  (namespace-lookup bi name)))
      
      (py-raise 'NameError
		"Name ~A is not defined" name)))

(defun eval-return (val)
  (throw 'function-block (if val 
			     (py-eval val)
			   *None*)))

(defun eval-dict (data)
  ;; eval keys and values in this order: key1, val1, key2, val2, ...
  (mapc (lambda (kv) (setf (car kv) (py-eval (car kv))
			   (cdr kv) (py-eval (cdr kv))))
	data)
  (make-dict data))



(defun eval-listcompr (expr list-for-ifs)
  (let ((acc ()))
    (labels ((process-for/ifs (for-ifs)
	       (let ((clause (car for-ifs)))
		 (cond ((null for-ifs)                 (collect-expr))
		       ((eq (car clause) 'list-for-in) (process-for for-ifs))
		       ((eq (car clause) 'list-if)     (process-if for-ifs)))))
	     
	     (collect-expr () (push (py-eval expr) acc))
	     
	     (process-for (for-ifs)
	       (destructuring-bind ((_list-for-in exprlist source) &rest rest)
		   for-ifs
		 (declare (ignore _list-for-in))
		 (let ((esource (py-eval source)))
		   (py-iterate ($dummy$ esource)
			       (eval-assign-expr-1 exprlist $dummy$)
			       (process-for/ifs rest)))))
	     
	     (process-if (for-ifs)
	       (destructuring-bind ((_list-if condition) &rest rest)
		   for-ifs
		 (declare (ignore _list-if))
		 (when (py-val->lisp-bool (py-eval condition))
		   (process-for/ifs rest)))))
      
      (process-for/ifs list-for-ifs)
      (make-py-list-from-list (nreverse acc)))))

  
#+(or)
(defun eval-string-conv (tuple)
  (declare (ignore tuple))
  (error 'todo))

(defun eval-attributeref (obj attr)
  (assert (eq (car attr) 'identifier))
  (let ((eobj (py-eval obj))
	(attr-name (second attr)))
    (multiple-value-bind (val found)
	(internal-get-attribute eobj attr-name)
      (if found
	  val
	(py-raise 'AttributeError
		  "Object ~A has not attribute ~A" eobj attr-name)))))

(defun eval-subscription (primary data)
  (destructuring-bind (item-list comma?)
      data
    (assert (consp item-list))
    (let* ((eprim (py-eval primary))
	   (item-list-2 (mapcar #'py-eval item-list))
	   (item (if (or comma?
			 (> (length item-list) 1))
		     (make-tuple-from-list (mapcar #'py-eval item-list-2))
		   (car item-list-2))))
      
      (multiple-value-bind (getitem found)
	  (internal-get-attribute eprim '__getitem__)
	(if found
	    (__call__ getitem (list item))
	  (py-raise 'TypeError
		    "No __getitem__ found (~A)" eprim)))
      #+(or)(__getitem__ eprim item))))

(defun eval-slice (data)
  (destructuring-bind (start stop &optional step)
      data
    (make-slice (if start (py-eval start) *None*)
		(if stop (py-eval stop) *None*)
		(if step (py-eval step) *None*))))

#+(or)
(defun eval-lambda (params expr)
  'todo)

(defun eval-call (primary args)
  (let ((eprim (py-eval primary))) ;; XX check order of this eval
    (if (null args)
	
	(__call__ eprim () ())
      
      (let* ((*-arg (find '* args :key (lambda (x) (when (consp x)
						     (car x)))))
	     (**-arg (find '** args :key (lambda (x) (when (consp x)
						       (car x)))))
	     (temp-key-args (loop for a in args
				when (and (consp a)
					  (eq (car a) '=))
				collect a))
	     
	     (key-args (mapcar (lambda (a)
				 (destructuring-bind (= key val)
				     a
				   (declare (ignore =))
				   (assert (eq (car key) 'identifier) ()
				     "for key=val arg, supply simple varname as ~
                                      key (got: ~A)" a)
				   (cons (second key) val)))
			       temp-key-args))
	     
	     (pos-args (remove *-arg (remove **-arg
					     (let ((x args))
					       (dolist (a temp-key-args)
						 (setf x (delete a x)))
					       x)))))
	     
	(setf *-arg (second *-arg)  ;; remove leading stars in list
	      **-arg (second **-arg))
	
	#+(or)(format t "EVAL-CALL: pos: ~A  key: ~A  *: ~A  **: ~A~%" 
		      pos-args key-args *-arg **-arg)
	  
	(setf pos-args (mapcar #'py-eval pos-args)
	      pos-args (append pos-args (when *-arg
					  (slot-value (py-eval *-arg) 'list)))
	      key-args (loop for (k . v) in key-args
			   collect `(,k . ,(py-eval v)))
	      key-args (append key-args (when **-arg
					  (dict->alist (py-eval **-arg)))))
	;; **-arg can only contain already evaluated stuff, is the assumption
	#+(or)(format t "call: ~A~%" `(__call__ ,eprim ,pos-args ,key-args))
	(__call__ eprim pos-args key-args)))))


(defun eval-for-in (targets sources suite else-suite)
  (let ((esource (py-eval sources))
	(take-else t))
    (py-iterate ($dummy$ esource)
		(setf take-else nil)
		(eval-assign-expr-1 targets $dummy$)
		(py-eval suite))
    (when take-else
      (py-eval else-suite))))


;; a = b,c = [1,2]
;;  -> a = [1,2]
;;     b = 1, c = 2

(defun eval-assign-expr (items)
  (destructuring-bind (val &rest targets)
      (reverse items)
    (if targets

	;; real assignment statement
	(let ((eval (py-eval val)))
	  (dolist (tar targets)
	    (eval-assign-expr-1 tar eval)))
      
      ;; just variable reference
      (py-eval val))))
    

(defun eval-assign-expr-1 (targets evalue)
  "Assign EVALUE to TARGETS.
   Assumes EVALUE already evaluated."
  (assert (member (car targets) '(testlist exprlist) :test 'eq))
  (let* ((etargets (eval-assignment-targets (second targets)))
	 (num-targets (length etargets))
	 
	 ;; both testlist and exprlist have comma? as third list element
	 (comma? (third targets))
	 (target-is-list (or (> num-targets 1)
			     comma?)))
    
    ;; (format t "eval-ass-targets: ~S~%" etargets)
    (cond
     ((= num-targets 0)
      (error "no assignment targets?! ~W ~W" etargets evalue))
       
     ((and (= num-targets 1)
	   (not target-is-list))
      (eval-assign-one (car etargets) evalue))
       
     ((or (>= num-targets 2)
	  target-is-list)
      (let ((i 0)
	    (acc ()))
	(py-iterate (x evalue)
		    (incf i)
		    (when (> i num-targets)
		      (py-raise
		       'ValueError
		       "Too many values to unpack (needed exactly ~A values), ~@
                        got already one more than that." num-targets))
		    (push x acc))
	(when (< i num-targets)
	  (py-raise 'ValueError
		    "Too few values to unpack (needed exactly ~A values, ~
                     but got only ~A)."
		    num-targets i))
	  
	;; arriving here means we got exactly enough values.
	(setf acc (nreverse acc))
	(loop for val in acc
	    for tar in etargets
	    do (eval-assign-one tar val))))
     
     (t (error "shouldn't come here")))))
       
(defun eval-assign-one (target val)
  "TARGET is evaluated as far as possible, but it still contains ~@
   identifier, subscription, attributeref. VAL is the fully evaluated ~@
   value to be assigned."
  
  (cond ((eq (car target) 'identifier)
	 ;; (identifier name)
	 (namespace-bind *scope* (second target) val))
	
	((eq (car target) 'attributeref)
	 ;; (attributeref <primary> (identifier attr-name))
	 (destructuring-bind (primary (identifier attr-name))
	     (cdr target)
	   (declare (ignore identifier))
	   ;;(assert (eq (car attr-name) 'identifier))
	   (internal-set-attribute primary attr-name val)))
	
  	((eq (car target) 'subscription)
	 ;; (subscription <primary> <subs>)
	 (destructuring-bind (primary subs)
	     (cdr target)
	   
	   (multiple-value-bind (setitem found)
	       (internal-get-attribute primary '__setitem__)
	     (if found
		 (__call__ setitem (list subs val))
	       (py-raise 'TypeError "No __setitem__ found (~A)" primary)))
	   
	   #+(or) ;; not correct for UDC with method __setitem__
	   (__setitem__ primary subs val)))
	
	((or (numberp target) (stringp target))
	 (error "Literal target -- (should have been catched already in ~@
                 EVAL-ASSIGNMENT-TARGETS) A~" target))

	((eq (car target) 'testlist)
	 (destructuring-bind (item-list comma?)
	     (cdr target)
	   (let ((new-target (if (or comma?
				 (> (length item-list) 1))
			     `(list ,@item-list)
			   (car item-list))))
	     (eval-assign-one new-target val))))
	  
	((eq (car target) 'list)
	 ;; (list <A> <B> ...)
	 ;; List, tuple are both made `list' by EVAL-ASSIGNMENT-TARGETS.
	 ;; VAL must be iterable, otherwise py-iterate raises TypeError
	 (let* ((targets (cdr target))
		(num-targets (length targets))
		(i 0)
		(acc ()))
	   (py-iterate (x val)
		       (incf i)
		       (push x acc)
		       (when (> i num-targets)
			 (py-raise
			  'ValueError "Too many values to unpack (needed exactly ~A ~
                                       values, got already one more than that."
			  num-targets)))
	   (when (< i num-targets)
	     (py-raise 'ValueError
		       "Too few values to unpack (needed exactly ~A values, ~
                        but got only ~A)." num-targets i))
	   (setf acc (nreverse acc))
	   (loop for val in acc
	       for tar in targets
	       do (eval-assign-one tar val))))
	
	(t
	 (error "uncatched situation in EVAL-ASSIGN-ONE: ~@
                 target = ~A  val = ~A" target val))))


(defun eval-assignment-targets (targets)
  (labels ((do-eval (tg)
	     (when (atom tg)
	       (py-raise 'SyntaxError
			 "Cannot assign to a literal (got target: ~A)" tg))
	     (case (car tg)
	       ;; (IDENTIFIER a) : remains the same
	       (identifier tg)
	       
	       ;; (TUPLE A B) and (LIST A B) both go to (LIST <A> <B>)
	       ((tuple list) `(list ,@(mapcar #'do-eval (second tg))))
	       
	       ;; foo.x => (attributeref (identifier foo) (identifier x))
	       ;; 
	       ;; The primary is evaluated; the attribute identifier
	       ;; is just a name and can't be further evaluated.
	       (attributeref `(attributeref ,(py-eval (second tg))
					    ,(third tg)))
	       
	       ;; x[1,2,3]
	       ;; => (subscription (identifier x) ((1 2 3) nil))) nil)
	       ;;
	       ;; Both primary and expr-list are evaluated; result is
	       ;;  (subscription <object> <item-or-tuple>)
	       (subscription 
		(destructuring-bind (primary (subs comma?))
		    (cdr tg)
		  `(subscription ,(py-eval primary)
				 ,(let* ((esubs (mapcar #'py-eval subs))
					 (make-tuple (or (> (length subs) 1)
							 comma?)))
				    (if make-tuple
					(make-tuple-from-list esubs)
				      (car esubs))))))
	       
	       ;; todo: slices
	       (testlist
		`(list ,@(loop for x in (second tg)
			     collect (do-eval x))))
	       
	       (t
		(warn "EVAL-ASSIGNMENT-TARGETS: assuming ~A is an expression ~
                       to be evaluated." tg)
		(py-eval tg)))))
    
    (loop for tg in targets
	collect (do-eval tg)))) ;; local function: can't do direct mapcar

(defun find-in-lists (item list)
  "Find ITEM in LISTS. LISTS consists of lists and atoms"
  (if (atom list)
      (eq list item)
    (loop for x in list
	if (find-in-lists item x)
	do (return t)
	finally (return nil))))

(defun eval-funcdef (fname params suite)
  "In the current namespace, FNAME becomes bound to a function object ~@
   with given formal parameters and function body."
  
  ;; The default argument values are evaluated only once, at function
  ;; definition time.
  ;; 
  ;; given as (k . v), not (= k v) as somewhere else XXX
  (destructuring-bind (pos-kw-params *-par **-par)
      params
    (let* ((kw-params (loop for x in pos-kw-params if (consp x) collect x))
	   (pos-params (set-difference pos-kw-params kw-params))
	   (kw-params (loop for (k . v) in kw-params
			  collect `(,k . ,(py-eval v)))))
      
      (let* ((params (list pos-params kw-params *-par **-par))
	     ;; (dummy (format t "FUNCDEF params: ~A~%" params))
	     (f (make-user-defined-function
		 :name fname
		 :ast suite
		 :namespace (make-namespace
			     :name (format nil "<ns for function ~A>" fname)
			     :inside *scope*)
		 :params params
		 :call-rewriter (apply #'make-call-rewriter params)
		 :enclosing-scope *scope*)))
	
	;; Content of function is not evaluated yet; only when called.
	;; Bind function name to function object in current namespace:
	(namespace-bind *scope* fname f)
	f))))


(defun eval-classdef (cname inheritance suite)
  (unless inheritance
    (setf inheritance `(testlist () nil)))
  
  ;; The inheritance list will be evaluated, but MAKE-PYTHON-CLASS
  ;; expects a list of *names*, therefore after each of the items is
  ;; evaluated and has a class object as value, CLASS-NAME must be
  ;; called.
  
  (assert (eq (car inheritance) 'testlist))
  (let* ((ns (make-namespace :name (format nil "<ns for class ~A>" cname)
			     :inside *scope*))
	 
	 (supers (mapcar (lambda (x)
			   (let ((c (py-eval x)))
			     (if (typep c 'class)
				 (class-name c)
			       (py-raise 'TypeError
					 "Cannot have non-classes in ~
                                          superclass list (got: ~A)" c))))
			 (second inheritance)))
				       
	 (c (make-python-class :name cname
			       :module "ModuleName"
			       :supers supers
			       :namespace ns)))
    
    ;; In the current namespace, the name of the class that is defined
    ;; now becomes bound to the class object that results from
    ;; evaluating the classdef.
    (namespace-bind *scope* cname c)
    
    ;; Evaluate SUITE now, in the new namespace inside the class:
    ;; methods and class attributes are defined in it, plus there may
    ;; be other side effects.
    (when suite
      (let ((*scope* ns))
	(py-eval suite)))
    
    ;; Finally, return the class (purely for debuggin: classdef has no
    ;; return value, as it is a stmt, not an expr.
    c))

(defun new-eval-classdef (cname inheritance suite)
  ;; XXX for easy testing only!
  (when (null inheritance)
    (setf inheritance '(tuple)))
  (assert (eq (car inheritance) 'tuple) ()
    "Expected TUPLE inheritance list, but got ~S" inheritance)
  (let* ((ns (make-namespace :name (format nil "<ns for class ~A>" cname)
			     :inside *scope*))
	 (c (make-python-class :name cname
			       :module "ModuleName"
			       :supers (mapcar #'py-eval (cdr inheritance))
			       :namespace ns)))
    
    ;; In the current namespace, the name of the class that is defined
    ;; now beceoms bound to the class object that results from
    ;; evaluating the classdef.
    #+(or)(namespace-bind *scope* cname c)
    
    ;; Evaluate SUITE now, in the new namespace inside the class:
    ;; methods and class attributes are defined in it, plus there may
    ;; be other side effects.
    (when suite
      (let ((*scope* ns))
	(py-eval suite)))
    
    ;; Finally, return the class (purely for debuggin: classdef has no
    ;; return value, as it is a stmt, not an expr.
    c))

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((res (make-array (or (file-length stream)
			       10000)
			   :element-type 'character
			   :fill-pointer 0
			   :adjustable t)))
      (let ((c (read-char stream nil nil)))
	(loop while c do
	      (vector-push-extend c res)
	      (setf c (read-char stream nil nil))))
      res)))

(defun eval-import (data nl)
  (assert (null nl))
  (destructuring-bind (normal (modname nl2))
      data
    (assert (eq normal 'normal))
    (assert (eq nil nl2))
    (let* ((file-name (concatenate 'string (string modname) ".py"))
	   (file-contents (read-file file-name)))
      
      ;; In CPython, when the toplevel of modules is executed, the
      ;; name of the module is not yet bound to the module object
      
      (let* ((module-ns (make-namespace
			 :name (format nil "namespace for module ~A" modname)
			 :builtins t))
	     (module-ast (parse-python-string file-contents)))
	
	(let ((*scope* module-ns))
	  (declare (special *scope*))
	  (namespace-bind module-ns '__name__ modname)
	  (namespace-bind module-ns '__file__ file-name)
	  (py-eval module-ast))
	
	;; Now bind module name to the object in the enclosing namespace
	(let ((m (make-module :name modname
			      :namespace module-ns)))
	  (namespace-bind *scope* modname m))))))
      

(defun eval-if (clauses else-suite)
  (loop for (expr suite) in clauses
      when (py-val->lisp-bool (py-eval expr))
      do (py-eval suite)
	 (return)
      finally
	(when else-suite
	  (py-eval else-suite))))


(defun eval-comparison (operator left right)
  "Does comparison, returns Python boolean"
  (check-type operator symbol) ;; not a _quoted_ symbol
  (let ((comp-fun (cdr (assoc operator *math-cmp-assoc*))))
    (assert comp-fun () "No comparison function corresponding to ~
                         comparison operator ~A ?!~%~A~%~A"
	    operator comp-fun *math-cmp-assoc*)
    ;;(format t "trying py-eval left...~%")
    (let ((e-left (py-eval left)))
      ;;(format t "e=left: ~A~%" e-left)
      (let ((e-right (py-eval right)))
	;;(format t "e-right: ~A~%" e-right)
	(make-bool (funcall comp-fun e-left e-right))))))




(defun eval-unary (operator val)
  (declare (special *math-unary-assoc*))
  (let ((eval (py-eval val))
	(k (assoc operator *math-unary-assoc*)))
    (unless k
      (error "No operator for ~A ?! EVAL-UNARY" operator))
    (funcall (cdr k) eval)))


(defun eval-binary (operator left right)
  (declare (special *math-op-mapping*)) ;; defined in mathops.cl
  (let ((func (gethash operator *math-op-mapping*)))
    (assert func ()
      "Operator ~A has no corresponding py-~A function?! ~A"
      operator operator (gethash operator *math-op-mapping*))
    (funcall func (py-eval left) (py-eval right))))

(defun eval-tuple (&rest content)
  (let ((c (mapcar #'py-eval content)))
    (apply #'make-tuple-from-list c)))

(defun get-slice (obj start end &optional (step nil))
  (declare (ignore obj start end step))
  'todo)

(defun get-item (obj item)
  ;;declare (ignore obj item))
  (__getitem__ obj item))


;; XXX print todo: lookup sys.stdout instead of blindly using `t' as
;; stdout stream.

(defun eval-print (objs comma?)
  "Print OBJS to STDOUT; trailing comma means no newline at the end."
  (eval-print-stream t objs comma?))

(defun eval-print>> (stream objs comma?)
  "Print OBJS to STREAM ~@
   If STREAM is None, then use STDOUT.
   Trailing comma means no newline at the end."
  (eval-print-stream (if (eq stream *None*) t stream)
		     objs
		     comma?))

(defun eval-print-stream (stream objs comma?)
  "The Python PRINT statement (a rough approximation)"
  
  (dolist (x objs)
    (let* ((ex (py-eval x))
	   (str (call-attribute-via-class ex '__str__)))
      (format stream "~A " str)))
  
  (unless comma?
    (terpri)))


;;; Given:  def foo(a, b, c=3, d=4, *args, **kws)
;;;  -> func-args = ((a b) ((c . 3) (d . 4)) args kws)
;;;
;;; foo(9,8,7,6,5,4, foo=17, bar=12)
;;;  -> a,b,c,d=9,8,7,6 args=(5,4) kws={'foo':17, 'bar':12}
;;; foo(1,2) -> a,b,c,d = (1,2,3,4) args=() kws={}
;;; foo(1,2, zut=12, c=9) -> a,b,c,d = 1,2,9,4 args=() kws={'zut':12}
;;;
;;; x = (1,2,3)
;;; d = {'sd': 12}
;;; funcarg.foo(1,2, zut=12, q=9, *x, **d)
;;;  -> a,b,c,d = 1,2,11,12 args=(13,) kws={'q':9, 'zut': 12, 'sd': 12}


#+(or) ;; probably not needed, because it's enforced by the parser already
(defun is-ok-arg-list (args)
  "Keyword arguments should be after positional arguments"
  (let ((last-pos (position-if #'atom args :from-end t))
	(first-kw (position-if #'consp args)))
    (if (and last-pos first-kw)
	(< last-pos first-kw)
      t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

(defun subst-treelist (old new tree)
  "Like SUBST, but works on lists with N elements instead of conses ~@
   with CAR,CDR branches."
  (cond ((equal old tree)
	 new)
	((atom tree) tree)
	((consp (cdr tree))
	 (mapcar (lambda (tr) (subst-treelist old new tr))
		 tree))
	(t tree)))

(defun subst-treelist-all (repl tree)
  "REPL is ASSOC list with (old . new) pairs"
  (loop for (old . new) in repl
      do (setf tree (subst-treelist old new tree))
      finally (return tree)))


#+(or)
(py-eval '(list ((identifier x)
		 ((list-for-in (exprlist ((identifier x)) nil) (list (1 2 3)))
		  (list-for-in (exprlist ((identifier y)) nil)
		   (identifier foo))
		  (list-if (comparison > (identifier x) (identifier y)))
		  (list-if (comparison == (identifier y) 3))))))


#+(or)
(eval-listcompr-m (testlist (identifier x))
		  ((list-for-in (exprlist ((identifier x)) nil) (list (1 2 3)))
		   (list-for-in (exprlist ((identifier y)) nil) (list (4 5 6)))
		   (list-if (comparison > (identifier x) (identifier y)))))


#+(or)(progn not working yet
	     ("a" in "asdf"))

#+(or)(working
       list-comprehensions 
       arith
       string-methods
       "for x,y in [(1,2)]: print x+y"
       "if then else"
       "class def"
       )
