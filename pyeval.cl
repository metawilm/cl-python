(in-package :python)

(defparameter *scope* nil "Current execution namespace")
(defparameter *__debug__* 1) ;; CPython readonly variable `__debug__' (see EVAL-ASSERT)
(defparameter *__future__.division* nil)


#+(or) ;; namespace re-use optimization
(defparameter *namespace-cache* (make-array 100 :fill-pointer 0 :adjustable nil))
    
;;; Evaluation

(defun user-py-eval (ast &optional namespace)
  "Evaluate some Python code. This is the function to be called for ~
   evaluating user-supplied code in the form of an AST, and
   optionally a namespace."
  
  (let ((*scope* (or namespace 
		     (make-namespace :name "<new user-py-eval ns>"
				     :builtins t))))
    (namespace-bind *scope* '__name__ "__main__")
    (py-eval ast)))


(defvar *py-eval-handler-set* nil)


;; During evaluation of Python code, a some Lisp errors may
;; occur. Some of them are catched and converted to the
;; corresponding Python exception.

(defmacro with-py-error-handlers (&body body)
  `(handler-bind
       ((division-by-zero
	 (lambda (c)
	   (declare (ignore c))
	   (py-raise 'ZeroDivisionError "Division or modulo by zero")))
	
	#+allegro
	(excl:synchronous-operating-system-signal
	 (lambda (c)
	   (if (string= (slot-value c 'excl::format-control)
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
  "Evaluate AST. Assumes *scope* is set appropriately."
  (if *py-eval-handler-set*
      (py-eval-1 ast)
    (with-py-error-handlers
	(let ((*py-eval-handler-set* t))
	  (py-eval-1 ast)))))

(defun py-eval-1 (ast)
  "Evaluate AST. Assumes *scope* and error handlers are set."
  (typecase ast
    (python-object (return-from py-eval-1 ast)) ;; already evaluated
    (python-type (return-from py-eval-1 ast))
    (number (return-from py-eval-1 ast)) ;; Lisp objects (for efficiency)
    (string (return-from py-eval-1 ast))
    ((and symbol
      (not (eql nil)))
     (return-from py-eval-1 ast)) ;; string designator
    ((eql nil) (error "PY-EVAL of NIL")))
    
  (when (eq ast (load-time-value (find-class 'python-type)))
    (return-from py-eval-1 ast))
  
  ;; This allows all kinds of Lisp values to be used directly in Python,
  ;; e.g. using `clpy(...)' -- experimental
  (unless (listp ast)
    (return-from py-eval-1 ast))
  
  (case (car ast)
    (inline-lisp (eval-inline-lisp (second ast)))
    (file-input (apply #'eval-file-input (cdr ast)))
    (testlist (apply #'eval-testlist (cdr ast)))
    
    ;; special hook for inserting Lisp code directly in AST
    ;; (implicit PROGN)
    #+(or)(lisp (eval `(progn ,@(cdr ast))))
    
    ;; these bind names and create new scope:
    (module (apply #'eval-module "mod_name_here" (cdr ast)))
    (funcdef (apply #'eval-funcdef (cdr ast)))
    (class (apply #'eval-classdef (cdr ast)))
    
    (import (apply #'eval-import (cdr ast)))
    (import-from (apply #'eval-import-from (cdr ast)))
    
    (assign-expr (apply #'eval-perhaps-assign-expr (cdr ast)))
    (augassign-expr (apply #'eval-augassign-expr (cdr ast)))
    (del (eval-del (second ast)))
    (global (eval-global (second ast)))

    (try-except (apply #'eval-try-except (cdr ast)))
    (raise (apply #'eval-raise (cdr ast)))
    
    ;; expressions:
    (identifier (eval-identifier (second ast)))
    (string (make-py-string (cdr ast)))
    (number (check-type (cdr ast) number)
	    (make-py-number (cdr ast)))
    
    (list (apply #'eval-list (cdr ast)))
    (tuple (make-tuple-from-list (mapcar #'py-eval-1 (cdr ast))))
    (dict (eval-dict (second ast)))
    (backticks (eval-backticks (second ast)))
    
    (call (apply #'eval-call (cdr ast)))
    
    (comparison (apply #'eval-comparison (cdr ast)))
    (unary (apply #'eval-unary (cdr ast)))
    (binary (apply #'eval-binary (cdr ast)))
    (binary-lazy (apply #'eval-binary-lazy (cdr ast)))
    
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
    (pass) ;; nothing
    (suite (eval-suite (second ast)))
      
    (for-in (apply #'eval-for-in (cdr ast)))
    (while (apply #'eval-while (cdr ast)))
    (break (eval-break))
    (continue (eval-continue))
    (if (apply #'eval-if (cdr ast)))
            
    (return (eval-return (cadr ast)))
    (assert (apply #'eval-assert (cdr ast)))
    
    (t (error "uncatched in py-eval: ~S~%" ast))))


(defun eval-inline-lisp (form)
  (eval form))

(defun eval-try-except (suite except-clauses else-clause)
  ;;(declare (optimize (debug 3)))
  
  ;; Note that the Exception class that an 'except' clause catches, is
  ;; evaluated after an exception is thrown, not earlier; so long as
  ;; there is no exception thrown, it is not evaluated.

  ;; XXX todo: `class' may be a tuple, catching all exceptions in the tuple.
  
  (handler-bind 
      ;; Not handler-case: we don't want to unwind for uncatched exceptions
      ((Exception (lambda (exc)
		    (loop for ((cls/tuple parameter) handler-form) in except-clauses
			do (cond ((and cls/tuple  ;; `except Something:'  where Something a class or tuple
				      (let ((ecls/tuple (py-eval-1 cls/tuple)))
					(typecase ecls/tuple
					  (class    (typep exc ecls/tuple))
					  (py-tuple (loop for cls in (tuple->lisp-list ecls/tuple)
							when (typep exc cls)
							do (return t)
							finally (return nil)))
					  (t (warn "Non-class as `except' specializer: ~S"
						   ecls/tuple)
					     nil))))
				  (when parameter
				    (assert (eq (first parameter) 'identifier))
				    (namespace-bind *scope* (second parameter) exc)) ;; right scope??
				  (py-eval-1 handler-form)
				  (return-from eval-try-except nil))
				 
				 ((null cls/tuple) ;; a bare `except:' matches all exceptions
				  (py-eval-1 handler-form)
				  (return-from eval-try-except nil))
				 
				 (t (error "assumed unreachable")))))))
    
    ;; The `py-error-handlers' were already set in py-eval. Need to
    ;; set them here again, because when one of the py-eval
    ;; handler-bind handlers takes control, the handler above for
    ;; Exception is not active anymore.
    
    (with-py-error-handlers
	(py-eval-1 suite)))

  (when else-clause
    (py-eval-1 else-clause)))


(defun eval-raise (first second third) ;; exctype value traceback)
  ;; Complicated interpretation of parameters. See Python Reference Manual, par 6.9
  
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
	       
	(t
	 ;; "If the first object is an instance, the type of the
	 ;; exception is the class of the instance, the instance itself
	 ;; is the value, and the second object must be None."
  	 (if (not second)
	     (error first)
	   (py-raise 'ValueError
		     "RAISE: when first arg is instance, second argument must ~@
                      be None or not supplied (got: ~A)" second)))))


(defun eval-global (varlist)
  (let ((ns *scope*))
    (dolist (varname varlist)
      (namespace-declare-global ns varname))))

		    
(defun eval-assert (test expr)
  "Test whether assertion holds. Is only executed when __debug__ is true"
  (when (py-val->lisp-bool *__debug__*)
    (unless (py-val->lisp-bool (py-eval-1 test))
      (py-raise 'AssertionError (py-eval-1 expr)))))

(defun eval-del (exprlist)
  (assert (eq (car exprlist) 'exprlist))
  (dolist (place (second exprlist))
    (let ((place2 (eval-one-assignment-target place)))
      #+(or)(warn "place to del: ~A" place2)
      (ecase (car place2)
	
	(identifier   (namespace-delete *scope* (second place2)))
	(subscription (warn "deletion of subscription: buggy")
		      (call-attribute-via-class (second place2) '__delitem__
						(cddr place2)))
	(attributeref (warn "deletion of attribute: buggy")
		      (destructuring-bind (obj (identifier? att-name))
			  (cdr place2)
			(assert (and (eq identifier? 'identifier)))
			;; XXX call __delattr__ always?
			(call-attribute-via-class obj '__delattr__ (list att-name))))))))

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

    (make-py-list-from-list (map-into data #'py-eval-1 data))))
    
(defun eval-testlist (items comma?)
  (unless (or items comma?)
    (return-from eval-testlist (make-tuple)) ;; XXX see if this gives problems
    #+(or)(py-raise 'SyntaxError
		    "Empty tuple is invalid -- use this instead: (,)"))
  (let ((make-tuple (or comma?
			(>= (length items) 2))))
    (if make-tuple
	(make-tuple-from-list (mapcar #'py-eval-1 items))
      (py-eval-1 (car items)))))
   
(defun eval-file-input (data)
  (cons :file-input (mapcar #'py-eval-1 data)))

(defun eval-suite (stmts)
  "Evaluate all statements in suite; return None"
  (mapc #'py-eval-1 stmts)
  *None*)

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
  (let ((res (if val (py-eval-1 val) *None*)))
    
    #+(or) ;; namespace reuse optimization
    (vector-push *scope* *namespace-cache*)
    
    (throw 'function-block res)))

(defun eval-dict (data)
  ;; eval keys and values in this order: key1, val1, key2, val2, ...
  (mapc (lambda (kv) (setf (car kv) (py-eval-1 (car kv))
			   (cdr kv) (py-eval-1 (cdr kv))))
	data)
  (make-dict data))



(defun eval-listcompr (expr list-for-ifs)
  (let ((acc ()))
    (labels ((process-for/ifs (for-ifs)
	       (let ((clause (car for-ifs)))
		 (cond ((null for-ifs)                 (collect-expr))
		       ((eq (car clause) 'list-for-in) (process-for for-ifs))
		       ((eq (car clause) 'list-if)     (process-if for-ifs)))))
	     
	     (collect-expr () (push (py-eval-1 expr) acc))
	     
	     (process-for (for-ifs)
	       (destructuring-bind ((list-for-in? exprlist source) &rest rest)
		   for-ifs
		 (assert (eq list-for-in? 'list-for-in))
		 (map-over-py-object (lambda (x)
				       (eval-real-assign-expr exprlist x)	 
				       (process-for/ifs rest))
				     (py-eval-1 source))))
	     
	     (process-if (for-ifs)
	       (destructuring-bind ((_list-if condition) &rest rest)
		   for-ifs
		 (declare (ignore _list-if))
		 (when (py-val->lisp-bool (py-eval-1 condition))
		   (process-for/ifs rest)))))
      
      (process-for/ifs list-for-ifs)
      (make-py-list-from-list (nreverse acc)))))

  
(defun eval-backticks (lst)
  (let* ((ev (mapcar #'py-eval-1 lst))
	 (obj (if (cdr lst)
		  (make-tuple-from-list ev)
		(car ev))))
    (multiple-value-bind (res found)
	(call-attribute-via-class obj '__repr__)
      (unless found
	(error "no __repr__ for ~A, in backticks" obj))
      res)))

(defun eval-attributeref (obj attr)
  (assert (eq (car attr) 'identifier))
  (let ((eobj (py-eval-1 obj))
	(attr-name (second attr)))
    (multiple-value-bind (val found)
	(internal-get-attribute eobj attr-name)
      (if found
	  val
	(py-raise 'AttributeError
		  "Object ~A has not attribute ~A" eobj attr-name)))))

(defun eval-subscription (primary data)
  (let ((e-prim (if (consp primary) 
		    (py-eval-1 primary)
		  primary))
	(e-item-to-get
	 (if (consp data)
	     
	     (progn 
	       (destructuring-bind (item-list comma?)
		   data
		 (assert (consp item-list))
		 (let* ((item-list-2 (mapcar #'py-eval-1 item-list))
			(item (if (or comma?
				      (> (length item-list) 1))
				  (make-tuple-from-list (mapcar #'py-eval-1 item-list-2))
				(car item-list-2))))
		   item)))
	   (progn (warn "assuming subs ~A evaluated already" data)
		  data))))
    	   
    (multiple-value-bind (res found)
	(call-attribute-via-class e-prim '__getitem__ (list e-item-to-get))
      (if found
	  res
	(py-raise 'TypeError
		  "No __getitem__ found (~A)" e-prim)))))


(defun eval-slice (data)
  (destructuring-bind (start stop &optional step)
      data
    (make-slice (if start (py-eval-1 start) *None*)
		(if stop (py-eval-1 stop) *None*)
		(if step (py-eval-1 step) *None*))))


(defun eval-call (primary args)
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
      
      (py-call eprim pos-args kw-args))))


(defun eval-for-in (targets sources suite else-suite)
  (let ((take-else t))
    (catch 'break
      (map-over-py-object
       (lambda (x) (setf take-else nil)
	       (eval-real-assign-expr targets x)
	       (catch 'continue
		 (py-eval-1 suite)))
       (py-eval-1 sources))
      (when (and take-else
		 else-suite)
	(py-eval-1 else-suite)))))

(defun eval-continue ()
  (throw 'continue nil))

(defun eval-break ()
  (throw 'break nil))


;; a = b,c = [1,2]
;;  -> a = [1,2]
;;     b = 1, c = 2

(defun eval-perhaps-assign-expr (items)
  (destructuring-bind (val &rest targets)
      (reverse items)
    (if targets
	(let ((eval (py-eval-1 val))) ;; real assignment statement
	  (unless eval
	    (error "PY-EVAL returned NIL for value to be assigned: ~A" val))
	  (dolist (tar targets)
	    (eval-real-assign-expr tar eval)))
      (py-eval-1 val)))) ;; just variable reference

 
(defun eval-real-assign-expr (targets evalue)
  "Assign EVALUE to TARGETS.
   Assumes EVALUE already evaluated."
  
  (assert (member (car targets) '(testlist exprlist) :test 'eq))
  (let* ((etargets (eval-assignment-targets (second targets)))
	 (num-targets (length etargets))
	 
	 (comma? (third targets)) ;; both testlist and exprlist have 
	 (target-is-list (or (> num-targets 1) ;; comma? as third list element
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
	(map-over-py-object
	 (lambda (x) 
	   (incf i)
	   (when (> i num-targets)
	     (py-raise 'ValueError "Too many values to unpack (needed exactly ~A ~@
                        values), got already one more than that." num-targets))
	   (push x acc))
	 evalue)
	
	(when (< i num-targets)
	  (py-raise 'ValueError
		    "Too few values to unpack (needed exactly ~A values, but got only ~A)."
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
  
  (cond ((eq (car target) 'identifier)   ;; x = 5
	 ;; (identifier name)
	 (namespace-bind *scope* (second target) val))
	
	((eq (car target) 'attributeref)  ;; x.y = 5
	 ;; (attributeref <primary> (identifier attr-name))
	 (destructuring-bind (primary (identifier attr-name))
	     (cdr target)
	   (declare (ignore identifier))
	   ;;(assert (eq (car attr-name) 'identifier))
	   (internal-set-attribute primary attr-name val)))
	
  	((eq (car target) 'subscription)  ;; x[y] = 5
	 ;; (subscription <primary> <subs>)
	 (destructuring-bind (primary subs)
	     (cdr target)
	   
	   (multiple-value-bind (setitem found)
	       (internal-get-attribute primary '__setitem__)
	     (if found
		 (py-call setitem (list subs val))
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
	   (map-over-py-object
	    (lambda (x)
	      (incf i)
	      (push x acc)
	      (when (> i num-targets)
		(py-raise 'ValueError "Too many values to unpack (needed exactly ~A ~
                           values, got already one more than that." num-targets)))
	    val)
		       
	   (when (< i num-targets)
	     (py-raise 'ValueError "Too few values to unpack (needed exactly ~A ~@
                        values, but got only ~A)." num-targets i))
	   
	   (setf acc (nreverse acc))
	   (loop for val in acc
	       for tar in targets
	       do (eval-assign-one tar val))))
	
	(t
	 (error "uncatched situation in EVAL-ASSIGN-ONE: ~@
                 target = ~A  val = ~A" target val))))


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
  (case (car tg)
    ;; (IDENTIFIER a) : remains the same
    (identifier tg)
    
    ;; (TUPLE A B) and (LIST A B) both go to (LIST <A> <B>)
    ((tuple list) `(list ,@(mapcar #'eval-one-assignment-target (second tg))))
    
    ;; foo.x => (attributeref (identifier foo) (identifier x))
    ;; 
    ;; The primary is evaluated; the attribute identifier
    ;; is just a name and can't be further evaluated.
    (attributeref `(attributeref ,(py-eval-1 (second tg))
				 ,(third tg)))
    
    ;; x[1,2,3]
    ;; => (subscription (identifier x) ((1 2 3) nil))) nil)
    ;;
    ;; Both primary and expr-list are evaluated; result is
    ;;  (subscription <object> <item-or-tuple>)
    (subscription 
     (destructuring-bind (primary (subs comma?))
	 (cdr tg)
       `(subscription ,(py-eval-1 primary)
		      ,(let* ((esubs (mapcar #'py-eval-1 subs))
			      (make-tuple (or (> (length subs) 1)
					      comma?)))
			 (if make-tuple
			     (make-tuple-from-list esubs)
			   (car esubs))))))
    
    ;; todo: slices
    (testlist ;; how about comma?
     `(list ,@(mapcar #'eval-one-assignment-target (second tg))))
    
    (t
     (warn "EVAL-ASSIGNMENT-TARGETS: assuming ~A is an expression ~
                       to be evaluated." tg)
     (py-eval-1 tg))))


(defun eval-augassign-expr (target operator expr)
  ;; e()[ f() ] += g()  is evaluated as follows:
  ;; 
  ;;   e() -> m  \__in eval-one-assignment-target
  ;;   f() -> p  /
  ;; 
  ;;   g() -> q
  ;;   m[p] -> r
  ;; try  r.__iadd__(q)
  ;; if no __iadd__method found, try:  x[p] = r + q
  
  (destructuring-bind (testlist? (source-place &rest other-source-places) comma?)
      target
    (assert (eq testlist? 'testlist))
    (cond ((or comma? other-source-places)
	   (py-raise 'SyntaxError
		     "Augmented assign to multiple places not possible~%AST or target: ~A"
		     target))
	  ((not (member (car source-place) '(identifier subscription attributeref)))
	   (py-raise 'SyntaxError
		     "Augmented assign to forbidden place (maybe TODO)~%AST of place: ~A"
		     source-place)))
    
    (assert (eq (car expr) 'testlist))
    (let* ((et (eval-one-assignment-target source-place))
	   (evexpr (py-eval-1 expr))
	   (lhs-value-now (py-eval-1 et))
	   (op-funcs (cdr (assoc operator *math-inplace-op-assoc*)))
	   (py-@= (car op-funcs))
	   (py-@ (cdr op-funcs)))
	
      (assert op-funcs)

      ;; try __iadd__ first, otherwise __add__ + store
	
      (unless (funcall py-@= lhs-value-now evexpr) ;; returns true iff __iadd__ found
	  
	(let ((value-to-store (funcall py-@ lhs-value-now evexpr)))
	  (eval-assign-one et value-to-store)))))
  *None*)

	
(defun eval-funcdef (fname params suite)
  "In the current namespace, FNAME becomes bound to a function object ~@
   with given formal parameters and function body."
  
  ;; Special-case: the function, when called, will return a
  ;; generator. If this is the case, treat it a bit special.
  ;; 
  ;; Note that 'yield' inside "exec" or "eval" is not allowed, so it
  ;; can be statically checked whether the function returns a
  ;; generator or not.
  
  (if (generator-ast-p suite)
      
      (let* ((params (multiple-value-list (parse-function-parameter-list params)))
	     (f (make-python-function-returning-generator (symbol-name fname) params suite)))
	(namespace-bind *scope* fname f))
  
    (let* ((params (multiple-value-list (parse-function-parameter-list params)))
	   (f (make-user-defined-function 
	       :name (symbol-name fname)
	       :ast (add-return-None-to-suite suite)
	       :namespace (make-namespace
			   :name (format nil "ns for function ~A" fname)
			   :inside *scope*)
	       :params params
	       :call-rewriter (apply #'make-call-rewriter fname params))))
	
      ;; Content of function is not evaluated yet; only when called.
      ;; Bind function name to function object in current namespace:
      (namespace-bind *scope* fname f)))
  *None*)

(defun add-return-None-to-suite (suite)
  (assert (eq (car suite) 'suite))
  (destructuring-bind
      (dummy (&rest suites)) suite
    (declare (ignore dummy))
    `(suite (,@suites (return (testlist ((identifier None)) nil))))))
  
(defun eval-lambda (params expr)
  (let ((parsed-params (multiple-value-list (parse-function-parameter-list params))))
    #+(or)(break "lambda")
    (make-lambda-function
     :name "lambda"
     :ast expr
     :namespace (make-namespace :name "lambda namespace"
				:inside *scope*)
     :params parsed-params
     :call-rewriter (apply #'make-call-rewriter 'lambda parsed-params))))

(defun parse-function-parameter-list (params)
  "Returns POS-PARAMS, KW-PARAMS, *-PAR, **-PAR as multiple values"

  (destructuring-bind (pos-kw-params *-par **-par)
      params
    
    ;; Keyword arguments must come after positional arguments.
    ;; The grammar doesn't restrict that, so check it here.
    
    (let ((found-kw nil))
      (mapc (lambda (x)
	      (cond ((consp x)
		     (setf found-kw x))
		    ((and found-kw (symbolp x))
		     (py-raise 'SyntaxError
			       "Non-default argument '~A' follows default argument '~A'"
			       x (car found-kw)))
		    (t)))
	    pos-kw-params))
    
    (loop for x in pos-kw-params
	if (consp x) 
	collect (progn (setf (cdr x) (py-eval-1 (cdr x)))
		       x)
	into kw-args
	     
	else collect x into pos-args
	finally (return (values pos-args kw-args *-par **-par)))))


#+(or)
(defun eval-classdef (cname inheritance suite)
  
  ;; cls = Metaclass.__new__(Metaclass, name, basis, dict
  ;; assert cls.__class__ == Metaclass
  ;; M.__init__(cls, name, bases, dict)
  
  )

  
(defun eval-classdef (cname inheritance suite)
  
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
	c))))


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

(defun make-module-object (module-name)
  (let* ((file-name (concatenate 'string (string module-name) ".py")))
    
    (loop
      (with-simple-restart (continue "Reload, retry import")
	
	;; In CPython, when the toplevel of modules is executed, the
	;; name of the module is not yet bound to the module object
	
	(let* ((module-ns (make-namespace
			   :name (format nil "namespace for module ~A" module-name)
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
		 (with-simple-restart (continue "Reload, retry import")

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
		 (with-simple-restart (continue "Reload, retry import")

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


(defun eval-import (items)
  (dolist (item items)
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

;; XXX todo
(defun eval-import-from (mod items)
  ;; (break "import: TODO")
  ;; "from a.b.c import d"  -->  (import-from (dotted a b c) ((as d d)))
  ;; "from a import b"      -->  (import-from a ((as b b)))
  
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
			     (namespace-bind *scope* name-here obj))))))))))

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

      
(defun eval-if (clauses else-suite)
  (loop for (expr suite) in clauses
      when (py-val->lisp-bool (py-eval-1 expr))
      do (py-eval-1 suite)
	 (return)
      finally
	(when else-suite
	  (py-eval-1 else-suite))))

(defun eval-while (test suite else-suite)
  (let ((taken nil))
    (catch 'break
      (loop while (py-val->lisp-bool (py-eval-1 test))
	  do (catch 'continue
	       (setf taken t)
	       (py-eval-1 suite))))
    
    (when (and (not taken) else-suite)
      (py-eval-1 else-suite))))

(defun eval-comparison (operator left right)
  "Does comparison, returns Python boolean"
  (let ((comp-fun (cdr (assoc operator *math-binary-cmp-assoc*))))
    (assert comp-fun () "No comparison function corresponding to ~
                         comparison operator ~A?! ~A"
	    operator *math-binary-cmp-assoc*)
    (lisp-val->py-bool (funcall comp-fun (py-eval-1 left) (py-eval-1 right)))))

(defun eval-unary (operator val)
  (declare (special *math-unary-op-assoc*))
  (let ((func (cdr (assoc operator *math-unary-op-assoc*))))
    (unless func
      (error "No function for unary operator ~A?! ~A" operator *math-unary-op-assoc*))
    (funcall func (py-eval-1 val))))

(defun eval-binary (operator left right)
  (declare (special *math-binary-op-assoc*)) ;; defined in mathops.cl
  (when (and (eq operator '/)
	     *__future__.division*)
    (setf operator '/t/))
  (let ((func (cdr (assoc operator *math-binary-op-assoc*))))
    (assert func () "Operator ~A has no corresponding py-~A function?! ~A"
	    operator operator *math-binary-op-assoc*)
    (funcall func (py-eval-1 left) (py-eval-1 right))))

(defun eval-binary-lazy (operator left right)
  (declare (special *math-binary-lazy-op-assoc*)) ;; mathops.cl
  (let ((func (cdr (assoc operator *math-binary-lazy-op-assoc*))))
    (funcall (or func
		 (error "no func for lazy ~A operator" operator))
	     left right))) ;; left, right not yet evaluated!

(defun eval-tuple (&rest content)
  (let ((c (mapcar #'py-eval-1 content)))
    (make-tuple-from-list c)))

(defun get-slice (obj start end &optional (step nil))
  (declare (ignore obj start end step))
  'todo)


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
    (let* ((ex (py-eval-1 x))
	   (str (call-attribute-via-class ex '__str__)))
      (format stream "~A " str)))
  
  (unless comma?
    (terpri)))
