(in-package :python)

;;; Calling objects

(defgeneric __call__ (x &optional pos-args kwds-args)
  (:documentation "Call X with given arguments. Call arg rewriting must take ~@
                   place _before_ calling __call__ (or in __CALL__ :AROUND ?)"))


;; None of the built-in functions take keyword argument, so give an
;; error in case kw args are given.

(defmethod __call__ ((x generic-function) &optional pos-args kwd-args)
  (when kwd-args
    (warn "supplying keyword args to generic function ~A (kw: ~S)" x kwd-args))
  #+(or)(py-raise 'ValueError "built-in function ~S does not take keyword arguments (got: ~A)"
		  x kwd-args)
  (apply x pos-args))

(defmethod __call__ ((x function) &optional pos-args kwd-args)
  (when kwd-args
    (warn "__call__ on regular Lisp Function ~A got keyword args?! ~@
           (pos-args: ~A; kwd-args: ~A - ignored)"
	  x pos-args kwd-args))
  (apply x pos-args))


;;; user-defined functions/methods:   

(defmethod __call__ :around ((x user-defined-function) &optional pos-args key-args)
  "Evaluate function body in function definition namespace."
  (declare (ignore pos-args key-args))
  
  ;; If the AST contains `return', it exists from this FUNCTION-BLOCK
  ;; (or a FB inside this one). `return' can only occur inside a
  ;; function body.
  ;; When the function doesn't do `return', None is returned implicitly.

  (catch 'function-block
    (call-next-method)
    *None*))


(defmethod __call__ ((x python-function) &optional pos-args key-args)
  ;; used in user-defined functions
  (with-slots (call-rewriter namespace ast) x
    (let ((actual-args (funcall call-rewriter pos-args key-args)))
      
      ;; Evaluate function AST in the function definition namespace
      ;; extended with the current argument values.

      (loop for (arg . val) in actual-args
	  do (namespace-bind namespace arg val))
      
      (let ((*scope* namespace))
	(declare (special *scope*))
	(py-eval ast)))))


(defmethod __call__ ((x python-function-returning-generator) &optional pos-args key-args)
  (with-slots (call-rewriter generator-creator) x
    (declare (special *scope*))
    (let ((ns (make-namespace :name "generator namespace"
			      :inside *scope*))
	  (value-producing-f (funcall generator-creator)))
      
      (loop for (arg . val) in (funcall call-rewriter pos-args key-args)
	  do (namespace-bind ns arg val))
      
      (make-iterator-from-function (lambda () 
				     (let ((*scope* ns))
				       (declare (special *scope*))
				       (funcall value-producing-f)))))))
		

(defmethod __call__ ((x py-bound-method) &optional pos-args kwd-args)
  "The instance enclosed in the bound method is prefixed to pos-args"
  (unless (listp pos-args)
    (error "pos-args should be list (got: ~A)" pos-args))
  (__call__ (slot-value x 'func)
	    (cons (slot-value x 'self)
		  pos-args)
	    kwd-args))


(defmethod __call__ ((x py-unbound-method) &optional pos-args kwd-args)
  "X must be of right class, then call class method with given args."
  (unless pos-args
    (py-raise 'TypeError "Unbound method~% ~X~% must be called with instance as ~
                          first argument (got no positional args instead)" x))
  (with-slots ((um-class class) (um-func func)) x
    
    (let ((inst (car pos-args)))
      
      (unless (or (typep inst um-class) ;; most common case
		  (subtypep (__class__ inst) um-class) ;; first arg is a python value designator or subclass instance
		  (and (typep inst 'class) ;; method is a __new__ method, and first arg is same or subclass
		       (subtypep inst um-class)
		       (is-a-__new__-method um-func)))
	(py-raise 'TypeError
		  "Unbound method ~A must be called with instance of ~
                   class ~%~A ~%as first argument (got as first arg: ~S)"
		  x um-class inst))
    
      (__call__ um-func pos-args kwd-args))))



;;;; Calling a class creates an instance

#+(or)
(defmethod __call__ :around ((x builtin-class) &optional pos-args kwd-args)
  (when kwd-args
    (py-raise 'ValueError "Built-in classes do not take keyword arguments (XXX ?!) (got: ~S)" kwd-args))
  ;; XXX or allow?
  (call-next-method))

(defmethod __call__ ((cls python-type) &optional pos-args kwd-args)
  ;; <cls>.__new__(<cls>, ..) creates and returns a new instance
  ;; <cls>.__init__(<cls instance>, ..) initializes it but doesn't return anything useful
  
  (multiple-value-bind (meth found)
      (internal-get-attribute cls '__new__)
    (if found
	
	(let ((inst (__call__ meth (cons cls pos-args) kwd-args))) ;; call with class as first arg
	  (multiple-value-bind (res found)
	      (call-attribute-via-class inst '__init__ pos-args kwd-args)
	    (declare (ignore res))
	    (unless found
	      (error "Class ~S has no method __init__" cls)))
	  inst)
      
      (error "Class ~S has no method __new__" cls))))


#+(or)
(defmethod __call__ ((cls user-defined-class) &optional pos-args kwd-args)
  ;;; Create an instance of CLS, by calling the __new__ and __init__
  ;;; methods of CLS; both are called with ARGS.
  ;;; XXX todo: rename make-instance to __new__ everywhere
  (declare (ignore pos-args kwd-args))
  (make-instance cls))



#+(or)
(let ((cr (make-call-rewriter '(a b) '((c . 3)(d . 4)) 'args 'kwd))
      (posargs '(1 7 8 9 1 2 3))
      (kwargs '((e . 5)))
      )
  (funcall cr posargs kwargs))

(defun make-call-rewriter (fpos fkey f* f**)
  "For a given function with formal arguments FPOS, FKEY, F*, F**
   where FPOS list of atoms, FKEY list of (atom . value), F* atom or nil,
   F** atom or nil,  return a function that parses the arguments to it
   and return it as a list of (key . val) pairs."
  ;; Example:
  ;; (setq r (make-call-rewriter
  ;;          '()
  ;;          '((fget . *None*) (fset . *None*) (fdel . *None*) (doc . *None*))
  ;;          nil nil))
  ;; (funcall r '("asdf") '((doc . "doc!")))
  ;; -> ((fget . "asdf") (fset . *None*) (fdel . *None*) (doc . "doc!"))
  (let* ((fpos-len (length fpos))
	 (farg-len (+ fpos-len (length fkey)))
	 (argnames-list (concatenate 'list fpos (mapcar #'car fkey)))
	 (argnames-v (make-array farg-len
				 :element-type 'atom
				 :initial-contents argnames-list))
	 (pos-arg-marker 'pos-arg)
	 (opt-arg-marker 'opt-arg)
	 (defvalues-list
	     (append 
	      (mapcar (lambda (x) (declare (ignore x)) pos-arg-marker) fpos)
	      (mapcar (lambda (kv) (cons opt-arg-marker (cdr kv))) fkey)))
	 (argvalues-v (make-array farg-len :initial-contents defvalues-list)))
    
    (unless (equalp argnames-list (remove-duplicates argnames-list))
      (error "Duplicated formal argument names found: should be different (~A ~A)"
	     fpos fkey))
    (when (and f* (member f* argnames-list :test 'eq))
      (error "Formal argument name for f* (~A) is already used (~A ~A)"
	     f* fpos fkey))
    (when (and f** (member f** (cons f* argnames-list) :test 'eq))
      (error "Formal argument name for f** (~A) already used (~A ~A ~A)"
	     f** fpos fkey f*))
    
    (lambda (pos-args key-args)
      #+(or)(format t "call rewriter for {pos: ~A, key: ~A, f*: ~A, f**: ~A}"
		    fpos fkey f* f**)
      #+(or)(format t "got ~A ~A~%" pos-args key-args)
      
      (let ((result ()))
	(declare (dynamic-extent result))
	;; Because the result is only used for extending an
	;; environment with the proper values, the RESULT list can be
	;; allocated just once.
	;; 
	;; Alternatively, there is no need for a RESULT list if the
	;; environment to be extended is passed as an argument to the
	;; LAMBDA.
	
	;; reset argvalues-v (as its overwritten every time this lambda is called)
	;; 
	;; XXX maybe this should happen right after creating the
	;; RESULT list, to enable GC?
	(map-into argvalues-v #'identity defvalues-list)
	
	;; f* -> |apos| not limited
	;; f** -> |akey| not limited
	;;
	;; always:  |apos| + |akey| >= |fpos| (all positionals must be covered)
	;;          
	;; not f** ->  |akey| <= |fpos| + |fkey|
	;;    (all args can be covered by kwargs, not more)
	;; not f*  ->  |apos| <= |fpos| + |fkey| 
	;;    (all args can be covered by nonkwargs, not more)
	
	;; Assign positional args
	(let ((final-pos-arg 'foo)
	      (pos-arg-2 pos-args))
	   
	  (loop for arg-index from 0 below farg-len
	      until (or (not pos-arg-2)
			(= arg-index farg-len))
	      do (setf (aref argvalues-v arg-index) (car pos-arg-2)
		       pos-arg-2 (cdr pos-arg-2))
	      finally (setf final-pos-arg pos-arg-2))
	   
	  (assert (not (eq final-pos-arg 'foo)))
	   
	  (if f*
	      (push (cons f* (make-tuple-from-list pos-arg-2)) result)
	    (if pos-arg-2
		(py-raise 'TypeError
			  "Too many positional args supplied"))))
	 
	;; Assign kw args
	(let ((for-f** ()))
	  (dolist (kw key-args)
	    (let* ((key (car kw))
		   (val (cdr kw))
		   (found (loop for k from 0 below farg-len
			      when (eq (aref argnames-v k) key)
			      do (let ((oldval (aref argvalues-v k)))
				   (unless
				       (or (eq oldval pos-arg-marker)
					   (and (consp oldval)
						(eq (car oldval) opt-arg-marker)))
				     (error "Got multiple values for parameter ~A (~A, ~A)"
					    key oldval val)))
				 (setf (aref argvalues-v k) val)
				 (return t)
			      finally (return nil))))
	       
	      (unless found
		(if f**
		    (push kw for-f**)
		  (error "Got unexpected keyword argument: ~A" key)))))
	 
	  (when f**
	    (push (cons f** (make-dict for-f**)) result)))
		     
	;; Collect all formal arguments except f*, f**
	(loop for i from 0 below farg-len
	    do (let ((val (aref argvalues-v i)))
		 (cond
		  ((eq val pos-arg-marker)
		   (error "Got no value for argument: ~A" (aref argnames-v i)))
		  ((and (consp val) (eq (car val) opt-arg-marker))
		   (setf val (cdr val))))
		 (push (cons (aref argnames-v i) val) result)))
	
	;; NREVERSE is only to easy debugging
	;; (format t "call rewriter: result: ~A~%" (reverse result))
	(nreverse result)))))
