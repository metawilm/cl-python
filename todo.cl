(in-package :python)

#|
nested classes/functions  print-object  dotted:
http://sourceforge.net/tracker/?group_id=5470&atid=105470&func=detail&aid=633930
sys.exc... are deprecated; sys.exc_info instead:
http://www.google.com/search?as_q=exc_traceback%20deprecated&num=100&hl=en&ie=UTF-8&btnG=Google+Search&as_epq=&as_oq=&as_eq=&lr=&as_ft=i&as_filetype=&as_qdr=all&as_nlo=&as_nhi=&as_occt=any&as_dt=i&as_sitesearch=&safe=images

str is a type -- list of all builtin types:  xrange is type
http://mail.python.org/pipermail/python-dev/2002-May/024346.html

|#

;; stuff tODO

(defmethod make-init-instance ((cls python-type) &rest args)
  (let ((x (make-instance cls)))
    (multiple-value-bind (init found)
	(get-attribute x '__init__)
      (when found
	(if (unbound-method-p init)
	    (apply #'__call__ init x args)
	  (error "Attribute `__init__' of class ~A is not a ~
                  function (it is: ~S)" cls init))))
    x))



;;; Calling functions, methods, and objects in general

#+(or)
(defgeneric call ((x python-object) &rest args)
  (:documentation "Call FUNCTION with arguments ARGS."))

#+(or)
(defmethod call ((f python-function) &rest args)
  "Call a PYTHON-FUNCTION F by calling its underlying CL function"
  (apply (slot-value f '_clpy_func) args))

#+(or)
(defmethod call ((m python-bound-method) &rest args)
  "Prepend INSTANCE to arguments,then call the method's function."
  (apply #'call (slot-value m 'im_func) (slot-value m 'im_self) args))

#+(or)
(defmethod call ((m python-unbound-method) &rest args)
  "Must be called with instance of class to which method is bound."
  (let ((inst (car args))
	(class (slot-value m 'im_class)))
    (cond ((not inst) (error "Unbound method ~A must be called with ~A ~
                              instance as first argument (got no ~
                              arguments at all)."
			     m (slot-value m 'im_class)))
	  ((typep inst (slot-value m 'im_class))
	   (apply #'call (slot-value m 'im_func) args))
	  (t (error "Can't call ~A with instance ~A because it is not ~
                     an instance of class ~A."
		    m inst class)))))

#+(or)
(defmethod call ((x python-object) &rest args)
  "A Python class instance may be called when its class defines a
   method named `__call__'."
  (assert (python-class-instance-p x))
  (multiple-value-bind (val found)
      (get-attribute x '__call__)
    (cond ((and found (bound-method-p val))
	   (apply #'call val args))
	  (found 
	   (error "Instance ~A can't be called, because it's ~
                   `__call__' attribute is not a function (got: ~S)"
		  x val))
	  (t
	   (error "Instance ~A can't be called, because instances of ~
	           class ~A don't have a `__call__' method."
		  x (class-of x))))))


#+(or)
(defmethod call ((cls python-type) &rest args)
  "A python class may be called to create an instance of that class."
  (assert (python-class-p cls))
  (let ((inst (make-instance cls)))
    (multiple-value-bind (init found)
	(get-attribute cls '__init__)
      (cond ((and found (unbound-method-p init))
	     (apply #'call init inst args))
	    (found
	     (error "Attribute `__init__' of class ~A is not a ~
                     function (it is: ~S)" cls init))
	    (t))
      inst)))
  


;;; printing objects

#+(or)
(defmethod print-object ((x python-type) stream)
  (if (python-class-p x)
      (print-unreadable-object (x stream :type nil)
	(format stream "python-class ~A"
		(__name__ x)))
    (call-next-method)))

#+(or)
(defmethod print-object ((x python-object) stream)
  (print-unreadable-object (x stream :type t :identity *print-escape*)
    (format stream "instance")))

#+(or)
(defmethod print-object ((x python-bound-method) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~A.~A of instance ~A"
	    (__name__ (slot-value x 'im_class))
	    (__name__ (slot-value x 'im_func))
	    (slot-value x 'im_self))))

#+(or)
(defmethod print-object ((x python-unbound-method) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~A.~A"
	    (__name__ (slot-value x 'im_class))
	    (__name__ (slot-value x 'im_func)))))

#+(or)
(defmethod print-object ((n python-none) stream)
  (print-unreadable-object (n stream)
    (format stream "python-object None")))

;;; inspecting objects

#+(or)
(defgeneric dir (x)
  (:documentation "Get an alphabetized list of names of all attributes of X."))

#+(or)
(defmethod dir ((x python-object))
  "Return a sorted list of all attributes of object X."
  (let ((res ())
	(cls x))
    
    (when (python-class-instance-p x)
      (maphash (lambda (k v) (declare (ignore v)) (push k res))
	       (__dict__ x))
      (setf cls (class-of x)))
    
    ;; As long as all items for which `inherited-attribute-p' is true
    ;; are slot names of python-object, no need to check if slot
    ;; may be inherited from superclass.
    (loop for class in (mop:class-precedence-list cls)
	until (eq class (load-time-value (find-class 'standard-object)))
	do (mapcar (lambda (slot) (push (slot-value slot 'excl::name) res))
		   (mop:class-slots cls))
	   (maphash (lambda (k v) (declare (ignore v)) (push k res))
		    (__dict__ cls)))
    (sort (remove-duplicates res) #'string< :key #'symbol-name)))
    

#+(or)
(defmethod dir2 ((x python-object))
  "Print all attributes and their values from object X."
  (format t "--- Attributes of ~W ---~%" x)
  (mapcar (lambda (attr)
	    (format t "~W~20,5t: " attr)
	    (multiple-value-bind (val found)
		(get-attribute x attr)
	      (if found
		  (format t "~W" val)
		(format t "<<unbound>>"))
	      ;; (unbound-slot () (format t "#<unbound>")))
	      (format t "~%")))
	  (dir x))
  (values))


;;; Test


;; Always run these tests to make sure the types are correct.

#+(or)
(let* ((c (make-python-class :name dummy))
       (m (make-instance c)))
  ;; A (user-defined or built-in) Python class...
  (assert (typep c 'python-object)) ;; is an instance of python-object
  (assert (subtypep c 'python-object)) ;; is a subtype of python-object
  (assert (subtypep c 'python-type)) ;; is a (user def/b.i.) Python class
  (assert (python-class-p c)) ;; using predicate function
  
  ;; A Python class instance...
  (assert (typep m 'python-object)) ;; is an instance of python-object
  (assert (eq (class-of m) c)) ;; is an instance of its class
  (assert (python-class-instance-p m))) ;; nice predicate function


;; The Python class type will not be exposed, but is used internally
;; to deal with classes. The Python class type...
#+(or)
(progn
  (assert (subtypep (find-class 'python-type) ;; is a subtype of Python object
		    'python-object))
  (assert (eq (class-of (find-class 'python-type)) ;; is of type python-type
	      (find-class 'python-type)))
  (assert (not (subtypep 'python-type ;; is not a subtype of python-type
			 'python-type))))

#+(or)
(let* ((c (make-python-class :name dummy2))
       (m (make-instance c)))
  ;; A Python class instance...
  (assert (typep m 'python-object)) ;; is an instance of python-object
  (assert (eq (class-of m) c))) ;; is an instance of its class

#+(or)
(defun test ()
  (let* ((bazz (make-function :name 'bazz :func (lambda () 2)))
	 (foo (make-python-class :name Foo))
	 (deriv (make-python-class :name Deriv :supers (Foo))))
    (def-class-method foo 'm (lambda (x) (get-attribute x 'att)))
    (def-class-method foo '__getattr__ 
      (lambda (self attname)
	(declare (ignore self))
	(if (member attname '(__call__ __init__) :test 'eq)
	    (values nil nil)
	  (concatenate 'string "foo " (string attname)))))
      
    (macrolet ((try-these (&rest list)
		 `(progn ,@(loop for item in list
			       collect `(progn (format t "~A~%  --> " ',item)
					       (format t "~S~%" ,item))))))
      (let ((x (make-instance foo))
	    (d (make-instance deriv)))
	(set-attribute x 'att 42)
	(set-attribute d 'att 99)
	(try-these
	 bazz				; <python-function 'bazz'>
	 (call bazz)			; 2
	 x				; <Foo @ ...>
	 'aap
	 (get-attribute foo 'm)		; <unbound-method Foo.m>
	 (get-attribute foo '__getattr__) ; <unbound-method Foo.__getattr__>
	 (call (get-attribute foo '__getattr__) x "rr") ; "foo rr"
	 (get-attribute x 'att)		; 42
	 'noot
	 (get-attribute x 'mx)		; "foo mx"
	 (get-attribute x 'm)		; bound method Foo.m
	 (get-attribute x '__getattr__)	; bound method Foo.__getattr__
	 'mies
	 (get-attribute (get-attribute x '__getattr__) 'im_self) ; <Foo @ ...>
	 (call (get-attribute x '__getattr__) "mx") ; "foo mx"
	 (call (get-attribute foo 'm) x) ; 42
	 'wim
	 (get-attribute deriv 'm)	; <unbound-method Deriv.m>
	 (get-attribute deriv '__getattr__) ; <unbound-method Deriv.__getattr__>
	 (get-attribute d "sdf")	; "foo sdf" 
	 (call (get-attribute deriv 'm) d) ; 99
	 'zus
	 (dir (class-of x))
	 (dir x)
	 'jet
	 ;(dir2 (class-of x))
	 'teun
	 ;(dir2 x)
	 (multiple-value-list (get-attribute x '__name__))
       ;;; These should fail, because they do erroneous things:
	 ;; (call (get-attribute deriv 'm) x) ; ERROR: x not instance of Deriv
	 ;; (call (get-attribute deriv 'm)) ; ERROR: need instance argument
       
	 )))))

#+(or)
(defun test-2 ()
  (let* ((foo123 (make-python-class :name Foo123))
	 (x (make-instance foo123))
	 (y (call foo123)))
    (def-class-method foo123 '__call__
      (lambda (self val) (declare (ignore self)) (+ 3 val)))
    (values (call x 39)
	    (call y 39))))

#+(or)
(defun test-3 ()
  (let ((foo (make-python-class :name Foo)))
    (def-class-method foo '__init__ 
      (lambda (self x) (set-attribute self 'x x)))
    (let ((i (call foo 5)))
      (get-attribute i 'x))))		; 5 t

Descriptor explanation (incl. {object,type,super}.__getattribute__)
http://users.rcn.com/python/download/Descriptor.htm