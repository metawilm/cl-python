(in-package :python)


;; Python metatype
;; 
;; Class `type' and subclasses thereof are instances of
;; py-meta-type.

(defclass py-meta-type (standard-class)
  ((dict :initarg :dict :accessor py-class-dict)
   (mro  :initarg :mro  :accessor py-class-mro))
  ())

(defmethod make-instance
    ((cls (eql (find-class 'py-meta-type))) &rest initargs)
  ;; Create a new Python metatype
  ...)

#+(or)
(progn ;; use regular slots
  (defmethod compute-slots ((cls py-meta-type) &rest initargs)
    ;; Determine the slots of a Python metaclass
    ...)
  
  (defmethod initialize-instance ((cls py-meta-type) &rest initargs)
    ;; Initialize a Python metatype
    ...)
  
  (defmethod slot-value-using-class ((x (eql (find-class 'py-meta-type))) instance slot-name)
    ;; Get attribute of an instance of a Python metatype
    (py-attr-get instance slot-name))
  
  (defmethod (setf slot-value-using-class)
      (new-value (cls (eql (find-class 'py-meta-type))) instance slot-name)
    ;; Set attribute value of a Python metatype
    (py-attr-set instance slot-name new-value))
  
  (defmethod slot-boundp-using-class
      ((cls (eql (find-class 'py-meta-type))) instance slot-name)
    ;; Does a Python metatype have the attribute?
    (not (null (py-attr-get instance slot-name))))
  
  (defmethod slot-makunbound-using-class
      ((cls (eql (find-class 'py-meta-type))) instance slot-name)
    ;; Remove attribute of a Python metatype
    (py-attr-del instance slot-name)))


;; A class for Python classes
;; 
;; These are instances of python-meta-type

(defclass py-type (standard-class)
  ((dict :initarg :dict :accessor py-class-dict)
   (mro  :initarg :mro  :accessor py-class-mro)
   (builtin-superclass :initarg :bi-super :accessor py-class-builtin-superclass))
  (:metaclass py-meta-type))


  
(defmethod make-instance ((cls py-meta-type) &rest initargs)
  ;; Create a new Python class that has CLS as metaclass
  ...)

(defmethod compute-class-precedence-list ((cls py-meta-type))
  ;; Compute CPL of new Python class
  ...)

(defmethod allocate-instance ((cls py-type) &rest initargs) 
  ;; __new__
  )

(defmethod initialize-instance ((cls py-type) &rest initargs)
  ;; Initialize a Python class
  ;;  - call __init__ method
  ...)

#+(or)
(progn 
  (defmethod compute-slots ((cls py-type))
    ;; Determine the slots of a Python class
    ;;  - take into account value of SLOTS slot. 
    ...)

  (defmethod slot-value-using-class ((cls py-meta-type) instance slot-name)
    ;; Get attribute of a Python class
    (py-attr-get instance slot-name))

  (defmethod (setf slot-value-using-class)
      (new-value (cls py-meta-type) instance slot-name)
    ;; Set atttribute value of a Python class
    (py-attr-set instance slot-name new-value))

  (defmethod slot-boundp-using-class ((x py-meta-type) instance slot-name)
    ;; Does instance have the attribute?
    (not (null (py-attr-get instance slot-name))))

  (defmethod slot-makunbound-using-class ((x py-meta-type) instance slot-name)
    ;; Remove attribute of a Python class
    (py-attr-del instance slot-name)))


;; Regular Python classes are instances of python-type (or a subclass
;; thereof)

(defclass py-object (standard-object)
  ((dict :initarg :dict :initform (make-hash-table :test #'eq) :accessor py-instance-dict))
  (:metaclass py-type))

(defmethod make-instance ((cls py-type) &rest initargs)
  ;; Create an instance of a Python class.
  ...)


(defmethod initialize-instance ((x py-object) &rest initargs)
  ;; Initialize a Python object (an instance of a Python class)
  ...)

#+(or)
(progn
  (defmethod slot-value-using-class ((x py-type) instance slot-name)
    ;; Get attribute of an instance of a Python class
    (py-attr-get instance slot-name))

  (defmethod (setf slot-value-using-class)
      (new-value (x py-type) instance slot-name)
    ;; Set atttribute value of an instance of a Python class
    (py-attr-set instance slot-name new-value))

  (defmethod slot-boundp-using-class ((x py-type) instance slot-name)
    ;; Does instance have the attribute?
    (not (null (py-attr-get instance slot-name))))

  (defmethod slot-makunbound-using-class ((x py-type) instance slot-name)
    ;; Remove attribute of an instance of a Python class
    (py-attr-del instance slot-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Python functions

(defclass py-function (standard-generic-function)
  ;; MOP:FUNCALLABLE-STANDARD-CLASS has :name initarg
  ((dict :initform nil))
  (:metaclass mop:funcallable-standard-class))

(defmethod make-py-function ((name symbol) (f function))
  (let ((x (make-instance 'py-function :name name)))
    (mop:set-funcallable-instance-function x f)
    x))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass py-method (py-object)
  ((func :initarg :func :accessor py-method-func))
  (:metaclass py-type))


(defclass py-bound-method (py-object)
  ((instance :initarg :instance :accessor py-method-instance))
  (:metaclass py-type))

(defmethod make-bound-method (x (f function))
  (make-instance 'py-bound-method :instance x :func f))


(defclass py-unbound-method (py-method)
  ((class :initarg :class :accessor py-method-class))
  (:metaclass py-type))

(defmethod make-unbound-method ((x class) (f function))
  (make-instance 'py-unbound-method :class x :func f))


(defclass py-static-method (py-method)
  ()
  (:metaclass py-type))

(defmethod make-static-method ((f function))
  (make-instance 'py-static-method :func f))


(defclass py-class-method (py-method)
  (#+(or)(class :initarg :class :accessor py-method-class)) ;; not bound to class(?)
  (:metaclass py-type))

(defmethod make-class-method ((f function))
  (make-instance 'py-class-method :func f))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The objects in Python world are  are either "core objects" (represented directly as Lisp
;; object) or "user objects" (represented as instance of a Python
;; class)


(defgeneric py-lisp-object-p (x) ;; represented as lisp object; subclassable
  (:method ((x number)) t)
  (:method ((x string)) t)
  (:method ((x vector)) t)
  (:method ((x list))   t)
  (:method ((x t))     nil))

(defgeneric py-core-object-p (x) ;; represented as python class instance; not subclassable
  (:method ((x py-module))   t)
  (:method ((x py-function)) t)
  (:method ((x py-property)) t)
  (:method ((x py-super))    t)
  (:method ((x py-xrange))   t))   ;; not: py-file

(defmethod py-user-object-p (x) ;; represented as python class instance; subclassable
  (and (not (py-lisp-object-p x))
       (not (py-core-object-p x))))


(defgeneric py-class-of (x)
  (:method ((x integer)) (load-time-value (find-class 'py-integer)))
  (:method ((x real))    (load-time-value (find-class 'py-float  )))
  (:method ((x complex)) (load-time-value (find-class 'py-complex)))
  (:method ((x string))  (load-time-value (find-class 'py-string )))
  (:method ((x vector))  (load-time-value (find-class 'py-list   )))
  (:method ((x list))    (load-time-value (find-class 'py-tuple  )))
  (:method ((x class))   (if (eq x (load-time-value
				    (find-class 'py-type))) ;; py-type is its own class
			     x
			   (class-of x)))
  (:method ((x t))       (class-of x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize the dict of class PY-TYPE.

(defvar *py-type-attr* '((__name__ #'py-type.__name__)
			 (__file__ #'py-type.__file__)
			 (__getattribute__ #'py-type.__getattribute__)))

(defun initialize-py-type-dict ()
  (setf (py-class-dict (find-class 'py-type)) (make-attr-ht *py-type-attr*)))

(defun make-attr-ht (kv)
  (loop with ht = (make-hash-table :test #'eq)
      for (key val) in kv do (setf (gethash key ht) val)
      finally (return ht)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attributes: getting, setting, deleting

(defvar *integer-attr-ht*)
(defvar *real-attr-ht*)
(defvar *number-attr-ht*)
(defvar *complex-attr-ht*)
(defvar *string-attr-ht*)
(defvar *list-attr-ht*)
(defvar *tuple-attr-ht*)

(defgeneric py-attr (x attr)
  
  (:method ((x integer) attr) (maybe-make-bound-method
			       x (or (gethash attr *integer-attr-ht*)
				     (gethash attr *real-attr-ht*)
				     (gethash attr *number-attr-ht*))))
  
  (:method ((x real)    attr) (maybe-make-bound-method
			       x (or (gethash attr *real-attr-ht*)
				     (gethash attr *number-attr-ht*))))
  
  (:method ((x complex) attr) (case attr
				(real (realpart x))
				(imag (imagpart x))
				(conjugate (conjugate x))
				(t (maybe-make-bound-method
				    x (gethash attr *number-attr-ht*)))))
  
  (:method ((x string) attr) (maybe-make-bound-method
			      x (gethash attr *string-attr-ht*)))
  
  (:method ((x vector) attr) (maybe-make-bound-method
			      x (gethash attr *list-attr-ht*)))
  
  (:method ((x list)   attr) (maybe-make-bound-method
			      x (gethash attr *tuple-attr-ht*)))
  
  (:method ((x py-module)   attr) (gethash attr (py-module-dict x)))
  (:method ((x py-function) attr) (gethash attr (py-function-dict x)))
  
  (t (py-call (py-attr-class x '__getattribute__ t) x attr)))

(defun maybe-make-bound-method (x meth)
  (typecase meth
    (py-attribute (funcall attr x))
    (function     (make-bound-method :self x :func attr))
    (t            meth)))
  
(defun py-call-ht-attr (x attr ht)
  (let ((val (gethash attr ht)))
    (when val
      (py-call val x)))) ;; or funcall, if that works for static methods etc


(defun py-attr-class (x attr &optional assert-exist-p)
  "The value of attribute of X, that should be in its class-dict, or NIL"
  (or (gethash (py-class-dict (py-class-of x)) attr)
      (when assert-exist-p
	(error "Class ~A has no attribute ~A, but it should have."))))


(defvar *__getattribute__-level* 0)

(defun py-type.__getattribute__ (x attr)
  (if (= *__getattribute__-level* 0)
      
      (let ((*__getattribute__-level* 1))
	(py-attr x attr))
    
    (progn (assert (not (py-core-object-p x))) ;; core and lisp objects are
	   (assert (not (py-lisp-object-p x))) ;; handled in py-attr
	   
	   (cond ((typep x 'class)
		  (assert (or (subtypep x (find-class 'py-type))
			      (subtypep x (find-class 'py-object))))
		  ;; x is one of:
		  ;;  1) class `type'
		  ;;  2) a user-defined metatype that is a subtype of `type'
		  ;;  3) class `object'
		  ;;  4) a user-defined subclass of `object'
		  
		  (loop for cls in (py-class-mro x)
		      for val = (gethash attr (py-class-dict cls))
		      when val
		      do (return-from py-type.__getattribute__ val)))
		 
		 (t 
		  ;; not a class, but only a class instance
		  (assert (typep x (find-class 'py-object)))
		  ;; xxx check here for __getattribute__, __getattr__ etc
		  (or (gethash attr (py-instance-dict x))
		      (let ((cls (py-class-of x)))
			(or (loop for cls in (py-class-mro cls)
				for val = (gethash attr (py-class-dict cls))
				when val do (return-from py-type.__getattribute__ 
					      (maybe-make-bound-method x val)))
			    (let ((bi-cls (py-class-builtin-superclass cls)))
			      (when bi-cls
				(let ((*__getattribute__-level* 0))
				  (maybe-make-bound-method x (py-attr bi-cls attr)))))))))))))

(defvar *number-attr* ((__new__     (make-static-method #'number-__new__))
		       (__hash__    #'number.__hash__)
		       (__repr__    #'number.__repr__)
		       (__nonzero__ #'number.__nonzero__)
		       (__neg__     #'number.__neg__)
		       (__pos__     #'number.__pos__)
		       (__abs__     #'number.__abs__)))

(defvar *complex-attr* ((real #'complex.real)
			(imag #'complex.imag)))


(setf *number-attr-ht* (make-attr-ht *number-attr*))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Optimize attribute lookup when the name of the attribute is
;; recognized as a built-in attribute/method.
;; 
;; The lookup of builtin-object-attr-lookup-fun is moved to
;; compile-time.

#+(or)
(if (py-user-object-p primary)
    (py-attr-user-object primary attr :perhaps nil)
  (let ((boalf (builtin-object-attr-lookup-fun attr)))
    (if boalf
	(funcall boalf primary)
      (py-raise 'AttributeError "Object ~S has no attribute ~S" primary attr))))

#+(or) ;; not yet
(define-compiler-macro py-attr (&whole whole primary attr)
  
  (if (and (listp attr)
	   (= (length attr) 2)
	   (eq (first attr) 'quote) 
	   (symbolp (second attr)))
      
      (let* ((attr-name (second attr))
	     (bi-attr-p (builtin-attr-p attr))
	     (boalf     (and bi-attr-p (builtin-object-attr-lookup-fun attr-name))))
	
	`(let ((primary ,primary))
	   (if (py-user-object-p primary)
	       (py-attr-user-object primary ,attr-name ,bi-attr-p ,boalf)
	     ,(if boalf
		  `(funcall ,boalf primary)
		`(py-raise 'AttributeError
			   "Object ~S has no attribute ~S [builtin, no boalf]"
			   primary ',attr-name)))))
    whole))

#+(or)
(defun py-attr-user-object (x attr bi-attr-p bi-attr-fun)
  "Lookup attribute ATTR of the user-object X. Returns NIL if attribute lookup
failed; otherwise the resulting object (perhaps bound to X)."

  ;; ATTR        : a symbol
  ;; BI-ATTR-P   : t | nil | :perhaps
  ;; BI-ATTR-FUN : a function if (EQ BI-ATTR-P T), otherwise NIL
  
  (multiple-value-bind (val kind)
      (py-attr-user-object-1 x attr)
    
    (cond (val              (values val kind))
	  ((eq bi-attr-p t) (funcall bi-attr-fun x))
	  ((eq bi-attr-p :perhaps) (let ((f (builtin-object-attr-lookup-fun attr)))
				     (when f (funcall f x))))
	  (t (break :unexpected)))))



;; XXX all modules and functions are of type user-object
;; maybe user-object represents here `object with dynamic attributes/methods' ?

#+(or)
(defgeneric py-attr-user-object-1 (x attr bi-attr-p bi-attr-fun)
  (:method ((x py-module)   attr) (module.__getattribute__ x attr))
  (:method ((x py-function) attr) (function.__getattribute__ x attr))
  
  (:method ((x py-type)     attr)
	   ;; X is a class. Its metaclass determines attribute lookup
	   ;; behaviour.
	   (let ((metaclass (class-of x)))
	     (if (eq metaclass (load-time-value (find-class 'py-type)))
		 (metatype.__getattribute__ x attr)
	       
	       ;; call (metatype.__getattribute__ x attr)
	       (break :todo))))
  
  (:method ((x py-meta-type) attr)
	   ;; X is a metaclass. The metaclass of X is always class py-type
	   (metatype.__getattribute__ x attr))
	   
  (:method 
   
  (:method ((x user-object) meth)
	   (ns-lookup (etypecase x
			(py-type      (py-type-namespace x))
			(py-meta-type (py-meta-type-namespace x)))
		      meth)))

	    

	   (multiple-value-bind (res kind)
      (py-attr-get-1 x attr)
    (when res
      (ecase kind
	((nil)           res)
	(:user-value     (if (eq attr '__get__)
			     res
			   (maybe-__get__ res x)))
	(:unbound-method (make-unbound-method :class x    :method res))
	(:bound-method   (make-bound-method   :instance x :method res))
	
	#+(or)((:static-method  (make-static-method res))
	       (:class-method-got-class    (make-bound-method :instance x :method res))
	       (:class-method-got-instance (make-bound-method :instance (class-of x)
						       :method res)))))))

#+(or)
(defgeneric maybe-__get__ (obj instance class)
  ;; Maybe do obj.__get__(inst, cls)
  (:method ((obj user-object) instance class)
	   (let ((get-meth (user-object-class-method obj '__get__)))
	     (if get-meth
		 (funcall get-meth obj instance (or class (class-of obj)))
	       obj)))
  (:method (obj instance class)
	   obj))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; (py-call primary args)

(defun py-call (primary args)
  (py-call-1 primary args))

(define-compiler-macro py-call (&whole whole primary args)
  (cond ((and (listp primary)
	      (eq (car primary) 'py-attr))
	 ;; (py-call (py-attr x attr) args)
	 ;; Skip binding of attribute
	 :todo)
	
	;; more optimizations here...
	
	(t whole)))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun py-attr-call (x attr &rest args)
  "Returns NIL if attribute lookup failed; otherwise returns the values
returned by the call."
  ;; TODO: macro, so py-attr-get-1 macro can do its optimization.
  (multiple-value-bind (res kind)
      (py-attr-get-1 x attr)
    (when res
      (ecase kind
	((nil)           (apply #'funcall res args))
	(:user-value     (if (eq attr '__get__)
			     (apply #'funcall res args)
			   (apply #'funcall (maybe-__get__ res x) args)))
	(:bi-value       (apply #'funcall res args))
	(:unbound-method (unless (and args (typep (car args) x))
			   (error "unbound method called with no args or wrong first ~
                                  arg ~A ~A" args x))
			 (apply #'funcall res args))
 	(:bound-method   (apply #'funcall res x args))
	#+(or)((:static-method  (apply #'funcall res args))
	       (:class-method-got-class    (apply #'funcall res x args))
	       (:class-method-got-instance (apply #'funcall res (class-of x) args)))))))


#+(or)


(defgeneric user-object-class-method (x meth)
  todo)



(defgeneric builtin-object-attr-lookup-fun (attr)
  (:documentation "Returns a function (lambda (x) (look ATTR of built-in object X))")
  
  (:method ((attr string))
	   (let ((sym (find-symbol attr 'py-bi-attr)))
	     (if sym
		 (progn (assert (fboundp sym))
			(symbol-function sym))
	       nil)))
  
  (:method ((attr symbol))
	   (builtin-object-attr-lookup-fun (symbol-name attr))))

(defgeneric bi-attr-p (attr)
  (:documentation "Can ATTR refer to a built-in attribute?")
  (:method ((attr symbol))
	   (bi-attr-p (symbol-name attr)))
  (:method ((attr string))
	   (let ((sym (find-symbol attr 'py-bi-attr)))
	     (when sym (assert (fboundp sym)))
	     (and sym t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Setting attributes

(defgeneric py-attr-set (x attr val)
  (:documentation "Returns NIL is attribute setting failed; otherwise returns T")
  (:method ((x user-object) (attr symbol) val)
	   (py-attr-set-user-object x attr val))
  (:method (x (attr symbol) val)
	   nil))

(defun py-attr-set-user-object (x attr val)
  (check-type x user-object)
  'todo)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Deleting attributes
;; 

(defgeneric py-attr-del (x attr)
  (:documentation "Returns NIL if attribute deletion failed; otherwise T")
  (:method ((x user-object) (attr symbol) val)
	   (py-attr-del-user-object x attr))
  (:method (x (attr symbol) val)
	   nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Built-in attributes
;; 

(defclass py-attr-gf (standard-generic-function)
  ())

(defmethod no-applicable-method ((g py-bi-attr-gf) &rest args)
  "Return NIL instead of signalling an error"
  nil)


;; The following py-bi-attr:<ATTR-NAME>(x, attr) functions return one of:
;;   
;;   NIL                   -- if the lookup failed
;;   FUNC :unbound-method  -- if the result is a unbound method (bound to class of X)
;;   FUNC :bound-method    -- if the result is a bound method (bound to X)
;; 
;; The (un)bound method is not created immediately, as the caller may
;; be able to continue without creating the (un)bound method object.


;; `__getattribute__' intercepts all attribute lookups
(defgeneric py-bi-attr:__getattribute__ (x)
  (:method ((x py-object))
	   (values #'py-attr-get :bound-method))
  (:method (x)
	   (values #'py-attr-get :unbound-method))
	   ;; maybe set flag to prevent infinite recursion
  (:generic-function-class py-bi-attr-gf))


;; `__new__' is a static method of all classes
(defgeneric py-bi-attr:__new__ (x) 
  (:documentation
   "Get attribute __new__ of a built-in Python object (class or instance).")
  (:method ((x py-meta-type))
	   (assert (eq x (find-class 'py-type)))
	   #'py-type.__new__)
  (:method ((x py-type))
	   (cond ((eq x (load-time-value (find-class 'py-dict)))
		  #'py-type.__new__)
		 ((eq x (load-time-value (find-class 'py-list)))
		  #'py-list.__new__)
		 ((eq x (load-time-value (find-class 'py-string)))
		  #'py-string.__new__)))
  (:method ((x string))  #'string.__new__)
  (:method ((x py-dict)) #'py-dict.__new__)
  (:method ((x py-list)) #'py-list.__new__)
  (:generic-function-class py-bi-attr-gf))


;; `imag' is an attribute (not method) of (complex) numbers
(defgeneric py-bi-attr:imag (x)
  (:method ((x number)) (imagpart x))
  (:generic-function-class py-bi-attr-gf))


;; `popitem' is a method on dictionaries
(defgeneric py-bi-attr:popitem (x)
  (:method ((x py-dict))
	   (values #'py-dict.popitem :bound-method))
  (:method ((x (eql (find-class 'py-dict))))
	   (values #'py-dict.popitem :unbound-method))
  (:generic-function-class py-bi-attr-gf))

	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OLD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass user-defined-class (user-defined-object python-type)
  ((__name__ :initarg :name
	     :type string
	     :documentation "class name")
   (__module__ :initarg :module
	       :type string
	       :documentation "the module this class was defined in")
   (__dict__ :initarg :namespace
	     :documentation "UDC class namespace, containing its methods and class attributes"
	     :initform (make-namespace) ))
  ;; this is NOT the _instance_ dict -- UDCs are *instances* of this class
  (:documentation "Represents all Python classes created within Python (at runtime)."))


;; The reason for having the following class, is that if a UDC Foo
;; defined fixed slots for its instances, then the list of fixed slot
;; names is stored as an attribute in the class Foo itself, namely in
;; the `__slots__' slot.

(defclass user-defined-class-w/slots (user-defined-class)
  ((__slots__ :type list :initarg :slots
	      :documentation "The slots all instances have"))
  (:documentation "Represents all Python classes whose instances have fixed slots."))


;; Python code:
;;   
;;   class Foo:
;;     def meth(self, ..): ..
;; 
;; makes Foo an instance of USER-DEFINED-CLASS, with
;;  `__name__' = "Foo"
;;  `__dict__' = {"meth": <method 'meth'>}

(defclass udc-instance (user-defined-object)
  ()
  (:documentation "Mix-in for UDCs. Idea: all UDC instances are of class UDC-INSTANCE"))

(defclass udc-instance-w/dict (udc-instance)
  ((__dict__ :initarg :dict :initform (make-namespace) ;; was (make-dict) 20041226
	     ))
  (:documentation "Represents all UDC's whose instances have a __dict__."))

(defclass udc-instance-w/slots (udc-instance) ()
  (:metaclass python-type)
  (:documentation "Represents all UDC's whose instances have fixed slots. ~@
                   All UDC instances whose class is of type user-defined-class-w/slots ~@
                   should be of type udc-instance-w/slots."))

(defclass udc-instance-w/dict+slots (udc-instance-w/slots udc-instance-w/dict) ()
	  ;; note: first w/slots, then w/dict - important for attribute lookup methods
  (:metaclass python-type)
  (:documentation "Represents all UDC's whose instances have both some fixed slots and a __dict__."))

  

;; MAKE-PYTHON-CLASS (this documentation needs updating...)
;; 
;; XXX make sure inheritance order is correct when subclassing a buitin class.
;; 
;; XXX Here is an issue: in Python, there can be two classes with the
;; same name, while in CL there can't.
;; 
;; We need to keep track of that at class creation time, by using
;; different Lisp names for every user-defined class, but putting the
;; user-supplied name of it in a slot.
;; 
;; Realize that has-dict and fixed-slots are not mutually exclusive:
;; if __dict__ is explicitly mentioned in the list of slots, this
;; class' instances will have fixed slots, of which __dict__ happens
;; to be one.
;; 
;; To enable fine type dispatching, this is the order in which the
;; superclasses appear in the CPL:
;; 
;;  udc-with-dict udc-with-slots udo
;; 
;; So, if there are slots of which __dict__ is one, we want to treat
;; it as a regular instance with a __dict__, not as a slot-restricted
;; instance.
;; 
;; Defmethods restricted to types udc-with-dict and udc-with-slot will
;; successfully sort out that case.
;; 
;; User-defined-object must come before the builtin-classes (except
;; udo-w-slots and udo-w-dict), so instances of a subclassed built-in
;; class are regarded as UDO first and BIO second, in that order.
;; 
;; XXX maybe class hierarchy can be cleaned up.
;;
;; XXX behaviour different from CPython w.r.t. subclassing slotted
;; classes, and the value of __slots__.

(defun make-python-class (&key name (module "ModuleName")
			       (supers nil) (slots nil) (has-slots nil)
			       (namespace nil) (metaclass nil)
			       (documentation nil))
  (assert (symbolp name))
  (assert (every (lambda (x) (typep x 'class)) supers))
  
  (when (eq metaclass (find-class 'python-type))
    (setf metaclass nil))
  
  (cond (metaclass (warn "make-u-d-class with given metaclass: ~A" metaclass)
		   (assert (typep metaclass 'class))
		   #+(or)(unless (subtypep metaclass 'python-type)
			   (py-raise 'TypeError
				     "Metaclass for class ~A is ~A, which is not a subtype of `type'."
				     name (class-name metaclass)))
		   (when has-slots
		     #+(or)(warn "Class ~A has __metaclass__ specified. Therefore, the value for ~@
                            __slots__ is ignored (value of __slots__: ~A)"
			   name (call-attribute-via-class slots '__repr__)))
		   (return-from make-python-class
		     (make-u-d-class-with-given-metaclass name metaclass
							  :namespace namespace
							  :module module
							  :supers supers)))
	
	
	((some (lambda (cls) (subtypep cls 'python-type))
	       supers)
	 (return-from make-python-class
	   (make-u-d-class-derived-from-type name supers :namespace namespace :module module))))
  
  
  ;; All classes that are accessible from within Python
  ;; inherit from python-object, so throw it away when it is
  ;; explicitly mentioned.
  ;; 
  ;; TODO: this hides some circularity errors in CPL.
  
  (setf supers
    (loop for s in supers
	do (assert (typep s 'class))
	unless (eq s (find-class 'python-object))
	collect s))

  ;; Determine the slots the instances of this class are supposed to have.
  ;; 
  ;; This happens in multiple steps, as the logic is a bit tricky.
  ;; 
  ;; First step: analyze the class options supplied. If no explicit
  ;; `slots' specified, give instances a __dict__ for all attributes.
  ;; 
  ;; If __slots__ is...      then effectively...
  ;;  ()                 ->   slots=[],     (no dict) -- effectively no attribs at all
  ;;  ('__dict__')       ->   slots=['__dict__']      -- same as if slots were not specified
  ;;  ('__dict__', 'a')  ->   slots=['__dict__', 'a'] -- allocate `a' in special slot, rest in dict
  ;;  ('a')              ->   slots=['a']   (no dict) -- only allow `a', which has its own slot
  ;;  <absent>           ->   slots=['__dict__']      -- everything in dict

  (let* ((inst-have-dict (or (not has-slots)
			     (and has-slots (member '__dict__ slots))))
	 (the-other-slots (loop
			      with lst = slots
			      for s in '(__dict__ __slots__ __name__)
			      do (setf lst (remove s lst :test 'eq))
			      finally (return lst)))
	 (inst-have-other-slots the-other-slots)
	 
	 (supers-cls supers)) ;; XXX clean up

    ;; Determine the appropriate mix-in class. This can't be more
    ;; restricted than any of the UDC superclasses.
    ;;  
    ;;  udc-instance               -- no dict, no slots, nothing
    ;;  udc-instance-w/dict        -- just a dict, no other fixed slots
    ;;  udc-instance-w/slots       -- no dict, but some other slots
    ;;  udc-instance-w/dict+slots  -- both a dict and other slots

    (loop for cls in supers-cls
	if (subtypep cls 'udc-instance-w/dict)
	do (setf inst-have-dict t)
	if (subtypep cls 'udc-instance-w/slots)
	do (setf inst-have-other-slots t))
    
    (let ((the-mixin (cond ((and inst-have-other-slots inst-have-dict) 'udc-instance-w/dict+slots)
			   (inst-have-dict 'udc-instance-w/dict)
			   (inst-have-other-slots 'udc-instance-w/slots)
			   (t 'udc-instance)))
	  
	  (the-metaclass (progn (assert (not metaclass)) ;; no __metaclas__ given by user
				(if inst-have-other-slots 
				    'user-defined-class-w/slots
				  'user-defined-class))))
	     
      ;; Now determine the superclasses. 
      ;; 
      ;; The order of superclasses is:
      ;;  1) all classes mentioned in SUPERS
      ;;  2) the UDC-* mixin (determined above)
      ;;  3) if there are built-in classes in SUPERS: BIO.
      ;; 
      ;; We always want BIO af the end, so that when built-in classes
      ;; are subclassed, a method specialized on UDC-I takes
      ;; precedence above one specialized on BIO.
      ;; 
      ;; Here are some examples of the resulting class precedence
      ;; lists:
      ;; 
      ;;                CPL (after Foo)           meta
      ;;  ------------------------------------------------------------
      ;;  Foo           udc-i-w/d                 udc
      ;;  Foo(Sup)      Sup, udc-i-w/d            udc
      ;;  Foo(int)      udc-i-w/d, int, bio       udc <- note: bio at the end, after udc-*
      ;;  Foo(Bar,int)  Bar, udc-i-w/d, int, bio  udc
      ;;  Foo(int,Bar)  int, Bar, udc-i-w/d, bio  udc <- note: bio at the end, after udc-*
      ;;
      ;; udo = user-defined-object
      ;; udc = user-defined-class
      ;; udc/slots = user-defined-class-w/slots
      ;; bio  = built-in-object
      ;; udc-i-w/d = udc-instance-w/dict
      ;; 
      ;; [ When class has list of slots, meta becomes udc/slots, and in
      ;; [ CPL replace udc-i-w/d with udc-i-w/s.
      ;; [ When class has both slots and dict, meta also becomes
      ;; [ udc/slots, in CPL replace udc-i-w/d with udc-i-w/d+s.

      (let* ((has-a-builtin-super (some (lambda (c) (typep c 'builtin-class))
					supers-cls))
	     (the-supers `(,@supers ,the-mixin ,@(when has-a-builtin-super
						   '(builtin-instance builtin-object))))
	     
	     (dummy #+(or)(warn "Creating ~A: supers = ~A  meta = ~A" name the-supers the-metaclass))
	     
	     ;; Finally, create the class
	     (k (mop:ensure-class
		 name
		 :direct-superclasses the-supers
		 :metaclass the-metaclass
		 :documentation documentation
		 :direct-slots
		 `( ,@(mapcar
		       (lambda (slot-name)
			 (if (eq slot-name '__dict__)
			     `(:name ,slot-name :iniform '(make-namespace))
			   `(:name ,slot-name)))
		       the-other-slots) ))))
	(declare (ignore dummy))
	
	(mop:finalize-inheritance k) ;; Not sure if this is needed?

	;; Some bookkeeping
	
	(let ((namespace
	       (or namespace
		   (make-namespace
		    :name (format nil "ns for class ~A, created by default"
				  (string name))))))

	  (setf (slot-value k '__name__) (string name)
		(slot-value k '__module__) module
		(slot-value k '__dict__) namespace)
	  
	  (when the-other-slots
	    (setf (slot-value k '__slots__) the-other-slots))
	  
	  k)))))

(defclass udc-derived-from-type ()
  ())
(mop:finalize-inheritance (find-class 'udc-derived-from-type))

(defmethod make-u-d-class-derived-from-type ((name symbol) (supers list)
					     &key module namespace)
  (let* ((real-supers (remove (find-class 'user-defined-class)
			      (remove (find-class 'python-type)
				      supers)))
	 (klass (mop:ensure-class
		 name
		 :direct-superclasses `(,@real-supers
					user-defined-class
					udc-derived-from-type)
		 :metaclass 'user-defined-class)))
    (mop:finalize-inheritance klass)
    (setf (slot-value klass '__dict__) namespace
	  (slot-value klass '__name__) (symbol-name name)
	  (slot-value klass '__module__) module)
    klass))

(defmethod make-u-d-class-with-given-metaclass
    ((name symbol) (metaclass class) &key supers module namespace)
  (let ((klass
	 (mop:ensure-class
	  name
	  :direct-superclasses `(,@(remove (find-class 'python-object) supers)
				   #+(or)user-defined-class
				   udc-instance-w/dict python-object) ;; udc?
	  :metaclass #+(or)metaclass
	  (metaclass-for-udc-with-ud-metaclass metaclass)
	  :direct-slots '((:name __dict__)))))
    (mop:finalize-inheritance klass)
    (setf (slot-value klass '__dict__) namespace
	  (slot-value klass '__name__) name
	  (slot-value klass '__module__) module)
    klass)) ;; XXX user-defined-object mixin?


(defclass udc-with-ud-metaclass (#+(or)user-defined-class udc-instance-w/dict)
  ()) ;; metatype?

(mop:finalize-inheritance (find-class 'udc-with-ud-metaclass))
		  

(defmethod metaclass-for-udc-with-ud-metaclass ((mc class))
  (let* ((name (intern
		(format nil "udc-ud-mc+~A" (class-name mc)) #.*package*))
	 (k (mop:ensure-class
	     name
	     :direct-superclasses (list mc 'udc-with-ud-metaclass 
					#+(or)'standard-class))))
    (mop:finalize-inheritance k)
    k))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New design of user-defined classes

(defclass py-udc ()
  ((name :initarg :name)
   (metaclass :initarg :metaclass :initform nil)
   (baseclasses :initarg :baseclasses :initform nil)
   (subclasses :initarg :subclasses :initform nil)
   (slots :initarg :slots :initform nil)
   (has-slots :initarg :has-slots)
   (namespace :initarg :namespace)
   (mro :initarg :mro)
   (subtype-of-type :initarg :subtype-of-type :initform nil)))

(defclass py-udc-inst ()
  ((class :initarg :class)
   (attributes))) ;; alist


(defun make-udc (&rest args)
  (apply #'make-instance (find-class 'py-udc) args))

(defmethod make-udc-inst ((cls py-udc))
  (make-instance cls))
