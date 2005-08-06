(in-package :python)

;;;; Python classes and metaclasses; the built-in classes including
;;;; their methods.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Class frameword:  py-meta-type, py-type, py-object

(defclass py-dict-mixin ()
  ((dict :initarg :dict :initform (make-hash-table :test #'eq) :accessor dict)))

(defmethod dict ((x symbol)) ;; (dict <symbol>) is designator for (dict <class>)
  (dict (find-class x)))
(defmethod dict ((x t))
  nil)

(defun dict-get (x key)
  (gethash key (dict x)))

(defun dict-del (x key)
  (remhash (dict x) key))

(defun (setf dict-get) (new-val x key)
  (setf (gethash key (dict x)) new-val))


(defclass py-class-mixin (py-dict-mixin)
  ((mro :initarg :mro :accessor class-mro)))


;; Python metatype. Class `type' and subclasses thereof are instances
;; of py-meta-type.

(defclass py-meta-type (py-class-mixin standard-class)
  ())

#+(or)
(defmethod make-instance
    ((cls (eql (find-class 'py-meta-type))) &rest initargs)
  ;; Create a new Python metatype, i.e. subclass of `type'
  TODO)


;; A class for Python classes. This is an instance of py-meta-type

(defclass py-type (py-class-mixin standard-class)
  ()
  (:metaclass py-meta-type))

#+(or)
(defmethod make-instance ((cls py-meta-type) &rest initargs)
  ;; Create a new Python class that has CLS as metaclass
  TODO)

#+(or)
(defmethod compute-class-precedence-list ((cls py-meta-type))
  ;; Compute CPL of new Python class
  TODO)

#+(or)
(defmethod allocate-instance ((cls py-type) &rest initargs) 
  ;; __new__
  TODO)

#+(or)
(defmethod initialize-instance ((cls py-type) &rest initargs)
  ;; Initialize a Python class
  ;;  - call __init__ method
  TODO)


;; Python classes are instances of (a subclass of) python-type

(defclass py-object (py-dict-mixin standard-object)
  ()
  (:metaclass py-type))

#+(or)
(defmethod make-instance ((cls py-type) &rest initargs)
  ;; Create an instance of a Python class.
  TODO)

#+(or)
(defmethod initialize-instance ((x py-object) &rest initargs)
  ;; Initialize a simple instance of a Python class.
  TODO)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Built-in Python object types:
;;; 
;;; -- type ----------- repr ----- subclassable -- examples -------------------
;;;  py-lisp-object:  lisp value    yes       number, string, tuples, vectors, dict
;;;  py-core-object:  py cls inst   no        function, method
;;;  py-user-object:  py cls inst   yes       file, module, property
;;;

;;; Support methods on built-in Python classes

(defmacro def-py-method (cls.meth &rest args)
  (let* ((cm (symbol-name cls.meth))
	 (dot-pos (or (position #\. cm)
		      (error "Need dot in name: (def-py-method classname.methodname ..)")))
	 (cls  (intern (subseq cm 0 dot-pos) #.*package*))
	 (meth (intern (subseq cm (1+ dot-pos)) #.*package*))
	 (modifiers (loop while (keywordp (car args)) collect (pop args))))

    (assert (<= (length modifiers) 1) () "Multiple modifiers for a py-method: bug?")
    
    (destructuring-bind (args &body body) args
      `(progn (defun ,cls.meth ,args 
		,@body)
	      (let* ((cls (or (find-class ',cls) (error "No such class: ~A" ',cls))))
		(unless (dict cls)
		  (error "Class ~A has no dict" cls))
		(setf (dict-get cls ',meth)
		  ,(ecase (car modifiers)
		     ((nil)               `(function ,cls.meth))
		     (:static-method      `(make-instance 'py-static-method
					     :func (function ,cls.meth))))))))))

;;; Attributes are a fundamental thing: getting, setting, deleting

(defgeneric py-attr (x attr)
  (:documentation "Get attribute ATTR of X"))

(defgeneric (setf py-attr) (val x attr)
  (:documentation "Set attribute ATTR of X to VAL"))

(defgeneric py-del-attr (x attr)
  (:documentation "Remove attribute ATTR of X"))


;; By default, when an object has a dict, attributes are looked up in
;; the dict.

#+(or);; attributs classes should go via metatype, not directly like this
(defmethod py-attr ((x py-dict-mixin) attr)
  (if (typep x 'class)
      (py-recursive-class-dict-lookup x attr)
    (dict-get x attr)))

#+(or)
(defmethod (setf py-attr) (val (x py-dict-mixin) attr)
  (setf (dict-get x attr) val))

#+(or)
(defmethod py-del-attr ((x py-dict-mixin) attr)
  (dict-del x attr))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Core objects (function, method, None...; not subclassable by the user)

(defclass py-core-object (py-object) ())
(defclass py-core-type   (py-type)   ())

;; Function (Core object)

(defclass funcallable-python-class (mop:funcallable-standard-class py-core-type)
  ;; When subclassable python classes also get such a metatype,
  ;; s/py-core-type/py-type/
  ())

(defclass py-function (standard-generic-function py-core-object py-dict-mixin)
  () ;; mop:funcallable-standard-class defines :name initarg
  (:metaclass funcallable-python-class))

(defmethod make-py-function ((name symbol) (f function))
  (let ((x (make-instance 'py-function :name name)))
    (mop:set-funcallable-instance-function x f)
    ;; fill dict?
    x))

(def-py-method py-function.__get__ (func obj class)
  (if (none-p obj)
      (make-instance 'unbound-method :func func :class class)
    (make-instance 'bound-method :func func :instance obj)))

;; Method (Core object)

(defclass py-method (py-core-object)
  ((func :initarg :func :accessor py-method-func))
  (:metaclass py-core-type))

(def-py-method py-method.__get__ (func obj class)
  (declare (ignore obj class))
  func)

(defclass py-bound-method (py-method)
  ((instance :initarg :instance :accessor py-method-instance))
  (:metaclass py-core-type))

(defclass py-unbound-method (py-method)
  ((class :initarg :class :accessor py-method-class))
  (:metaclass py-core-type))

(defclass py-static-method (py-method)
  ()
  (:metaclass py-core-type))

(defclass py-class-method (py-method)
  ((class :initarg :class))
  (:metaclass py-core-type))


(defclass py-none (py-core-object)
  ()
  (:metaclass py-core-type))

(defvar *the-none-value* (make-instance 'py-none))

(defun none-p (x)
  (eq x *the-none-value*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; User objects (Object, Module, File, Property)

(defclass py-user-type   (py-type) ())

(defclass py-user-object (py-object)
  ()
  (:metaclass py-user-type))


;; Object (User object)

(def-py-method py-object.__getattribute__ (x attr)
  (let ((class-attr-val   nil)
	(__getattr__      nil)
	(x.class          (py-class-of x)))

    (loop for c in (mop:class-precedence-list x.class)
	until (or (eq c (load-time-value (find-class 'standard-class)))
		  (eq c (load-time-value (find-class 'py-dict-mixin))))
	      
	for c.dict = (dict c)
		     
	do (let ((getattribute-meth (gethash '__getattribute__ c.dict)))
	     (when (and getattribute-meth
			(not (eq getattribute-meth #'py-object.__getattribute__)))
	       (return-from py-object.__getattribute__
		 (py-call getattribute-meth x attr))))
	   
	unless class-attr-val
	do (let ((val (gethash attr c.dict)))
	     (when val 
	       (setf class-attr-val val)))
	   
	unless (or class-attr-val __getattr__)
	do (let ((getattr-meth (gethash '__getattr__ c.dict)))
	     (when getattr-meth (setf __getattr__ getattr-meth))))

    (flet ((bind-val (val)
	     (let ((get-meth (py-recursive-class-dict-lookup
			      (py-class-of val) '__get__)))
	       (py-call get-meth x x.class))))

      ;; Arriving here means: no __getattribute__, but perhaps
      ;; __getattr__ or class-attr-val.
      
      ;; A class attribute that is a data descriptor (i.e. has a
      ;; `__set__' attribute) has higher priority than an instance
      ;; attribute.
      
      (return-from py-object.__getattribute__
	(cond ((and class-attr-val (data-descriptor-p class-attr-val))
	       (bind-val class-attr-val))
	      
	      ;; Only now is an instance attribute value relevant.
	      ((dict x)
	       (dict-get x attr))
	      
	      ;; Fall back to a class attribute that is not a `data descriptor'.
	      (class-attr-val
	       (bind-val class-attr-val))
	      
	      ;; Fall back to the __getattr__ hook.
	      (__getattr__
	       (py-call (bind-val __getattr__) (symbol-name attr)))
	      
	      ;; Give up.
	      (t (error "No such attribute: ~A . ~A" x attr)))))))

(defmethod py-recursive-class-dict-lookup ((cls class) attr)
  ;; Look for ATTR in class CLS and all its superclasses.
  ;; and finally (which is in this implementation not a superclass of a class).
  (loop for c in (mop:class-precedence-list cls)
      until (or (eq c (load-time-value (find-class 'standard-class)))
		(eq c (load-time-value (find-class 'py-dict-mixin))))
	    ;; XXX standard-class is after py-dict-mixin etc
      when (dict-get c attr) return it
      finally (return nil)
	      
	      #+(or) ;; this seems not needed, in the finally clause
	      (let ((obj-attr (dict-get 'py-object attr)))
		(when obj-attr
		  (warn "rec van py-object: ~A" attr)
		  (return obj-attr)))))


;; Type (User object)

(def-py-method py-type.__mro__ (x)
	       (mop:class-precedence-list x))

;; XXX should default to `object'

;; Module (User object)

(defclass py-module (py-user-object py-dict-mixin)
  ()
  (:metaclass py-user-type))

(def-py-method py-module.__new__ :static-method (cls name)
	       (let ((x (make-instance cls)))
		 (setf (dict-get x '__name__) name)
		 x))
(def-py-method py-module.__init__ (&rest args)
	       (declare (ignore args))
	       nil)


;; File (User object)

(defclass py-file (py-user-object py-dict-mixin)
  ((file-handle :initform nil :accessor py-file-handle))
  (:metaclass py-user-type))

(def-py-method py-file.__new__ :static-method (x filename)
	       (setf (py-file-handle x) (open-lisp-file-todo filename)))

(def-py-method py-file.__init__ (&rest args)
	       (declare (ignore args))
	       nil)


;; Property (User object)

(defclass py-property (py-user-object)
  ()
  (:metaclass py-user-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Lisp objects (proxies around Lisp values: number, string, list, tuple, dict)

(defclass py-lisp-type (py-type)
  ()
  (:documentation "Metaclass for proxy classes"))

(defclass py-lisp-object (py-object)
  ((lisp-object :accessor proxy-lisp-val))
  (:metaclass py-lisp-type)
  (:documentation "Base class for proxy classes"))


(defgeneric maybe-proxy-lisp-val (x)
  (:method ((x py-lisp-object))  (proxy-lisp-val x))
  (:method ((x t))               x))

(defmethod py-attr ((x py-lisp-object) attr)
  (py-attr (proxy-lisp-val x) attr))

(defmethod (setf py-attr) (val (x py-lisp-object) attr)
  (setf (py-attr (proxy-lisp-val x) attr) val))

;;(eval-when (:compile-toplevel :load-toplevele :execute)
(defmacro def-proxy-class (py-name &optional supers)
  `(progn (defclass ,py-name ,(or supers '(py-lisp-object))
	    ()
	    (:metaclass py-lisp-type))
	  (mop:finalize-inheritance (find-class ',py-name))))
;()

;; Number (Lisp object: number)

(def-proxy-class py-number)

(def-py-method py-number.__eq__ (x y)
	       (= (maybe-proxy-lisp-val x) (maybe-proxy-lisp-val y)))

;; Complex

(def-proxy-class py-complex (py-number))

;; Real

(def-proxy-class py-real (py-number))

;; Integer

(def-proxy-class py-int (py-real))

(def-py-method py-int.__new__ :static-method (cls &optional (arg 0))
	       (if (eq cls (find-class 'py-int))
		   arg
		 (let ((i (make-instance cls)))
		   (setf (proxy-lisp-val i) arg)
		   i)))

(def-py-method py-int.__init__ (&rest args) nil)

(def-py-method py-int.__str__ (x) (format nil "~A" (maybe-proxy-lisp-val x)))

;; Float

(def-proxy-class py-float (py-real))


;; List (Lisp object: adjustable array)

(def-proxy-class py-list)

(def-py-method py-list.__new__ :static-method (cls)
	       (if (eq cls (find-class 'py-list))
		   (make-array 0 :adjustable t :fill-pointer 0)
		 (make-instance cls)))
		    
(def-py-method py-list.__init__ (x &optional iterable)
	       (let ((x (maybe-proxy-lisp-val x)))
		 (when iterable
		   (loop for item in (py-iterate->lisp-list iterable)
		       do (vector-push-extend item x)))))

(def-py-method py-list.__str__ (x)
	       (let ((x (maybe-proxy-lisp-val x)))
		 (format t "[~{~A~^, ~}]" (loop for i across x collect i))))


;; Tuple (Lisp object: consed list)

(def-proxy-class py-tuple)

(def-py-method py-tuple.__new__ :static-method (cls &rest args)
	       (if (eq cls (find-class 'py-tuple))
		   args
		 (make-instance cls)))

(defvar *the-empty-tuple* (make-instance 'py-tuple))


;; String (Lisp object: string)

(def-proxy-class py-string)

(def-py-method py-string.__len__ (x)
	       (let ((x (maybe-proxy-lisp-val x)))
		 (length x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(defgeneric py-class-of (x)
    (:method ((x integer)) (load-time-value (find-class 'py-int    )))
    (:method ((x real))    (load-time-value (find-class 'py-float  )))
    (:method ((x complex)) (load-time-value (find-class 'py-complex)))
    (:method ((x string))  (load-time-value (find-class 'py-string )))
    (:method ((x vector))  (load-time-value (find-class 'py-list   )))
    (:method ((x list))    (load-time-value (find-class 'py-tuple  )))
    
    (:method ((x py-lisp-type)) (find-class 'py-type))
    (:method ((x py-core-type)) (find-class 'py-type))
    (:method ((x py-user-type)) (find-class 'py-type))
    
    (:method ((x class))   (if (eq x (load-time-value (find-class 'py-type)))
			       x  ;; py-type is its own class
			     (class-of x)))
    (:method ((x t))       (class-of x)))

(defmethod py-attr (x attr)
  
  ;; x.a => x.__getattribute'__(x, attr).__get'__(x)
  ;; 
  ;; where the accent after xxx in __xxx'__ means that the method from
  ;; the class is used, not an instance attribute (if it would exist)
  
  (let* ((ga-meth  (py-recursive-class-dict-lookup (py-class-of x) '__getattribute__)))
    (py-call ga x attr)))



(defgeneric py-call (f &rest args)
  (:method ((f function) &rest args) (apply f args)))
