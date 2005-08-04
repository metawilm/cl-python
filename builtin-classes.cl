(in-package :python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class frameword:  py-meta-type, py-type, py-object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass py-dict-mixin ()
  ((dict :initarg :dict :initform (make-hash-table :test #'eq) :accessor dict)))

(defmethod dict ((x symbol)) ;; (dict <symbol>) is designator for (dict <class>)
  (dict (find-class x)))

(defun dict-get (x key)
  (gethash (dict x) key))

(defun dict-del (x key)
  (remhash (dict x) key))

(defun (setf dict-get) (new-val x key)
  (setf (gethash (dict x) key) new-val))


(defclass py-class-mixin (py-dict-mixin)
  ((mro  :initarg :mro  :accessor class-mro)))

;; Python metatype. Class `type' and subclasses thereof are instances
;; of py-meta-type.

(defclass py-meta-type (py-class-mixin standard-class)
  ())

(defmethod make-instance
    ((cls (eql (find-class 'py-meta-type))) &rest initargs)
  ;; Create a new Python metatype, i.e. subclass of `type'
  ...)

;; A class for Python classes. This is an instance of py-meta-type

(defclass py-type (py-class-mixin standard-class)
  ()
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

;; Python classes are instances of (a subclass of) python-type

(defclass py-object (py-dict-mixin standard-object)
  ()
  (:metaclass py-type))

(defmethod make-instance ((cls py-type) &rest initargs)
  ;; Create an instance of a Python class.
  ...)

(defmethod initialize-instance ((x py-object) &rest initargs)
  ;; Initialize a simple instance of a Python class.
  ...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Built-in Python object types:
;; 
;; -- type ----------- repr ----- subclassable -- examples -------------------
;;  py-lisp-object:  lisp value    yes       number, string, tuples, vectors, dict
;;  py-core-object:  py cls inst   no        function, method
;;  py-user-object:  py cls inst   yes       file, module, property
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Support methods on built-in Python classes

(defmacro def-py-method (cls meth &rest args)
  (let ((modifiers (loop while (keywordp (car args)) collect (pop args))))
    (destructuring-bind (args &body body) args
      `(def-py-method-1 ',cls ',meth ',modifiers (lambda ,args ,@body)))))

(defun def-py-method-1 (cls meth modifiers lambda)
  (assert (find-class cls))
  (assert (dict cls))
  (setf (dict-get cls meth)
    (ecase modifiers
      ((nil)               lambda)
      (:static-method      (make-static-method lambda))
      (:instance-attribute (make-attribute-method lambda)))))


;;; Attributes are a fundamental thing: getting, setting, deleting

(defgeneric py-attr (x attr)
  (:documentation "Get attribute ATTR of X"))

(defgeneric (setf py-attr) (val x attr)
  (:documentation "Set attribute ATTR of X to VAL"))

(defgeneric py-del-attr (x attr)
  (:documentation "Remove attribute ATTR of X"))


;; By default, when an object has a dict, attributes are looked up in
;; the dict.

(defmethod py-attr ((x py-dict-mixin) attr)
  (dict-get x attr))

(defmethod (setf py-attr) (val (x py-dict-mixin) attr)
  (setf (dict-get x attr) val))

(defmethod py-del-attr ((x py-dict-mixin) attr)
  (dict-del x attr))


;;; Core objects (function, method; not subclassable by the user)

(defclass py-core-object (py-object) ())
(defclass py-core-type   (py-type)   ())

;; Function

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

;; Method

(defclass py-method (py-core-object)
  ((func :initarg :func :accessor py-method-func))
  (:metaclass py-core-type))

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


;;; User objects

(defclass py-user-type   (py-type) ())

(defclass py-user-object (py-object)
  ()
  (:metaclass py-user-type))

;; Module

(defclass py-module (py-user-object py-dict-mixin)
  ()
  (:metaclass py-user-type))

(def-py-method py-module __new__ :static-method (cls x name)
	       (let ((x (make-instance cls)))
		 (setf (dict-get x '__name__) name)
		 x))
(def-py-method py-module __init__ (&rest args)
	       (declare (ignore args))
	       nil)

;; File

(defclass py-file (py-user-object py-dict-mixin)
  ((file-handle :initform nil))
  (:metaclass py-user-type))

(def-py-method py-file __new__ :static-method (x filename)
	       (setf (file-handle x) (open-lisp-file-todo filename)))

(def-py-method py-file __init__ (&rest args)
	       (declare (ignore args))
	       nil)

;; Property 

(defclass py-property (py-user-object)
  ()
  (:metaclass py-user-type))


;;; Lisp objects (proxies around Lisp values)

(defclass py-lisp-type (py-type)
  ()
  (:documentation "Metaclass for proxy classes"))

(defclass py-lisp-object (py-object)
  ((lisp-object :accessor lisp-proxy-val))
  (:metaclass py-lisp-type)
  (:documentation "Base class for proxy classes"))


(defgeneric maybe-proxy-lisp-val (x)
  (:method ((x py-lisp-object))  (lisp-proxy-val x))
  (:method ((x t))               x))

(defmethod py-attr ((x py-lisp-object) attr)
  (py-attr (proxy-lisp-val x) attr))

(defmethod (setf py-attr) (val (x py-lisp-object) attr)
  (setf (py-attr (proxy-lisp-val x) attr) val))


(defmacro def-proxy-class (py-name &optional supers)
  `(defclass ,py-name ,(or supers '(py-lisp-object))
     ()
     (:metaclass py-lisp-type)))

;; Number

(def-py-proxy-class py-number)

;; Complex

(def-py-proxy-class py-complex (py-number))

;; Real

(def-py-proxy-class py-real (py-number))

;; Integer

(def-py-proxy-class py-int (py-real))

(def-py-method py-int __new__ :static-method (cls &optional (arg 0))
	       (method-assert-subclass cls 'py-int py-int.__new__)
	       (if (eq cls (find-class 'py-int))
		   arg
		 (let ((i (make-instance cls)))
		   (setf (proxy-lisp-val i) arg)
		   i)))

(def-py-method py-int __init__ (&rest args) nil)

(def-py-method py-int __str__ (x) (format nil "~A" (maybe-proxy-lisp-val x)))

;; Float

(def-py-proxy-class py-float (py-real))


;; List

(def-py-proxy-class py-list)

(def-py-method py-list __new__ :static-method (cls)
	       (if (eq cls (find-class 'py-list))
		   (make-array 0 :adjustable t :fill-pointer 0)
		 (make-instance cls)))
		    
(def-py-method py-list __init__ (x &optional iterable)
	       (let ((x (maybe-proxy-lisp-val x)))
		 (when iterable
		   (loop for item in (py-iterate->lisp-list iterable)
		       do (vector-push-extend item x)))))

(def-py-method py-list __str__ (x)
	       (let ((x (maybe-proxy-lisp-val x)))
		 (format t "[~{~A~^, ~}]" (loop for i across x collect i))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(defgeneric py-lisp-object-p (x)
  (:method ((x number)) t)
  (:method ((x string)) t)
  (:method ((x vector)) t)
  (:method ((x list))   t)
  (:method ((x t))    nil))

(defgeneric py-core-object-p (x)
  (:method ((x py-module))   t)
  (:method ((x py-function)) t)
  (:method ((x py-property)) t)
  (:method ((x py-super))    t)
  (:method ((x py-xrange))   t)
  (:method ((x t))         nil))   ;; not: py-file

(defmethod py-user-object-p (x)
  (and (not (py-lisp-object-p x))
       (not (py-core-object-p x))))

(defgeneric py-class-of (x)
  (:method ((x integer)) (load-time-value (find-class 'py-int    )))
  (:method ((x real))    (load-time-value (find-class 'py-float  )))
  (:method ((x complex)) (load-time-value (find-class 'py-complex)))
  (:method ((x string))  (load-time-value (find-class 'py-string )))
  (:method ((x vector))  (load-time-value (find-class 'py-list   )))
  (:method ((x list))    (load-time-value (find-class 'py-tuple  )))
  (:method ((x class))   (if (eq x (load-time-value (find-class 'py-type)))
			     x  ;; py-type is its own class
			   (class-of x)))
  (:method ((x t))       (class-of x)))


;; XXX

(defmethod py-attr (x attr)
  (let* ((cls (py-class-of x))
	 (ga  (py-attr cls '__getattribute__)))
    (assert ga)
    (py-call ga x attr)))

