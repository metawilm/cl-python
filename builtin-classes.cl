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
  (let ((d (dict x)))
    (assert d () "dict-get: object ~A has no dict (key ~A)" x key)
    (gethash key (dict x))))

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

    (assert (<= (length modifiers) 1) ()
      "Multiple modifiers for a py-method: ~A. Todo?" modifiers)

    `(progn
       ,(destructuring-bind (func-args &body func-body) args
	  (loop with real-args
	      with body = `(locally ,@func-body) ;; allows DECLARE forms at head

	      for sym in func-args
	      for sym-name = (when (symbolp sym) (symbol-name sym))

	      if (not (symbolp sym))
	      do (push sym real-args)
		 
	      else if (char= #\^ (aref sym-name (1- (length sym-name))))
	      do (let ((real-name (intern (subseq sym-name 0 (1- (length sym-name)))
					  #.*package*)))
		   (push real-name real-args)
		   (setf body `(let ((,real-name (deproxy ,real-name)))
				 ,body)))
		 
	      else do (push sym real-args)
	      finally (return `(defun ,cls.meth ,(nreverse real-args)
				 ,body))))
       
       (let* ((cls (or (find-class ',cls) (error "No such class: ~A" ',cls))))
	 (unless (dict cls)
	   (error "Class ~A has no dict" cls))
	 (setf (dict-get cls ',meth)
	   ,(ecase (car modifiers)
	      ((nil)               `(function ,cls.meth))
	      (:static             `(make-instance 'py-static-method
				      :func (function ,cls.meth)))
	      (:attribute          `(make-instance 'py-attribute-method
				      :func (function ,cls.meth)))))))))

(defconstant *the-true* 1)
(defconstant *the-false* 0)

(defun py-bool (lisp-val)
  (if lisp-val *the-true* *the-false*))

(define-compiler-macro py-bool (lisp-val)
  `(if ,lisp-val *the-true* *the-false*))

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
      (recursive-class-dict-lookup x attr)
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

(defclass py-attribute-method (py-method)
  ()
  (:metaclass py-core-type))

(def-py-method py-attribute-method.__get__ (x inst class)
  (declare (ignore class))
  (py-call (slot-value x 'func) inst))

(def-py-method py-attribute-method.__set__ (x inst class)
  (declare (ignore class))
  (error "Attribute ~A of object ~A is read-only (value: ~A)"
	 x inst (py-call (slot-value x 'func) inst)))
	 



;; None

(defclass py-none (py-core-object) () (:metaclass py-core-type))
(defvar *the-none* (make-instance 'py-none))
(defun none-p (x) (eq x *the-none*))

;; Ellipsis

(defclass py-ellipsis (py-core-type) () (:metaclass py-core-type))
(defvar *the-ellipsis* (make-instance 'py-ellipsis))

;; NotImlemented
(defclass py-notimplemented (py-core-type) () (:metaclass py-core-type))
(defvar *the-notimplemented* (make-instance 'py-notimplemented))


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
		  (eq c (load-time-value (find-class 'py-dict-mixin)))
		  (eq c (load-time-value (find-class 'py-class-mixin))))
	      
	for c.dict = (dict c) ;; may be NIL
		     
	when c.dict
	do (let ((getattribute-meth (gethash '__getattribute__ c.dict)))
	     (when (and getattribute-meth
			(not (eq getattribute-meth #'py-object.__getattribute__)))
	       (return-from py-object.__getattribute__
		 (py-call getattribute-meth x attr))))
	   
	   (unless class-attr-val
	     (let ((val (gethash attr c.dict)))
	       (when val 
		 (setf class-attr-val val))))
	   
	   (unless (or class-attr-val __getattr__)
	     (let ((getattr-meth (gethash '__getattr__ c.dict)))
	       (when getattr-meth (setf __getattr__ getattr-meth)))))

    (flet ((bind-val (val)
	     (warn "bind-val ~A to ~A" val x)
	     (let ((get-meth (recursive-class-dict-lookup
			      (py-class-of val) '__get__)))
	       (if get-meth
		   (py-call get-meth val x x.class)
		 val))))

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

(defmethod recursive-class-dict-lookup ((cls class) attr)
  ;; Look for ATTR in class CLS and all its superclasses.
  ;; and finally (which is in this implementation not a superclass of a class).
  (loop for c in (mop:class-precedence-list cls)
      until (or (eq c (load-time-value (find-class 'standard-class)))
		(eq c (load-time-value (find-class 'py-dict-mixin))))
	    ;; XXX standard-class is after py-dict-mixin etc
      when (and (dict c) (dict-get c attr)) return it
      finally #+(or)(return nil)
	      
	      ;; this seems not needed, in the finally clause
	      (let ((obj-attr (dict-get 'py-object attr)))
		(when obj-attr
		  (warn "rec van py-object: ~A" attr)
		  (return obj-attr)))))


(def-py-method py-object.__get__ (value instance class)
  (declare (ignore instance class))
  value)


;; Type (User object)

(def-py-method py-type.__mro__ :attribute (x)
	       (mop:class-precedence-list x))

;; XXX should default to `object'

;; Module (User object)

(defclass py-module (py-user-object) ;; no dict-mixin!?
  ((globals-names  :initarg :globals-names  :type vector :initform #())
   (globals-values :initarg :globals-values :type vector :initform #())
   (dyn-globals    :initarg :dyn-globals    :type hash-table)
   (name           :initarg :name           :type symbol :initform "__main__"))
  (:metaclass py-user-type))

(defun make-module (&rest options)
  (apply #'make-instance 'py-module options))

(def-py-method py-module.__new__ :static (cls name)
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

(def-py-method py-file.__new__ :static (x filename)
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


(defgeneric deproxy (x)
  (:method ((x py-lisp-object))  (proxy-lisp-val x))
  (:method ((x t))               x))

(defmethod py-attr ((x py-lisp-object) attr)
  (py-attr (proxy-lisp-val x) attr))

(defmethod (setf py-attr) (val (x py-lisp-object) attr)
  (setf (py-attr (proxy-lisp-val x) attr) val))

(defmacro def-proxy-class (py-name &optional supers)
  `(progn (defclass ,py-name ,(or supers '(py-lisp-object))
	    ()
	    (:metaclass py-lisp-type))
	  (mop:finalize-inheritance (find-class ',py-name))))

;; Number (Lisp object: number)

(def-proxy-class py-number)

(def-py-method py-number.__repr__ (x^) (format nil "~A" x))

(def-py-method py-number.__eq__ (x^ y^)
  (if (and (numberp x) (numberp y))
      (py-bool (= x y))
    *the-notimplemented*))

(def-py-method py-number.real :attribute (x^)
	       (realpart x))

(def-py-method py-number.imag :attribute (x^)
	       (imagpart x))

(def-py-method py-number.conjugate (x^)
  (conjugate x))



;; Complex

(def-proxy-class py-complex (py-number))

;; Real

(def-proxy-class py-real (py-number))

;; Integer

(def-proxy-class py-int (py-real))

(def-py-method py-int.__new__ :static (cls &optional (arg 0))
	       (if (eq cls (find-class 'py-int))
		   arg
		 (let ((i (make-instance cls)))
		   (setf (proxy-lisp-val i) arg)
		   i)))

(def-py-method py-int.__init__ (&rest args) nil)

;; Float

(def-proxy-class py-float (py-real))


;; List (Lisp object: adjustable array)

(def-proxy-class py-list)

(def-py-method py-list.__new__ :static (cls)
	       (if (eq cls (find-class 'py-list))
		   (make-array 0 :adjustable t :fill-pointer 0)
		 (make-instance cls)))
		    
(def-py-method py-list.__init__ (x^ &optional iterable)
  (when iterable
    (loop for item in (py-iterate->lisp-list iterable)
	do (vector-push-extend item x))))

(def-py-method py-list.__str__ (x^)
  (format t "[~{~A~^, ~}]" (loop for i across x collect i)))


;; Tuple (Lisp object: consed list)

(def-proxy-class py-tuple)

(def-py-method py-tuple.__new__ :static (cls &rest args)
	       (if (eq cls (find-class 'py-tuple))
		   args
		 (make-instance cls)))

(defvar *the-empty-tuple* (make-instance 'py-tuple))


;; String (Lisp object: string)

(def-proxy-class py-string)

(def-py-method py-string.__len__ (x^)
  (length x))

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
  
  (let ((ga-meth  (recursive-class-dict-lookup (py-class-of x) '__getattribute__)))
    (assert ga-meth () "Object ~A (py-class: ~A) does not have a __getattribute__ method"
	    x (py-class-of x))
    (py-call ga-meth x attr)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Descriptors

;; The Python Reference Manual says:
;; 
;;   3.3.2.2 Implementing Descriptors
;;   
;;   In general, a descriptor is an object attribute with ``binding
;;   behavior'', one whose attribute access has been overridden by
;;   methods in the descriptor protocol: __get__(), __set__(), and
;;   __delete__().  If any of those methods are defined for an object,
;;   it is said to be a descriptor.
;; 
;; A descriptor is called a "data descriptor" iff it implements
;; `__set__'. Whether this is the case influences the attribute lookup
;; order.

(defun descriptor-p (x)
  (let ((x.class (py-class-of x)))
    (or (recursive-class-dict-lookup x.class '__get__)
	(recursive-class-dict-lookup x.class '__set__)
	(recursive-class-dict-lookup x.class '__delete__))))

(defun data-descriptor-p (x)
  "Returns DES-P, __SET__"
  ;; check for None?
  (recursive-class-dict-lookup (py-class-of x) '__set__))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Calling objects (functions, classes, instances)

(defgeneric py-call (f &rest args)
  (:method ((f function) &rest args) (apply f args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Math ops:  + - << ~  (etc)  and inplace variants:  += -= <<= ~=  (etc)

;; a + b   ->  (py-+ a b)   ->  a.__add__(b) or b.__radd__(a)
;; 
;; x += y  ->  (py-+= x y)  ->  x.__iadd__(y)
;;                              fallback: x = x.__add__(y)
;; 
;; The fall-back is not part of the py-+= function, because it
;; operates on /places/, while the function only tries the in-place
;; operation on a /value/.
;; 
;; The return value of py-+= indicates whether the in-place method
;; __iadd__ was found.

(defun raise-invalid-operands (operation left &optional right)
  ;; XXX merge into one pretty printer string
  ;; XXX TypeError
  (error (if right
	     (format nil "Invalid operands for operation ~A: ~A and ~A"
		     operation left right)
	   (format nil "Invalid operand for operation ~A: ~A" operation left))))

(defvar *binary-op-funcs-ht* (make-hash-table :test #'eq))
(defvar *binary-iop-funcs-ht* (make-hash-table :test #'eq))

(defun get-binary-op-func (op) (gethash op *binary-op-funcs-ht*))
(defun get-binary-iop-func (iop) (gethash iop *binary-iop-funcs-ht*))

(defmacro def-math-func (op-syntax op-func l-meth r-meth iop-syntax iop-func i-meth)
  `(progn
     
     (defgeneric ,op-func (x y)
       (:method ((x t) (y t))
		(let* ((x.class (py-class-of x))
		       (y.class (py-class-of y))
		       (y-sub-of-x (and (not (eq x.class y.class))
					(subtypep y.class x.class))))
		  
		  (loop
		      with try-right = y-sub-of-x
		      with finish = nil
		      do (let* ((op-meth (recursive-class-dict-lookup
					  (if try-right y.class x.class)
					  (if try-right ',r-meth ',l-meth)))
				(res (and op-meth 
					  (py-call op-meth
						   (if try-right y x)
						   (if try-right x y)))))
			   (when res
			     (unless (eq res (load-time-value *the-notimplemented*))
			       (return res))))
			 
			 (if finish
			     (raise-invalid-operands ',op-syntax x y)
			   (setf try-right (not try-right)
				 finish t))))))
     
     (setf (gethash ',op-syntax *binary-op-funcs-ht*) (function ,op-func))
     
     (defgeneric ,iop-func (x val)
       (:method ((x t) (val t))
		(let* ((iop-meth (recursive-class-dict-lookup (py-class-of x) ',i-meth))
		       (res (and iop-meth 
				 (py-call iop-meth x val))))
		  (and iop-meth
		       (not (eq res (load-time-value *the-notimplemented*)))))))
     
     (setf (gethash ',iop-syntax *binary-iop-funcs-ht*) (function ,iop-func))))


;; /t/ is not Python syntax, but a hack to support __future__ feature
;; `true division'
(def-math-func +   py-+    __add__      __radd__       +=   py-+=   __iadd__      )
(def-math-func -   py--    __sub__      __rsub__       -=   py--=   __isub__      )
(def-math-func *   py-*    __mul__      __rmul__       *=   py-*=   __imul__      )
(def-math-func /t/ py-/t/  __truediv__  __rtruediv__   /t/  py-/t/= __itruediv__  )
(def-math-func //  py-//   __floordiv__ __rfloordiv__  //=  py-//=  __ifloordiv__ ) 
(def-math-func /   py-/    __div__      __rdiv__       /=   py-/=   __idiv__      )
(def-math-func %   py-%    __mod__      __rmod__       %=   py-%=   __imod__      )
(def-math-func <<  py-<<   __lshift__   __rlshift__    <<=  py-<<=  __ilshift__   )
(def-math-func >>  py->>   __rshift__   __rrshift__    >>=  py->>=  __irshift__   )
(def-math-func &   py-&    __and__      __rand__       &=   py-&=   __iand__      )
(def-math-func \|  py-\|   __or__       __ror__        \|=  py-\|=  __ior__       )
(def-math-func ^   py-^    __xor__      __rxor__       ^=   py-^=   __ixor__      )

;; a**b (to-the-power) is a special case:
;;   
;; - method __pow__ takes an optional third argument: the c in
;;   (a**b)%c that argument can be supplied using the built-in
;;   function POW, but not syntactically using **
;;  
;; - there are no __rpow__ methods
;;   
;; - so, this function py-** always gets 2 arguments when called for
;;   the a**b syntax, but may have a third argument when called for
;;   the built-in function POW.

(defun py-** (x y &optional z)
  (let* ((x.class (py-class-of x))
	 (op-meth (recursive-class-dict-lookup x.class '__pow__))
	 (res (and op-meth (if z
			       (py-call op-meth x y z)
			     (py-call op-meth x y)))))
    
    (if (and res (not (eq res (load-time-value *the-notimplemented*))))
	res
      (raise-invalid-operands '** x y))))

(setf (gethash '** *binary-op-funcs-ht*) #'py-**)

;; **= has similar ugliness

(defun py-**= (x y &optional z)
  (let* ((x.class (py-class-of x))
	 (iop-meth (recursive-class-dict-lookup x.class '__ipow__))
	 (res (and iop-meth (if z
				(py-call iop-meth x y z)
			      (py-call iop-meth x y)))))
    
    (if (and iop-meth (not (eq res (load-time-value *the-notimplemented*))))
	res
      nil)))

(setf (gethash '**= *binary-iop-funcs-ht*) #'py-**=)



(defvar *unary-op-funcs-ht* (make-hash-table :test #'eq))
(defun get-unary-op-func (op) (gethash op *unary-op-funcs-ht*))

(defmacro def-unary-op-func (syntax fname meth)
  `(progn (defgeneric ,fname (x)
	    (:method ((x t))
		     (let* ((x.class (py-class-of x))
			    (op-meth (recursive-class-dict-lookup x.class ',meth))
			    (res (and op-meth (py-call op-meth x))))
		       (if (or (null op-meth)
			       (eq res (load-time-value *the-notimplemented*)))
			   (raise-invalid-operands ',syntax x)
			 res))))
	  (setf (gethash ',syntax *unary-op-funcs-ht*) (function ,fname))))

(def-unary-op-func ~  py-unary-~  __invert__ )
(def-unary-op-func +  py-unary-+  __pos__    )
(def-unary-op-func -  py-unary--  __neg__    )

(defgeneric py-not (x)
  (:method ((x t))
	   (py-bool (not (py-val->lisp-bool x)))))

(setf (gethash 'not *unary-op-funcs-ht*) #'py-not)

;; Equality and membership testing:  a in b, a not in b, a is b, a is not b

(defvar *binary-test-funcs-ht* (make-hash-table :test #'eq))
(defun get-binary-test-func (op) (gethash op *binary-test-funcs-ht*))

(defgeneric py-in (x seq)
  (:method ((x t) (seq t))
	   ;; use __contains__, fall back on iterator
	   (let ((contains-meth (recursive-class-dict-lookup
				 (py-class-of seq) '__contains__)))
	     (if contains-meth
		 (let ((res (py-call contains-meth seq x)))
		   (cond ((eq res t)   (load-time-value *the-true*))
			 ((eq res nil) (load-time-value *the-false*))
			 (t            res)))
	       (loop with f = (get-py-iterate-fun seq)
		   for seq-item = (funcall f)
		   while seq-item
		   when (py-== x seq-item)
		   return (load-time-value *the-true*)
		   finally (return (load-time-value *the-false*)))))))
	     
(defgeneric py-not-in (x seq)
  (:method ((x t) (seq t))
	   (py-not (py-in x seq))))

(setf (gethash 'in *binary-op-funcs-ht*) #'py-in)
(setf (gethash '|not in| *binary-op-funcs-ht*) #'py-not-in)

(defgeneric py-is (x y)
  (:method ((x t) (y t))
	   (if (eq x y) *the-true* *the-false*)))

(defgeneric py-is-not (x y)
  (:method ((x t) (y t))
	   (if (eq x y) *the-false* *the-true*)))

(setf (gethash 'is *binary-op-funcs-ht*) #'py-is)
(setf (gethash '|is not| *binary-op-funcs-ht*) #'py-is-not)



;; Binary comparison operations
;; 
;; It appears that all comparisons -- < > <= >= == != <> -- are
;; defined in terms of the outcome of built-in function `cmp'
;; (#'py-cmp).
;; 
;; #'py-< implements the logic for "a < b"
;; 
;; Note this important point: "a < b" is NOT directly translated into
;; a.__lt__(b), although this __lt__ method *might* be called by
;; __cmp__, as might b.__ge__ in this case.
;; 
;;  '<  -->  #'py-<
;; 
;; This mapping is used by the interpreter, in EVAL-COMPARISON
;; (pyeval.cl).

(defvar *binary-comparison-funcs-ht* (make-hash-table :test #'eq))

(defmacro def-comparison (syntax func test-x-y)
  `(progn (defgeneric ,func (x y)
	    (:method ((x t) (y t))
		     #+(or)(declare (optimize (speed 3) (safety 1) (debug 0)))
		     (if ,test-x-y
			 (load-time-value *the-true*)
		       (load-time-value *the-false*))))
	  (setf (gethash ',syntax *binary-comparison-funcs-ht*) (function ,func))))
	    
;; pyb:cmp returns -1, 0 or 1 (or TypeError if user-supplied
;; method returns bogus comparison result; that TypeError is not
;; catched here but goes to user code.)

(def-comparison  <  py-<   (=  (the (integer -1 1) (pyb:cmp x y)) -1))
(def-comparison  >  py->   (=  (the (integer -1 1) (pyb:cmp x y))  1))
(def-comparison ==  py-==  (=  (the (integer -1 1) (pyb:cmp x y))  0))
(def-comparison !=  py-!=  (/= (the (integer -1 1) (pyb:cmp x y))  0)) ;; parser: <> -> !=
(def-comparison <=  py-<=  (<= (the (integer -1 1) (pyb:cmp x y))  0))
(def-comparison >=  py->=  (>= (the (integer -1 1) (pyb:cmp x y))  0))


(defgeneric py-val->lisp-bool (x)
  (:method ((x number)) (/= x 0)))
