(in-package :python)

(declaim (optimize (debug 3)))

;;; Built-in classes and their methods


;; XXX todo:  eval-for-in calls __iter__
;;  from __future__ import division
;;  unicode strings don't print properly (question marks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The `magic methods' shared by all objects.
;;; 
;;; Those not shared by all objects (like `__len__', `__eq__') are
;;; defined as Generic Function in magicmeths.cl, and registered for
;;; the right class (number) below.
;;; 
;;; The methods shared by all standard objects are (using `dir(object)'):
;;; 
;;;   __class__, __delattr__, __doc__, __getattribute__, __hash__,
;;;   __init__,  __new__, __reduce__, __reduce_ex__, __repr__,
;;;   __setattr__, __str__
;;; 
;;; Using metaclasses, classes can be created that might not have
;;; these methods. XXX todo figure that out

;; XXX for now (?) add default equality tester
(defmethod __eq__ (x y)
  (declare (ignore x y) (special *False*))
  #+(or)(warn "default equality tester used, may indicating something TODO: ~A ~A" x y)
  *False*)

(defgeneric __class__ (x) (:documentation "The class of X"))
(defmethod __class__ ((x integer))       (find-class 'py-int))
(defmethod __class__ ((x real))          (find-class 'py-float))
(defmethod __class__ ((x complex))       (find-class 'py-complex))
(defmethod __class__ ((x string))        (find-class 'py-string))
(defmethod __class__ ((x user-defined-class))   (let ((k (class-of x)))
						  (if (subtypep k 'udc-with-ud-metaclass)
						      (loop for cls in (mop:class-precedence-list k)
							  when (typep cls 'user-defined-class)
							  do (return cls)
							  finally (error "no udc for metaclass?"))
						    (find-class 'python-type))))
(defmethod __class__ ((x builtin-class)) (find-class 'python-type))
(defmethod __class__ ((x function))      (find-class 'python-type)) ;; XXX show function name
(defmethod __class__ ((x python-object)) (class-of x)) ;; XXX check
(defmethod __class__ ((x symbol))        (find-class 'py-string))


;; PYTHON-OBJECT is both an instance and a subclass of PYTHON-TYPE.
(defmethod __class__ ((x (eql (find-class 'python-object)))) (find-class 'python-type))

;; PYTHON-TYPE is it's own type.
(defmethod __class__ ((x (eql (find-class 'python-type)))) x)

(register-bi-class-attr/meth (find-class 't) '__class__ (make-bi-class-attribute #'__class__))


;;; Classes have a `__mro__' attribute, which is the "Method
;;; Reslution Order" or Class Precedence List.

(defgeneric __mro__ (cls)
  (:documentation "Method resolution order (as tuple, including itself)"))

(defmethod py-class-mro ((x class))
  (loop for cls in (mop:class-precedence-list x)
      if (or (typep cls 'user-defined-class)
	     (typep cls 'builtin-class)
	     (eq cls (find-class 'python-type)))
      collect cls))
   
(defmethod __mro__ ((c class))
  (make-tuple-from-list (py-class-mro c)))

(register-bi-class-attr/meth (find-class 'class) '__mro__
			     (make-bi-class-attribute #'__mro__))


;;; Classes have a `__bases__' attribute, indicating the direct superclasses

(defgeneric __bases__ (cls) (:documentation "Direct bases classes"))

(defmethod __bases__ ((c class))
  ;; XXX check if semantics of modifying this attribute are defined
  (make-tuple-from-list (loop for cls in (mop:class-direct-superclasses c)
			    if (or (typep cls 'user-defined-class)
				   (typep cls 'builtin-class))
			    collect cls)))

(register-bi-class-attr/meth (find-class 'class) '__bases__
			     (make-bi-class-attribute #'__bases__))


;;; Object creation: __new__ and __init__
;;; 
;;; __new__ is an exceptional special-cased method takes a class as
;;; first argument, not an instance. Its purpose is allocating an instance.
;;; 
;;; __init__ initializes the allocated object.
;;; 
;;; Both are called with any parameters supplied:  x = ClassName(1,2, key=42) 

(defgeneric __new__ (cls &rest args)
  (:documentation "Create a new instance of class CLS"))

#+(or)
(defmethod __new__ ((cls class) &rest args)
  ;; (break "__new__ class: correct?")
  #+(or)(when args
    (warn (format nil "Default __new__ ignoring args: ~A" args)))
  (make-instance cls))


#+(or)
(register-bi-class-attr/meth (find-class 'class)
			     '__new__
			     (make-static-method #'__new__))


(defmethod py-type (x)
  (__class__ x))


(defmethod python-type-__new__ (metaclass &optional name bases dict)
  
  #+(or)(when (not (or name bases dict)) ;; "type(x) -> <type-of-x>"
    (return-from python-type-__new__ (py-type metaclass)))
    
  (if (subtypep metaclass 'python-type)
      
      ;; Create a class with this as metaclass
      (progn (ensure-py-type name string "class name must be string (got: ~A)")
	     (multiple-value-bind (slots has-slots)
		 (let ((s (py-dict-gethash dict '__slots__)))
		   (if s 
		       (values (py-iterate->lisp-list s) t)
		     (values nil nil)))
      
	       (return-from python-type-__new__
		 (make-python-class :name (intern name #.*package*)
				    :supers (if bases (py-iterate->lisp-list bases) nil)
				    :slots slots
				    :has-slots has-slots
				    :namespace dict
				    :metaclass metaclass))))
    (progn #+(or)(when (or name bases dict)
		   (warn "type.__new__ ignoring arguments" name bases dict)
		   #+(or)(break "type.__new__ with unexpected args"))
	   (return-from python-type-__new__
	     (make-instance metaclass)))))
    

(register-bi-class-attr/meth (find-class 't) '__new__
			     (make-static-method #'python-type-__new__))
(register-bi-class-attr/meth (find-class 'class) '__new__
			     (make-static-method #'python-type-__new__))

(defgeneric __init__ (x &rest args)
  (:documentation "Object initialization"))

(defmethod __init__ (x &rest args)
  (declare (ignore x args)
	   (special *None*))
  *None*)

(register-bi-class-attr/meth (find-class 'class) '__init__ #'__init__)


;;; String representation of Python objects:
;;;    __str__       is a representation targeted to humans
;;;    __repr__      if possible,  eval(__repr__(x)) should be equal to x
;;; 
;;;  __str__ defaults to __repr__
;;;  __repr__ defaults to print-unreadable-object

(defgeneric __str__ (x) (:documentation "String representation of X, intended for humans"))
(defmethod __str__ (x) (call-attribute-via-class x '__repr__)) ;; defaults to __repr__
(register-bi-class-attr/meth (find-class 't) '__str__ #'__str__)

(defgeneric __repr__ (x) (:documentation "String representation of X, preferably eval-able"))
(defmethod __repr__ (x) (with-output-to-string (s) 
		 (print-unreadable-object (x s :identity t :type t))))
(register-bi-class-attr/meth (find-class 't) '__repr__ #'__repr__)


;;; Attribute setting, getting, deleting


(defgeneric __getattribute__ (x attr) (:documentation "Intercepts all attribute lookups"))

(defmethod __getattribute__ :around (x (attr string))
  (__getattribute__ x (intern attr #.*package*)))

(defvar *default-__getattribute__-running* nil)

(defmethod __getattribute__ (x attr)
  (declare (special *default-__getattribute__-running*))
  (or (let ((*default-__getattribute__-running* t)) ;; to avoid infinite recursion XXX hack
	(internal-get-attribute x attr))
      (py-raise 'AttributeError "~A ~A" x attr)))
	
(register-bi-class-attr/meth (find-class 't) '__getattribute__ #'__getattribute__)


(defvar *default-__setattr__-running* nil)

(defgeneric __setattr__ (x attr val) (:documentation "Set attribute ATTR of X to VAL"))
(defmethod __setattr__ (x attr val) (let ((*default-__setattr__-running* t))
				      (internal-set-attribute x attr val)))
(register-bi-class-attr/meth (find-class 't) '__setattr__ #'__setattr__)

(defgeneric __delattr__ (x attr) (:documentation "Delete attribute named ATTR of X"))
(defmethod __delattr__ (x attr) (internal-del-attribute x attr))
(register-bi-class-attr/meth (find-class 't) '__delattr__ #'__delattr__)


;;; Documentation string
;;; 
;;; Functions, classes and modules (ore?) can have a `docstring'. By
;;; default, it's None (not AttributeError).

(defparameter *doc-strings* (make-hash-table :test #'eq))

(defun register-doc-string (x string)
  (setf (gethash x *doc-strings*) string))

(defun lookup-doc-string (x)
  (gethash x *doc-strings*))

(defgeneric __doc__ (x)
  (:documentation "Documentation for X"))

(defmethod __doc__ (x)
  (declare (special *None*))
  (or (lookup-doc-string x)
      *None*))

(register-bi-class-attr/meth (find-class 't) '__doc__ (make-bi-class-attribute #'__doc__))

;;; XXX todo: register docstrings of all builtin-functions and builtin-classes


;;; An object can have a hash code. Some mutable objects (lists) are
;;; not hashable because equality for those object is defined
;;; recursively as equality of the items they contain. (However, they
;;; can be subclassed, and for the subclass hashing can be defined.)
;;; 
;;; Modules are hashable, because the equality between two modules
;;; doesn't change when a module is changed.

(defgeneric __hash__ (x) (:documentation "Hash value (integer)"))
(defmethod __hash__ (x) (pyb:id x)) ;; hash defaults to id, the (fake) pointer value
(register-bi-class-attr/meth (find-class 't) '__hash__ #'__hash__)


;; XXX __reduce__ ?   they have something to do with pickling, but are not documented?
;; XXX __reduce_ex__ ?



;; experiment: give all object __get__ method
(defmethod __get__ (x inst cls)
  (declare (ignore inst cls))
  x)

(register-bi-class-attr/meth (find-class 't) '__get__ #'__get__)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The built-in Python object types

;;; These macros ease GF method definition

(defmacro def-unary-meths (py-type cl-type py->cl-form meths)
  `(progn ,@(loop for (methname result) in meths
		collect `(progn (defmethod ,methname ((x ,py-type))
				  (let ((x ,py->cl-form))
				    ,result))
				(defmethod ,methname ((x ,cl-type))
				  ,result)))))


(defmacro def-binary-meths (py-type cl-type py->cl-form-x py->cl-form-y data)
  `(progn ,@(loop for (methname result) in data
		collect `(progn (defmethod ,methname ((x ,py-type) (y ,py-type))
				  (let ((x ,py->cl-form-x)
					(y ,py->cl-form-y))
				    ,result))
				(defmethod ,methname ((x ,py-type) (y ,cl-type))
				  (let ((x ,py->cl-form-x))
				    ,result))
				(defmethod ,methname ((x ,cl-type) (y ,py-type))
				  (let ((y ,py->cl-form-y))
				    ,result))
				(defmethod ,methname ((x ,cl-type) (y ,cl-type))
				  ,result)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some special singleton classes: None, Ellipsis, NotImplemented

(defmacro def-static-singleton-classes (data)
  `(progn
     ,@(loop for (class-name class-doc object object-doc object-repr object-hash) in data
	   collect 
	     (progn (assert (typep object-hash 'fixnum))
		    (let ((__new__-name (intern (concatenate 'string (string class-name)
							     "-__new__")
						#.*package*)))
		      `(progn (defclass ,class-name (builtin-object) ()
					(:documentation ,class-doc)
					(:metaclass builtin-class))
			      (mop:finalize-inheritance (find-class ',class-name))
			      
			      (defvar ,object (make-instance ',class-name) ,object-doc)
			      
			      ;; CPython disallows creating instances of these.
			      
			      (defmethod ,__new__-name ((x class) &rest args)
				(declare (ignore args))
				(py-raise 'TypeError
					  "Cannot create '~A' instances" ',class-name))
			    
			      (defmethod __repr__ ((c ,class-name))
				,object-repr)
			      			      
			      (defmethod __hash__ ((c ,class-name))
				,object-hash)))))))

(def-static-singleton-classes
    ;; They don't have any special methods, other than those of all objects
    ((py-none "The NoneType class"
	      *None* "The Python value/object `None', similar to CL's `nil'"
	      "None" 239888)
     
     (py-ellipsis "The EllipsisType class"
		  *Ellipsis* "Represent `...' in things like `x[1,...,1]'"
		  "Ellipsis" 177117)

     (py-notimplemented "To signal unsupported arguments for binary operation"
			*NotImplemented* "An operation is not implemented"
			"NotImplemented" -99221188)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Number
;; 
;; The Python number classes are subtypes of this class. CPython has
;; no corresponding class.

(defclass py-number (builtin-instance)
  ((val :type number :initarg :val :initform 0))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-number))

(defmethod print-object ((x py-number) stream)
  (print-unreadable-object (x stream :identity t :type t)
    (format stream ":val ~A" (slot-value x 'val))))


(defmethod number-__new__ ((cls class) &rest args)
  (break "number-__new__ accessible, apparently?!") ;; I think not accessible by user code
  (assert (subtypep cls 'py-number))
  (when (cdr args)
    (py-raise 'ValueError "__new__ for py-number takes max 1 pos arg (got: ~A)" args))
  (let ((inst (make-instance cls)))
    (setf (slot-value inst 'val) (if args
				     (convert-to-number (car args) 'number)
				   0))
    inst))

(register-bi-class-attr/meth (find-class 'py-number) '__new__ 
			     (make-static-method #'number-__new__))

#+(or) ;; needed?
(register-bi-class-attr/meth (find-class 'number) '__new__ 
			     (make-static-method #'number-__new__))


(deftype py-number-designator ()
  `(or number py-number))

(defun py-number-designator-p (x)
  "Returns NUM-DES-P, LISP-NUM"
  (typecase x
    (number (values t x))
    (py-number (values t (slot-value x 'val)))
    (t nil)))

(defmethod py-number-designator-val ((x number)) x)
(defmethod py-number-designator-val ((x py-number)) (slot-value x 'val))

(defmethod make-py-number ((val number))
  "Make a PY-NUMBER instance for Lisp number VAL"
  (etypecase val
    (integer (make-int val))
    (real (make-float (coerce val 'long-float)))
    (complex (make-complex val))))

  
(defmethod __hash__ ((x py-number))
  (__hash__ (slot-value x 'val)))


(def-unary-meths 
    py-number number (slot-value x 'val)
    (
     ;; CPython prints *sys-neg-maxint* <= x <= *sys-pos-maxint* as X,
     ;; outside that range as XL:  3 vs 3L. Let's not bother.
     (__repr__     (format nil "~A" x))
     
     (__nonzero__  (lisp-val->py-bool (/= x 0)))
     (__neg__      (- x))
     (__pos__      x)
     (__abs__      (abs x))
     (__complex__  (make-complex x))))

(defmethod __repr__ ((x integer))
  (format nil "~A" x))

(loop for name in `(__nonzero__ __neg__ __pos__ __abs__ __complex__)
    do (loop for cls in `(,(find-class 'number) ,(find-class 'py-number))
	   do (register-bi-class-attr/meth cls name (symbol-function name))))


(def-binary-meths py-number number (slot-value x 'val) (slot-value y 'val)

		  ;; comparison -> t/nil	
		  ((__eq__  (= x y))
		   (__ne__  (/= x y))
		   
		   ;; arithmethic -> lisp number
		   (__add__      (+ x y))
		   (__radd__     (+ x y))
		   (__sub__      (- x y))
		   (__mul__      (* x y))
		   (__rmul__     (* x y))
		   (__truediv__  (/ x y))
		   (__rtruediv__ (/ y x))
		   (__rsub__     (- y x))))

(loop for name in `(__eq__ __ne__ __add__ __radd__ __sub__ __mul__ __rmul__
			   __truediv__ __rtruediv__ __rsub__)
    do (loop for cls in `(,(find-class 'number) ,(find-class 'py-number))
	   do (register-bi-class-attr/meth cls name (symbol-function name))))


;; Power
;; 
;; pow(a,b)   <=> a**b
;; pow(a,b,c) <=> (a**b) % c
;; 
;; As CPython has deprecated the use of modulo and division on complex
;; numbers and Lisp doesn't support it, it's not supported here
;; either.
;; 
;; However, pow(x,y) with x,y complex numbers is no problem.

(defmethod __pow__ (x y &optional m)
  (macrolet ((check-real (var)
	       `(typecase ,var
		  (real)
		  (py-real (setf ,var (slot-value ,var 'val)))
		  (t (py-raise 'TypeError "Unsupported operands for power (got: ~A ~A ~A)"
			       x y m))))
	     (check-number (var)
	       `(typecase ,var
		  (number)
		  (py-number (setf ,var (slot-value ,var 'val)))
		  (t (py-raise 'TypeError "Unsupported operands for power (got: ~A ~A ~A)"
			       x y m)))))
    (if m
	(progn (check-real x)
	       (check-real y)
	       (check-real m)
	       (mod (expt x y) m))
      (progn (check-number x)
	     (check-number y)
	     (expt x y)))))


;; Built-in function pow() will not call __rpow__ with 3 arguments,
;; but user code might.

(defmethod __rpow__ (x y &optional m)
  (__pow__ y x m))


(register-bi-class-attr/meth (find-class 'number) '__pow__ #'__pow__)
(register-bi-class-attr/meth (find-class 'py-number) '__pow__ #'__pow__)

(register-bi-class-attr/meth (find-class 'number) '__rpow__ #'__rpow__)
(register-bi-class-attr/meth (find-class 'py-number) '__rpow__ #'__rpow__)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Real
;; 
;; Corresponds to Lisp type `real'. CPython has no corresponding class.

(defclass py-real (py-number)
  ((val :type real :initform 0))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-real))

(deftype py-real-designator ()
  `(or real py-real))

(defun py-real-designator-p (x)
  (typecase x
    (real (values t x))
    (py-real (values t (slot-value x 'val)))
    (t nil)))

(def-unary-meths
    py-real real (slot-value x 'val)
    ((__int__     (make-int (truncate x))) ;; CPython: returns a long int for X large enough
     (__long__    (make-int (truncate x))) ;; CPython: returns long int
     (__float__   (make-float x))
     
     (__repr__    (let* ((v (make-array 4 :adjustable t
					:fill-pointer 0 :element-type 'character)))
		    
		    ;; Show number simply with a few decimals if
		    ;; feasible, otherwise use exp notation
		    
		    (format v "~,,2,,,,vG" #\E x)
		    (decf (fill-pointer v) (loop for i downfrom (1- (length v))
					       while (char= (aref v i) #\Space)
					       count 1))
		    (when (or (position #\E v :test #'char=)
			      (> (length v) 8))
		      (setf (fill-pointer v) 0)
		      (format v "~11,,,,,,ve" #\E x))
		    
		    (let ((len (length v)))  ;; remove final "E+0"
		      (when (and (> len 3)
				 (char= (aref v (- len 1)) #\0)
				 (char= (aref v (- len 2)) #\+))
			(decf (fill-pointer v) 3)))
		    v))
     ))

(loop for name in `(__int__ __long__ __float__)
    do (loop for cls in `(,(find-class 'real) ,(find-class 'py-real))
	   do (register-bi-class-attr/meth cls name (symbol-function name))))


(def-binary-meths
    py-real real (slot-value x 'val) (slot-value y 'val)
    
    ;; comparison -> t/nil	
    ;; These are not defined on complexes, only on reals.
    ((__lt__  (< x y))
     (__gt__  (> x y))
     (__le__  (<= x y))
     (__ge__  (>= x y))
     (__mod__ (mod x y))
     (__rmod__ (mod y x))
 
     ;; As FLOOR takes REAL arguments, not COMPLEX, some operations
     ;; that Python allows (although they are deprecated) on complexes
     ;; are not allowed here.
    
     (__cmp__ (cond ((< x y) -1)
		    ((> x y)  1)
		    ((= x y)  0)))
     (__div__       (/ x y))
     (__rdiv__      (__div__ y x))
     (__floordiv__  (values (floor x y)))
     (__rfloordiv__ (__floordiv__ y x))
     (__divmod__    (make-tuple-from-list (multiple-value-list (floor x y))))
     (__rdivmod__   (__divmod__ y x))))

(loop for name in `(__mod__ __rmod__ __cmp__ __div__ __rdiv__ __floordiv__
			    __rfloordiv__ __divmod__ __rdivmod__)
    do (loop for cls in `(,(find-class 'real) ,(find-class 'py-real))
	   do (register-bi-class-attr/meth cls name (symbol-function name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complex (corresponds to Lisp type `complex')

(defclass py-complex (py-real)
  ((val :type complex :initarg :val :initform #C(0 0)))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-complex))

(defmethod py-complex-__new__((cls class) &rest args)
  (assert (subtypep cls (find-class 'py-complex)))
  (cond ((null args)       (make-complex 0))
	((null (cdr args)) (make-complex (convert-to-number (car args) 'complex)))
	((null (cddr args)) (make-complex  ;; both args can be complex
			     (+ (convert-to-number (first args) 'complex)
				(* #C(0 1) 
				   (convert-to-number (second args) 'complex)))))
	(t (py-raise 'ValueError "Too many args for complex.__new__ (got: ~A)" args))))

(register-bi-class-attr/meth (find-class 'py-complex) '__new__ 
			     (make-static-method #'py-complex-__new__))
(register-bi-class-attr/meth (find-class 'complex) '__new__ 
			     (make-static-method #'py-complex-__new__))

(defun make-complex (&optional (val #C(0 0)))
  (make-instance 'py-complex :val (coerce val 'complex)))

(def-unary-meths
    py-complex complex (slot-value x 'val)
    ((__hash__ (if (= (imagpart x) 0)
		   (__hash__ (realpart x))
		 (sxhash x)))
     (__repr__ (cond ((= (complex-imag x) 0) (__repr__  (complex-real x)))
		     ((= (complex-real x) 0) (format nil "~Aj" (__repr__ (complex-imag x))))
		     (t (if (>= (imagpart x) 0)
			    (format nil "(~A+~Aj)"
				    (__repr__ (complex-real x))
				    (__repr__ (complex-imag x)))
			  (format nil "(~A-~Aj)"
				  (__repr__ (complex-real x))
				  (__repr__ (* -1 (complex-imag x))))))))
     #+(or)((complex-real (realpart x))
	    (complex-imag (imagpart x))
	    (complex-conjugate (conjugate x)))))


;; Slight difference between Python and Lisp regarding types of real
;; and imaginary components of complex number:
;; 
;; Lisp: (imagpart #C(3.0 0)) == 3
;; Python: it's 0

(defmethod complex-real ((x number)) (let ((r (realpart x)))
				       (if (= r 0) 0 r)))
(defmethod complex-imag ((x number)) (let ((i (imagpart x)))
				       (if (= i 0) 0 i)))
(defmethod complex-conjugate ((x number)) (conjugate x))

(loop for (meth . func) in `((real . ,(make-bi-class-attribute #'complex-real))
			     (imag . ,(make-bi-class-attribute #'complex-imag))
			     (conjugate . ,(make-bi-class-attribute #'complex-conjugate)))
    do (register-bi-class-attr/meth (find-class 'py-number) meth func) 
       (register-bi-class-attr/meth (find-class 'number) meth func))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Float (corresponding to Lisp type `long-float')

(defclass py-float (py-real)
  ((val :type long-float :initform 0L0))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-float))

(defmethod py-float-__new__ ((cls class) &rest args)
  (assert (subtypep cls 'py-float))
  (cond ((null args)       (make-float 0))
	((null (cdr args)) (make-float (convert-to-number (first args) 'real)))
	(t (py-raise 'ValueError "Too many args for float.__new__ (got: ~A)" args))))

(register-bi-class-attr/meth (find-class 'py-float) '__new__ 
			     (make-static-method #'py-float-__new__))
(register-bi-class-attr/meth (find-class 'float) '__new__ 
			     (make-static-method #'py-float-__new__))

(defun make-float (&optional (val 0d0))
  (make-instance 'py-float :val val))

(defmethod __hash__ ((x float))
  (multiple-value-bind (int-part float-part)
      (truncate x)
    (if (= float-part 0)
	(__hash__ int-part) ;; hash(3.0) must equal hash(3)
      (sxhash x)))) ;; hash(3.xxx) doesn't matter

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Integer (corresponding to Lisp type `integer')

;; these are the min-max Python `normal' integer values; outside this
;; range it becomes a `long'. Not used yet: we don't separate the two
;; integer types anywhere, as CPython doesn't do that often, either.
#+(or)(progn (defconstant *sys-pos-maxint* 2147483647)
	     (defconstant *sys-neg-maxint* -2147483648))

(defclass py-int (py-real)
  ((val :type integer :initarg :val :initform 0))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-int))


(defmethod py-int-__new__ ((cls class) &rest pos-args)
  ;; takes an optional `base' arg
  (assert (subtypep cls (find-class 'py-int)))
  (cond ((py-string-designator-p (car pos-args))
	 (destructuring-bind (str &optional base) pos-args
	   (setf str (py-string-designator-val str)
		 base (cond ((null base) (return-from py-int-__new__
					   (with-input-from-string (*standard-input* str)
					     (read-number))))
			    ((py-int-designator-p base)
			     (let ((b (py-int-designator-val base)))
			       (cond ((<= 2 b 36) b)
				     ((= 0 b) (return-from py-int-__new__
						(with-input-from-string (*standard-input* str)
						  (read-number))))
				     (t (py-raise 'ValueError "int(): invalid base: ~A" b)))))
			    (t (py-raise 'ValueError "int(): invalid base: ~S" base))))
	   (let ((int-val (loop with res = 0
			      for x across str
			      when (digit-char-p x base)
			      do (setf res (+ (* base res) (digit-char-p x base)))
			      else do (py-raise 'ValueError
						"int(): non-digit found (got: ~S, base: ~A)" x base)
			      finally (return res))))
	     (if (eq cls (find-class 'py-int))
		 int-val
	       (let ((inst (make-instance cls)))
		 (setf (slot-value inst 'val) int-val)
		 inst)))))
	((py-number-designator-p (car pos-args))
	 (let ((int-val (truncate (py-number-designator-val (car pos-args))))) ;; CPython truncates
	   (if (eq cls (find-class 'py-int))
	       int-val
	     (let ((inst (make-instance cls)))
	       (setf (slot-value inst 'val) int-val)
	       inst))))))

(register-bi-class-attr/meth (find-class 'py-int) '__new__
			     (make-static-method #'py-int-__new__))
(register-bi-class-attr/meth (find-class 'integer) '__new__
			     (make-static-method #'py-int-__new__))

;; noop __init__


(defun make-int (&optional (val 0))
  (make-instance 'py-int :val val))

(deftype py-int-designator ()
  `(or integer py-int))

(defun py-int-designator-p (x)
  "Return DESIGNATOR-P, LISP-VAL where LISP-VAL only makes sense if it's ~
   indeed a designator"
  (cond ((integerp x)      (values t x))
	((typep x 'py-int) (values t (slot-value x 'val)))
	(t nil)))


(defgeneric py-int-designator-val (x)
  (:documentation "Return the Lisp int value of a Python integer designator.")
  (:method ((x integer)) x)
  (:method ((x py-int)) (slot-value x 'val))
  (:method (x) (py-raise "Integer expected (got: ~S)" x)))

		     
(def-binary-meths
    py-int integer (slot-value x 'val) (slot-value y 'val)
    
    ;; division is floor division (unless "from __future__ import division")
    ((__div__ (values (floor x y)))
     
     ;; bit operations -> lisp integer
     (__and__ (logand x y))
     (__xor__ (logxor x y))
     (__or__  (logior x y))
    
     ;; ASH accepts both positive and negative second argument, Python only positive.
     (__lshift__ (if (>= y 0)
		     (ash x y)
		   (py-raise 'ValueError "Negative shift count")))
     (__rlshift__ (__lshift__ y x))
     (__rshift__ (if (>= y 0)
		     (ash x (- y))
		   (py-raise 'ValueError "Negative shift count")))
     (__rrshift__ (__rshift__ y x))
     ))

(loop for name in `(__and__ __xor__ __or__ __lshift__ __rlshift__ __rshift__ __rrshift__)
    do (loop for cls in `(,(find-class 'integer) ,(find-class 'py-int))
	   do (register-bi-class-attr/meth cls name (symbol-function name))))


(defmethod mod-to-fixnum ((x integer))
  "Return result of MODding i with most-positive-fixnum (which is ~@
   always a fixnum)"
  ;; important property for use in hashes: 3 remains 3; -3 remains -3.
  (let ((x (if (>= x 0) 
	       (mod x (+ 1 most-positive-fixnum))
	     (mod x (- most-negative-fixnum 1)))))
    (assert (typep x 'fixnum))
    x))


(def-unary-meths
    py-int integer (slot-value x 'val)
    ((__invert__  (lognot x))
     (__hash__    (mod-to-fixnum x)) ;; hash(3) == 3; hash(-3) == -3
      
     ;; don't worry about conversion issues - assume CL takes care of them.
     (__complex__ (coerce x 'complex))
     (__int__     (make-int (truncate x)))
     (__long__    (make-int (truncate x)))
     (__float__   (coerce x 'long-float))
     
     ;; string representations
     (__oct__  (format nil "0~O" x))
     (__hex__  (format nil "0x~X" x))     
     ))

(loop for name in `(__invert__ __hash__ __complex__ __int__ __long__ __float__ __oct__ __hex__)
    do (loop for cls in `(,(find-class 'integer) ,(find-class 'py-int))
	   do (register-bi-class-attr/meth cls name (symbol-function name))))
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bool (derived from Integer)
;; 
;; In Python, booleans are a type of their own: a subclass of
;; `int'. The only members of the `bool' type are `True' and
;; `False'. In numeric contexts they have integer values 1 and 0,
;; respectively.
;; 
;; Some predicate functions return boolean values. They are printed as
;; `True' and `False', not as numbers.

(defclass py-bool (py-int)
  ((val :initform 0 :initarg :val :type bit))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-bool))

(defvar *True* (make-instance 'py-bool :val 1))
(defvar *False* (make-instance 'py-bool :val 0))

(defmethod py-bool-__new__ ((cls class) &rest args)
  ;; Contrary to int.__new__, bool.__new__ accepts any arg and will
  ;; check its truth value.
  (assert (subtypep cls 'py-bool))
  (if (eq cls (find-class 'py-bool))
      
      (cond ((null args) *False*)
	    ((cdr args) (py-raise 'TypeError "bool() takes at most one arg (got: ~A)" args))
	    (t  (if (py-val->lisp-bool (car args)) *True* *False*)))
    
    (let ((inst (make-instance cls)))
      (cond ((null args) (setf (slot-value inst 'val) 0))
	    (t (setf (slot-value inst 'val)
		 (if (py-val->lisp-bool (car args)) 1 0))))
      inst)))

(register-bi-class-attr/meth (find-class 'py-bool) '__new__
			     (make-static-method #'py-bool-__new__))


(defmethod make-instance ((c py-bool) &rest initargs &key val)
  (if val *True* *False*))

(defun lisp-val->py-bool (&optional val)
  "Make a BOOL (True of False) for given Lisp VAL."
  (check-type val (not python-object) "Lisp value, not a Python value")
  (if val *True* *False*))

(defun py-bool-p (x)
  (typep x 'py-bool))

(defun py-bool-designator-p (x)
  "Returns DESIGNATOR-P, LISP-VALUE (t/nil)"
  (cond ((typep x '(integer 0 1)) (values t (= x 1)))
	((typep x 'py-int) (let ((val (slot-value x 'val)))
			     (cond ((= val 1) (values t t))
				   ((= val 0) (values t nil))
				   (t nil))))
	(t nil)))

(defmethod __repr__ ((x py-bool))
  (if (eq x *True*) "True" "False"))


(defun py-val->lisp-bool (x)
  "VAL is either a Python value or one of the Lisp values T, NIL. ~@
   Returns a generalized Lisp boolean."
  (cond
   ;; T/NIL, True/False/None
   ((member x (load-time-value (list t *True*)) :test 'eq) t)
   ((member x (load-time-value (list nil *False* *None*)) :test 'eq) nil)
   
   ((numberp x) (/= x 0))
   ((stringp x) (not (string= x "")))

   (t (py-lisp-bool-1 x))))

(defun py-lisp-bool-1 (x)
  "Determine truth value of X, by trying the __nonzero__ and __len__ methods. ~@
   Returns a generalized Lisp boolean."
  
  (multiple-value-bind (val found)
      (call-attribute-via-class x '__nonzero__)
    
    (if found 
	(py-val->lisp-bool val)
  
      (multiple-value-bind (val found)
	  (call-attribute-via-class x '__len__)
	(if found 
	    (/= 0 (py-int-designator-val val))
	  ;; If a class defined neither __nonzero__ nor __len__, all
	  ;; instances are considered `True'.
	  t)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dictionary

(defun safe-py-hash (x)
  "ACL requires hash value to be fixnum: make sure it is."
  (assert (python-object-designator-p x) ()
    "Attempt to put a non-Python value in a Python dict: ~A" x)
  (let ((hash-value (call-attribute-via-class x '__hash__)))
    (assert (typep hash-value 'fixnum) () "Hash code should be fixnum! (~A ~A)"
	    x hash-value)
    hash-value))

    
(defclass py-dict (builtin-instance)
  ;; TODO: if there are only a very few items in the dict, represent
  ;; it as an alist or something similarly compact.
  ((hash-table :initform (make-hash-table :test 'py-==
					  :hash-function 'safe-py-hash)))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-dict))


(defmethod py-dict-__new__ ((cls class) &rest args)
  (assert (subtypep cls 'py-dict))
  (make-instance cls)) ;; :initform creates empty hashtable

(register-bi-class-attr/meth (find-class 'py-dict) '__new__
			     (make-static-method #'py-dict-__new__))


;; The __init__ of py-dict is exceptional, in that it takes keyword
;; arguments. (Nearly?) None of the other built-in functions do.

(defmethod py-dict-__init__ (pos-arg key-arg)
  
  (let ((x (car pos-arg)))
    (assert (typep x 'py-dict))
    (setf pos-arg (cdr pos-arg))
  
    ;; Either:   - no arguments (empty dict);
    ;;           - one positional argument (mapping)
    ;;           - keyword arguments: the content of the dict (only strings as keys)
    
    (cond ((not (or pos-arg key-arg)) )

	  ((and (car pos-arg)
		(not (cdr pos-arg))
		(not key-arg))
	 
	   (let ((src (car pos-arg))
		 (ht (slot-value x 'hash-table)))
	   
	     ;; Try it as mapping first, using  keys()  and  __getitem__()
	   
	     (multiple-value-bind (src-keys found)
		 (call-attribute-via-class src 'keys)

	       (if found
		   (let ((getitem-meth (getattr-of-class src '__getitem__)))
		     (map-over-py-object
		      (lambda (key) (let ((val (py-call getitem-meth (list src key))))
				      (setf (gethash key ht) val)))
		      src-keys))
	       
		 ;; Otherwise try it as iterable.
	       
		 (map-over-py-object
		  (lambda (item) (let ((kv (py-iterate->lisp-list item)))
				   (unless (= (length kv) 2)
				     (py-raise 'TypeError
					       "dict(seq): seq must contains elements ~@
                                              of length 2 (got: ~A for (key value))" kv))
				   (setf (gethash (first kv) ht) (second kv))))
		  src)))))
	
	  ((and key-arg
		(not pos-arg)) 
	   (loop with ht = (slot-value x 'hash-table)
	       for (k . v) in key-arg
	       do (setf (gethash (symbol-name k) ht) v)))
	  
	  (t (error "unhandled case in dict.__init__")))
    *None*))

(register-bi-class-attr/meth (find-class 'py-dict) '__init__
			     (make-lisp-function-accepting-kw-args #'py-dict-__init__))


(defun make-dict (&optional data)
  ;; data: alist '((key1 . val1)(key2 . val2)...)
  (let ((d (make-instance 'py-dict)))
    (when data
      (let ((ht (slot-value d 'hash-table)))
	(loop for (k . v) in data
	    do (setf (gethash k ht) v))))
    d))

(defmethod dict->alist ((x py-dict))
  ;; for internal use (`apply' in builtin-funcs.cl)
  (let ((res ()))
    (maphash (lambda (k v) (push (cons k v) res))
	     (slot-value x 'hash-table))
    res))


(defmethod __cmp__ ((x py-dict) (y py-dict))
  (when (eq x y)
    (return-from __cmp__ 0))
  
  (let* ((hx (slot-value x 'hash-table))
	 (hy (slot-value y 'hash-table))
	 (hcx (hash-table-count hx))
	 (hcy (hash-table-count hy)))
    
    (cond ((< hcx hcy) (return-from __cmp__ -1))
	  ((> hcx hcy) (return-from __cmp__ 1)))
    
    (maphash (lambda (key val-x)
	       (multiple-value-bind (val-y found)
		   (gethash key hy)
		 (if found
		     (let ((res (pyb:cmp val-x val-y)))
		       (cond ((= 0 res)) ; cont.
			     ((< 0 res) (return-from __cmp__ -1))
			     ((> 0 res) (return-from __cmp__ 1))))
		   (return-from __cmp__ 1))))
	     hx))
  0)

(defmethod __contains__ ((x py-dict) y)
  (maphash (lambda (k v)
	     (declare (ignore v))
	     (when (py-== k y)
	       (return-from __contains__ *True*)))
	   (slot-value x 'hash-table))
  *False*)


(defmethod __eq__ ((x py-dict) (y py-dict))
  "Returns T or NIL."
  (= (__cmp__ x y) 0))

(defmethod py-dict-gethash ((d py-dict) key)
  (gethash key (slot-value d 'hash-table)))

(defmethod py-dict-sethash ((d py-dict) key val)
  (setf (gethash key (slot-value d 'hash-table)) val))
		 
(defmethod __getitem__ ((d py-dict) key)
  (multiple-value-bind (val found)
      (gethash key (slot-value d 'hash-table))
    (if found
	val
      (py-raise 'KeyError "No such key: ~A" (py-str key)))))

(defmethod __iter__ ((d py-dict))
  (dict-iter-keys d))

(defmethod __setitem__ ((d py-dict) key val)
  (setf (gethash key (slot-value d 'hash-table) d) val))

(defmethod __delitem__ ((d py-dict) key)
  (remhash key (slot-value d 'hash-table)))
    
		 
;; dicts have no __add__

(defmethod __repr__ ((d py-dict))
  (with-output-to-string (s)
    (format s "{")
    (let ((is-first t))
      (maphash (lambda (key val)
		 (unless is-first
		   (write-string ", " s))
		 (format s "~A: ~A" (py-repr key) (py-repr val))
		 (setf is-first nil))
	       (slot-value d 'hash-table)))
    (format s "}")))

(defmethod __len__ ((d py-dict))
  (hash-table-count (slot-value d 'hash-table)))

(defmethod __nonzero__ ((d py-dict))
  (lisp-val->py-bool (/= 0 (hash-table-count (slot-value d 'hash-table)))))

(loop for name in '(__cmp__ __contains__ __eq__ __getitem__ __setitem__
		    __delitem__ __iter__ __len__ __nonzero__)
    do (register-bi-class-attr/meth (find-class 'py-dict) name (symbol-function name)))


;;;; Dict-specific methods, in alphabetic order

(defmethod dict-clear ((d py-dict))
  "Clear all items"
  (clrhash (slot-value d 'hash-table))
  (values))

(defmethod dict-copy ((d py-dict))
  "Create and return copy of dict. Keys and values themselves are shared, ~@
   but the underlying hash-table is different."
  (let* ((new (make-dict))
	 (new-ht (slot-value new 'hash-table)))
    (maphash (lambda (k v)
	       (setf (gethash k new-ht) v))
	     (slot-value d 'hash-table))
    new))

(defmethod dict-fromkeys (seq &optional (val *None*))
  (let* ((d (make-dict))
	 (ht (slot-value d 'hash-table)))
    (map-over-py-object (lambda (key) (setf (gethash key ht) val)) seq)
    d))

(defmethod dict-get ((d py-dict) key &optional (defval *None*))
  "Lookup KEY and return its val, otherwise return DEFVAL"
  (multiple-value-bind (val found-p)
      (gethash key (slot-value d 'hash-table))
    (if found-p
	val
      defval)))

(defmethod dict-has-key ((d py-dict) key)
  "Predicate"
  (multiple-value-bind (val found-p)
      (gethash key (slot-value d 'hash-table))
    (declare (ignore val))
    (lisp-val->py-bool found-p)))

(defmethod dict-items ((d py-dict))
  "Return list of (k,v) tuples"
  (let* ((h (slot-value d 'hash-table))
	 (res ()))
    (maphash (lambda (k v) (push (make-tuple (make-array 2 :initial-contents (list k v)))
				 res))
	     h)
    (make-py-list-from-list res)))

(defmethod dict-iter-items ((d py-dict))
  "Return iterator that successively returns all (k,v) pairs as tuple"
  (let ((res (with-hash-table-iterator (next-f (slot-value d 'hash-table))
	       (make-iterator-from-function
		(lambda () 
		  (multiple-value-bind (ret key val) 
		      (next-f)
		    (when ret
		      (make-tuple (make-array 2 :initial-contents (list key val))))))))))
    res))

(defmethod dict-iter-keys ((d py-dict))
  "Return iterator that successively returns all keys"
  (let ((res (with-hash-table-iterator (next-f (slot-value d 'hash-table))
	       (make-iterator-from-function
		(lambda () (multiple-value-bind (ret key val) 
			       (next-f)
			     (declare (ignore val))
			     (when ret
			       (if (symbolp key)
				   (symbol-name key)
				 key))))))))
    res))

(defmethod dict-iter-values ((d py-dict))
  "Return iterator that successively returns all values"
  (let ((res (with-hash-table-iterator (next-f (slot-value d 'hash-table))
	       (make-iterator-from-function
		(lambda () 
		  (multiple-value-bind (ret key val) 
		      (next-f)
		    (declare (ignore key))
		    (when ret
		      val)))))))
    res))

(defmethod dict-keys ((d py-dict))
  "List of all keys"
  (let* ((h (slot-value d 'hash-table))
	 (res ()))
    (maphash (lambda (k v)
	       (declare (ignore v))
	       (push k res))
	     h)
    (make-py-list-from-list res)))

(defmethod dict-pop ((d py-dict) key &optional (default nil default-p))
  "Remove KEY from D, returning its value. If KEY absent, DEFAULT ~
   is returned or KeyError is raised."
  (with-slots (hash-table) d
    (multiple-value-bind (val found)
	(gethash key hash-table)
      (cond (found (remhash key hash-table)
		   val)
	    (default-p default)
	    (t (py-raise 'KeyError "No key ~A in dict" key))))))

(defmethod dict-popitem ((d py-dict))
  (with-slots (hash-table) d
    (with-hash-table-iterator (iter hash-table)
      (multiple-value-bind (entry? key val)
	  (iter)
	(if entry?
	    (progn
	      (remhash key hash-table)
	      (make-tuple (make-array 2 :initial-contents (list key val))))
	  (py-raise 'KeyError "popitem: dictionary is empty"))))))

(defmethod dict-setdefault ((d py-dict) key &optional (defval *None*))
  "Lookup KEY and return its val;
   If KEY doesn't exist, add it and set its val to DEFVAL, then return DEFVAL"
  (multiple-value-bind (val found-p)
      (gethash key (slot-value d 'hash-table))
    (if found-p
	val
      (setf (gethash key (slot-value d 'hash-table)) defval))))

(defmethod dict-values ((d py-dict))
  "List of all values"
  (let* ((h (slot-value d 'hash-table))
	 (res ()))
    (maphash (lambda (k v)
	       (declare (ignore k))
	       (push v res))
	     h)
    (make-py-list-from-list res)))

(loop for (k v) in `((clear      ,#'dict-clear)
		     (copy       ,#'dict-copy)
		     (fromkeys   ,(make-static-method #'dict-fromkeys))
		     (get        ,#'dict-get)
		     (has_key    ,#'dict-has-key)
		     (items      ,#'dict-items)
		     (iteritems  ,#'dict-iter-items)
		     (iterkeys   ,#'dict-iter-keys)
		     (itervalues ,#'dict-iter-values)
		     (keys       ,#'dict-keys)
		     (pop        ,#'dict-pop)
		     (popitem    ,#'dict-popitem)
		     (setdefault ,#'dict-setdefault)
		     (values     ,#'dict-values))
    do (register-bi-class-attr/meth (find-class 'py-dict) k v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Namespace
;; 
;; The methods and attributes of a class, and the lexical scope inside
;; a function, are represented by namespace objects. A namespace
;; behaves like a py-dict, except that all its keys are SYMBOLS.
;; 
;; Compared to dics, namespaces have extra attributes `name' (for
;; debugging, mostly) and `enclosing-ns' (referring to the namespace
;; in which this namespace is enclosed: for classes defined at
;; top-level, this is the module namespace).
;; 
;; When a key doesn't exist, no KeyError is raised (as py-dict does);
;; instead, (nil nil) are returned as values.
;; 
;; (This class might correlate to CPython's Dictproxy, not sure to
;; what degree. Dictproxies don't allow manipulation by the user
;; directly, so d.__getitem__ and d.__setitem__ don't work, although
;; d.items() does.)

#+(or)
(defclass namespace (py-dict)
  ((name :initarg :name :type string)
   (enclosing-ns :initarg :inside :initform nil)
   (hash-table :initform (make-hash-table :test 'eq)))
  (:metaclass builtin-class))

#+(or)
(mop:finalize-inheritance (find-class 'namespace))


#+(or)
(defun make-namespace (&key inside name builtins)

  "Make a new namespace.
   BUILTINS indicates whether attribute `__builtins__ should ~
     be created and pointed to the namespace with built-in functions
     `<__builtin__ module>.__dict__', available as *__builtin__-module-namespace*.
   INSIDE gives the enclosing scope(s)."

  (let ((ns (make-instance 'namespace :name name :inside inside)))
    (declare (special *__builtin__-module-namespace*))
    (when builtins
      (namespace-bind ns '__builtins__ *__builtin__-module-namespace*))
    ns))

#+(or) ;; this is an optimization: re-use old namespaces
(defmethod make-namespace-from-namespace ((x namespace) &key inside name builtins)
  (declare (special *__builtin__-module-namespace*))
  (setf (slot-value x 'name) name
	(slot-value x 'enclosing-scope) inside)
  (when builtins
    (namespace-bind x '__builtins__ *__builtin__-module-namespace*))
  x)


;; Be paranoid regarding the rule that all keys are symbols. This
;; becomes important when the __dict__ objects is read directly, or
;; assigned to directly (like "x.__dict__ = ...").

#+(or)
(defmethod check-only-symbol-keys ((x namespace))
  #+(or) ;; useful for debugging
  (maphash (lambda (k v)
	     (declare (ignore v))
	     (check-type k symbol))
	   (slot-value x 'hash-table)))

#+(or)
(defmethod namespace-bind ((x namespace) var val)
  (ensure-py-type var attribute-name "Invalid attribute name: ~A")
  (check-type var symbol)
  #+(or)(check-only-symbol-keys x)
  (setf (gethash var (slot-value x 'hash-table)) val))

#+(or)
(defmethod namespace-lookup ((x namespace) (var symbol))
  "Recursive lookup. Returns two values:  VAL, FOUND-P"
  #+(or)(check-only-symbol-keys x)
  (multiple-value-bind (val found) ;; m-v-b, as NIL is valid VAR
      (gethash var (slot-value x 'hash-table))
    (cond (found                        (values val t))
	  ((slot-value x 'enclosing-ns) (namespace-lookup
					 (slot-value x 'enclosing-ns) var))
	  (t                            nil))))

#+(or)
(defmethod namespace-delete ((x namespace) (var symbol))
  "Delete the attribute."
  ;; todo: when in an enclosing namespace
  (check-type var symbol)
  #+(or)(check-only-symbol-keys x)
  (let ((res (remhash var (slot-value x 'hash-table))))
    (unless res
      (py-raise 'NameError
		"No variable with name ~A" var))))

#+(or)
(defmethod namespace-declare-global ((x namespace) (var-name symbol))
  (let* ((module-namespace (loop with namespace = x
			       with encl-ns = (slot-value namespace 'enclosing-ns)
			       while encl-ns do
				 (setf namespace encl-ns
				       encl-ns (slot-value namespace 'enclosing-ns))
			       finally (return namespace))))
    
    (if (eq module-namespace x)
	(warn "Bogus top-level 'global' declaration for variable ~A" var-name)
    
      (let ((lookup-meth (make-instance 'standard-method
			   :specializers (list (mop:intern-eql-specializer x)
					       (mop:intern-eql-specializer var-name))
			   :lambda-list '(x var)
			   :function (lambda (x var)
				       (declare (ignore var x))
				       (namespace-lookup module-namespace var-name))))
	 
	    (bind-meth (make-instance 'standard-method
			 :specializers (list (mop:intern-eql-specializer x)
					     (mop:intern-eql-specializer var-name)
					     (find-class 't))
			 :lambda-list '(x var val)
			 :function (lambda (x var val)
				     (declare (ignore x var))
				     (namespace-bind module-namespace var-name val)))))
			 
	(add-method (ensure-generic-function 'namespace-lookup) lookup-meth)
	(add-method (ensure-generic-function 'namespace-bind)   bind-meth)))))
  
#+(or)
(defmethod namespace-copy ((x namespace))
  #+(or)(check-only-symbol-keys x)
  (with-slots (name enclosing-ns hash-table) x
    (let* ((x-copy (make-namespace :inside enclosing-ns
				   :name name))
	   (ht-copy (slot-value x-copy 'hash-table)))
      (clrhash ht-copy)
      (maphash (lambda (k v) (setf (gethash k ht-copy) v))
	       hash-table)
      x-copy)))

#+(or)
(defmethod namespace-clear ((x namespace))
  (clrhash (slot-value x 'hash-table))
  (setf (slot-value x 'enclosing-ns) nil))

;; py-dict-like methods

#+(or)
(defmethod __getitem__ ((x namespace) key)
  "Contrary to PY-DICT, does not raise KeyError."
  #+(or)(check-only-symbol-keys x)
  (cond ((symbolp key))
	((stringp key) (setf key (intern key #.*package*)))
	((typep key 'py-string) (setf key (intern (slot-value key 'val) #.*package*)))
	(t (error "__getitem__ for namespace wants symbol (or string) KEY, got: ~A" key)))
  (check-type key symbol)
  (gethash key (slot-value x 'hash-table)))

#+(or)
(defmethod __repr__ ((x namespace))
  #+(or)(check-only-symbol-keys x)
  (with-output-to-string (stream)
    (pprint-logical-block (stream nil)
      (format stream "{")
      (maphash (lambda (k v) (format stream "'~A': ~A,~_ " k (__repr__ v)))
	       (slot-value x 'hash-table)))
    (format stream "}")))

;; XXX register?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mapping
;; 
;; Abstract class, superclass for PY-ALIST and PY-DICT.
;; 
;; Calls these methods for the actual mutating work:
;;  __setitem__

(defclass py-mapping (builtin-instance)
  ()
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-mapping))


;; default __new__

(defmethod py-mapping-__init__ (pos-arg key-arg)
  (let ((x (car pos-arg)))
    (assert (typep x 'py-mapping))
    (setf pos-arg (cdr pos-arg))
  
    ;; Either:   - no arguments (empty mapping);
    ;;           - one positional argument (mapping object)
    ;;           - keyword arguments: the key-values of the mapping
    
    (cond ((not (or pos-arg key-arg)) )

	  ((and (car pos-arg) (not (cdr pos-arg)) (not key-arg))

	   ;; Try it as mapping first, using  keys()  and  __getitem__().
	   ;; If that fails because the methods are not present, try as iterable.
	   
	   (let* ((src (car pos-arg))
		  (src-keys (call-attribute-via-class src 'keys))
		  (getitem-meth (and src-keys (getattr-of-class src '__getitem__))))
	     (if getitem-meth 
		 
		 (map-over-py-object
		  (lambda (key) (__setitem__ x key (py-call getitem-meth (list key))))
		  src-keys)
	       
	       (map-over-py-object
		(lambda (item)
		  (typecase item
		    
		    (py-list/tuple ;; common case
		     (let ((v (slot-value item 'vec)))
		       (if (= (length v) 2)
			   (__setitem__ x (aref v 0) (aref v 1))
			 (py-raise 'TypeError
				   "dict(seq): seq must contains elements ~@
                                    of length 2 (got: ~A for #(key value))" v))))
		    
		    (t (let* ((iter-f (get-py-iterate-fun item))
			      (key (funcall iter-f))
			      (val (and key (funcall iter-f)))
			      (third (and val (funcall iter-f))))
			 (when (and val (not third))
			   (__setitem__ x key val))))))
		src))))))
  *None*)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A-List
;; 
;; As a replacement for dictionary.

(defclass py-alist (py-mapping)
  ((alist :initform () :initarg :alist))
  (:metaclass builtin-class))

(defmethod __getitem__ ((x py-alist) key)
  (with-slots (alist) x
    (or (cdr (assoc key alist :test #'py-==))
	(py-raise 'KeyError "No such key: ~A" (py-str key)))))

(defmethod __setitem__ ((x py-alist) key val)
  (with-slots (alist) x
    (let ((k (assoc key alist :test #'py-==)))
      (if k
	  (setf (cdr k) val)
	(let ((k (cons key val)))
	  (push k alist))))))

(defmethod __delitem__ ((x py-alist) key)
  (with-slots (alist) x
    (if (py-== (caar alist) key)
	(setf alist (cdr alist))
      (loop with prev = alist
	  with curr = (cdr alist)
	  do (cond ((null curr) )
		   ((py-== (caar curr) key)
		    (setf (cdr prev) (cdr curr))
		    (return))
		   (t (setf prev curr
			    curr (cdr curr))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EQ A-list
;; 
;; Used for implementing namespaces. 
;; Differences with py-alist:
;;  - returns NIL for not found
;;  - uses EQ as test, not PY-==

(defclass py-eq-alist (py-alist)
  ()
  (:metaclass builtin-class))

(defun make-py-eq-alist ()
  (make-instance 'py-eq-alist))

(defmethod __getitem__ ((x py-eq-alist) (key symbol))
  (with-slots (alist) x
    (cdr (assoc key alist))))

(defmethod __setitem__ ((x py-eq-alist) (key symbol) val)
  (with-slots (alist) x
    (let ((k (assoc key alist :test 'eq)))
      (if k
	  (setf (cdr k) val)
	(let ((k (cons key val)))
	  (push k alist))))))

(defmethod __delitem__ ((x py-eq-alist) (key symbol))
  (with-slots (alist) x
    (if (eq (caar alist) key)
	(setf alist (cdr alist))
      (loop with prev = alist
	  with curr = (cdr alist)
	  do (cond ((null curr) (return))
		   ((eq (caar curr) key)
		    (setf (cdr prev) (cdr curr))
		    (return))
		   (t (setf prev curr
			    curr (cdr curr))))))))

(defstruct (al-namespace #+(or)(:type :vector))
  (enclosing-ns nil)
  (alist        nil)
  (global-ns    nil)
  (builtins     nil))
	   
(defun make-namespace (&key inside name builtins)
  ;;(declare (ignore name))
  ;;(format t "make-namespace ~A  in: ~A  bi: ~A~%" name inside builtins)
  (let ((ns (make-al-namespace :enclosing-ns inside)))
    (when builtins
      (locally (declare (special *__builtin__-module-namespace*))
	(namespace-bind ns '__builtins__ *__builtin__-module-namespace*)
	(setf (al-namespace-builtins ns) *__builtin__-module-namespace*)))
    ns))

(defun namespace-bind (ns key val)
  (assert (symbolp key))
  ;;(format t "binding ~A -> ~A~%" key val)
  (let* ((alist (al-namespace-alist ns))
	 (k (assoc key alist :test 'eq)))
    (if k
	(if (eq (cdr k) :global)
	    (namespace-bind (al-namespace-global-ns ns) key val)
	  (setf (cdr k) val))
      (setf (al-namespace-alist ns) (cons (cons key val) alist)))))

(defun namespace-lookup (ns key)
  ;;(format t "lookup ~A~%" key)
  ;;(break)
  (let ((k (assoc key (al-namespace-alist ns) :test 'eq)))
    (if k
	(if (eq (cdr k) :global)
	    (namespace-lookup (al-namespace-global-ns ns) key)
	  (values (cdr k) t))
      (when (al-namespace-enclosing-ns ns)
	;;(format t "trying enclosing for ~A~%" key)
	(namespace-lookup (al-namespace-enclosing-ns ns) key)))))

(defun namespace-delete (ns key)
  (let ((alist (al-namespace-alist ns)))
    (if (eq (caar alist) key)
	(setf (al-namespace-alist ns) (cdr alist))
      (loop with prev = alist
	  with curr = (cdr alist)
	  do (cond ((null curr) (return))
		   ((eq (caar curr) key)
		    (setf (cdr prev) (cdr curr))
		    (return))
		   (t (setf prev curr
			    curr (cdr curr))))))))

(defun namespace-declare-global (ns key)
  ;;(format t "ns-decl-global~%")
  ;; Asumes name not already bound or declare global
  (setf (al-namespace-alist ns) 
    (cons (cons key :global) (al-namespace-alist ns)))
  (unless (al-namespace-global-ns ns)
    (loop with curr-ns = ns
	with enc = (al-namespace-enclosing-ns curr-ns)
	while enc do
	  (setf curr-ns enc
		enc (al-namespace-enclosing-ns curr-ns))
	finally (assert curr-ns)
		(setf (al-namespace-global-ns ns) curr-ns))))

(defun namespace-copy (ns)
  (let ((new-ns (make-al-namespace :enclosing-ns (al-namespace-enclosing-ns ns))))
    (let ((bi-ns (al-namespace-builtins ns)))
      (when bi-ns
	(namespace-bind new-ns '__builtins__ bi-ns)
	(setf (al-namespace-builtins new-ns) bi-ns)))
    new-ns))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function

(defmethod __get__ ((x function) inst class)
  (if (eq inst *None*)
      (make-unbound-method :func x :class class) ;; <Class>.meth
    (make-bound-method :func x :object inst))) ;; <instance>.meth

(defmethod __repr__ ((x function))
  (with-output-to-string (s)
    (print-unreadable-object (x s :identity t :type t))))

(defmethod __repr__ ((x generic-function))
  (with-output-to-string (s)
    (print-unreadable-object (x s :identity t :type t)
      (format s "~A" (mop:generic-function-name x)))))


(register-bi-class-attr/meth (find-class 'function) '__get__ #'__get__)


;; Python-function

(defmethod print-object ((x python-function) stream)
  (print-unreadable-object (x stream :identity t :type t)
    (format stream "~A" (slot-value x 'name))))

(defmethod __class__ ((x python-function)) (find-class 'python-function))

(defmethod py-function-name ((x python-function))
  (slot-value x 'name))

(register-bi-class-attr/meth (find-class 'python-function) '__name__
			     (make-bi-class-attribute #'py-function-name))

;; TODO: __new__, __init__

(defmethod __get__ ((x python-function) inst class)
  (if (eq inst *None*) ;; Hmm what if class of None were subclassable?!
      (make-unbound-method :func x :class class) ;; <Class>.meth
    (make-bound-method :func x :object inst))) ;; <instance>.meth

(register-bi-class-attr/meth (find-class 'python-function) '__get__ #'__get__)


;; Lambda

(defun make-lambda-function (&rest options)
  (apply #'make-instance 'py-lambda-function options))

(defmethod __repr__ ((x py-lambda-function))
  (with-output-to-string (s)
    (print-unreadable-object (x s :type t :identity t))))


;; User-defined-function

(defmethod __repr__ ((x user-defined-function))
  (with-output-to-string (s)
    (print-unreadable-object (x s :type t :identity t)
      (format s "~A" (slot-value x 'name)))))

(defun make-user-defined-function (&rest options &key namespace &allow-other-keys)
  "Make a python function"
  (declare (ignorable namespace))
  #+(or)(check-type namespace namespace)
  (apply #'make-instance 'user-defined-function options))


;; Python-function-returning-generator

(defun make-python-function-returning-generator (fname params ast)
  (make-instance 'python-function-returning-generator
    :name fname
    :call-rewriter (apply #'make-call-rewriter fname params)
    :generator-creator (let ((lambda-lst (create-generator-function ast)))
			 #+(or)(break "lambda-lst ~A" lambda-lst)
			 (check-type lambda-lst list)
			 (compile nil lambda-lst))))

(defmethod __get__ ((x python-function-returning-generator) inst class)
  (declare (special *None*))
  (if (eq inst *None*)
      (make-unbound-method :func x :class class)
    (make-bound-method :func x :object inst)))

(register-bi-class-attr/meth (find-class 'python-function-returning-generator)
			     '__get__ #'__get__)

(defmethod __name__ ((x python-function-returning-generator))
  (slot-value x 'name))

(register-bi-class-attr/meth (find-class 'python-function-returning-generator)
			     '__name__ (make-bi-class-attribute #'__name__))
 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package/Module
;; 
;; A package is the same as a module, only behaviour regarding
;; `import' is a different.

(defclass py-package/module (builtin-instance)
  ((namespace :initarg :namespace
	      #+py-namespace-dict :type #+py-namespace-dict namespace))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-package/module))

(defmethod package/module-dict ((x py-package/module))
  (slot-value x 'namespace))

#+(or)
(defmethod namespace-lookup ((x py-package/module) var)
  (namespace-lookup (slot-value x 'namespace) var))

#+(or)
(register-bi-class-attr/meth (find-class 'py-package/module) '__dict__
			     (make-bi-class-attribute #'package/module-dict))

(defmethod __repr__ ((x py-package/module))
  (with-output-to-string (stream)
    (print-unreadable-object (x stream :type t)
      (with-slots (namespace module) x
	(let ((name (namespace-lookup namespace '__name__))
	      (file (namespace-lookup namespace '__file__)))
	  (format stream "~A" (or name ""))
	  (when file
	    (format stream " from file ~A" file)))))))


;;; Module

(defclass py-module (py-package/module)
  ((module :initarg :module))
  (:metaclass builtin-class))

(defun make-py-module (&rest options)
  (apply #'make-instance 'py-module options))


;;; Package

(defclass py-package (py-package/module)
  ((init-file :initarg :init-file)
   (directory :initarg :directory))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-package))

(defun make-py-package (&rest options)
  (apply #'make-instance 'py-package options))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods and attributes of Methods

(defmethod __get__ ((x class-method) inst class)
  ;; Not sure if CLASS is a required argument or if it may be None,
  ;; so take into account the situation in which it is not provided.
  (let ((klass (if (eq class *None*)
		   (__class__ inst)
		 class)))
    (make-bound-method :func (slot-value x 'func)
		       :object klass)))

(register-bi-class-attr/meth (find-class 'class-method) '__get__ #'__get__)
  

#+(or)
(defmethod __get__ ((x bi-class-attribute) inst class)
  ;; It's an attribute of the instance, not a method of the class.
  (declare (ignore class))
  (assert (not (eq inst *None*)))
  (py-call (slot-value x 'func) (list inst)))

(defmethod __get__ ((x bi-class-attribute) inst class)
  ;; It's an attribute of the instance, not a method of the class.
  (when (eq class *None*)
    (error "Attribute is only for instances of class ~A, not the class itself" class))
  (py-call (slot-value x 'func) (list inst)))


(register-bi-class-attr/meth (find-class 'bi-class-attribute) '__get__ #'__get__)


(defmethod __get__ ((x static-method) inst class)
  ;; whether looked up via class or instance, underlying value is returned unbound
  (declare (ignore inst class))
  (slot-value x 'func))

(register-bi-class-attr/meth (find-class 'static-method) '__get__ #'__get__)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slice object
;; 
;; Denotes sub-items of a sequence-like object.
;; 
;; slice( [start,] stop [, step )
;;
;; foo[a] -> foo.__getitem__(a)
;; foo[a:b] -> foo.__getitem__( slice(a,b,None) )
;; foo[a:b:c] -> foo.__getitem__( slice(a,b,c) )
;; 
;; However, there's interplay with `Ellipsis':
;; 
;; foo[1:2,...,4:3]
;;  -> foo.__getitem__(
;;           (slice(1, 2, None), Ellipsis, slice(4, 3, None))  <-- tuple!
;;     )

(defclass py-slice (builtin-instance)
  ((start :initarg :start) ;; these can be any type
   (stop  :initarg :stop)
   (step :initarg :step))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-slice))

(defun make-slice (x &optional y z)
  ;; X,Y,Z don't have to be integers
  (cond (z (make-instance 'py-slice  :start x       :stop y  :step z))
	(y (make-instance 'py-slice  :start x       :stop y  :step *None*))
	(t (make-instance 'py-slice  :start *None*  :stop x  :step *None*))))

(defmethod print-object ((x py-slice) stream)
  (print-unreadable-object (x stream :type t)
    (with-slots (start stop step) x
      (format stream ":start ~A  :stop ~A  :step ~A" start stop step))))

(defmethod indices ((x py-slice) length)
  "Return tuple with three integers: start, stop, step.~@
   In case of empty range, returns (length,length,1)."
  (multiple-value-bind (start stop step)
      (slice-indices x length)
    (make-tuple (make-array 3 :initial-content (list start stop step)))))

;; Function SLICE-INDICES returns multiple values, best explained by example:
;; Assume x = [0,1,2] so LENGTH = 3
;; 
;;  -slice- -extr- -assignment-                -values- 
;;  x[:0]    []     x[:0] = [42] => [42,0,1,2] :empty-slice-before
;;  x[3:]    []     x[3:] = [42] => [0,1,2,42] :empty-slice-after
;;  x[1:1]   []     x[1:1] = [42]=> [0,42,1,2] :empty-slice-between 0 1  = BEFORE-I, AFTER-I
;;  x[1:2]   [1]    x[1:2] = [42]=> [0,42,2]   :nonempty-slice 1 1       = FIRST-I, LAST-I
;;  x[1:]    [1,2]  x[1:] = [42] => [0,42]     :nonempty-slice 1 2
;;  x[:1]    [0]    x[:1] = [42] => [42,1 2]   :nonempty-slice 0 0
;;  x[:] [0,1,2](copy) x[:] = [42] => [42](modifies) :nonempty-slice 0 2
;;  x[2:1]   []     (error)                    :empty-slice-bogus
;;  x[100:200] []                              :empty-slice-bogus
;; 
;; These have implicit step=1. Other steps result in a `stepped-slice'
;; Now with other step values, using x = range(10) = [0,1,2,3,4,5,6,7,8,9] so LENGTH = 10
;;
;;  -slice-   -extr-       -assignment-                -values- 
;;  x[::0]    - - - - - step=0 is error - - - - - - - - - - - - - - - -
;;  x[::2]    [0,2,4,6,8]  :extended-slice 0 9 2       = FIRST LAST STEP
;;  x[::-2]   [9,7,5,4,1]  x[..] = [1,2,3,4,5] =>      :nonempty-stepped-slice 9 1 -2
;;                           [0, 5, 2, 4, 4, 3, 6, 2, 8, 1]
;;  x[0:2:-1] []           (error)                     (error)
;;  x[100:200:2] []        (error)                     :empty-slice-bogus
;;  x[0:0:4]  []           x[..] = [] => ok            :empty-slice-bogus
;;  x[0:1:4]  [0]          x[..] = [42] => [42, 2, ..] :nonempty-stepped-slice 0 0 4
;; 
;; Assigning to a stepped-slice can only when the thing assigned
;; containes the same number of items as teh stepped slice. For an
;; empty-slice-bogus, this means only the empty list (or another
;; iterable containing 0 items) can be assigned to it.


(defun slice-test ()
  (loop with num-err = 0
      for ((start stop step) length values) in
	'(( (0 5 1) 10 (:nonempty-slice 0 4 5))
	  ( (0 0 1) 10 (:empty-slice-before))
	  ( (-100 0 1) 10 (:empty-slice-before))
	  ( (0 1 2) 10 (:nonempty-stepped-slice 0 0 2 1))
	  ( (4 4 1) 4  (:empty-slice-after))
	  ( (nil nil 1) 10 (:nonempty-slice 0 9 10))
	  ( (nil 1 1) 10 (:nonempty-slice 0 0 1))
	  ( (-1 nil 1) 10 (:nonempty-slice 9 9 1))
	  ( (-2 nil 1) 10 (:nonempty-slice 8 9 2))
	  ( (0 nil nil) 10 (:nonempty-slice 0 9 10))
	  ( (0 0 2) 10 (:empty-slice-bogus))
	  ( (0 3 2) 10 (:nonempty-stepped-slice 0 2 2 2))
	  ( (4 2 1) 10 (:empty-slice-bogus))
	  ( (-100 -99 2) 10 (:empty-slice-bogus))
	  ( (0 10 3) 10 (:nonempty-stepped-slice 0 9 3 4))
	  ( (0 10 -3) 10 (:empty-slice-bogus))
	  ( (10 0 -3) 10 (:nonempty-stepped-slice 9 3 -3 3))
	  ( (10 1 -3) 10 (:nonempty-stepped-slice 9 3 -3 3))
	  ( (11 1 -3) 10 (:nonempty-stepped-slice 9 3 -3 3))
	  ( (4 0 -1) 10 (:nonempty-stepped-slice 4 1 -1 4))
	  ( (4 nil -1) 10 (:nonempty-stepped-slice 4 0 -1 5))
	  )
	
      do (warn "trying: (~A ~A ~A) ~A" start stop step length)
	 (let ((res (multiple-value-list (slice-indices (make-slice (or start *None*)
								    (or stop *None*)
								    (or step *None*))
							length))))
	   (if (equal res values)
	       (warn "(~A ~A ~A) ~A gave ~S  OK" start stop step length res values)
	     (progn
	       (warn "(~A ~A ~A) ~A gave ~S, expected ~S" start stop step length res values)
	       (incf num-err)))
	   (format t "~%"))
      finally 
	(warn "~A errors" num-err)))

#+(or)
(slice-test)
		    

(defmethod slice-indices ((x py-slice) (length integer))
  (let* ((start (slot-value x 'start)) ;; not with-slots, as we change them
	 (stop  (slot-value x 'stop))
	 (step  (slot-value x 'step)))
    
    (setf step (if (eq step *None*) 1 (py-int-designator-val step)))
    
    (let ((reversed (< step 0)))
      
      (setf start (if (eq start *None*) 
		      (if reversed (1- length) 0)
		    (py-int-designator-val start))
	    stop  (if (eq stop *None*)
		      (if reversed -1 length)
		    (py-int-designator-val stop)))
      #+(or)
      (if reversed
	  (warn "indices (1):  (~A : ~A] step ~A" start stop step)
	(warn "indices (1):  [~A : ~A) step ~A" start stop step))
    
      (when (< start 0) (incf start length))
      (when (< stop  0) (unless (and reversed (eq stop -1)) ;; XXX right?
			  (incf stop  length)))
      
      #+(or)
      (if (> step 0)
	  (warn "indices (2):  [~A : ~A) step ~A" start stop step)
	(warn "indices (2):  (~A : ~A] step ~A" start stop step))
    
      (cond ((= step 0)
	     (py-raise 'ValueError "Slice step cannot be zero"))
	  
	    ((or (and (> step 0) (or (> start length)
				     (< stop 0)
				     (> start stop)))
		 (and (< step 0) (or (< start 0)
				     (>= stop length)
				     (< start stop))))
	     (values :empty-slice-bogus))
	  
	    ((= step 1)
	     (setf start (max 0 start)
		   stop (min length stop))
	     (cond ((and (= start stop) (<= 0 start stop length))
		    (cond ((= start 0)      (values :empty-slice-before))
			  ((= start length) (values :empty-slice-after))
			  (t                (values :empty-slice-between (1- start) start))))
	   
		   ((and (<= 0 start stop length) (< start stop))
		    (values :nonempty-slice start (1- stop) (- stop start)))
	   
		   (t (break "never here: start=~A stop=~A step=~A length=~A" start stop step length))))
	  
	    ((/= step 1)
	     (cond ((= start stop) (values :empty-slice-bogus))
		 
		   ((and (> step 0) (< start stop))
		    (let* ((start (max start 0))
			   (stop  (min stop (1- length)))
			   (num-increments (floor (- stop start) step))
			   (real-stop (+ start (* step num-increments))))
		      (unless (<= 0 start real-stop stop (1- length))
			(break "not (<= 0 start real-stop stop (1- length)): sta:~A sto:~A rsto:~A 1-len:~A"
			      start real-stop stop (1- length)))
		      (values :nonempty-stepped-slice start real-stop step (1+ num-increments))))
		 
		   ((and (< step 0) (> start stop))
		    (let* ((start (min start (1- length)))
			   (stop  (max (incf stop) 0))
			   (num-increments (floor (- stop start) step))
			   (real-stop (- start (* (- step) num-increments))))
		      (unless (<= 0 stop real-stop start (1- length))
			(break "not (<= 0 stop real-stop start length): sto:~A rs:~A sta:~A 1-len:~A"
			      stop real-stop start (1- length)))
		      (values :nonempty-stepped-slice start real-stop step (1+ num-increments))))
		 
		   (t (break "never here 2: start=~A stop=~A step=~A length=~A" start stop step length))))))))


(defmethod extract-list-slice ((list cons) (slice py-slice))
  "Given a (Lisp) list, extract the sublist corresponding to the slice as a fresh Lisp list."
  (multiple-value-bind (nonempty start stop step)
      (slice-indices slice (length list))
    
    (unless nonempty
      (return-from extract-list-slice ()))
    
    (let ((in-reverse (< step 0)))
      
      (when in-reverse
	(rotatef start stop)
	(setf step (* -1 step)))
      
      (let* ((current (subseq list start (1+ stop)))
	     (acc ())
	     (i start))
	(loop
	  (push (car current) acc)
	  (cond ((= i stop) (return-from extract-list-slice
			      (if in-reverse
				  acc
				(nreverse acc))))
		((null current) (error "internal error: slice indices incorrect")))
	  (setf current (nthcdr step current))
	  (incf i step))))))


(defmethod extract-list-item-by-index ((list cons) (index integer))
  ;; todo: support subclassed integers
  (ensure-py-type index integer
		  "internal error: ~A")
  (let ((len (length list)))
    
    (when (< index 0)
      (incf index len))
    
    (when (or (< index 0)
	      (> index (1- len)))
      (py-raise 'IndexError
		"List index out of range (got: ~A, valid range: 0..~A)"
		index (- len 1)))
    
    (nth index list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Superclass shared by tuple and list

(defclass py-list/tuple (builtin-instance)
  ((vec :type vector :initarg :vec))
  (:metaclass builtin-class))

(defmethod __cmp__ ((x py-list/tuple) (y py-list/tuple))
  (__cmp__-vectors (slot-value x 'vec) (slot-value y 'vec)))

(defmethod __cmp__-vectors ((x vector) (y vector))
  "If all elements eq, longest wins, otherwise they are eq."
  (let ((x-len (length x)) (y-len (length y)))
    (cond ((= x-len y-len) (loop for xi across x 
			       for yi across y
			       do (unless (eq xi yi)
				    (let ((res (pyb:cmp xi yi)))
				      (cond ((= res 0)) ;; continue...
					    ((< res 0) (return-from __cmp__-vectors -1))
					    ((> res 0) (return-from __cmp__-vectors 1)))))
			       finally (return-from __cmp__-vectors 0)))
	  ((< x-len y-len) -1)
	  (t                1))))

(defmethod __contains__ ((x py-list/tuple) item)
  (if (member item (slot-value x 'vec) :test #'py-==) *True* *False*))

(defmethod __eq__-vectors ((x vector) (y vector))
  (if (= (length x) (length y))
      (if (mismatch x y :test (lambda (a b) (or (eq a b) (py-val->lisp-bool (py-== a b)))))
	  *False*
	*True*)
    *False*))

(defmethod __eq__ ((x py-list/tuple) y)
  (declare (ignore y))
  *False*)

(defmethod __getitem__ ((x py-list/tuple) item)
  ;; Returns a list
  (let ((vec (slot-value x 'vec)))
    (typecase item
      
      (py-int-designator
       (let ((i (py-int-designator-val item)))
	 (when (< i 0)
	   (incf i (length vec)))
	 (if (<= 0 i (1- (length vec)))
	     (aref vec i)
	   (py-raise 'IndexError "List index out of bounds (got: ~A, len: ~A)"
		     i (length vec)))))
      
      (py-slice
       (multiple-value-bind (kind p1 p2 p3 p4) (slice-indices item (length vec))
	 (ecase kind
	   
	   ((:empty-slice-before :empty-slice-after :empty-slice-between :empty-slice-bogus)
	    (make-empty-py-list))
	   
	   (:nonempty-slice
	    (let* ((first-i p1)
		   (last-i p2)
		   (new-vec (make-array (1+ (- last-i first-i)) :adjustable t :fill-pointer 0)))
	      (loop for i from first-i to last-i by 1
		  do (vector-push (aref vec i) new-vec))
	      (make-py-list new-vec)))
	   
	   (:nonempty-stepped-slice
	    (let ((first-i p1)
		  (last-i p2)
		  (step p3)
		  (length p4))
	      (assert (= 0 (mod (- last-i first-i) step)))
	      (let ((reversed (when (< step 0)
				(rotatef first-i last-i)
				(setf step (- step))
				t)))
		(assert (and (<= 0 first-i last-i) (> step 0)))
		(let ((new-vec (make-array length :adjustable t :fill-pointer 0)))
		  (loop for i from first-i to last-i by step
		      do (vector-push (aref vec i) new-vec))
		  (when reversed
		    (setf new-vec (nreverse new-vec)))
		  (make-py-list new-vec))))))))
      
      (t (py-raise 'TypeError "list.__getitem__ expected integer or slice (got: ~A)" item)))))

(defmethod __iter__-fun ((x py-list/tuple))
  (let ((vec (slot-value x 'vec))
	(i -1))
    (lambda ()
      (if (<= 0 (incf i) (1- (length vec))) ;; may be modified in between
	  (values (aref vec i) t)
	nil))))
  
(defmethod __iter__ ((x py-list/tuple))
  (make-iterator-from-function
   (__iter__-fun x)))

(defmethod __len__ ((x py-list/tuple))
  (length (slot-value x 'vec)))

(defmethod __nonzero__ ((x py-list/tuple))
  (if (= (length (slot-value x 'vec)) 0) *False* *True*))

(defmethod print-list/tuple ((x py-list/tuple) &key (kind :repr) prefix suffix empty-seq)
  (assert (member kind '(:repr :str)))
  (with-slots (vec) x
    (if (= (length vec) 0)
	empty-seq
      (let* ((size-est (* (length vec) 4)) ;; just a guess
	     (res (make-array size-est :adjustable t :fill-pointer 0
			      :element-type 'character)))
	(vector-push prefix res)
	(loop with print-func = (if (eq kind :repr) #'py-repr #'py-str)
	    for item across vec
	    do (loop for ch across (py-string-designator-val (funcall print-func item))
		   do (vector-push-extend ch res))
	       (vector-push-extend #\, res)
	       (vector-push-extend #\Space res))
	(decf (fill-pointer res) 2)  ;; added ", " one time too many
	(vector-push-extend suffix res)
	res))))

(defmethod __reversed__ ((x py-list/tuple))
  "Return a reverse iterator"
  (let* ((vec (slot-value x 'vec))
	 (i (length vec)))
    (make-iterator-from-function
     (lambda ()
       (if (<= 0 (decf i) (1- (length vec))) ;; may be modified in between
	   (aref vec i)
	 nil)))))

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tuple
;; wpytuple

(defclass py-tuple (py-list/tuple)
  ()
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-tuple))

(defmethod make-tuple (&optional (vec (make-array 0)))
  (make-instance 'py-tuple :vec vec))

;; As tupls are immutable, popular ones can be cached.
(defvar *the-empty-tuple* (make-tuple (make-array 0)))

(defun make-tuple-from-list (list)
  (make-instance 'py-tuple
    :vec (make-array (length list) :initial-contents list)))

(defun tuple->lisp-list (tup)
  (declare (ignore tup))
  (error "outdated"))

;;;; magic methods

(defmethod py-tuple-__new__ ((cls class) &rest args)
  (assert (subtypep cls 'py-tuple))
  (cond ((null args) *the-empty-tuple*)
	((cdr args) (py-raise 'TypeError
			      "tuple.__new__(): takes at most one pos arg (got: ~A)" args))
	(t (loop with f = (get-py-iterate-fun (car args))
	       with val = (funcall f)
	       while val
	       collect val into vals
	       count 1 into num-vals
	       do (setf val (funcall f))
	       finally (return (if vals
				   (make-tuple (make-array num-vals :initial-contents vals))
				 *the-empty-tuple*))))))

(register-bi-class-attr/meth (find-class 'py-tuple) '__new__
			     (make-static-method #'py-tuple-__new__))

;; default noop __init__

(defmethod __add__ ((x py-tuple) (y py-tuple))
  (cond ((eq x *the-empty-tuple*) y)
	((eq y *the-empty-tuple*) x)
	(t (make-tuple (concatenate 'vector (slot-value x 'vec) (slot-value y 'vec))))))

;; CPython tuples: no __radd__

(defmethod __eq__ ((x py-tuple) (y py-tuple))
  (if (eq x y)
      *True*
    (__eq__-vectors (slot-value x 'vec) (slot-value y 'vec))))

(defmethod __eq__ ((x py-tuple) y)
  (declare (ignore y))
  *False*)

(defmethod __getitem__ ((x py-tuple) (item py-slice))
  (let ((res (call-next-method))) ;; defined on py-list/tuple; returns a py-list
    (check-type res py-list)
    (make-tuple (slot-value res 'vec))))

(defmethod __hash__ ((x py-tuple))
  (sxhash (slot-value x 'vec)))
   
(defmethod __mul__ ((x py-tuple) n)
  ;; n <= 0 => empty list
  (setf n (py-int-designator-val n))
  (cond ((<= n 0) *the-empty-tuple*)
	((= n 1) x)
	(t (make-tuple (loop with vec = (slot-value x 'vec) ;; this differs from
			   with res = (copy-seq vec)        ;;  the code for py-list
			   for i from 1 to n                ;;  as tuple vectors are
			   do (setf res (concatenate 'vector res vec)) ;; not adjustable
			   finally (return res))))))
  
(defmethod __repr__ ((x py-tuple))
  (print-list/tuple x :prefix #\( :suffix #\) :empty-seq "()" :kind :repr))

;; __reversed__ ?

(defmethod __setitem__ ((x py-tuple) key val)
  (declare (ignore key val))
  (py-raise 'TypeError
	    "Cannot set items of tuples"))

(defmethod __str__ ((x py-tuple))
  (print-list/tuple x :prefix #\( :suffix #\) :empty-seq "()" :kind :str))


(loop for name in '(__add__ __cmp__ __contains__ __eq__ __getitem__ __init__
		    __iter__ __len__ __mul__ __nonzero__ __setitem__)
    do (register-bi-class-attr/meth (find-class 'py-tuple) name (symbol-function name)))

;;; there are no tuple-specific non-magic methods

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List : represented by adjustable vector
;; wpylist

(defclass py-list (py-list/tuple)
  ()
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-list))

(defmethod make-py-list ((vec vector))
  (let ((i (make-instance 'py-list :vec vec)))
    (assert (slot-value i 'vec))
    i))

(defmethod make-py-list-from-list ((lst list))
  (make-instance 'py-list :vec (make-array (length lst) :initial-contents lst
					   :adjustable t :fill-pointer t)))
		 
(defmethod make-empty-py-list ()
  (make-instance 'py-list :vec (make-array 0 :adjustable t :fill-pointer 0)))


;; constructor

(defmethod py-list-__new__ ((cls class) &rest args)
  (assert (subtypep cls 'py-list))
  (make-instance cls :vec (make-array 0 :adjustable t :fill-pointer 0)))

(register-bi-class-attr/meth (find-class 'py-list) '__new__ 
			     (make-static-method #'py-list-__new__))

(defmethod __init__ ((x py-list) &rest args)
  (with-slots (vec) x
    (cond ((null args) )
	  ((cdr args) (py-raise 'TypeError
				"list.__init__(): takes at most one pos arg (got: ~A)" args))
	  (t (loop with f = (get-py-iterate-fun (car args))
		 with val = (funcall f)
		 while val do (vector-push-extend val vec)
			      (setf val (funcall f))))))
  x)
    
;; regular magic methods

(defmethod __add__ ((x py-list) (y py-list))
  "Structure is not shared"
  (with-slots ((x-vec vec)) x
    (with-slots ((y-vec vec)) y
      (make-py-list (let ((new-vec (make-array (+ (length x-vec) (length y-vec))
					       :adjustable t :fill-pointer 0)))
		      (loop for xi across x-vec do (vector-push xi new-vec))
		      (loop for yi across y-vec do (vector-push yi new-vec))
		      new-vec)))))


(defmethod __delitem__ ((x py-list) item)
  (typecase item
    (py-int-designator (list-delitem-integer x (py-int-designator-val item)))
    (py-slice (list-delitem-slice x item))
    (t (py-raise 'TypeError "list.__delitem__: expected integer of slice (got: ~A)" item))))

(defmethod list-delitem-integer ((x py-list) (index integer))
  (let* ((vec (slot-value x 'vec))
	 (len (length vec)))
    (when (< index 0) 
      (incf index len))
    (unless (<= 0 index (1- len))
      (py-raise 'IndexError "List index out of range (got: ~A, len: ~A)" index len))
    (loop for i from index below (1- len)
	do (setf (aref vec i) (aref vec (1+ i))))
    (setf (aref vec (1- len)) nil) ;; enable GC
    (decf (fill-pointer vec))))

(defmethod list-delitem-slice ((x py-list) (slice py-slice))
  (let* ((vec (slot-value x 'vec))
	 (len (length vec)))
    (multiple-value-bind (kind p1 p2 p3 p4) (slice-indices slice len)
      (ecase kind
	
	((:empty-slice-before :empty-slice-after :empty-slice-between :empty-slice-bogus) )
	
	(:nonempty-slice
	 (let ((first-i p1)
	       (last-i p2))
	   (loop for old-i from (1+ last-i) below len by 1
	       for new-i from first-i by 1
	       do (shiftf (aref vec new-i) (aref vec old-i) nil))
	   (decf (fill-pointer vec) (1+ (- last-i first-i))))) ;; set to nil for GC
	
	(:nonempty-stepped-slice
	 (let ((first-i p1)
	       (last-i p2)
	       (step p3)
	       (length p4))
	   (assert (= 0 (mod (- last-i first-i) step)))
	   (when (< step 0)
	     (rotatef first-i last-i)
	     (setf step (- step)))
	   (assert (> step 1))
	     
	   ;; remove indices start, start+step, start+2step, ..., stop
	   ;; items with indices in [start+1, start+step-1]   move 1 to the left
	   ;;            ,,    [start+step+1, start+2*step-1] move 2 to the left
	   ;;            ,,  [start+2*step+1, start+3*step-1] move 3 to the left
	   ;; and finally
	   ;;  items with indices >= {start + (length-1)*step} move length to the left
	     
	   (loop for move-delta from 1 to (1- length) ;; so this loops (1- length) times
	       for move-start-i from (1+ first-i) by step
	       for move-end-i from (+ first-i step -1) by step
	       do (loop for i from move-start-i to move-end-i
		      do (setf (aref vec (- i move-delta))
			   (aref vec i))))
	   (loop for i from (+ first-i (* (1- length) step)) below len
	       do (setf (aref vec (- i length)) (aref vec i)))
	   (loop for i from length below len
	       do (setf (aref vec i) nil)) ;; enable GC
	   (decf (fill-pointer vec) length)))))))

(defmethod __eq__ ((x py-list) (y py-list))
  (if (eq x y)
      *True*
    (__eq__-vectors (slot-value x 'vec) (slot-value y 'vec))))

(defmethod __eq__ ((x py-list) y)
  (declare (ignore y))
  *False*)

(defmethod __hash__ ((x py-list))
  (py-raise 'TypeError "List objects are unhashable"))

(defmethod __mul__ ((x py-list) n)
  (setf n (py-int-designator-val n))
  (if (<= n 0) 
      (make-empty-py-list)
    (let* ((vec (slot-value x 'vec))
	   (len (length vec))
	   (new-len (* n len))
	   (new-vec (make-array new-len :adjustable t :fill-pointer new-len)))
      (loop for copy from 0 below n  ;; maybe there's a better way to do this?
	  do (loop for x across vec  ;;  (result must be adjustable)
		 for i from 0
		 do (setf (aref new-vec (+ (* len copy) i)) x)))
      (make-py-list new-vec))))

(defmethod __repr__ ((x py-list))
  (print-list/tuple x :prefix #\[ :suffix #\] :empty-seq "[]" :kind :repr))


(defmethod move-vec-items ((vec vector) (delta integer) &key (start 0) (stop (1- (length vec))) make-nil)
  "Move subset of items in vector to the left or right"
  
  (cond ((> start stop)
	 (error "The :start index (~A) is greater than :stop index (~A)" start stop))
	 
	((= delta 0) )
	
	((< delta 0) ;; shift to the left
	 (when (< (+ start delta) 0)
	   (error "Array index of first element will become lower than 0"))
	 (loop for src-i from start to stop
	     for dest-i from (+ start delta)
	     do (setf (aref vec dest-i) (aref vec src-i)))
	 (when make-nil
	   (loop for i from (1+ (+ stop delta)) to stop
	       do (setf (aref vec i) nil))))
	
	((> delta 0) ;; shift to the right
	 (when (>= (+ stop delta) (length vec))
	   (error "Array index of first element will become greater than vector length"))
	 (loop for src-i from stop downto start
	     for dest-i downfrom (+ stop delta)
	     do (setf (aref vec dest-i) (aref vec src-i)))
	 (when make-nil
	   (loop for i from start to (1- (+ start delta))
	       do (setf (aref vec i) nil)))))
  vec)

(defmethod __setitem__ ((x py-list) item new-item)
  (let* ((vec (slot-value x 'vec))
	 (len (length vec)))
    (typecase item
      (py-int-designator
       (let ((i (py-int-designator-val item)))
	 (when (< i 0)	
	   (incf i len))
	 (if (<= 0 i (1- len))
	     (setf (aref vec i) new-item)
	   (py-raise 'ValueError "list.__setitem__: index out of range (got: ~A; len: ~A)"
		     item len))))
      
      (py-slice
       (let* ((source-vec (if (typep new-item 'py-list)
			      (slot-value new-item 'vec)
			    (let ((tmp-vec (make-array 5 :adjustable t :fill-pointer 0))
				  (iter-fun (get-py-iterate-fun new-item)))
			      (declare (dynamic-extent tmp-vec))
			      (loop with val = (funcall iter-fun)
				  while val do (vector-push-extend val tmp-vec)
					       (setf val (funcall iter-fun))
				  finally (return tmp-vec)))))
	      (source-len (length source-vec)))
	 
	 (declare (dynamic-extent source-vec))
	 (multiple-value-bind (kind p1 p2 p3 p4) (slice-indices item (length vec))    	
	   
	   (ecase kind
	     
	     (:empty-slice-bogus
	      ;; Can only "store" an empty list
	      (unless (= source-len 0)
		(py-raise 'ValueError "list.__setitem__: Bogus empty slice can only ~
                           store empty list (got items: ~A)" source-vec)))
	   
	     ((:empty-slice-before :empty-slice-after :empty-slice-between :nonempty-slice)
	      (flet ((resize-vec-to (new-vec-len)
		       (unless (= new-vec-len len)
			 (adjust-array vec new-vec-len :adjustable t :fill-pointer new-vec-len))))
		
		(let ((insertion-index
		       (ecase kind
			 (:empty-slice-before (resize-vec-to (+ len source-len))
					      (move-vec-items vec source-len
							      :start 0 :stop (1- len))
					      0)
			 
			 (:empty-slice-after (resize-vec-to (+ len source-len))
					     len)
			 
			 (:empty-slice-between (resize-vec-to (+ len source-len))
					       (let ((just-after p1)
						     (just-before p2))
						 (move-vec-items vec source-len
								 :start just-after
								 :stop (1- len))
						 just-before))
			 
			 (:nonempty-slice (let* ((first-i p1)
						 (last-i p2)
						 (old-slice-len (1+ (- last-i first-i)))
						 (delta-len (- source-len old-slice-len))
						 (new-vec-len (+ len delta-len)))
					    (resize-vec-to new-vec-len)
					    (move-vec-items vec delta-len
							    :start (1+ last-i)
							    :stop (1- len))
					    first-i)))))
		  #+(or)(warn "for insertion: ins-ind=~A, source-vec=~A, vec=~A"
			      insertion-index source-vec vec)
		  (loop for i from insertion-index
		      for x across source-vec
		      do (setf (aref vec i) x)))))
	      
	     (:nonempty-stepped-slice
	      (let* ((first-i p1)
		     (last-i  p2)
		     (step    p3)
		     (num-items p4))
		(unless (= num-items source-len)
		  (py-raise 'TypeError
			    "list.__setitem__: Stepped slice wants ~A items (got: ~A)"
			    num-items source-len))
		(when (< step 0)
		  (setf step (1- step))
		  (rotatef first-i last-i)
		  (setf source-vec (nreverse source-vec)))
		
		(loop for i from first-i to last-i by step
		    for j from 0
		    do (setf (aref vec i) (aref source-vec j))))))))))))

(defmethod __str__ ((x py-list))
  (print-list/tuple x :prefix #\[ :suffix #\] :empty-seq "[]" :kind :str))


(loop for name in '(__init__ __add__ __cmp__ __contains__ __delitem__ __eq__
		    __getitem__ __hash__ __iter__ __len__ __mul__
		    __rmul__ __nonzero__ __reversed__ __setitem__)
    do (register-bi-class-attr/meth (find-class 'py-list) name (symbol-function name)))

;;; list-specific methods

(defmethod list-append ((x py-list) item)
  (vector-push-extend item (slot-value x 'vec))
  *None*)

(defmethod list-count ((x py-list) item)
  (loop for i across (slot-value x 'vec)
      count (py-== i item)))

(defmethod list-extend ((x py-list) (y py-list))
  (with-slots ((x-vec vec)) x
    (with-slots ((y-vec vec)) y
      (loop for item across y-vec
	  do (vector-push-extend item x-vec))))
  *None*)
   
(defmethod list-extend ((x py-list) iterable)
  (let ((iter-fun (get-py-iterate-fun iterable))
	(vec (slot-value x 'vec)))
    (loop with val = (funcall iter-fun)
	while val do (vector-push-extend val vec)
		     (setf val (funcall iter-fun))))
  *None*)

(defmethod list-index ((x py-list) item &optional start stop)
  (or (position item (slot-value x 'vec) :test #'py-== :start (or start 0) :end stop)
      (when start
	(py-raise 'ValueError
		  "list.index(x): value ~A not found in this part of the list" item))
      (py-raise 'ValueError
		"list.index(x): value ~A not found in this part of the list" item)))

(defmethod list-insert ((x py-list) index object)
  "Insert ITEM before INDEX"
  (setf index (py-int-designator-val index))
  (let* ((vec (slot-value x 'vec))
	 (len (length vec)))
    (when (< index 0) ;; XXX TODO make a method for this
      (incf index len))
    (when (> index len)
      (setf index len))
    (when (< index 0)
      (setf index 0))
    (assert (<= 0 index len))
    
    ;; make room; move items starting from the end; insert object
    (vector-push-extend nil vec)
    (loop for src-i from (1- len) downto index
	do (setf (aref vec (1+ src-i))
	     (aref vec src-i)))
    (setf (aref vec index) object))
  *None*)

(defmethod list-pop ((x py-list) &optional index)
  "Remove and return item at index (default: last item)"
  (let* ((vec (slot-value x 'vec))
	 (len (length vec))
	 (index (if index
		    (prog1 (setf index (py-int-designator-val index))
		      (unless (<= 0 index (1- len))
			(py-raise 'ValueError
				  "list.pop(): index out of range (got: ~A; len: ~A)"
				  index len)))
		  (1- len))))
    (when (= 0 len)
      (py-raise 'IndexError "list.pop(): list is empty"))
    
    (prog1
	(aref vec index)
      (loop for i from index below (1- len)
	  do (setf (aref vec i) (aref vec (1+ i))))
      (setf (aref vec (1- len)) nil) ;; enable GC
      (decf (fill-pointer vec)))))

(defmethod list-remove ((x py-list) item)
  "Remove first occurance of item"
  (list-pop x (or (position item (slot-value x 'vec) :test #'py-==)
		  (py-raise 'ValueError "list.remove(): item not in list (got: ~A)" item)))
  *None*)

(defmethod list-reverse ((x py-list))
  "In-place"
  (with-slots (vec) x
    (setf vec (nreverse (slot-value x 'vec))))
  *None*)

(defmethod list-sort ((x py-list) &optional (cmpfunc *None*))
  "Stable sort, in-place"
  (let ((lt-pred (if (eq cmpfunc *None*)
		     #'py-<
		   (lambda (x y) (< (py-call cmpfunc (list x y)) 0)))))
    (with-slots (vec) x
      (setf vec (stable-sort vec lt-pred))))
  *None*)

(loop for (k v) in `((append  ,#'list-append)
		     (count   ,#'list-count)
		     (extend  ,#'list-extend)
		     (index   ,#'list-index)
		     (insert  ,#'list-insert)
		     (pop     ,#'list-pop)
		     (remove  ,#'list-remove)
		     (reverse ,#'list-reverse)
		     (sort    ,#'list-sort))
    do (register-bi-class-attr/meth (find-class 'py-list) k v))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Iterator
;; 
;; <http://www.python.org/peps/pep-0234.html>
;; <<
;; The two methods correspond to two distinct protocols:
;;     1. An object can be iterated over with "for" if it implements
;;        __iter__() or __getitem__().
;;     2. An object can function as an iterator if it implements next().
;; >>

(defclass py-iterator (builtin-instance)
  ()
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-iterator))

(defmethod __iter__ ((x py-iterator))
  "It's defined that an iterator is its own iterator, so people
   can do:  for i in iter(iter(iter(iter(foo))))."
  x)

(register-bi-class-attr/meth (find-class 'py-iterator) '__iter__ #'__iter__)


(defvar *StopIteration* '|stop-iteration|)

(defclass py-func-iterator (py-iterator)
  ((func :initarg :func :type function)
   (stopped-yet :initform nil)
   (end-value :initarg :end-value))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-func-iterator))

(defun make-iterator-from-function (f &optional (end-value nil))
  "Create an iterator that calls f again and again. (F somehow has to keep ~@
   its own state.) When F returns a value EQL to END-VALUE (default: nil), ~@
   it is considered finished and will not be called any more times."
  (check-type f function)
  (make-instance 'py-func-iterator :func f :end-value end-value))

(defmethod iterator-next ((f py-func-iterator))
  "This is the only function that an iterator has to provide."
  (flet ((err-finished ()
	   (py-raise 'StopIteration "Iterator ~S has finished" f)))
    (if (slot-value f 'stopped-yet)
	(err-finished)
      (let ((res (funcall (slot-value f 'func))))
	(if (eql res (slot-value f 'end-value))
	    (progn (setf (slot-value f 'stopped-yet) t)
		   (err-finished))
	  res)))))

(register-bi-class-attr/meth (find-class 'py-iterator) 'next #'iterator-next)

(defmethod __name__ ((x py-func-iterator))
  (internal-get-attribute (slot-value x 'func) '__name__))

(register-bi-class-attr/meth (find-class 'py-func-iterator) '__name__
			     (make-bi-class-attribute #'__name__))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String
;; 
;; Lisp strings are designators for Python string objects, but Lisp
;; characters are not.

(defclass py-string (builtin-instance)
  ((string :type string :initarg :string))
  (:documentation "The String type")
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-string))

(deftype py-string-designator ()
  `(or py-string string))

(defun py-string-designator-p (s)
  "Return STRING-DESIGNATOR-P, LISP-STRING"
  (cond ((typep s 'string) (values t s))
	((typep s 'py-string) (values t (slot-value s 'string)))
	(t nil)))

(defmethod py-string-designator-val ((s string))
  s)

(defmethod py-string-designator-val ((s py-string))
  (slot-value s 'string))

(defun make-py-string (&optional (s ""))
  (check-type s string "A Lisp string")
  (make-instance 'py-string :string s))

(defmethod py-string->symbol ((x py-string))
  (intern (slot-value x 'string)))

(defmethod print-object ((x py-string) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~S" (slot-value x 'string))))

(defmethod py-string-__new__ (cls &rest args)
  (assert (subtypep cls 'py-string))
  
  (let ((res (cond ((null args) "")
	
		   ((and (py-string-designator-p (first args))
			 (second args))
		    ;; decode string
		    (if (py-string-designator-p (second args))
			(py-decode-unicode (py-string-designator-val (first args))
					   (py-string-designator-val (second args)))
		      (py-raise 'TypeError
				"string.__new__(..): (optional) second arg must be ~
                                 string indicating encoding (got: ~A)" (second args))))
	
		   (t (if (py-string-designator-p (first args))
			  (py-decode-unicode (py-string-designator-val (first args))
					     "ascii")
			(call-attribute-via-class (car args) '__str__))))))
    
    (if (eq cls (find-class 'py-string))
	res
      (let ((inst (make-instance cls)))
	(setf (slot-value inst 'string) res)
	inst))))

(register-bi-class-attr/meth (find-class 'string) '__new__
			     (make-static-method #'py-string-__new__))
(register-bi-class-attr/meth (find-class 'py-string) '__new__
			     (make-static-method #'py-string-__new__))


(defmethod __mul-1__ ((x string) n)
  (multiple-value-bind (des val)
      (py-int-designator-p n)
    (unless des
      (return-from __mul-1__ *NotImplemented*))
    (setf n val))
  (let ((s ""))
    (loop for i from 1 to n
	do (setf s (concatenate 'string s x))
	finally (return s))))


(defmethod unicode-string-p ((x string))
  
  ;; By our definition, it's a unicode string iff it contains a
  ;; character with code > 255. A regular Python string containing
  ;; character 0xFF can be written as: "\xff".
  
  (loop for ch across x
      when (> (char-code ch) 255)
      do (return t)
      finally (return nil)))

(defmethod unicode-char-p ((x character))
  (> (char-code x) 255))

(defmethod string-__repr__ ((x string))
  (let* ((single-quotes 0) ;; minimize number of escaped quotes in the returned string
	 (double-quotes 0)
	 (unicode nil)
	 (data (with-output-to-string (s)
		 (loop for ch across x
		     do (cond ((unicode-char-p ch)
			       (setf unicode t)
			       (let ((num (char-code ch)))
				 (cond ((<= num #xFFFF) (format s "\\u~X" num))
				       ((<= num #xFFFFFFFF) (format s "\\U~X" num))
				       (t (error
					   "Unicode code for char too large: ~S" ch)))))
				
			      ;; XXX make these a vector lookup
			      ((char= ch #\') (incf single-quotes) (write-char ch s))
			      ((char= ch #\") (incf double-quotes) (write-char ch s))
			      ((char= ch #\Bell)      (write-string "\\b" s))
			      ((char= ch #\Backspace) (write-string "\\b" s))
			      ((char= ch #\Page)      (write-string "\\f" s))
			      ((char= ch #\Newline)   (write-string "\\n" s))
			      ((char= ch #\Return)    (write-string "\\r" s))
			      ((char= ch #\Tab)       (write-string "\\t" s))
			      ((char= ch #\VT)        (write-string "\\v" s))
			      ((char= ch #\\)         (write-string "\\\\" s))
			      
			      ;; The printable ASCII characters.
			      ((and (<= (char-code ch) 127)  ;; <-- only ASCII
				    (graphic-char-p ch))       (write-char ch s))
			      
			      (t 
			       ;; Consider it non-printable: hex escaping
			       (let ((code (char-code ch)))
				 (assert (<= code 255))
				 (format s "\\x~2,v,X" #\0 code)))))))
	   
	 ;; Prefer single to double quotes, if equal number of escapes needed.
	 (delimit-quote (if (<= single-quotes double-quotes) #\' #\")))
      
    (with-output-to-string (s)
      (when unicode
	(write-char #\u s))
      (write-char delimit-quote s)
      (loop for ch across data
	  when (char= ch delimit-quote)
	  do (write-char #\\ s)
	     (write-char ch s)
	  else do (write-char ch s))
      (write-char delimit-quote s))))
			  
      
(defmethod string-__str__ ((x string))
  (if (unicode-string-p x)
      (py-raise 'UnicodeEncodeError
		"string.__str__(): String contains Unicode character (got: ~S)" x)
    (with-output-to-string (s)
      (loop for ch across x
	  do (write-char ch s)))))

#+allegro
(defmethod py-unicode-external-format->lisp-external-format (name)
  ;; Returns NAME, MAX-CHAR-CODE, MAX-ENCODED-OCTET-CODE
  
  ;; Based on:
  ;;  http://meta.kabel.utwente.nl/specs/Python-Docs-2.3.3/lib/node127.html
  ;;  http://www.franz.com/support/documentation/7.0/doc/iacl.htm#external-formats-1
  ;; For now only ASCII and UTF-8 are supported.
  
  (setf name (string-lower (py-string-designator-val name)))
  (cond 
   ((member name '("ascii" "646" "us") :test 'string=) 
    (values :latin1 127 127))
   
   ((member name '("latin" "latin1" "latin-1") :test 'string=) 
    (values :latin1 255 255))
   
   ((member name '("utf8" "utf_8" "utf-8" "utf" "u8") :test 'string=)
    (values :utf8 #16x0010FFFF 255))
   
   (t (py-raise 'UnicodeError "Unrecognized Unicode external format: ~A" name))))


#+allegro
(defmethod py-encode-unicode ((string string) &optional (external-format "ascii") errors)
  (when errors
    (error "TODO: errors parameter for unicode encode"))
  
  ;; EXCL:OCTETS-TO-STRING replaces characters out of range of
  ;; external format with question marks #\?, but we want to get
  ;; an error instead.
  ;; 
  ;; The result is a string (containing characters), not a vector of
  ;; octets, because Python doesn't have vectors. Python could use
  ;; regular lists, but strings are immutable so more efficient.
  
  (multiple-value-bind (ex-format max-code max-octet-code)
      (py-unicode-external-format->lisp-external-format external-format)
    (declare (ignore max-octet-code))
    (loop for ch across string
	do (let ((code (char-code ch)))
	     (when (> code max-code)
	       (py-raise 'UnicodeEncodeError
			 "During encoding of string, encountered a character whose ~
                          code is out of the allowed range (got character code: ~A; ~
                          external format: ~A; max code allowed for external format: ~A)"
			 code external-format max-code))))
    (multiple-value-bind (octets num-bytes-copied)
	(excl:string-to-octets string :external-format ex-format
			       :null-terminate nil)
      (when (< num-bytes-copied (length string))
	#+(or)(warn "encode: <")
	(py-raise 'UnicodeEncodeError "Not all bytes valid"))
      
      (loop with string = (make-array (length octets) :element-type 'character)
	  for oc across octets
	  for i from 0
	  do (setf (aref string i) (code-char oc))
	  finally (return string)))))

#-allegro
(defmethod py-encode-unicode ((string string) &optional (external-format "ascii") errors)
  (declare (ignorable string external-format errors))
  (error "TODO: string.encode()"))

#+allegro
(defmethod py-decode-unicode ((string string) &optional (external-format "ascii") errors)
  
  (when errors
    (error "TODO: `errors' parameter for unicode encode"))

  ;; Python has no separate data type for the returned vector of
  ;; octets: that's also a string.

  (multiple-value-bind (ex-format max-code max-octet-code)
      (py-unicode-external-format->lisp-external-format external-format)
    (declare (ignore max-code))
    (let ((vec (make-array (length string) :element-type '(unsigned-byte 8))))
      (declare (dynamic-extent vec))
      (map-into vec #'char-code string)
      (loop for code across vec
	  when (> code max-octet-code)
	  do (py-raise 'UnicodeDecodeError
		       "During decoding of string, encountered a character whose ~
                        code is out of allowed range (got character code: ~A; ~
                        external format: ~A; max octet code allowed for external format: ~A)"
		       code ex-format max-octet-code))
      (multiple-value-bind (string-res chars-copied octets-used)
	  (excl:octets-to-string vec :external-format ex-format)
	(declare (ignore chars-copied))
	(when (< octets-used (length vec))
	  #+(or)(warn "decode: octets-used < length vec")
	  (py-raise 'UnicodeDecodeError "Not all octets valid"))
	string-res))))

#-allegro
(defmethod py-decode-unicode ((string string) &optional (external-format "ascii") errors)
  (declare (ignorable string external-format errors))
  (error "TODO: string.decode()"))

;;; string-specific methods

(defmethod string-center-1 ((s string) width)
  "'a'.center(2) -> 'a '"
  (ensure-py-type width integer "string.center() requires integer width (got: ~A)")
  (let* ((len (length s))
	 (diff (- width len)))
    (if (> diff 0)
	(let* ((before (floor (/ diff 2)))
	       (v (make-array (max width len) :element-type 'character)))
	  (loop for i from 0 below before
	      do (setf (aref v i) #\Space))
	  (loop for c across s
	      for i from before
	      do (setf (aref v i) c))
	  (loop for i from (+ before len) below width
	      do (setf (aref v i) #\Space))
	  v)
      s)))

(defmethod string-count-1 ((x string) (sub string))
  "Count number of occurances of SUB in X."
  (let ((count 0)
	(start-pos 0)
	(sublen (length sub)))
    (loop
      (let ((res (search sub x :start2 start-pos)))
	(if res
	    (progn
	      (incf count)
	      (setf start-pos (+ res sublen))) ;; "aaa".count("aa") = 1
	  (return))))
    count))

#+(or)
(defmethod string-decode-1 (x &optional encoding errors)
  (declare (ignore x encoding errors))
  (error "todo: string-decode"))

#+(or)
(defmethod string-encode-1 (x &optional encoding errors)
  (declare (ignore x encoding errors))
  (error "todo: string-decode"))

(defmethod string-endswith-1 ((x string) (suffix string) &optional (start 0) (end nil))
  (error "needs work")
  (ensure-py-type start integer "String indices must be integers (got: ~A)")
  (when end
    (ensure-py-type end integer "String indices must be integers (got: ~A)"))
  
  (when (string= suffix "") ;; trivial
    (return-from string-endswith-1 t))
  
  (destructuring-bind (start stop step)
      (tuple->lisp-list (indices (make-slice start (or end (1- (length x))) 1)
				 (length x)))
    (declare (ignore step))
    (let ((subseq-start (- (1+ stop) (length suffix))))
      
      (when (< subseq-start start)
	(return-from string-endswith-1 nil))
		  
      (string= (subseq x subseq-start (1+ stop))
	       suffix))))

(defmethod string-expandtabs-1 ((x string) &optional (tabsize 8))
  (ensure-py-type tabsize integer "string.expandtabs: tabsize must be integer (got: ~A)")
  (let ((s (make-array (length x) :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for c across x
	do (if (char= c #\Tab)
	       (dotimes (i tabsize)
		 (vector-push-extend #\Space s))
	     (vector-push-extend c s)))
    s))

(defmethod string-find-1 ((x string) (sub string) &optional (start 0) end)
  (error "needs work")
  (ensure-py-type start integer "String indices must be integers (got: ~A)")
  (when end
    (ensure-py-type end integer "String indices must be integers (got: ~A)"))

  (when (string= sub "") ;; trivial
    (return-from string-find-1 0))

  (destructuring-bind (start stop step)
      (tuple->lisp-list (indices (make-slice start (or end (1- (length x))) 1)
				 (length x)))
    (declare (ignore step))
    (let ((res (search sub (subseq x start stop))))
      (if res
	  (+ start res)
	-1))))

(defmethod string-index-1 ((x string) (sub string) start end)
  (let ((res (string-find-1 x sub start end)))
    (if (= res -1)
	(py-raise 'ValueError "Substring not found")
      res)))

;; Predicates

(defmethod string-isalnum-1 ((x string))
  (lisp-val->py-bool (every #'alphanumericp x)))

(defmethod string-isalpha-1 ((x string))
  (lisp-val->py-bool (every #'alpha-char-p x)))

(defmethod string-isdigit-1 ((x string))
  (lisp-val->py-bool (every #'digit-char-p x)))

(defmethod string-islower-1 ((x string))
  (lisp-val->py-bool (every #'lower-case-p x)))

(defmethod string-isspace-1 ((x string))
  (lisp-val->py-bool (every (lambda (c)
			      (member c (load-time-value (list #\Space #\Tab #\Newline))))
			    ;; XX check what is whitespace
			    x)))

(defmethod string-istitle-1 ((x string))
  ;; It is defined to be a titel iff first char uppercase, rest lower; with anything non-alpha (even
  ;; non-printable characters) in between, like "Abc De"
  ;; 
  ;; Algorithm taken from `Objects/stringobject.c', function `string_istitle'.
  (let ((got-cased nil)
	(previous-is-cased nil))
    (loop for c across x
	do (cond ((upper-case-p c) (when previous-is-cased
				     (return-from string-istitle-1 *False*))
				   (setf previous-is-cased t)
				   (setf got-cased t))
		 
		 ((lower-case-p c) (unless previous-is-cased
				     (return-from string-istitle-1 *False*)))
		 
		 (t 		   (setf previous-is-cased nil))))
    (lisp-val->py-bool got-cased)))

  
(defmethod string-isupper-1 ((x string))
  (lisp-val->py-bool (every #'upper-case-p x)))

(defmethod string-join-1 ((x string) sequences)
  "Join a number of strings"
  (apply #'concatenate 'string x 
	 (mapcar 
	  (lambda (s)
	    (ensure-py-type s string
			    "string.join() can only handle real strings (got: ~A)")
	    s)
	  (py-iterate->lisp-list sequences))))

(defmacro def-unary-string-meths (data)
  `(progn ,@(loop for (name args body) in data
		do (assert (eq (car args) 'x))
		collect (let ((rest (cdr args)))
			  `(progn (defmethod ,name ((x py-string) ,@rest)
				    (let ((x (slot-value x 'string)))
				      ,body))
				  (defmethod ,name ((x string) ,@rest)
				    ,body))))))

(defmacro def-binary-string-meths (names)
  `(progn ,@(loop for (name args body) in names
		do (assert (and (eq (first args) 'x)
				(eq (second args) 'y)))
		collect (let* ((rest (cddr args))
			       (rest2 (remove '&optional rest)))
			  `(progn (defmethod ,name ((x py-string) y ,@rest)
				    (let ((x (slot-value x 'string)))
				      (,name x y ,@rest2)))
				  (defmethod ,name (x (y py-string) ,@rest)
				    (let ((y (slot-value y 'string)))
				      (,name x y ,@rest2)))
				  (defmethod ,name ((x string) (y string) ,@rest)
				    ,body))))))

(def-unary-string-meths 
    ((__getitem__ (x index) (progn (ensure-py-type index integer
						   "String indices must be integer (slices: todo)")
				   (when (< index 0) ;; XXX slice support
				     (incf index (length x)))
				   (string (char x index))))
     (__hash__  (x) (sxhash x))
     (__iter__  (x) (let ((i 0))
		      (make-iterator-from-function
		       (lambda () (when (< i (length x))
				    (prog1 (string (aref x i))
				      (incf i)))))))
     (__len__  (x) (length x))
     (__mod__  (x args) (py-format-string x args))
     
     ;; rmod, rmul
     (__mul__  (x n) (__mul-1__ x n))
     
     ;; __reduce__ : todo
     
     ;; __repr__ : with quotes (todo: if string contains ', use " as quote etc)
     ;; __str__  : without surrounding quotes
     (__repr__ (x) (string-__repr__ x))
     (__str__  (x) (string-__str__ x))
     
     (py-string-capitalize (x) (string-capitalize x)) ;; Lisp function
     (string-center (x width)                     (string-center-1 x width))
     #+(or)(string-decode (x &optional encoding errors) (string-decode-1 x encoding errors))
     #+(or)(string-encode (x &optional encoding errors) (string-encode-1 x encoding errors))
     (string-expandtabs (x &optional tabsize)     (string-expandtabs-1 x tabsize))
     (string-isalnum (x)  (string-isalnum-1 x))
     (string-isalpha (x)  (string-isalpha-1 x))
     (string-isdigit (x)  (string-isdigit-1 x))
     (string-islower (x)  (string-islower-1 x))
     (string-isspace (x)  (string-isspace-1 x))
     (string-istitle (x)  (string-istitle-1 x))
     (string-isupper (x)  (string-isupper-1 x))
     (string-join    (x seq) (string-join-1 x seq))
     (string-ljust (x width &optional (fillchar " "))
		   (progn (ensure-py-type width integer
					  "string.ljust(): integer width expected (got: ~A)")
			  (setf fillchar (py-string-designator-val fillchar))
			  (let ((res (copy-seq x)))
			    (loop while (< (length res) width)
				do (setf res (concatenate 'string fillchar res))
				finally (return res)))))
     (string-lower (x) (string-downcase x))
     (string-lstrip (x &optional chars)
		    (progn (setf chars (py-string-designator-val chars))
			   (let ((res (copy-seq x)))
			     (loop while (and res (member (aref res 0) '( #\Tab #\Space #\Newline )))
				 do (setf res (subseq res 1))
				 finally (return res)))))
     (string-replace (x old new &optional count)
		     (progn (setf old (py-string-designator-val old)
				  new (py-string-designator-val new)
				  count (when count (py-int-designator-val count)))
			    (substitute new old x :count count)))
     (string-rfind (x sub &optional start end)
		   (declare (ignorable x sub start end))
		   (error "string.rfind(): todo"))
     (string-rindex (x sub &optional start end)
		    (declare (ignorable x sub start end))
		    (error "string.rdindex(): todo"))
     (string-rjust (x width &optional (fillchar " "))
		   (reverse (string-ljust (reverse x) width fillchar)))
     (string-rsplit (x sep &optional maxsplit)
		    (declare (ignorable x sep maxsplit))
		    (error "string.rsplit(): todo"))
     (string-split (x sep &optional maxsplit)
		   (declare (ignorable x sep maxsplit))
		   (error "string.split(): todo"))
     (string-splitlines (x &optional keepends)
			(declare (ignorable x keepends))
			(error "string.splitlines(): todo"))
     (string-startswith (x prefix &optional start end)
			(declare (ignorable x prefix start end))
			(error "string.startswith(): todo"))
     (string-strip (x &optional chars)
		   (declare (ignorable x chars))
		   (error "string.strip(): todo"))
     (string-swapcase (x) (declare (ignorable x))
		      (error "string.swapcase(): todo"))
     (string-title (x) (declare (ignorable x))
		   (error "string.title(): todo"))
     (string-translate (x table &optional deletechars)
		       (declare (ignorable x table deletechars))
		       (error "string.translate(): todo"))
     (string-upper (x) (string-upcase x))
     (string-zfill (x width) (declare (ignorable x width))
		   (error "string.zfill(): todo"))))


(def-binary-string-meths
    ((__add__      (x y)  (concatenate 'string x y))
     (__radd__     (x y)  (__add__ y x))
     (__contains__ (x y)  (lisp-val->py-bool (search y x)))
     (__cmp__ (x y)       (cond ((string< x y) -1)
			        ((string= x y) 0)
			        (t 1)))
     (__eq__ (x y)        (string= x y))
     
     (string-count    (x y) (string-count-1 x y))
     (string-endswith (x y &optional start end)
		      (lisp-val->py-bool (string-endswith-1 x y start end)))
     (string-find     (x y &optional start end)  (string-find-1 x y start end))
     (string-index    (x y &optional start end)
		      (string-index-1 x y (or start 0) (or end 0)))))


(loop for name in '(__add__ __radd__ __contains__ __cmp__
		    __getitem__ __iter__ __len__ __mod__ __mul__)
    do (register-bi-class-attr/meth (find-class 'string) name (symbol-function name)) 
       (register-bi-class-attr/meth (find-class 'py-string) name (symbol-function name)))
				    
(loop for (k v) in `((capitalize ,#'string-capitalize) ;; these are unary...
		   (center   ,#'string-center-1)
		   #+(or)(decode   ,#'string-decode-1)
		   #+(or)(encode   ,#'string-encode-1)
		   (expandtabs  ,#'string-expandtabs-1)
		   (isalnum  ,#'string-isalnum-1)
		   (isalpha  ,#'string-isalpha-1)
		   (isdigit  ,#'string-isdigit-1)
		   (islower  ,#'string-islower-1)
		   (isspace  ,#'string-isspace)
		   (istitle  ,#'string-istitle-1)
		   (isupper  ,#'string-isupper-1)
		   (join     ,#'string-join-1)
		   
		   (ljust ,#'string-ljust)
		   (lower ,#'string-lower)
		   (lstrip ,#'string-lstrip)
		   (replace ,#'string-replace)
		   (rfind ,#'string-rfind)
		   (rindex ,#'string-rindex)
		   (rjust ,#'string-rjust)
		   (rsplit ,#'string-rsplit)
		   (split ,#'string-split)
		   (splitlines ,#'string-splitlines)
		   (startswith ,#'string-startswith)
		   (strip ,#'string-strip)
		   (swapcase ,#'string-swapcase)
		   (title ,#'string-title)
		   (translate ,#'string-translate)
		   (upper ,#'string-upper)
		   (zfill ,#'string-zfill)
		   
		   (count    ,#'string-count) ;; these are binary
		   (endswith ,#'string-endswith)
		   (find     ,#'string-find)
		   (index    ,#'string-index)
		   
		   (encode ,#'py-encode-unicode) ;; unicode handling
		   (decode ,#'py-decode-unicode))
		   
    do (register-bi-class-attr/meth (find-class 'string) k v)
       (register-bi-class-attr/meth (find-class 'py-string) k v))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File

;; XXX all this file object stuff is untested!

(defclass py-file (builtin-instance)
  ((stream :type file-stream)
   (newlines :initform ()) ;; list possibly containing 'n, 'r, 'rn
   ))

(defmethod file-check-open ((f py-file) &optional direction)
  (with-slots (stream) f
    (let ((open (and (open-stream-p stream)
		     (ecase direction
		       (nil t)
		       (:input (input-stream-p stream))
		       (:output (output-stream-p stream))))))
      (unless open
	(py-raise 'ValueError "I/O operation on closed file")))))

(defmethod file-close ((f py-file))
  ;; Calling CLOSE more than once is allowed in Python. No return value.
  (with-slots (closed stream) f
    (unless closed
      (close stream)))
  (values))

(defmethod file-flush ((f py-file))
  ;; Flush internal buffer.
  (file-check-open f)
  (with-slots (stream) f
    (when (and (open-stream-p stream)
	       (output-stream-p stream))
      (finish-output stream))
    (when (and (open-stream-p stream)
	       (input-stream-p stream))
      ;; call CLEAR-INPUT ?
      ))
  (values))

(defmethod file-fileno ((f py-file))
  (py-raise 'NotImplementedError
	    "Sorry, method `fileno' of file objects not implemented"))

(defmethod file-isatty ((f py-file))
  (py-raise 'NotImplementedError
	    "Sorry, method `isatty' of file objects not implemented"))

(defmethod __iter__ ((f py-file))
  ;; The PY-FILE itself implements NEXT.
  (file-check-open f :input)
  (file-readline f))

(defmethod next ((f py-file))
  (file-readline f))

(defmethod file-read ((f py-file) &optional (size nil size-p))
  (when size-p
    (ensure-py-type size integer
		    "Argument SIZE to file.read() must be ~@
                     positive integer (got: ~A)")
    (unless (>= size 0)
      (py-raise 'ValueError
		"Argument SIZEHINT to file.read() must be ~@
                 positive integer (got: ~A)" size)))
  
  (with-slots (stream newlines) f
    (let ((count 0)
	  (res (make-array 1000
			   :element-type 'character
			   :adjustable t
			   :fill-pointer 0)))
      (loop
	(when (and size-p
		   (= size count))
	  (return-from file-read res))
	(let ((ch (read-char stream nil nil)))
	  (if ch
	      (vector-push-extend ch res)
	    (return-from file-read res)))))))


(defmethod file-readline ((f py-file) &optional (maxsize nil maxsize-p))
  ;; Returns LINE, NUM-READ
  ;; Because Lisp doesn't treat "\r" as newline, we have to do this
  ;; ourselves (?).
  
  (file-check-open f :input)
  
  (when maxsize-p
    (ensure-py-type maxsize integer
		    "Argument SIZEHINT to file.readline() must be ~@
                     positive integer (got: ~A)")
    (unless (>= maxsize 0)
      (py-raise 'ValueError
		"Argument SIZEHINT to file.readline() must be ~@
                 positive integer (got: ~A)" maxsize)))
  (with-slots (stream newlines) f
    (let ((res (make-array 100
			   :element-type 'character
			   :adjustable t
			   :fill-pointer 0))
	  (num-read 0))
      (loop
	
	(when (and maxsize-p
		   (= num-read maxsize))
	  (return-from file-readline (values res num-read)))
	
	(let ((ch (read-char stream nil :eof)))
	  (case ch
	    (:eof	     (return-from file-readline (values res num-read)))
	    (#\Newline       (vector-push-extend ch res) ; '\n\
			     (pushnew 'n newlines)
			     (return-from file-readline (values res num-read)))
	    (#\Return        (vector-push-extend ch res) ; '\r' or '\r\n'
			     (let ((ch2 (peek-char nil stream nil :eof)))
			       (case ch2
				 (:eof            (pushnew 'r newlines)
						  (return-from file-readline (values res num-read)))
				 (#\Newline       (vector-push-extend ch2 res)
						  (pushnew 'rn newlines)
						  (return-from file-readline (values res num-read)))
				 (t               (return-from file-readline (values res num-read))))))))))))


(defmethod file-readlines ((f py-file) &optional (sizehint nil sizehint-p))
  (when sizehint-p
    (ensure-py-type sizehint integer
		    "Argument SIZEHINT to file.readlines() must be ~@
                     positive integer (got: ~A)")
    (unless (>= sizehint 0)
      (py-raise 'ValueError
		"Argument SIZEHINT to file.readlines() must be ~@
                 positive integer (got: ~A)" sizehint)))
  
  (let ((res ())
	(num-read 0))
    (loop
      (multiple-value-bind (line n)
	  (file-readline f)
	(push line res)
	(when sizehint-p
	  (incf num-read n)
	  (when (<= sizehint num-read)
	    (return)))))
    (make-py-list-from-list (nreverse res))))


(defmethod file-xreadlines ((f py-file))
  (__iter__ f))


(defmethod file-seek ((f py-file) offset &optional (whence nil whence-p))
  ;; Set FILE position.
  ;; Whence: 0 = absolute; 1 = relative to current; 2 = relative to end
  ;; There is no return value, but an IOError is raised for invalid arguments.
  
  (ensure-py-type offset integer
		  "file.seek() OFFSET argument must be integer (got: ~A)")
  (let ((reference
	 (if whence-p
	     (progn (ensure-py-type whence integer
				    "file.seek() WHENCE argument must be integer (got: ~A)")
		    (case whence
		      (0 :absolute)
		      (1 :current)
		      (2 :end)
		      (t (py-raise 'ValueError
				   "file.seek() WHENCE argument not in 0..2 (got: ~A)" whence))))
	   :absolute)))
    
    (file-check-open f)
    (with-slots (stream) f      
      (ecase reference
	    
	(:absolute
	 (cond ((>= offset 0) (unless (file-position stream offset)
				(py-raise 'IOError
					  "File seek failed (absolute; offset: ~A)"
					  offset))) ;; catch more?
	       ((< offset 0) (py-raise 'IOError
				       "Negative offset invalid for absolute file.seek() (got: ~A)"
				       offset))))
	     
	(:end   ;; XX check off-by-one for conditions
	 (cond ((<= offset 0) (unless (file-position stream (+ (file-length stream) offset))
				(py-raise 'IOError
					  "File seek failed (from-end; offset: ~A)"
					  offset)))
	       ((> offset 0) (py-raise 'IOError
				       "Positive offset invalid for file.seek() from end (got: ~A)"
				       offset))))
	(:relative
	 (unless (unless (file-position stream (+ (file-position stream) offset))
		   (py-raise 'IOError
			     "File seek failed (relative; offset: ~A)"
			     offset))))))))
	      
(defmethod file-tell ((f py-file))
  (with-slots (stream) f
    (file-position stream)))

(defmethod file-truncate ((f py-file) &optional (size nil size-p))
  (file-check-open f) ;; or not needed?
  (with-slots (stream) f
    (if size-p
	(progn
	  (ensure-py-type size integer
			  "file.truncate() expects non-negative integer arg (got: ~A)")
	  (when (< size 0)
	    (py-raise 'ValueError
		      "File.truncate() expects non-negative integer (got :~A)" size)))
      (setf size (file-position stream)))
    #+:allegro (handler-case (excl.osi::os-ftruncate f size)
		 (excl.osi:syscall-error ()
		   (py-raise 'IOError "Truncate failed (syscall-error)")))))
		 
(defmethod file-write ((f py-file) str)
  (ensure-py-type str string "file.write() takes string as arg (got: ~A)")
  (file-check-open f :output)
  (with-slots (stream) f
    (write-string str stream)))
   
(defmethod file-writelines ((f py-file) seq)
  (map-over-py-object (lambda (str)
			(ensure-py-type str string
					"file.writelines() requires sequence ~@
                                         of strings (got element: ~A)")	
			(file-write f str))
		      seq)
  *None*)

(defmethod file-closed ((f py-file))
  (with-slots (stream) f
    (not (open-stream-p stream))))

(defmethod file-encoding ((f py-file))
  (py-raise 'NotImplementedError
	    "Sorry, method `fileno' of file objects not implemented"))

(defmethod file-mode ((f py-file))
  ;; easy to add
  (py-raise 'NotImplementedError
	    "Sorry, method `fileno' of file objects not implemented"))

(defmethod file-name ((f py-file))
  ;; easy to add
  (py-raise 'NotImplementedError
	    "Sorry, method `fileno' of file objects not implemented"))
  
(defmethod file-newlines ((f py-file))
  ;; Return a tuple with the encountered newlines as strings.
  (with-slots (newlines) f
    (make-tuple-from-list (mapcar (lambda (nl)
				    (ecase nl
				      (r (string #\Return))
				      (n (string #\Newline))
				      (rn (format nil "~A~A" #\Return #\Newline))))
				  newlines))))

#+(or) ;; should we implement this, or is it a CPython implementation detail?!
(defmethod file-softspace ((f py-file))
  )

;; XXX todo register...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enumerate
;; 
;; Generator to iterate over an object, also yielding the index of the yielded item:
;; 
;; >>> x = enumerate("asdf")
;; >>> x
;; <enumerate object at 0x4021b6ac>
;; >>> x.next()
;; (0, 'a')
;; >>> x.next()
;; (1, 's')

(defclass py-enumerate (builtin-instance)
  ((generator :initarg :generator)
   (index :initarg :index :type integer))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-enumerate))

(defmethod py-enumerate-__new__ ((cls class) iterable)
  (assert (subtypep cls 'py-enumerate)) 
  (let ((x (make-instance cls)))
    (with-slots (generator index) x
      (setf generator (get-py-iterate-fun iterable)
	    index 0))
    x))

(register-bi-class-attr/meth (find-class 'py-enumerate) '__new__
			     (make-static-method #'py-enumerate-__new__))

(defmethod __iter__ ((x py-enumerate))
  x)

(defmethod next ((x py-enumerate))
  ;; Will raise StopIteration as soon as (next generator) does that.
  (with-slots (index generator) x
    (let ((res (funcall generator)))
      (if res
	  (prog1 (make-tuple (make-array 2 :initial-contents (list index res)))
	    (incf index))
	(py-raise 'StopIteration "Finished")))))
    
(register-bi-class-attr/meth (find-class 'py-enumerate) 'next #'next)
(register-bi-class-attr/meth (find-class 'py-enumerate) '__iter__ #'__iter__)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XRange object
;; 
;; Like built-in function `range', but lazy. It's a type of its own.

(defclass py-xrange (builtin-instance)
  ((start :type integer :initarg :start)
   (stop  :type integer :initarg :stop)
   (step  :type integer :initarg :step)
   (max-num-steps :type integer :initarg :max-num-steps))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-xrange))

(defun make-xrange (x &optional y z)
  ;; XX is there a need to check step has right sign? (also range())
  (flet ((xrange-2 (start stop step)
	   (ensure-py-type (start stop step) integer
			   "arguments to xrange() must be int (got: ~A)")
	   (let (max-num-steps)
	     (cond ((or (and (< start stop)
			     (< 0 step))
			(and (> start stop)
			     (> 0 step)))
		    ;; range ok
		    (setf max-num-steps (floor (/ (- (- stop 1) start)
						  step)))
		    (setf stop (+ start (* step max-num-steps))))
		   
		   (t
		    ;; bogus range: return no values at all
		    (setf start 0 stop 0 step 0 max-num-steps 0)))
	     
	     (make-instance 'py-xrange :start start :stop stop
			    :step step :max-num-steps max-num-steps))))
    
    (cond (z (xrange-2 x y z))
	  (y (xrange-2 x y 1))
	  (t (xrange-2 0 x 1)))))

(defmethod __new__ ((x (eql (find-class 'py-xrange))) &rest args)
  (unless args
    (py-raise 'ValueError "xrange(): at least 1 pos-arg required (got none)"))
  (apply #'make-xrange args)) 

(register-bi-class-attr/meth (find-class 'py-xrange) '__new__ (make-static-method #'__new__))

;; xrange has default noop __init__

(defmethod __iter__ ((x py-xrange))
  (let* ((start (slot-value x 'start))
	 (stop (slot-value x 'stop))
	 (step (slot-value x 'step))
	 (i start)
	 (stopped-already (= i stop)))
    (make-iterator-from-function
     (lambda ()
       (unless stopped-already
	 (setf stopped-already (= i stop))
	 (prog1 i
	   (incf i step)))))))

(defmethod __getitem__ ((x py-xrange) index)
  (ensure-py-type index integer
		  "index arguments to xrange[] must be int (got: ~A)")
  (let ((start (slot-value x 'start))
	(stop (slot-value x 'stop))
	(step (slot-value x 'step))
	(max-num-steps (slot-value x 'max-num-steps)))
    
    (cond ((and (>= index 0)
		(<= index max-num-steps))
	   (+ start (* step index)))
	  ((and (< index 0)
		(>= index (- max-num-steps)))
	   (- stop (* step (1+ index)))) ;; 1+ because `stop' is one too far
	  (t
	   (py-raise 'IndexError
		     "xrange(~A,~A,~A) index out of range (got: ~A)"
		     start stop step index)))))

(defmethod __len__ ((x py-xrange))
  (1+ (slot-value x 'max-num-steps)))

(defmethod print-object ((x py-xrange) stream)
  (print-unreadable-object (x stream :identity t :type t)
    (with-slots (start stop step) x
      (format stream ":start ~A  :stop ~A  :step ~A" start stop step))))

(defmethod __hash__ ((x py-xrange))
  (mod-to-fixnum (logxor (slot-value x 'start)
			 (slot-value x 'stop)
			 (slot-value x 'step))))

(loop for name in '(__len__ __getitem__ __iter__)
    do (register-bi-class-attr/meth (find-class 'py-xrange) name (symbol-function name)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Property

;; Property is a built-in class; it's instances implement all three of
;; the `descriptor' methods (even if not all three arguments are
;; filled by arguments given in the constructor).
;; 
;; Usage is like:
;; 
;;   class C:
;;     def getx(self): ..
;;     def setx(self, val): ..
;;     def delx(self): ..
;;     x = property(getx, setx, delx, "doc")
;; 
;; Relevant documentation on how CPython behaves:
;;  http://mail.python.org/pipermail/python-dev/2003-May/035791.html

(defclass py-property (builtin-object)
  ((get    :initarg :get)
   (set    :initarg :set)
   (delete :initarg :delete)
   (doc    :initarg :doc))
  (:documentation "The PROPERTY built-in class. User-defined `properties' ~
                   are instances of this class.")
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-property))

(defmethod py-property-__new__ (&optional pos-args kwd-args)
  (let ((cls (car pos-args)))
    (assert (and cls
		 (subtypep cls (find-class 'py-property))))
    (let* ((call-rewriter (make-call-rewriter 'property.__new__
					      () '((fget . nil) (fset . nil)
						   (fdel . nil) (doc . nil))
					      nil nil))
	   (args (funcall call-rewriter (cdr pos-args) kwd-args))
	   (fget (or (assoc 'fget args) *None*))
	   (fset (or (assoc 'fset args) *None*))
	   (fdel (or (assoc 'fdel args) *None*))
	   (doc  (or (assoc 'doc args) *None*))
	       
	   (inst (make-instance cls)))
      (setf (slot-value inst 'get) fget
	    (slot-value inst 'set) fset
	    (slot-value inst 'delete) fdel
	    (slot-value inst 'doc) doc)
      inst)))

(register-bi-class-attr/meth (find-class 'py-property) '__new__
			     (make-static-method
			      (make-lisp-function-accepting-kw-args #'py-property-__new__)))

(defmethod fget ((x py-property))
  (slot-value x 'get))

(defmethod fset ((x py-property))
  (slot-value x 'set))

(defmethod fdel ((x py-property))
  (slot-value x 'delete))

(register-bi-class-attr/meth (find-class 'py-property) 'fget (make-bi-class-attribute #'fget))
(register-bi-class-attr/meth (find-class 'py-property) 'fset (make-bi-class-attribute #'fset))
(register-bi-class-attr/meth (find-class 'py-property) 'fdel (make-bi-class-attribute #'fdel))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Super

(defclass py-super (builtin-object)
  ((object :initarg :object)
   (current-class :initarg :current-class))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-super))

;; super( <B class>, <C instance> ) where C derives from B:
;;   :instance = <C instance>
;;   :current-class = <B class>

#+(or) ;; old
(defmethod py-super-__new__ (cls type &optional obj)
  (break "super.__new__")
  (assert (subtypep cls (find-class 'py-super)))
  (if (eq cls (find-class 'py-super))
      (if obj
	  (make-bound-super type obj)
	(make-unbound-super type))
    (progn (assert (subtypep cls (find-class 'py-super)))
	   (error "todo: super(...) with cls != py-super"))))

#+(or) ;; old
(register-bi-class-attr/meth (find-class 'py-super) '__new__
			     (make-static-method #'py-super-__new__))

;; A typical use for calling a cooperative superclass method is:
;; 
;;  class C(B):
;;    def meth(self, arg):
;;      super(C, self).meth(arg)

(defmethod py-super-__new__ (cls class-arg &optional second-arg)
  (assert (subtypep cls (find-class 'py-super)))
  
  (cond ((not (typep class-arg 'class))
	 (py-raise 'TypeError
		   "First arg to super.__new__() must be class (got: ~A)" class-arg))
	 
	((null second-arg)
	 (warn "super() with one arg is TODO, but faking anyway")
	 (lambda (sec-arg) (py-super-__new__ cls class-arg sec-arg)))
	  
	((typep second-arg class-arg)

	 ;; like:  super( <B class>, <C instance> )
	 ;; in the CPL of class C, find the class preceding class B
	 ;; 
	 ;; CPython returns a `super' instance and not directly the
	 ;; class to allow subclassing class `super' and overriding
	 ;; the __getattribute__ method.
	 
	 (make-instance cls :object second-arg :current-class class-arg))
	 
	((typep second-arg 'class)
	 (unless (subtypep second-arg class-arg)
	   (py-raise 'TypeError "Calling `super' with two classes: second must ~@
                                  be subclass of first (got: ~A, ~A)"
		     class-arg second-arg))
	 (make-instance cls :object second-arg :current-class class-arg))
	
	(t (error "TODO super clause"))))

(register-bi-class-attr/meth (find-class 'py-super) '__new__ 
			     (make-static-method #'py-super-__new__))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type
;; 
;; The Python type from which all other types (classes) are derived.
;; It is defined in classes.cl.

#+(or)
(defmethod __call__ ((x (eql (find-class 'python-type))) &optional pos key)
  (declare (ignorable pos key))
  (break "__call__ on `type'"))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Python object stuff


;; Convert a number or string to a number (in numeric types constructors)

(defmethod convert-to-number :around (x cls)
  (declare (ignore x))
  (let ((res (call-next-method)))
    
    (cond ((and (eq cls 'complex) (typep res 'number))
	   res)
	  ((typep res cls)
	   res)
	  (t
	   (py-raise 'TypeError "Expected a ~A, got (perhaps after conversion) ~A" cls res)
	   res))))
     
(defmethod convert-to-number ((x string) cls)
  ;; Check that string is harmless; then call READ on it.
  (declare (ignore cls))
  (loop for c across x
      unless (or (digit-char-p c 16)
		 (member c '(#\x #\. #\- #\j)))
      do (py-raise 'ValueError "Can't convert string to number: ~W" x))
  (read-from-string x))

(defmethod convert-to-number ((x number) cls)
  (declare (ignore cls))
  x)

(defmethod convert-to-number ((x py-number) cls)
  (declare (ignore cls))
  (slot-value x 'val))

(defmethod convert-to-number ((x py-string) cls)
  (convert-to-number (slot-value x 'string) cls))

(defmethod convert-to-number (x cls)
  (declare (ignore cls))
  (py-raise 'TypeError "Can't convert to number: ~A" x))



(deftype attribute-name-designator ()
  `(or symbol string))

(defun attribute-name-designator-p (x)
  "Return ATT-DES-P, SYMBOL"
  (typecase x
    (symbol (values t x))
    (string (values t (intern x #.*package*)))
    (t      nil)))


;;;; python object?

(deftype python-object-designator ()
  `(or python-object number))

(defgeneric python-object-designator-p (x)
  (:documentation "Returns DESIGNATOR-P, PYVAL where PYVAL is a ~
                   Python object iff DESIGNATOR-P ")
  (:method ((x (eql (find-class 'builtin-instance)))) nil)
  (:method ((x python-object)) t)
  (:method ((x (eql (find-class 'python-type)))) t)
  (:method ((x number)) t)
  (:method ((x string)) t)
  (:method ((x symbol)) t)
  (:method (x) (declare (ignore x)) nil))


(defgeneric py-object-designator-val (x)
  (:method ((x python-object))                   x)
  (:method ((x (eql (find-class 'python-type)))) x) ;; needed?
  (:method ((x number))                        (make-py-number x))
  (:method ((x string))                        (make-py-string x))
  #+(or)(:method ((x symbol))                        (symbol-name x))
  (:method (x) (error "Uncatched in PY-OBJECT-DESIGNATOR-VAL: ~S" x)))


;;;; builtin object?

(defgeneric builtin-object-designator-p (x)
  (:documentation "Returns DESIGNATOR-P"))

;; basically, everything except user-defined stuff
(defmethod builtin-object-designator-p (x)
  (declare (ignore x))
  t)

(defmethod builtin-object-designator-p ((x user-defined-object))
  nil)


;;; useful shortcuts

(defmethod py-str (x)
  (call-attribute-via-class x '__str__))

(defmethod format-py-str (stream argument colon? at?)
  (declare (ignore colon? at?))
  (let ((s (py-str argument)))
    (setf s (py-string-designator-val s)) ;; as S may be py-string subclass instance
    (write-string s stream)))


(defmethod py-repr (x)
  (call-attribute-via-class x '__repr__))

(defmethod format-py-repr (stream argument colon? at?)
  (declare (ignore colon? at?))
  (let ((s (py-repr argument)))
    (setf s (py-string-designator-val s)) ;; as S may be py-string subclass instance
    (write-string s stream)))