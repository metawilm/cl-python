;; (in-package :bimeth)
(in-package :python)

(defvar *check-attr-exists* t)

;; Python's `magic' methods
;; 
;; Those that start and end with two underscores, like `__len__',
;; `__add__'. They are part of a "protocol": "a + b" => a.__add__(b),
;; "len(x)" => x.__len__().

;;; XXX __reduce__

(defmacro make-py-methods (&rest meths)
  `(progn
     ,@(loop for (methname params doc) in meths collect
	     `(progn
		(defgeneric ,methname ,params
			    (:documentation ,doc))

		#+(or) ;; can't discriminate like this
		(defmethod no-applicable-method ((f (eql #',methname)) &rest args)
		  (signal '%magic-method-missing%)
		  (py-raise 'TypeError
			    "No method ~A defined for args ~S [no-app-meth ~A]"
			    ',methname args ',methname))
		
		,@(let ((real-params (remove '&optional params)))
		    (declare (ignorable real-params))
		    `(
		      ;; In case of instances of user-defined classes,
		      ;; check if the built-in method is overruled in
		      ;; the class.
		      #+(or)
		      (defmethod ,methname :around ((,(car params) udc-instance) ,@(cdr params))
			(when *check-attr-exists*
			  (multiple-value-bind (meth found)
			      (internal-get-attribute (class-of ,(car params)) ',methname)  ;; or use __class__ instead of class-of?
			    (when found
				(let ((*check-attr-exists* t))
				  (__call__ meth (list ,@real-params))))))
			
			(py-raise 'TypeError
				  "No method ~A defined for user-defined class instance ~A"
				  ',methname ,(car params)))
		      
		      #+(or)
		      (defmethod ,methname :around ((,(car params) t) ,@(cdr params))
			(declare (ignore ,@(cdr params)))
			(if (next-method-p)
			    (call-next-method)
			  (py-raise 'TypeError
				    "No method ~A defined for arg ~S [XXX]"
				    ',methname ,(car params))))
		      
		      #+(or)
		      (defmethod ,methname ((,(car params) udc-instance) ,@(cdr params))
			;; When this method is called, it means the
			;; corresponding method is not just looked up
			;; but really that it is being called:
			;; `instance.methname(params..)'.

			(multiple-value-bind (meth found)
			    (internal-get-attribute ,(car params) ',methname)
			  
			  ;; Internal-get-attribute look at base classes too, also
			  ;; when a base class is a built-in classes.
			  ;; 
			  ;; This might return a bound method. We can
			  ;; avoid this binding by inlining the
			  ;; attribute lookup and calling, in a new
			  ;; method `internal-get-attribute-call',
			  ;; say.
			  
			  (cond
			   
			   ((and found (pyb:callable meth))
			    ;; XXX need to match keyword arguments here
			    ;; (like, accept `other' keyword in: def __lt__(self, other): ..
			    (let ((res (__call__ meth (list ,@(cdr real-params)))))
			      (declare (special *NotImplemented*))
			      (when (eq res *NotImplemented*)
				(signal '%not-implemented-result%)) ;; why?
			      res))
			   
			   (found
			    (py-raise 'TypeError "~A instance is not callable"
				      (class-of ,(car params))))
			   
			   ((not found)
			    ;; Maybe there is default behaviour defined on
			    ;; PYTHON-OBJECT (this is the case for `__repr__'
			    ;; for example (but not for `__eq__').  If not,
			    ;; call-next-method will give an AttributeError.
			    (if (next-method-p)
				(call-next-method)
			      (progn
				(signal '%magic-method-missing%)
				(py-raise 'AttributeError "~A instance has no attribute ~A"
					  ,(car params) ',methname)))))))))))))
		      


(make-py-methods
 (__doc__ (x) "documentation for X")

 ;;; Order as in the Python Reference Manual
 ;;; 3.3.1: Basic Customization
 
 ;; string representation
 ;; XXX these are handled in builtin-classes.cl
 (__repr__ (x) "repr(x) -> readable string representation of x") ;; both must return string
 (__str__  (x) "str(x) -> string representation of x") ;; fallback: __repr__

 ;; Comparison
 (__eq__ (x y) "x == y") ;; these can return any value, but bool (True/False) is common
 (__ne__ (x y) "x != y")
 (__lt__ (x y) "x < y")
 (__gt__ (x y) "x > y")
 (__le__ (x y) "x <= y")
 (__ge__ (x y) "x >= y")
 
 (__cmp__ (x y) "cmp(x,y) -- rich comparison") ;; int: - => x<y; + => x>y; 0 => x=y
 (__nonzero__ (x) "truth value testing") ;; True/False, or 1/0
 
 (__hash__ (x) "hash(x) -> int") ;; must be integer (XXX sxhash -> fixnum)
 (__unicode__ (x) "unicode") ;; XXX not implemented yet
 
 ;;; 3.3.2: Customizing attribute access
 (__getattr__ (x name) "when attribute lookup failed")
 (__setattr__ (x name value) "all attribute assignments")
 (__delattr__ (x name) "attribute deletion")
 
 ;;; 3.3.2.1: More attribute access for new-style classes
 (__getattribute__ (x name) "all attribute lookups")
 
 ;;; 3.3.2.2 Implementing Descriptors
 (__get__ (self instance owner-class) "Get property attribute value of INSTANCE of OWNER-CLASS")
 (__set__ (self instance value) "Set property attribute to VALUE on INSTANCE")
 (__del__ (self instance) "Delete property attribute of INSTANCE")

 ;;; 3.3.2.4 __slots__
 ;; __slots__ is represented by a CLOS slot
 
 ;;; 3.3.3 Customizing class creation
 ;; __metaclass__ is not supperted yet
 
 ;;; 3.3.4 Emulating callable objects
 ;; __call__ is done below, because it's special.
 
 ;;; 3.3.5 Emulating container types
 (__len__ (x) "len(x) -> integer >= 0")
 
 (__getitem__ (x key) "x[key]")
 (__setitem__ (x key val) "x[key] = val")
 (__delitem__(x key) "del x[key]")
 
 (__iter__ (x) "iter(x) -> iterator object")
 (__contains__ (x item) "if item in x: ...")
 
 ;;; 3.3.6 Additional methods for emulation of sequence types
 ;; __getslice__, __setslice__, __delslice__ are deprecated so not supported
 
 ;;; 3.3.7 Emulating numberic types

 ;; regular ops
 (__add__ (x y) "x + y")
 (__sub__ (x y) "x - y")
 (__mul__ (x y) "x * y")
 (__div__ (x y) "x / y")
 (__floordiv__ (x y) "x // y")
 (__mod__ (x y) "x % y")
 (__divmod__ (x y) "divmod(x,y)") ;; should be ( _floordiv_(), _mod_() )
 #+(or)(__pow__ (x y &optional z) "pow(x,y[,z])")
 (__lshift__ (x y) "x << y")
 (__rshift__ (x y) "x >> y")
 (__and__ (x y) "x & y")
 (__xor__ (x y) "x ^ y")
 (__or__ (x y) "x | y")
 
 (__truediv__ (x y) "x / y  when __future__.division in effect")
 ;; has priority over __div__ iif __future__.division in effect

 ;; For division operators, from PEP 238:
 ;;  http://www.python.org/peps/pep-0238.html
 ;;
 ;; When both arguments to div are ints, implementes FLOOR division:
 ;;   1/3 = 0
 ;;   1 // 3 = 0  (// means `floor division')
 ;; when at least one argument is float, implements TRUE division:
 ;;   1.0/3 = 1.0/3.0 = 0.333
 ;;
 ;; After `from __future__ import division':
 ;;   1/3 = 0.333
 ;;   1//3 = 0
 
 
 ;; reversed ops
 ;;  e.g. for  a ** b
 ;;       try  pow(a,b)
 ;;  then try  rpow(b,a)
 (__radd__ (x y) "reversed x + y")
 (__rsub__ (x y) "reversed x - y")
 (__rmul__ (x y) "reversed x * y")
 (__rdiv__ (x y) "reversed x / y")
 (__rfloordiv__ (x y) "reversed x // y")
 ;; __rtruediv__ is not supported either
 (__rmod__ (x y) "reversed x % y")
 (__rdivmod__ (x y) "reversed divmod(x,y)")
 #+(or)(__rpow__ (x y &optional z) "reversed x ** y")
 (__rlshift__ (x y) "reversed x << y")
 (__rrshift__ (x y) "reversed x >> y")
 (__rand__ (x y) "reversed x & y")
 (__rxor__ (x y) "reversed x ^ y")
 (__ror__ (x y) "reversed x | y")

 ;; in-place ops
 (__iadd__ (x y) "x += y")
 (__isub__ (x y) "x -= y")
 (__imul__ (x y) "x *= y")
 (__idiv__ (x y) "x /= y")
 (__ifloordiv__ (x y) "x //= y")
 ;; __itruediv__ is not supported either
 (__imod__ (x y) "x %= y")
 ;; there's no __idivmod__ (x y)
 (__ipow__ (x y &optional z) "x **= y  or  x.__ipow__(y,z)") ;; x **= y -- no way to specify z?
 (__ilshift__ (x y) "x <<= y")
 (__irshift__ (x y) "x >>= y")
 (__iand__ (x y) "x &= y")
 (__ixor__ (x y) "x ^= y")
 (__ior__ (x y) "x |= y")

 (__neg__(x) "-x")
 (__pos__(x) "+x")
 (__abs__(x) "abs(x)")
 (__invert__(x) "~x")
 
 (__complex__(x) "complex(x)")
 (__int__ (x) "int(x)")
 (__long__ (x) "long(x)")
 (__float__ (x) "float(x)")
 
 (__oct__ (x) "oct(x)")
 (__hex__ (x) "hex(x)")
 
 (__coerce__ (x y) "coercion")
 ;; should return (x', y') where x', y' are standard numbers
 ;; or None or NotImplemented, indicating coercion failure
 )


;;; These mappings are used for automatically generating the
;;; appropriate methods for builtin-int etc, in builtin-classes.cl

(defvar *reverse-method-mapping*
    '((__radd__      . __add__)
      (__rsub__      . __sub__)
      (__rmul__      . __mul__)
      (__rdiv__      . __div__)
      (__rtruediv__  . __truediv__)
      (__rfloordiv__ . __floordiv__)
      (__rmod__      . __mod__)
      (__rdivmod__   . __divmod__)
      (__rpow__      . __pow__)
      (__rlshift__   . __lshift__)
      (__rrshift__   . __rshift__)
      (__rand__      . __and__)
      (__rxor__      . __xor__)
      (__ror__       . __or__)))

(defvar *inplace-method-mapping*
    '((__iadd__      . __add__)
      (__isub__      . __sub__)
      (__imul__      . __mul__)
      (__idiv__      . __div__)
      (__itruediv__  . __truediv__)
      (__ifloordiv__ . __floordiv__)
      (__imod__      . __mod__)
      (__idivmod__   . __divmod__)
      (__ipow__      . __pow__)
      (__ilshift__   . __lshift__)
      (__irshift__   . __rshift__)
      (__iand__      . __and__)
      (__ixor__      . __xor__)
      (__ior__       . __or__)))



;; these are special

(defgeneric __new__ (class &rest args)
  (:documentation "Allocate new instance of CLASS"))

(defgeneric __init__ (x &rest args)
  (:documentation "Initialize class instance X"))


;;; Some of these methods have default behaviour.

;;(defmethod __cmp__ (x y)
;;  (if (eq x y)

#+(or)
(defmacro python:def-default-behaviours (clauses)
  `(progn ,@(loop for (meth params behaviour) in clauses
		collect (let ((real-params (remove '&optional params)))
			  `(defmethod ,meth ((,(car params) python-object) ,@(cdr params))
			     (declare (ignorable ,@real-params))
			     ,behaviour)))))

#+(or)
(python:def-default-behaviours
    ((__doc__ (x) *None*)

     (__repr__ (x) (with-output-to-string (s)
		     (print-unreadable-object (x s) :identity t :type t)))
     ;; str(x) defaults to repr(x), but method __str__ doesn't.
     
     ;; Comparison of objects is always possible. The fall-back is pointer comparison.
     (__eq__ (x y) (eq x y)) ;; __eq__ and __ne__ default to comparing pointers
     (__ne__ (x y) (not (eq x y)))
     
     (__lt__ (x y) (t))
     (__gt__ (x y) t)
     (__le__ (x y) t)
     (__ge__ (x y) t)

     (__hash__ (x) 123) ;; XXX pointer value
     ))     

;; some default behaviours
(defmethod __eq__ (x y)
  (declare (special *True* *False*))
  (if (eq x y) *True* *False*))

(defmethod __cmp__ (x y)
  (if (eq x y)
      0
    -1)) ;; or +1

;; there is some bug 
;;(defmethod __lt__ (x y)
;;  -1)