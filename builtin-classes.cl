(in-package :python)

;;; Built-in classes and their methods

;; TODO:
;;  - __mro__ attribute of classes
;;  - need for __eq__ when __cmp__ is already defined?

;; These macros ease GF method definition

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


;; (__call__ <<some-builtin-object>> args)
;; creates an instance of X based on (optional) args
;; for example, create a DICT whose initial content is ARGS)

(defmacro create-methods-all-classes (data)
  `(defvar *methods-of-all-classes*
       (let ((h (make-hash-table :test 'eq)))
	 ,@(loop for (name val) in data
	       collect `(setf (gethash ',name h) #',val))
	 h)))

(create-methods-all-classes
 ((__repr__   __repr__)
  (__eq__     __eq__)
  (__ne__     __ne__)
  ;; XXX __reduce__ (is a bit like `make-load-form')...
  ))
       

;; Regarding string representation of Python objects:
;;    __str__       is a representation targeted to humans
;;    __repr__      if possible,  eval(__repr__(x)) should be equal to x
;; 
;; To reduce duplication:
;;    print-object  falls back to writing __str__
;;    __str__       falls back to returning __repr__
;;    __repr__      returns by default the print-unreadable-object representation

(defmethod print-object ((x python-object) stream)
  (print-unreadable-object (x stream :identity t :type t))
  ;; it's not a good idea to fall back to __str__, because it gives
  ;; infinite loops when debugging __str__ methods, for example
  #+(or)(write-string (__str__ x) stream))

(defmethod __str__ (x) 
  ;; This method is not only for X of type python-object, but also for
  ;; regular numbers, for example.
  (__repr__ x))

(defmethod __repr__ (x)
  ;; Also for all X, not just Python objects.
  (with-output-to-string (s)
    (print-unreadable-object (x s :identity t :type t))))


(defgeneric __class__ (x)
  (:documentation "The class of X. X must be a python-object designator."))

(defmethod __class__ ((x integer))       (find-class 'py-int))
(defmethod __class__ ((x real))          (find-class 'py-float))
(defmethod __class__ ((x complex))       (find-class 'py-complex))
(defmethod __class__ ((x symbol))        (find-class 'py-string))
(defmethod __class__ ((x string))        (find-class 'py-string))
(defmethod __class__ ((x python-object)) (class-of x))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some special singleton classes: None, Ellipsis, NotImplemented

(defmacro def-static-singleton-classes (data)
  `(progn
     ,@(loop for (class-name class-doc object object-doc object-repr object-hash) in data
	   collect 
	     (progn (assert (typep object-hash 'fixnum))
		    `(progn (defclass ,class-name (builtin-object) ()
				      (:documentation ,class-doc)
				      (:metaclass builtin-class))
			    (mop:finalize-inheritance (find-class ',class-name))
			    
			    (defvar ,object (make-instance ',class-name) ,object-doc)
			    
			    ;; CPython disallows creating instances of these
			    (defmethod make-instance
				((c (eql (find-class ',class-name))) &rest initargs)
			      (declare (ignore initargs))
			      #1=(py-raise 'TypeError
					   "Cannot create '~A' instances" ',class-name))
			    (defmethod make-instance
				((c (eql ',class-name)) &rest initargs)
			      (declare (ignore initargs))
			      #1#)
			    
			    (defmethod __repr__ ((c ,class-name))
			      ,object-repr)
			    
			    (defmethod __hash__ ((c ,class-name))
			      ,object-hash))))))

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


(deftype py-number-designator ()
  `(or number py-number))

(defun py-number-designator-p (x)
  "Returns NUM-DES-P, LISP-NUM"
  (typecase x
    (number (values t x))
    (py-number (values t (slot-value x 'val)))
    (t nil)))


(defmethod make-py-number ((val number))
  "Make a PY-NUMBER instance for Lisp number VAL"
  (etypecase val
    (integer (make-int val))
    (real (make-float (coerce val 'double-float)))
    (complex (make-complex val))))

  
(defmethod __hash__ ((x py-number))
  (__hash__ (slot-value x 'val)))

(def-unary-meths 
    py-number number (slot-value x 'val)
    (
     ;; CPython prints *sys-neg-maxint* <= x <= *sys-pos-maxint* as X,
     ;; outside that range as XL:  3 vs 3L. Let's not bother.
     (__repr__     (format nil "~A" x))
     
     (__nonzero__  (make-bool (/= x 0)))
     (__neg__      (- x))
     (__pos__      x)
     (__abs__      (abs x))
     (__complex__  (make-complex x))))

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
     (__float__   (make-float x))))

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
     (__div__       (floor x y))
     (__rdiv__      (__div__ y x))
     (__floordiv__  (floor x y))
     (__rfloordiv__ (__floordiv__ y x))
     (__divmod__    (make-tuple-from-list (multiple-value-list (floor x y))))
     (__rdivmod__   (__divmod__ y x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complex (corresponds to Lisp type `complex')

(defclass py-complex (py-real)
  ((val :type complex :initarg :val :initform #C(0 0)))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-complex))

(defun make-complex (&optional (val #C(0 0)))
  (make-instance 'py-complex :val (coerce val 'complex)))


(def-unary-meths
    py-complex complex (slot-value x 'val)
    ((__hash__ (if (= (imagpart x) 0)
		   (__hash__ (realpart x))
		 (sxhash x)))
     (__repr__ (if (= (complex-imag x) 0)
		   (__repr__ (complex-real x))
		 (format nil "~(A + ~Aj)" (realpart x) (imagpart x))))
     (complex-real (realpart x))
     (complex-imag (imagpart x))
     (complex-conjugate (conjugate x))))


(defmacro def-class-specific-methods (class data)
  `(progn ,@(loop for (attname (kind . val)) in data
		collect `(setf (get ',attname (find-class ',class))
			   (cons ',kind #',val)))))

(def-class-specific-methods
    py-complex
    ((real (att . complex-real))
     (imag (att . complex-imag))
     (conjugate (meth . complex-conjugate))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Float (corresponding to Lisp type `double-float')

(defclass py-float (py-real)
  ((val :type double-float :initform 0d0))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-float))

(defun make-float (&optional (val 0d0))
  (make-instance 'py-float :val val))

(defmethod __hash__ ((x float))
  ;; general `float', not `double-float' as type!
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
#+(or)(defconstant *sys-pos-maxint* 2147483647)
#+(or)(defconstant *sys-neg-maxint* -2147483648)
    
(defclass py-int (py-real)
  ((val :type integer :initarg :val :initform 0))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-int))

(defun make-int (&optional (val 0))
  (make-instance 'py-int :val val))

(deftype py-int-designator ()
  `(or integer py-int))

(defun py-int-designator-p (x)
  "Return DESIGNATOR-P, LISP-VAL where LISP-VAL only makes sense if it's ~
   indeed a designator"
  (cond ((integerp x) (values t x))
	((typep x 'py-int) (values t (slot-value x 'val)))
	(t nil)))

(defun py-int-designator-p-2 (x)
  "Return one value: the int value"
  (cond ((integerp x) x)
	((typep x 'py-int) (slot-value x 'val))
	(t nil)))

(def-binary-meths
    py-int integer (slot-value x 'val) (slot-value y 'val)
    ;; bit operations -> lisp integer
    ((__and__ (logand x y))
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
     (__float__   (coerce x 'double-float))
     
     ;; string representations
     (__oct__  (format nil "0~O" x))
     (__hex__  (format nil "0x~X" x))))
     

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

(defmethod make-instance ((c py-bool) &rest initargs &key val)
  (if val *True* *False*))

(defun make-bool (&optional (val nil))
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

(defmethod print-object ((x py-bool) stream)
  (format stream 
	  (if (eq x *True*) "True" "False")))

(defmethod __repr__ ((x py-bool))
  (with-output-to-string (s)
    (print-object x s)))

;;; Conversions to a Python boolean

(defun py-val->lisp-bool (x)
  "Somewhat contrary to its name, VAL is either a Lisp or Python value. ~@
   Returns a generalized Lisp boolean."
  (declare (special *interned-empty-string*))
  (cond
   ;; Lisp: t/nil,  Python: True/False/None
   ((member x (load-time-value (list 't *True*)) :test 'eq) t)
   ((member x (load-time-value (list nil *False* *None*)) :test 'eq) nil)
       
   ((numberp x) (/= x 0))    ;; py-number designators
   ((symbolp x) (not (eq x *interned-empty-string*))) ;; py-string designator
   
   ;; class instance: check __nonzero__, __len__ methods
   (t (py-lisp-bool-1 x))))

(defun py-lisp-bool-1 (x)
  ;; Determine truth value of X. Returns a Lisp bool.
  (handler-case (let ((res (__nonzero__ x)))
		  (multiple-value-bind (bool-p lisp-bool)
		      (py-bool-designator-p res)
		    (unless bool-p
		      (py-raise 'TypeError
				"__nonzero__ should return a bool (got: ~A)" res))
		    (return-from py-lisp-bool-1 (not (__eq__ lisp-bool 0)))))
    (%magic-method-missing% ()
      (handler-case (let ((res (__len__ x)))
		      (multiple-value-bind (int-p int-len)
			  (py-int-designator-p res)
			(unless (and int-p (<= 0 int-len))
			  (py-raise 'TypeError
				    "__len__ should return a non-negative int (got: ~A)"
				    res))
			(return-from py-lisp-bool-1 (not (= int-len 0)))))
	(%magic-method-missing% ()
	  ;; If a class defined neither __nonzero__ nor
	  ;; __len__, all instances are considered `True'.
	  t)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dictionary

(defun safe-py-hash (x)
  "ACL requires hash value to be fixnum: make sure it is."
  (assert (python-object-designator-p x) ()
      "Attempt to put a non-Python value in a Python dict: ~A" x)
  (let ((hash-value (__hash__ x)))
    (assert (typep hash-value 'fixnum) () "Hash code should be fixnum! (~A ~A)"
	    x hash-value)
    hash-value))

(defclass py-dict (builtin-instance)
  ;; TODO: if there are only a very few items in the dict, represent
  ;; it as an alist or something similarly compact.
  ((hash-table :initform (make-hash-table :test '__eq__  :hash-function 'safe-py-hash)))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-dict))

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

(defmethod print-object ((x py-dict) stream)
  (format stream "{")
  (pprint-logical-block (stream nil)
    (maphash (lambda (k v)
	       (format stream "~A: ~A, ~_" k v))
	     (slot-value x 'hash-table)))
  (format stream "}"))

#+(or) ;; todo
(defmethod __new__ ((x py-dict)))

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
    
  

(defmethod __eq__ ((x py-dict) (y py-dict))
  "Returns T or NIL."
  (= (__cmp__ x y) 0))

(defmethod __getitem__ ((d py-dict) key)
  (multiple-value-bind (val found)
      (gethash key (slot-value d 'hash-table))
    (if found
	val
      (py-raise 'KeyError "No such key: ~A" (__str__ key)))))

(defmethod __setitem__ ((d py-dict) key val)
  (setf (gethash key (slot-value d 'hash-table) d) val))

(defmethod __delitem__ ((d py-dict) key)
  (remhash key (slot-value d 'hash-table)))
    
		 
;;; willem __add__ etc(?)

(defmethod __str__ ((d py-dict))
  (with-output-to-string (s)
    (format s "{")
    (pprint-logical-block (s nil)
      (maphash (lambda (k v)
		 (format s "~A: ~A, ~_" k v))
	       (slot-value d 'hash-table)))
    (format s "}")))

(defmethod __repr__ ((d py-dict))
  (__str__ d))


(defmethod __len__ ((d py-dict))
  (hash-table-count (slot-value d 'hash-table)))

(defmethod __nonzero__ ((d py-dict))
  (make-bool (/= 0 (hash-table-count (slot-value d 'hash-table)))))

;;;; Dict-specific methods, in alphabetic order

(defmethod dict-clear ((d py-dict))
  "Clear all items"
  (clrhash (slot-value d 'hash-table))
  (values))

(defmethod dict-copy ((d py-dict))
  "Create and return copy of dict. Keys and values themselves are not copied."
  (let ((m (make-dict)))
    (setf (slot-value m 'hash-table) (slot-value d 'hash-table))
    m))

(defmethod dict-fromkeys (seq &optional (val *None*))
  (let* ((d (make-dict))
	(ht (slot-value d 'hash-table)))
    (py-iterate (key seq)
		(setf (gethash key ht) val))
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
    found-p))

(defmethod dict-items ((d py-dict))
  "Return list of (k,v) tuples"
  (let* ((h (slot-value d 'hash-table))
	 (res ()))
    (maphash (lambda (k v) (push (make-tuple k v) res))
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
		      (make-tuple key val))))))))
    res))

(defmethod dict-iter-keys ((d py-dict))
  "Return iterator that successively returns all keys"
  (let ((res (with-hash-table-iterator (next-f (slot-value d 'hash-table))
	       (make-iterator-from-function
		(lambda () 
		  (multiple-value-bind (ret key val) 
		      (next-f)
		    (declare (ignore val))
		    (when ret
		      key)))))))
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

(defmethod dict-pop ((d py-dict) key &optional default)
  "Remove KEY from D, returning its value. If KEY absent, DEFAULT ~
   is returned or KeyError is raised."
  (with-slots (hash-table) d
    (multiple-value-bind (val found)
	(gethash key hash-table)
      (cond (found (remhash key hash-table)
		   val)
	    (default)
	    (t (py-raise 'KeyError "No key ~A in dict" key))))))

(defmethod dict-popitem ((d py-dict))
  (with-slots (hash-table) d
    (with-hash-table-iterator (it hash-table)
      (multiple-value-bind (entry? key val)
	  (it)
	(if entry?
	    (progn
	      (remhash key hash-table)
	      (make-tuple key val))
	  (py-raise 'KeyError "popitem: dictionary is empty"))))))

(defmethod dict-setdefault ((d py-dict) key defval)
  "Lookup KEY and return its val;
   if KEY doesn't exist, add it and set its val to DEFVAL, then return DEFVAL"
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

(def-class-specific-methods
    py-dict
    ((clear (meth . dict-clear))
     (copy  (meth . dict-copy))
     (fromkeys (meth . dict-fromkeys))
     (get (meth . dict-get))
     (has_key (meth . dict-has-key))
     (items (meth . dict-items))
     (iteritems (meth . dict-iter-items))
     (iterkeys  (meth . dict-iter-keys))
     (itervalues (meth . dict-iter-values))
     (keys (meth . dict-keys))
     (pop  (meth . dict-pop))
     (popitem (meth . dict-popitem))
     (setdefault (meth . dict-setdefault))
     (values (meth . dict-values))))
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Namespace
;; 
;; The methods and attributes of a class, and the lexical scope inside
;; a function, are represented by namespace objects. A namespace
;; behaves like a py-dict.
;; 
;; Compared to dics, namespaces have extra atrtibutes `name' (for
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

(defclass namespace (py-dict)
  ((name :initarg :name :type string)
   (enclosing-ns :initarg :inside :initform nil)
   (hash-table :initform (make-hash-table :test 'eq)))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'namespace))

(defun make-namespace (&key (inside nil) (name nil) (builtins nil))
  "Make a new namespace.
   BUILTINS indicates whether attribute `__builtins__ should ~
   be created and pointed to the namespace with built-in functions, denoted with *builtins*.
   INSIDE gives the enclosing scope(s)."
  (let ((ns (make-instance 'namespace :name name :inside inside)))
    (declare (special *builtins*))
    (when builtins
      (namespace-bind ns '__builtins__ *builtins*))
    ns))

(defmethod namespace-bind ((x namespace) var val)
  (ensure-py-type var attribute-name "Invalid attribute name: ~A")
  (setf (gethash var (slot-value x 'hash-table)) val))

(defmethod namespace-lookup ((x namespace) var)
  "Recursive lookup. Returns two values:  VAL, FOUND-P"
  (ensure-py-type var attribute-name "Invalid attribute name: ~A")
  (let ((res (gethash var (slot-value x 'hash-table))))
    (cond (res
	   (values res t))
	  ((slot-value x 'enclosing-ns)
	   (namespace-lookup (slot-value x 'enclosing-ns) var))
	  (t
	   nil))))

(defmethod namespace-delete ((x namespace) var)
  "Delete the attribute."
  ;; todo: when in an enclosing namespace
  (ensure-py-type var attribute-name "Invalid attribute name: ~A")
  (let ((res (remhash var (slot-value x 'hash-table))))
    (unless res
      (py-raise 'NameError
		"No variable with name ~A" var))))

(defmethod __getitem__ ((x namespace) key)
  (gethash key (slot-value x 'hash-table)))

(defmethod print-object ((x namespace) stream)
  (pprint-logical-block (stream nil)
    (format stream "Namespace{")
    (maphash (lambda (k v) 
	       (declare (special *builtins*))
	       (format stream "~S: ~S,~_ " k 
		       (if (eq v *builtins*)
			   '*builtins*
			 v)))
	     (slot-value x 'hash-table)))
  (format stream "}"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function
;; 
;; There are two types of Python functions:
;; 
;; BUILTIN-FUNCTION represents the functions present in de __builtin__
;; module (implemented as Lisp functions)
;; 
;; USER-DEFINED-FUNCTION is used for representing all functions
;; defined while running Python.

(defclass python-function (builtin-instance)
  ()
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'python-function))

  
      

#+(or) ;; unused -- built-in functions are directly called (after argument massaging)
(progn (defclass builtin-function (python-function)
	 ((name :initarg :name)
	  (func :initarg :func :type function :documentation "The Lisp function"))
	 (:documentation "A built-in function wraps a Lisp function. ~@
                   It may accept positional arguments, but no keyword arguments.")
	 (:metaclass builtin-class))
       ;; XXX there's no need for builtin-function
	 ;; to store the number of parameters as CL will check that upon
	 ;; call. Because keyword args are not allowed, the formal arg names
	 ;; don't matter either.


       (defun make-builtin-function (name func)
	 (check-type func function)
	 (make-instance 'builtin-function :name name :func func))


       (defmethod print-object ((x builtin-function) stream)
	 (print-unreadable-object (x stream)
	   (with-slots (name func) x
	     (format stream "built-in function ~A -- wrapper for ~A" name func))))


       (defun builtin-function-p (x)
	 (typep x 'builtin-function)))


(defclass user-defined-function (python-function)
  ((name :initarg :name :type string)
   (ast :initarg :ast :documentation "The AST of the function code")
   (namespace :initarg :namespace
	      :type namespace
	      :documentation "The namespace in which the function code is ~
                              executed (the lexical scope -- this is not ~
                              func.__dict__)")
   (params :initarg :params
	   :documentation "The formal function parameters ~
                           (e.g. '((a b) ((c . 3)(d . 4)) args kwargs")
   (call-rewriter :initarg :call-rewriter
		  :documentation "Function that normalizes actual arguments")
   (enclosing-scope :initarg :enclosing-scope
		    :documentation "The namespace enclosing `namespace'"))
  (:documentation "A Python function, user-defined or built-in.")
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'user-defined-function))

(defmethod __repr__ ((x user-defined-function))
  (with-output-to-string (s)
    (print-unreadable-object (x s :type t :identity t)
      (format s "~A" (slot-value x 'name)))))

(defun make-user-defined-function (&rest options &key namespace &allow-other-keys)
  "Make a python function"
  (check-type namespace namespace)
  (apply #'make-instance 'user-defined-function options))

(defun python-function-p (f)
  "Predicate: is F a python function?"
  (typep f 'user-defined-function))

(defmethod print-object ((f user-defined-function) stream)
  (print-unreadable-object (f stream)
    (with-slots (name params) f
      (format stream "user-defined function ~A with params ~A" name params))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module

(defclass py-module (builtin-instance)
  ((name :initarg :name :type string)
   (namespace :initarg :namespace :type namespace))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-module))

(defun make-module (&rest options)
  (apply #'make-instance 'py-module options))

(defmethod print-object ((x py-module) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~A" (slot-value x 'name))))

#+(or)
(defmethod __getattr__ ((x py-module) attrname)
  (ensure-py-type attrname attribute-name "module getattr: invalid attr: ~A")
  (namespace-lookup (slot-value x 'namespace) attrname))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods: they come in `bound' and `unbound' flavors
;;
;; An UNBOUND METHOD is a method that is looked up via the class,
;; while a BOUND-METHOD is the result of looking up a class method via
;; an instance. For example:
;;
;;   class C:
;;      def meth(self, ...): ...
;;
;;   C.meth  -> is an unbound method
;;   x = C()
;;   x.meth  -> is a bound method
;;
;; Python attributes of both bound and unbound methods:
;;  `im_class' : the class attribute (here: C)
;;
;; Extra Python attributes for bound methods:
;;  `im_self'  : the instance (here: x)
;;  `im_func'  : the function object (meth)

(defclass py-method (builtin-instance) ()
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-method))

;;;; Unbound

(defclass py-unbound-method (py-method)
  ((class :initarg :class
	  :documentation "The class from which the method is taken.")
   (func :initarg :func :type python-function
	 :documentation "The method itself (of type PYTHON-FUNCTION)."))
  (:documentation "A method from a Python class (NOT bound to ~
                   a class instance).")
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-unbound-method))

(defun make-unbound-method (&rest options)
  (apply #'make-instance 'py-unbound-method options))

(defun unbound-method-p (m)
  (typep m 'py-unbound-method))

(defmethod print-object ((x py-unbound-method) stream)
  (print-unreadable-object (x stream :identity nil :type t)
    (with-slots (class func) x
      (format stream "~_:class ~S~_:func ~S" class func))))


;;;; Bound

(defclass py-bound-method (py-method)
  ((class :initarg :class
	  :documentation "The class from which the method is taken.")
   (func :initarg :func :type python-function
	 :documentation "The method itself (of type PYTHON-FUNCTION).")
   (self :initarg :self
	 :documentation "The instance to which the method is bound."))
  (:documentation "A method from a Python class, bound to a class instance.")
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-bound-method))

;; Make bound method given attributes, or given an unbound method
;; (that will be destructively changed).

(defun convert-unbound-to-bound-method (unbound-method self)
  (check-type unbound-method py-unbound-method)
  (change-class unbound-method 'py-bound-method)
  (setf (slot-value unbound-method 'self) self)
  unbound-method)

(defun make-bound-method (&key self class func)
  (when class
    (check-type class class))
  (when (and self class)
    (check-type self class))
  (make-instance 'py-bound-method
    :self self :class (or class (class-of self)) :func func))

(defun bound-method-p (m)
  (typep m 'py-bound-method))

(defmethod print-object ((x py-bound-method) stream)
  (print-unreadable-object (x stream :identity nil :type t)
    (with-slots (class func self) x
      (format stream "~_:class ~S~_:func ~S~_:self ~S"
	      class func self))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List
;; 
;; For now, implemented internally as a List consed list. Perhaps an
;; adjustable vector is more efficient.

(defclass py-list (builtin-instance)
  ((list :type list :initarg :list))
  (:documentation "The List type")
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-list))

(defun make-py-list (&rest lst)
  "Make a Python list from the given CL list"
  (make-py-list-from-list lst))


(defun make-py-list-from-list (lst)
  (check-type lst list "A regular Lisp list")
  ;; XXX for now
  (loop for x in lst
      unless (python-object-designator-p x)
      do (warn "Non-python object:  ~A  encountered in MAKE-PY-LIST" x)
	 (return))
  (make-instance 'py-list :list lst))
  
(defmethod print-object ((x py-list) stream)
  (format stream "[~_~{~S~^, ~@_~}~_]" (slot-value x 'list))
  #+(or)(print-unreadable-object (x stream)
	  (format stream "list [~_~{~S~^, ~_~}~_]" (slot-value x 'list))))


;;;; magic methods

(defmethod __add__ ((x py-list) (y py-list))
  "structure is shared"
  (make-py-list-from-list
   (append (slot-value x 'list) (slot-value y 'list))))

;; CPython lists: no __radd__

(defmethod __cmp__ ((x py-list) (y py-list))
  (__cmp-list__ (slot-value x 'list) (slot-value y 'list)))

(defmethod __cmp-list__ (x y)
  "For tuples and lists: compare the underlying lists. If all elements ~@
   eq, longest wins, otherwise they are eq."
  (do ((x2 x (cdr x2))
       (y2 y (cdr y2)))
      ((not (and x2 y2))
       (cond ((and (null x2) (null y2)) 0)
	     (x2 1) ;; x longer = larger
	     (y2 -1))) ;; y longer = larger
    (let ((res (__cmp__ (car x2) (car y2))))
      (cond ((= res 0)) ;; cont.
	    ((< res 0) (return-from __cmp-list__ -1))
	    ((> res 0) (return-from __cmp-list__ 1))))))

(defmethod __contains__ ((x py-list) item)
  (if (some (lambda (y) (__eq__ item y))
	    (slot-value x 'list))
      *True*
    *False*))

(defmethod __delitem__ ((x py-list) item)
  (typecase item
    (py-int-designator (list-delitem-integer x item))
    (py-slice (list-delitem-slice x item))
    (t (py-raise 'TypeError
		 "List indices must be integers (got: ~A)" item))))

(defun list-delitem-integer (x index)
  (ensure-py-type index integer "internal error: ~A (list-delitem-integer)")
  (let* ((list (slot-value x 'list))
	 (len (length list)))
    (when (< index 0)
      (incf index len))
    (when (or (< index 0)
	      (> index (1- len)))
      (py-raise 'IndexError
		"List index out of range (got: ~A, len: ~A)"
		index len))
    
    (if (= index 0)
	(setf (slot-value x 'list) (cdr list))
      (let ((n-1 (nthcdr (1- index) list))
	    (n+1 (nthcdr (1+ index) list)))
	(setf (cdr n-1) n+1))))
  x)


(defun list-delitem-slice (x slice)
  (declare (ignore x slice))
  ;; XXX needs work
  #+(or)(let* ((list (slot-value x 'list))
	       (len (length list))
	       (destructuring-bind (start stop step)
		   (tuple->lisp-list (indices slice len))
      
		 (when (< start 0)
		   (setf start 0))
		 (when (>= stop len)
		   (setf stop (1- len)))
		 (when (< stop start) ;; = is ok
		   (return-from list-delitem-slice x))
      
		 (if (= start stop)
		     ;; insert slice in what is now empty
		     (((let ((n (nthcdr (
					 ))))))))))))

(defmethod __getitem__ ((x py-list) item)
  (let ((list (slot-value x 'list)))
    (typecase item
      (py-int-designator (extract-list-item-by-index list item))
      (py-slice          (make-py-list-from-list (extract-list-slice list item)))
      (t                 (py-raise 'TypeError
				   "List indices must be integers (got: ~A)" item)))))
  
(defun list-getitem-integer (list index)
  (ensure-py-type index integer
		  "internal error: ~A (list-getitem-integer)")
  (let ((len (length list)))
    (when (< index 0)
      (incf index len))
    (when (or (< index 0)
	      (> index (1- len)))
      (py-raise 'IndexError
		"List index out of range (got: ~A, len: ~A)"
		index len))
    (car (nthcdr index list))))

(defmethod __hash__ ((x py-list))
  (py-raise 'TypeError "List objects are unhashable"))

(defmethod __iter__ ((x py-list))
  (let ((list-copy (copy-list (slot-value x 'list)))) ;; copy-tree ?!
    (make-iterator-from-function
     (lambda ()
       (pop list-copy)))))

(defmethod __len__ ((x py-list))
  (length (slot-value x 'list)))

(defmethod __mul__ ((x py-list) (n integer))
  "structure is copied n times"
  ;; n <= 0 => empty list
  (make-py-list-from-list (loop for i from 1 to n
			      append (slot-value x 'list))))

(defmethod __rmul__ ((x py-list) (n integer)) 
  (__mul__ x n))

(defmethod __nonzero__ ((x py-list))
  (make-bool (/= 0 (length (slot-value x 'list)))))

(defmethod __repr__ ((x py-list))
  (with-output-to-string (s)
    (format s "[~{~A~^, ~}]" (mapcar #'__repr__ (slot-value x 'list)))))


(defmethod __setitem__ ((x py-list) item new-item)
  (typecase item
    (py-int-designator (list-setitem-integer x item new-item))
    (py-slice (list-setitem-slice x item new-item))
    (t (py-raise 'TypeError
		 "List indices must be integers (got: ~A)" item))))
	     
(defun list-setitem-integer (x index new-item)  
  (ensure-py-type index integer "List indices must be integers (got: ~A)")
  (let* ((list (slot-value x 'list))
	 (len (length list)))
    (when (< index 0)
      (incf index len)) ;; XXX this must be moved to an :around method or something
    (when (or (< index 0)
	      (> index (1- len)))
      (py-raise 'IndexError
		"List assignment index out of range (got: ~A, len: ~A)"
		index len))
    (setf (car (nthcdr index list)) new-item))
  x)

(defun list-setitem-slice (x slice new-items)
  (declare (ignore x slice new-items))
  (error "todo: setitem list slice")
  
  ;; new-items: iterable!
  #+(or)(let* ((list (slot-value x 'list))
	       (len (length list)))
	  (destructuring-bind (start stop step)
	      (tuple->lisp-list (indices slice len))
	    (when (< start 0)
	      (setf start 0))
	    (when (>= stop len)
	      (setf stop (1- len)))
      
	    (cond 
	     ((< stop start)) ;; bogus range: ignore)
	     ((= start stop)) ;; del empty range: ignore
	     ((= start 0)
	      (setf (slot-value x 'list)
		(nthcdr stop list)))
	     (t (let ((start-cons (nthcdr (1- start) x))
		      (rest-cons (nthcdr stop x))
		      (new-last-cons (last new-items)))
		  (setf (cdr start-cons) new-items
			(cdr new-last-cons) rest-cons)))))))


(defmethod __str__ ((x py-list))
  (format nil "[~:_~{~A~^, ~:_~}]"
	  (mapcar #'__str__ (slot-value x 'list))))


;;; list-specific methods

;; to ease debugging, for now the in-place operations return the
;; (modified) list they work on

(defmethod __reversed__ ((x py-list))
  "Return a reverse iterator"
  ;; new in Py ?.?
  (let ((rev (reverse (slot-value x 'list))))
    (make-iterator-from-function
     (lambda ()
       (pop rev)))))
  
(defmethod list-append ((x py-list) item)
  (setf (cdr (last (slot-value x 'list))) (cons item nil))
  x)

(defmethod list-count ((x py-list) item)
  (loop for i in (slot-value x 'list)
      count (__eq__ i item)))

(defmethod list-extend ((x py-list) iterable)
  (let ((res ()))
    (py-iterate (i iterable)
		(push i res))
    (setf (cdr (last (slot-value x 'list))) (nreverse res)))
  x)

(defmethod list-index ((x py-list) item &optional start stop)
  (let ((res (position-if (lambda (v) (__eq__ v item))
			  (slot-value x 'list)
			  :start (or start 0) :end stop)))
    (cond
     (res res)
     (start (py-raise 'ValueError
		      "list.index(x): value ~A not found in this part of the list"
		      item))
     (t (py-raise 'ValueError
		  "list.index(x): value ~A not found in this part of the list"
		  item)))))

(defmethod list-insert ((x py-list) index object)
  (ensure-py-type index integer
		  "list.insert(): index must be an integer (got: ~A)")
  (let ((list (slot-value x 'list)))
    (if (= index 0)
	(setf (slot-value x 'list) (cons object list))
      (let ((just-before (nthcdr (1- index) list))
	    (after (nthcdr index list)))
	(if (not after)
	    (setf (cdr (last list)) (cons object nil))
	  (setf (cdr just-before) (cons object after))))))
  x)

(defmethod list-pop ((x py-list) &optional index)
  (let* ((list (slot-value x 'list))
	 (len (length list)))

    (if index
	(progn (ensure-py-type index integer
			       "list.pop(x,i): index must be integer (got: ~A)")
	       (when (< index 0)
		 (incf index len)))
      (setf index (1- len)))
    
    (cond ((null list)
	   (py-raise 'IndexError
		     "Pop from empty list"))
	  
	  ((not (<= 0 index (1- len)))
	   (py-raise 'IndexError
		     "Pop index out of range (got: ~A, len: ~A)" index len))
	  
	  ((= index 0)
	   (setf (slot-value x 'list) (cdr list))
	   (car list))
	  
	  (t
	   (let ((cons-before (nthcdr (1- index) list)))
	     (prog1
		 (cadr cons-before)
	       (setf (cdr cons-before) (cddr cons-before))))))))

(defmethod list-remove ((x py-list) item)
  "Remove first occurance of item"
  (setf (slot-value x 'list)
    (delete item (slot-value x 'list) :test #'__eq__ :count 1))
  x)

(defmethod list-reverse ((x py-list))
  "In-place"
  (setf (slot-value x 'list)
    (nreverse (slot-value x 'list)))
  x)

(defmethod list-sort ((x py-list) &optional (cmpfunc *None*))
  "Stable sort, in-place"
  (let ((lt-pred (if (eq cmpfunc *None*)
		     #'py-<
		   (lambda (x y) (< (__call__ cmpfunc x y) 0)))))
    (setf (slot-value x 'list)
      (stable-sort (slot-value x 'list) lt-pred)))
  x)


;; XXX __str__ falls back to __repr__
;; XXX "x < y" => operator.lt => uses __lt__ if defined, otherwise __cmp__
;; XXX __lt__ never falls back to __cmp__

(def-class-specific-methods
    py-list
    ((append (meth . list-append))
     (count  (meth . list-count))
     (extend (meth . list-extend))
     (index (meth . list-index))
     (insert (meth . list-insert))
     (pop    (meth . list-pop))
     (remove (meth . list-remove))
     (reverse (meth . list-reverse))
     (sort (meth . list-sort))))

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

(defmethod next ((f py-func-iterator))
  "This is the only function that an iterator has to provide."
  (when (slot-value f 'stopped-yet)
    #1=(py-raise 'StopIteration
		 "Iterator ~S has finished" f))
  (let ((res (funcall (slot-value f 'func))))
    (when (eql res (slot-value f 'end-value))
      (setf (slot-value f 'stopped-yet) t)
      #1#)
    res))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tuple

(defclass py-tuple (builtin-instance)
  ((list :initarg :list :type list)
   (length :initarg :length :type integer))
  (:documentation "The Tuple type")
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-tuple))

(defun make-tuple (&rest lst)
  "Make a Python tuple from the given CL list"
  (check-type lst list "A regular Lisp list")
  (make-instance 'py-tuple :list lst :length (length lst)))

(defun make-tuple-from-list (list)
  (make-instance 'py-tuple :list list))

#+(or) ;; defer to __repr__
(defmethod print-object ((x py-tuple) stream)
  (format stream "(~{~_~S~^, ~})" (slot-value x 'list)))

(defun tuple->lisp-list (tup)
  "internal use only"
  (slot-value tup 'list))

;;;; magic methods

;;; XXX Many methods are similar as for py-list. Maybe move some to a
;;; shared superclass py-sequence. However, the implementation of
;;; py-list is likely to change, in order to allow efficient
;;; lookup-by-index of O(1). This change might remove much of the
;;; redundancy.

(defmethod __add__ ((x py-tuple) (y py-tuple))
  (make-tuple-from-list
   (append (slot-value x 'list) (slot-value y 'list))))

;; CPython tuples: no __radd__

(defmethod __cmp__ ((x py-tuple) (y py-tuple))
  (__cmp-list__ (slot-value x 'list) (slot-value y 'list)))

(defmethod __contains__ ((x py-tuple) item)
  (if (some (lambda (y) (__eq__ item y))
	    (slot-value x 'list))
      *True*
    *False*))

(defmethod __getitem__ ((x py-tuple) item)
  (let ((list (slot-value x 'list)))
    (typecase item
      (py-int-designator (extract-list-item-by-index list item))
      (py-slice          (make-tuple-from-list (extract-list-slice list item)))
      (t                 (py-raise 'TypeError
				   "Tuple indices must be integers (got: ~A)" item)))))

(defmethod __hash__ ((x py-tuple))
  ;; Try to avoid  hash( (x,(x,y)) ) = hash( (y) )
  ;; so being a bit creative here...
  (let ((hash-values #(1274 9898982 1377773 -115151511))
	(res 23277775)
	(pos 0))
    (dolist (xi (slot-value x 'list))
      (setf res (logxor res 
			(+ (__hash__ xi)
			   (aref hash-values (mod pos 4)))))
      (incf pos))
    (mod-to-fixnum res)))
   
(defmethod __iter__ ((x py-tuple))
  (let ((list-copy (copy-list (slot-value x 'list)))) ;; copy-tree ?!
    (make-iterator-from-function
     (lambda ()
       (pop list-copy)))))

(defmethod __len__ ((x py-tuple))
  (length (slot-value x 'list)))

(defmethod __mul__ ((x py-tuple) (n integer))
  "structure is copied n times"
  ;; n <= 0 => empty list
  (make-tuple-from-list (loop for i from 1 to n
			    append (slot-value x 'list))))

(defmethod __rmul__ ((x py-tuple) (n integer)) 
  (__mul__ x n))

(defmethod __repr__ ((x py-tuple))
  (with-output-to-string (s)
    (format s "(~{~A,~^ ~})" (mapcar #'__repr__ (slot-value x 'list)))))

;; __reversed__ ?

(defmethod __setitem__ ((x py-tuple) key val)
  (declare (ignore key val))
  (py-raise 'TypeError
	    "Cannot set items of tuples"))

;;; there are no tuple-specific methods

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String
;; 
;; Symbols act as designators for Python strings. They are interned in
;; the :python package, so they can be compared using EQ.

(defclass py-string (builtin-instance)
  ((string :type string :initarg :string))
  (:documentation "The String type")
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-string))

(deftype py-string-designator ()
  `(or py-string symbol))

(defun py-string-designator-p (s)
  "Return STRING-DESIGNATOR-P, LISP-STRING"
  (cond ((typep s 'string) (values t s))
	((typep s 'py-string) (values t (slot-value s 'string)))
	((symbolp s) (values t (symbol-name s)))
	(t nil)))
  
(defun make-py-string (&optional (s ""))
  (check-type s (or symbol string character) "A Lisp symbol, string or character")
  (make-instance 'py-string :string (typecase s
				      (string s)
				      (character (string s))
				      (symbol (symbol-name s)))))



(defmethod print-object ((x py-string) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~S" (slot-value x 'string))))


;;; Strings have 3 internal representations: symbol, string, py-string.

(defmacro def-unary-string-meths (data)
  `(progn ,@(loop for (name args body) in data
		do (assert (eq (car args) 'x))
		collect (let ((rest (cdr args)))
			  `(progn (defmethod ,name ((x py-string) ,@rest)
				    (let ((x (slot-value x 'string)))
				      ,body))
				  (defmethod ,name ((x symbol) ,@rest)
				    (let ((x (symbol-name x)))
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
				  (defmethod ,name ((x symbol) y ,@rest)
				    (let ((x (symbol-name x)))
				      (,name x y ,@rest2)))
				  (defmethod ,name (x (y py-string) ,@rest)
				    (let ((y (slot-value y 'string)))
				      (,name x y ,@rest2)))
				  (defmethod ,name (x (y symbol) ,@rest)
				    (let ((y (symbol-name y)))
				      (,name x y ,@rest2)))
				  (defmethod ,name ((x string) (y string) ,@rest)
				    ,body))))))

(def-unary-string-meths 
    ((__getitem__ (x index) (progn (ensure-py-type index integer
						   "String indices must be integer (slices: todo)")
				   (when (< index 0)
				     (incf index (length x)))
				   (string (char x index))))
     (__hash__ (x) (sxhash x))
     
     (__iter__  (x) (let ((i 0))
		      (make-iterator-from-function
		       (lambda ()
			 (when (< i (length x))
			   (prog1 (string (aref x i))
			     (incf i)))))))
     
     (__len__  (x) (length x))
     (__mod__  (x args) (locally (declare (ignore x args))
			  (error "todo: string mod")))
     ;; rmod, rmul
     (__mul__  (x n) (__mul-1__ x n))
     
     ;; __reduce__ : todo
     
     ;; __repr__ : with quotes (todo: if string contains ', use " as quote etc)
     ;; __str__  : without surrounding quotes
     (__repr__ (x) (format nil "~S" x))
     (__str__  (x) (format nil "~A" x))
     
     (py-string-capitalize (x) (string-capitalize x)) ;; lisp function
     (string-center (x width) (string-center-1 x width))
     (string-decode (x &optional encoding errors) (string-decode-1 x encoding errors))
     (string-encode (x &optional encoding errors) (string-encode-1 x encoding errors))
     (string-expandtabs (x &optional tabsize) (string-expandtabs-1 x tabsize))
     (string-isalnum (x) (string-isalnum-1 x))
     (string-isalpha (x) (string-isalpha-1 x))
     (string-isdigit (x) (string-isdigit-1 x))
     (string-islower (x) (string-islower-1 x))
     (string-istitle (x) (string-istitle-1 x))
     (string-isupper (x) (string-isupper-1 x))
     (string-join    (x seq) (string-join-1 x seq))
     ))


(def-binary-string-meths
    ((__add__      (x y) (concatenate 'string x y))
     (__radd__     (x y) (__add__ y x))
     (__contains__ (x y) (search y x)) ;; SEARCH returns an int or NIL, i.e. generalized bools
     (__cmp__ (x y)      (cond ((string< x y) -1)
			       ((string= x y) 0)
			       (t 1)))
     (__eq__ (x y)       (string= x y))
     
     (string-count (x y) (string-count-1 x y))
     (string-endswith (x y &optional start end) (string-endswith-1 x y start end))
     (string-find (x y &optional start end) (string-find-1 x y start end))
     (string-index (x y &optional start end) (string-index-1 x y (or start 0) (or end 0)))
    ))


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


;; When both strings are symbols, direct equality tests are very efficient
(defmethod __cmp__ ((x symbol) (y symbol))
  (if (eq x y) 
      0
    (__cmp__ (symbol-name x) (symbol-name y))))

(defmethod __eq__ ((x symbol) (y symbol))
  (eq x y))


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

(defmethod string-decode-1 (x &optional encoding errors)
  (declare (ignore x encoding errors))
  (error "todo: string-decode"))

(defmethod string-encode-1 (x &optional encoding errors)
  (declare (ignore x encoding errors))
  (error "todo: string-decode"))

(defmethod string-endswith-1 ((x string) (suffix string) &optional (start 0) (end nil))
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
   
(defmethod string-isalnum-1 ((x string))
  (make-bool (every #'alphanumericp x)))

(defmethod string-isalpha-1 ((x string))
  (make-bool (every #'alpha-char-p x)))

(defmethod string-isdigit-1 ((x string))
  (make-bool (every #'digit-char-p x)))

(defmethod string-islower-1 ((x string))
  (make-bool (every #'lower-case-p x)))

(defmethod string-isspace-1 ((x string))
  (make-bool (every (lambda (c) (member c (load-time-value (list #\Space #\Tab #\Newline))))
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
	do (cond ((upper-case-p c)
		  (when previous-is-cased
		    (return-from string-istitle-1 *False*))
		  (setf previous-is-cased t)
		  (setf got-cased t))
		     
		 ((lower-case-p c)
		  (unless previous-is-cased
		    (return-from string-istitle-1 *False*)))
		     
		 (t
		  (setf previous-is-cased nil))))
    (make-bool got-cased)))

  
(defmethod string-isupper-1 ((x string))
  (make-bool (every #'upper-case-p x)))

(defmethod string-join-1 ((x string) sequences)
  "Join a number of strings"
  (let ((acc ()))
    (py-iterate (str sequences)
		(format t "str: ~S~%" str)
		(ensure-py-type str string
				"string.join() can only handle real strings (got: ~A)")
		(push str acc))
    (apply #'concatenate 'string x (nreverse acc))))

(def-class-specific-methods
    py-string
    ((capitalize (meth . string-capitalize)) ;; lisp function
     (center (meth . string-center-1))
     (decode (meth . string-decode-1))
     (encode (meth . string-encode-1))
     (expandtabs  (meth . string-expandtabs-1))
     (isalnum  (meth . string-isalnum-1))
     (isalpha  (meth . string-isalpha-1))
     (isdigit  (meth . string-isdigit-1))
     (islower  (meth . string-islower-1))
     (istitle  (meth . string-istitle-1))
     (isupper  (meth . string-isupper-1))
     (join     (meth . string-join-1))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Interned strings: the string is represented by an interned symbol,
;;; enabling quick EQ comparison.

(defclass py-interned-string (py-string)
  ((interned-string :type symbol :initarg :interned-string))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-interned-string))

(defmethod unintern-string ((s symbol))
  ;; used in mathops.cl for when method of user-defined-object may be called.
  ;; XXX check if still used
  (symbol-name s))

(defvar *interned-strings* 
    (make-hash-table :test 'eq))

(defun make-interned-string (&optional (string ""))
  "Intern STRING, return corresponding SYMBOL."
  (multiple-value-bind (symbol status)
      (intern string #.*package*) ;; package arg needed?
    (if (eq status nil)
	(let ((hash (sxhash string)))
	  ;; XXX verify SXHASH appropriate (eg, need equality of unicode and regular strings)
	  (setf (gethash symbol *interned-strings*) (cons string hash)))) ;; CONS?!
    symbol))

(defvar *interned-empty-string*
    (make-interned-string ""))

(defun show-interned-strings ()
  (maphash (lambda (k v)
	     (declare (ignore k))
	     (format t "~S (~S)~%" (car v) (cdr v)))
	   *interned-strings*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File

;;; XXX not very important for now


;; __iter__: read all lines of file sequentially

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enumerate

(defclass py-enumerate (builtin-instance)
  ((generator :initarg :generator)
   (index :initarg :index :type integer))
  (:metaclass builtin-class))

(mop:finalize-inheritance (find-class 'py-enumerate))

(defun make-enumerate (iterable)
  (make-instance 'py-enumerate
    :index 0
    :generator (__iter__ iterable)))

(defmethod __iter__ ((x py-enumerate))
  x)

(defmethod next ((x py-enumerate))
  ;; Will raise StopIteration as soon as (next generator) does that.
  (with-slots (index generator) x
    (prog1
	(make-tuple index (next generator))
      (incf index))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XRange object

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
	     (cond
	      ((or (and (< start stop)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slice object
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
    (make-tuple start stop step)))

(defmethod slice-indices ((x py-slice) length)
  "Return 1 or 4 values (nonempty, start, stop, step) indicating requested slice.
   nonempty: T or nil
   if nonempty is T: START is the index of the first item of the resulting slice
                     STOP the index of the last item
                     STEP the amount by which to increase each time (can be negative; is not zero)
                     if step > 0:
                       0 <= start <= stop <= length-1
                     if step < 0:
                       0 <= stop <= start <= length-1"

  ;; CPython doesn't define the outcome of this method exactly. Like,
  ;; where is this documented:
  ;; 
  ;;  >>> s = slice(10,15,-1)
  ;;  >>> s.indices(5)
  ;;  (4, 5, -1)
  ;; 
  ;; (For such cases, maybe Python should have a way to specify 'empty
  ;; slice', for clarity.)
  ;; 
  ;; Here's what we do, for slice [x:y:s] and len L. (For CPython
  ;; compatibility, indices are required to be integers.)
  ;; 
  ;;  s == 0        [1:10: 0]  => error
  ;; 
  ;; Then, if X or Y is < 0, one time the length L is added to it.
  ;; Then we proceed as follows:
  ;; 
  ;;  x == y        [4: 4: 1]  => empty slice (L,L,1)
  ;;  x < y, s > 0  [2:10: 1]  => ok: (2,10,1)  (x < 0 => x = 0;  y > L => x = L)
  ;;  x < y, s < 0  [1:10:-1]  => empty slice (L,L,1)
  ;;  x > y, s < 0  [10:1:-2]  => ok: (10,1,-2)  (x > L => x = L;  y < 0 => y = 0)
  ;;  x > y, s < 0  [10:1:-1]  => empty slice (L,L,1)
  
  (ensure-py-type length integer
		  "Argument to 'indices' method must be integer (got: ~A)")
  (let ((start (slot-value x 'start))
	(stop  (slot-value x 'stop))
	(step  (slot-value x 'step)))
    
    (if (eq start *None*)
	(setf start 0)
      (progn (ensure-py-type start integer "Slice indices must be integers (got: ~A)")
	     (when (< start 0)
	       (incf start length))))
    
    (if (eq stop *None*)
	(setf stop length)
      (progn (ensure-py-type stop integer "Slice indices must be integers (got: ~A)")
	     (when (< stop 0)
	       (incf stop length))))
    
    (if (eq step *None*)
	(setf step 1)
      (ensure-py-type step integer "Slice indices must be integers (got: ~A)"))
    
    (flet ((empty-slice ()
	     (values nil)))
      
      (cond ((= step 0) 	  (py-raise 'ValueError "Slice step cannot be zero"))
       	    
	    ((= start stop)	  (empty-slice))
            
	    ((and (>= start length)
		  (> step 0))     (empty-slice))
	    
	    ((and (< start 0)
		  (< step 0))     (empty-slice))
	    
	    ((and (< start stop)
		  (> step 0))	  (let ((start (max start 0))
					(stop  (min stop length))
					(real-stop (+ start (* step (floor (- stop 1 start)
									   step)))))
				    (assert (<= real-stop stop))
				    (values t start real-stop step)))
	    
	    ((and (< start stop)
		  (< step 0))	  (empty-slice))

	    ((and (> start stop)
		  (< step 0))	  (let ((start (min start length))
					(stop  (max stop 0))
					(real-stop (+ start (* step (floor (- stop -1 start)
									   step)))))
				    (assert (>= real-stop stop))
				    (values t start real-stop step)))
	    
	    ((and (> start stop)
		  (> step 0))	  (empty-slice))))))


(defmethod extract-list-slice ((list cons) (slice py-slice))
  "Given a (Lisp) list, extract the sublist corresponding to the slice as a fresh list."
  (multiple-value-bind (nonempty start stop step)
      (slice-indices slice (length list))
    
    (unless nonempty
      (return-from extract-list-slice (make-py-list)))
    
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
			    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Python object stuff


(deftype attribute-name-designator ()
  `(or symbol string))

(defun attribute-name-designator-p (x)
  "Return ATT-DES-P, SYMBOL"
  (typecase x
    (symbol (values t x))
    (string (values t (make-interned-string x)))
    (t nil)))


;;;; python object?

(deftype python-object-designator ()
  `(or python-object number symbol))

(defgeneric python-object-designator-p (x)
  (:documentation "Returns DESIGNATOR-P, PYVAL where PYVAL is a ~
                   Python object iff DESIGNATOR-P "))

;; shield this class from Python
(defmethod python-object-designator-p ((x (eql (find-class 'builtin-instance)))) (values nil nil))

(defmethod python-object-designator-p ((x python-object)) (values t x))
(defmethod python-object-designator-p ((x number)) (values t (make-py-number x)))
(defmethod python-object-designator-p ((x symbol)) (values t (make-py-string x)))
(defmethod python-object-designator-p ((x string)) (values t (make-py-string x)))
(defmethod python-object-designator-p (x) (declare (ignore x)) nil)



;;;; builtin object?

(defgeneric builtin-object-designator-p (x)
  (:documentation "Returns DESIGNATOR-P"))

;; basically, everything except user-defined stuff
(defmethod builtin-object-designator-p (x)
  (declare (ignore x))
  t)

(defmethod builtin-object-designator-p ((x user-defined-object))
  nil)
