(in-package :python)

;;; Python's `magic methods' not shared by all objects.
;;; 
;;; (Those shared by all objects are defined in builtin-classes.cl).


;; As not all Python objects have the following methods, so only
;; define the generic function here, not a method (specialized on
;; types T).

(defgeneric __eq__ (x y) (:documentation "x == y"))
(defgeneric __ne__ (x y) (:documentation "x != y"))
(defgeneric __lt__ (x y) (:documentation "x < y"))
(defgeneric __gt__ (x y) (:documentation "x > y"))
(defgeneric __le__ (x y) (:documentation "x <= y"))
(defgeneric __ge__ (x y) (:documentation "x >= y"))

(defgeneric __cmp__ (x y) (:documentation "cmp(x,y) -- rich comparison"))
(defgeneric __nonzero__ (x) (:documentation "truth value testing"))
 
;; TODO: __unicode__
 
;;; Descriptor methods
(defgeneric __get__ (self instance owner-class) (:documentation "Get property attribute value of INSTANCE of OWNER-CLASS"))
(defgeneric __set__ (self instance value) (:documentation "Set property attribute to VALUE on INSTANCE"))
(defgeneric __del__ (self instance) (:documentation "Delete property attribute of INSTANCE"))

(defgeneric __len__ (x) (:documentation "len(x) -> integer >= 0"))
(defgeneric __getitem__ (x key) (:documentation "x[key]"))
(defgeneric __setitem__ (x key val) (:documentation "x[key] = val"))
(defgeneric __delitem__ (x key) (:documentation "del x[key]"))
(defgeneric __iter__ (x) (:documentation "iter(x) -> iterator object"))
(defgeneric __contains__ (x item) (:documentation "if item in x: ..."))


;; __getslice__, __setslice__, __delslice__ are deprecated, and not implemented here

;;; 3.3.7 Emulating numberic types

(defgeneric __add__ (x y) (:documentation "x + y"))
(defgeneric __sub__ (x y) (:documentation "x - y"))
(defgeneric __mul__ (x y) (:documentation "x * y"))
(defgeneric __div__ (x y) (:documentation "x / y"))
(defgeneric __floordiv__ (x y) (:documentation "x // y"))
(defgeneric __mod__ (x y) (:documentation "x % y"))
(defgeneric __divmod__ (x y) (:documentation "divmod(x,y)"))  ;; should be ( _floordiv_(), _mod_() )
  #+(or)(__pow__ (x y &optional z) "pow(x,y[,z])")
(defgeneric __lshift__ (x y) (:documentation "x << y"))
(defgeneric __rshift__ (x y) (:documentation "x >> y"))
(defgeneric __and__ (x y) (:documentation "x & y"))
(defgeneric __xor__ (x y) (:documentation "x ^ y"))
(defgeneric __or__ (x y) (:documentation "x | y"))
 
(defgeneric __truediv__ (x y) (:documentation "x / y  when __future__.division in effect"))
;; has priority over __div__ iif __future__.division in effect
;; 
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
(defgeneric __radd__ (x y) (:documentation "reversed x + y"))
(defgeneric __rsub__ (x y) (:documentation "reversed x - y"))
(defgeneric __rmul__ (x y) (:documentation "reversed x * y"))
(defgeneric __rdiv__ (x y) (:documentation "reversed x / y"))
(defgeneric __rfloordiv__ (x y) (:documentation "reversed x // y"))
;; XXX __rtruediv__
(defgeneric __rmod__ (x y) (:documentation "reversed x % y"))
(defgeneric __rdivmod__ (x y) (:documentation "reversed divmod(x,y)"))
#+(or)(__rpow__ (x y &optional z) "reversed x ** y")
(defgeneric __rlshift__ (x y) (:documentation "reversed x << y"))
(defgeneric __rrshift__ (x y) (:documentation "reversed x >> y"))

(defgeneric __rand__ (x y) (:documentation "reversed x & y"))
(defgeneric __rxor__ (x y) (:documentation "reversed x ^ y"))
(defgeneric __ror__ (x y) (:documentation "reversed x | y"))

;; in-place ops
(defgeneric __iadd__ (x y) (:documentation "x += y"))
(defgeneric __isub__ (x y) (:documentation "x -= y"))
(defgeneric __imul__ (x y) (:documentation "x *= y"))
(defgeneric __idiv__ (x y) (:documentation "x /= y"))
(defgeneric __ifloordiv__ (x y) (:documentation "x //= y"))
;; __itruediv__ XXX
(defgeneric __imod__ (x y) (:documentation "x %= y"))
;; there's no __idivmod__ (x y)
(defgeneric __ipow__ (x y &optional z) (:documentation "x **= y  or  x.__ipow__(y,z)"))  ;; x **= y -- no way to specify z?
(defgeneric __ilshift__ (x y) (:documentation "x <<= y"))
(defgeneric __irshift__ (x y) (:documentation "x >>= y"))
(defgeneric __iand__ (x y) (:documentation "x &= y"))
(defgeneric __ixor__ (x y) (:documentation "x ^= y"))
(defgeneric __ior__ (x y) (:documentation "x |= y"))

(defgeneric __neg__(x) (:documentation "-x"))
(defgeneric __pos__(x) (:documentation "+x"))
(defgeneric __abs__(x) (:documentation "abs(x)"))
(defgeneric __invert__(x) (:documentation "~x"))
 
(defgeneric __complex__(x) (:documentation "complex(x)"))
(defgeneric __int__ (x) (:documentation "int(x)"))
(defgeneric __long__ (x) (:documentation "long(x)"))
(defgeneric __float__ (x) (:documentation "float(x)"))
 
(defgeneric __oct__ (x) (:documentation "oct(x)"))
(defgeneric __hex__ (x) (:documentation "hex(x)"))
 
(defgeneric __coerce__ (x y) (:documentation "coercion"))
;; should return (x', y') where x', y' are standard numbers
;; or None or NotImplemented, indicating coercion failure


#+(or)
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

#+(or)
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
