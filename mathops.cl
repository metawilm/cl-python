(in-package :python)

;; Mathematical operations: + - * / & <<  etc]
;; 
;; The idea is, that in source code, "a OP b" will be replaced by
;; (py-OP a b):
;; 
;;  x + y   -->  (py-+  x y)
;;  x << y  -->  (py-<< x y)  etc

(declaim (special *NotImplemented*))


;; mapping  '+ -> #'py-+
;; 
;; Used by the interpreter, in EVAL-BINARY (pyeval.cl).
;; 
;; When compiling code, the corresponding GF for the operator can be
;; looked up here, to include in the compiled code. At run-time, it
;; should not consult this mapping (unless dealing with "eval"/"exec")

(defvar *math-op-mapping* (make-hash-table :test 'eq))


(defmacro def-python-arith (name py-punct py-op py-rop &optional inline-cl-op)
  ;;                  e.g. (py-+  +  __lshift__ __rlshift__      +)
  
  ;; For each operator:
  ;;  defines generic function: #'py-<<
  ;;  links it to punctuation:  '<<  ->  #'py-<<
  ;;  implementation calls magic methods: __lshift__ and __rlshift__
  ;; 
  ;; Magic methods __lshift__, __rlshift__ are NOT defined in this
  ;; macro (that occurs in magicmeth.cl
  ;; 
  ;; XXX todo: allow for inlining other than `number' (like `real')
  
  (symbol-macrolet ((type-error
			`(py-raise 'TypeError
			  "Unsupported operand types for '~A': '~A' and '~A'"
			  ',py-punct (class-of x) (class-of y))))
    
    (let ((name-2 (intern (concatenate 'string (symbol-name name) "-slow"))))
      `(progn
	 (defgeneric ,name (x y))
       
	 (setf (gethash ',py-punct *math-op-mapping*) #',name)
       
	 (defmethod no-applicable-method ((f (eql #',name)) &rest args)
	   (let ((x (first args))
		 (y (second args)))
	     ,type-error))
	 
	 
	 ;; go to general implementation when at least one of the args
	 ;; is udc-i.
	 ;; 
	 ;; Need to check for this, because below some operations are
	 ;; implemented inline when both arguments are of type
	 ;; [py-]number, and a udc-instance that is subclassed
	 ;; from [py-]number would also match that type specification.
       
	 (defmethod ,name ((x udc-instance) (y udc-instance))
	   (,name-2 x y))
       
	 (defmethod ,name ((x builtin-instance) (y udc-instance))
	   (,name-2 x y))

	 (defmethod ,name ((x udc-instance) (y builtin-instance))
	   (,name-2 x y))

	 
	 ;; udc-i + number: convert number to Python object, because udc-i
	 ;;                 can't deal with real integers
	 
	 (defmethod ,name ((x udc-instance) (y number))
	   (,name-2 x (make-py-number y)))
	 
	 (defmethod ,name ((x udc-instance) (y py-number))
	   (,name-2 x y))
	 
       	 (defmethod ,name ((x number) (y udc-instance))
	   (,name-2 (make-py-number x) y))
	 
	 (defmethod ,name ((x py-number) (y udc-instance))
	   (,name-2 x y))

	 
	 ;; udc-i + interned string: convert interned string to regular
	 ;; string because udc-i can't deal with interned strings
       
	 (defmethod ,name ((x udc-instance) (y symbol))
	   (,name-2 x (unintern-string y)))
       
	 (defmethod ,name ((x symbol) (y udc-instance))
	   (,name-2 (unintern-string x) y))

	 ;; Inline operation when it's only built-in numbers, for speed.
	 ;; 
	 ;; The method below these, specialized on (builtin-instance
	 ;; builtin-instance), does not capture all these cases, as
	 ;; `number' is not a built-in class.
	 ;; 
	 ;; (The presence of these methods here is based on the fact
	 ;; that subclasses of py-number have udc-i in the CPL before
	 ;; py-number.)
	 ;; 
	 ;; Sometimes operations on only numbers are inlined
	 ;; completely (e.g. using CL's `+') while others (that
	 ;; require more handling, like argument checking, or not
	 ;; being defined for complexes) are dispatched to the magic
	 ;; method implementation (e.g. the GF `__lshift___').
	 
	 ,@(let ((cl-meth-for-numbers (or inline-cl-op py-op)))
	     `((defmethod ,name ((x py-number) (y py-number))
		 (,cl-meth-for-numbers (slot-value x 'val) (slot-value y 'val)))
	       (defmethod ,name ((x py-number) (y number))
		 (,cl-meth-for-numbers (slot-value x 'val) y))
	       (defmethod ,name ((x number) (y py-number))
		 (,cl-meth-for-numbers x (slot-value y 'val)))
	       (defmethod ,name ((x number) (y number))
		 (,cl-meth-for-numbers x y))))
	 
	 
	 ;; When both arguments are built-ins (and not udc-i, so not
	 ;; subclassed built-ins), use the GF.
	 
	 (defmethod ,name ((x builtin-instance) (y builtin-instance))
	   (assert (and (not (typep x 'udc-instance))
			(not (typep y 'udc-instance)))
	       () "Shouldn't take this method when one of X,Y is UDC-I: ~A ~A" x y)
	   (,py-op x y))

	 
	 ;; XXX need something to catch `string' args etc
	 (defmethod ,name (x y)
	   (if (and (builtin-object-designator-p x)
		    (builtin-object-designator-p y))
	       (,py-op x y)
	     (,name-2 x y)))
	   
	 ;; General, slow implementation, when either of the arguments
	 ;; is a UDC instance.
	 
	 (defmethod ,name-2 (x y)
	   (let* ((x-class (__class__ x)) ;; not class-of, as x/y might be designator for BIC-I
		  (y-class (__class__ y))
		  (y-sub-of-x (subtypep y-class x-class)))
	     (if y-sub-of-x
		 ;; try rop, op
		 (progn (handler-case (,py-rop y x)
			  ((or %magic-method-missing% %not-implemented-result%) ()) ;; cont.
			  (:no-error (res)
			    (return-from ,name-2 res)))
			(handler-case (,py-op x y)
			  ((or %magic-method-missing% %not-implemented-result%) ()) ;; cont.
			  (:no-error (res)
			    (return-from ,name-2 res)))
			,type-error)
	       ;; try op, rop
	       (progn (handler-case (,py-op y x)
			((or %magic-method-missing% %not-implemented-result%) ()) ;; cont.
			(:no-error (res)
			  (return-from ,name-2 res)))
		      (handler-case (,py-rop x y)
			((or %magic-method-missing% %not-implemented-result%) ()) ;; cont.
			(:no-error (res)
			  (return-from ,name-2 res)))
		      ,type-error))))))))
	     		   

;; If first line in module is "from __future__ import division" then
;; generate `py_/t/' function calls for `x/y', otherwise `py_/'.

(defmacro do-def-python-arith (data)
  `(progn ,@(loop for row in data
		collect `(def-python-arith ,@row))))

(do-def-python-arith
    ;; GF  Py-syntax meth-l      meth-r    [inline-CL-op] <- only if it is
    ;;                                                     allowed on all NUMBERs,
    ;;                                                     including COMPLEX
    ((py-+   +      __add__      __radd__      +)
     (py--   -      __sub__      __rsub__      -)
     (py-*   *      __mul__      __rmul__      *)
     (py-/t/ /      __truediv__  __rtruediv__  /)
     (py-//  floor  __floordiv__ __rfloordiv__)
     (py-/   floor  __div__      __rdiv__)
     (py-%   mod    __mod__      __rmod__)
     (py-<<  <<     __lshift__   __rlshift__)
     (py->>  >>     __rshift__   __rrshift__)
     (py-&   &      __and__      __rand__)
     (py-\|  \|     __or__       __ror__)
     (py-^   ^      __xor__      __rxor__)
     (py-**  **     __pow__      __rpow__)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comparison operations
;; 
;; It appears that all comparisons -- < > <= >= == != <> -- are
;; defined in terms of the outcome of built-in function `cmp'
;; (#'py-cmp).
;; 
;; #'py-< implements the logic for "a < b"
;; 
;; Note this important point: "a < b" is NOT directly translated into
;; a.__lt__(b), although this __lt__ method *might* be called by
;; __cmp__ (as might b.__ge__, in this case).
;; 
;;  '<  -->  #'py-<
;; 
;; This mapping is used by the interpreter, in EVAL-COMPARISON
;; (pyeval.cl).
;; 
;; Compiled code should insert the GF that is the result of the lookup
;; directly in the compiled code.

(defvar *math-cmp-assoc* ())

(defmacro do-def-python-comparisons (data)
  `(progn ,@(loop for (syntax func-name func-body) in data
		collect `(progn (defun ,func-name (x y)
				  ,func-body)
				(push (cons ',syntax #',func-name)
				      *math-cmp-assoc*)))))


(do-def-python-comparisons
    
    ;; pyb:cmp returns -1, 0 or 1 (or TypeError if user-supplied
    ;; method returns bogus comparison result; that TypeError is not
    ;; catched here but goes to user code.)
    
    ((<   py-<   (=  (pyb:cmp x y) -1))
     (>   py->   (=  (pyb:cmp x y)  1))
     (>=  py->=  (>= (pyb:cmp x y)  0))
     (<=  py-<=  (<= (pyb:cmp x y)  0))
     (==  py-==  (=  (pyb:cmp x y)  0))
     (!=  py-!=  (/= (pyb:cmp x y)  0))
     (<>  py=<>  (/= (pyb:cmp x y)  0))))


;; (assoc '- *math-unary-assoc*)  -->  ('- #'__neg__)
;; 
;; Used by the interpreter, in  EVAL-UNARY (pyeval.cl).
;; 
;; When compiling code, insert GF that is result of lookup directly in
;; the compiled code.

(defvar *math-unary-assoc* 
    ;; abs() , __abs__ is handled in function py-abs in builtin-funcs.cl
    `(( ~ . ,#'__invert__ )
      ( + . ,#'__pos__    )
      ( - . ,#'__neg__    )))
