(in-package :python)

;;; Mathematical operations

(declaim (special *NotImplemented*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Binary operators
;; 
;; + - * / & << ...
;; 
;; The idea is, that in source code, "a OP b" will be replaced by
;; (py-OP a b):
;; 
;;  x + y   -->  (py-+  x y)
;;  x << y  -->  (py-<< x y)
;;  ...

(defmacro make-binary-op-method (fname op rop &optional cl-op err-py-op)
  `(progn (defgeneric ,fname (x y))
	  
	  ;; This :around method takes care of the case of (+ 1
	  ;; <something>) where there are methods of #'__add__ that
	  ;; are specialied on 1 (say, number) but among those, there
	  ;; are none that accept <something> as second argument. In
	  ;; that case, lookup of the '__add__ attribute of 1
	  ;; succeeds, but calling (__add__ 1 <something>) would give
	  ;; the error: no applicable method. Using this :around
	  ;; method, we intercept that situation and return
	  ;; *NotImplemented* instead.
	  
	  (defmethod ,op :around (x y)
	    (declare (ignore x y))
	    (if (next-method-p)
		(call-next-method)
	      *NotImplemented*))
	  
	  (defmethod ,rop :around (x y)
	    (declare (ignore x y))
	    (if (next-method-p)
		(call-next-method)
	      *NotImplemented*))
	  
	  ,@(when cl-op
	      `((defmethod ,fname ((x number) (y number))
		  (,cl-op x y))))
	  
	  (defmethod ,fname (x y)
	    (let* ((x-class (__class__ x)) ;; not class-of, as x/y might be designator for BIC-I
		   (y-class (__class__ y))
		   (y-sub-of-x (and (not (eq x-class y-class))
				    (subtypep y-class x-class))))
    
	      (flet ((try-op ()
		       (multiple-value-bind (meth found)
			   (internal-get-attribute x-class ',op)
			 (if found
			     (py-call meth (list x y))
			   *NotImplemented*)))
		     (try-rop ()
		       (multiple-value-bind (meth found)
			   (internal-get-attribute y-class ',rop)
			 (if (and found (not (eq meth *NotImplemented*)))
			     (py-call meth (list y x))
			   *NotImplemented*))))
      
		(let ((result (if y-sub-of-x
				  
				  (let ((res (try-rop)))
				    (if (eq res *NotImplemented*)
					(try-op)
				      res))
				
				(let ((res (try-op)))
				  (if (eq res *NotImplemented*)
				      (try-rop)
				    res)))))
		  (if (eq result *NotImplemented*)
		      (py-raise 'TypeError "Operands don't support ~A (got: ~A and ~A)" ',err-py-op x y)
		    result)))))))

;; This mapping is used by the internal-get-attribute, in EVAL-BINARY
;; (pyeval.cl).

(defvar *math-binary-op-assoc* ())

(defmacro generate-binary-methods ()
  `(progn ,@(loop for (fname py-syntax op       rop           cl-op) in
		  '((py-+   +      __add__      __radd__      +)
		    (py--   -      __sub__      __rsub__      -)
		    (py-*   *      __mul__      __rmul__      *)
		    (py-/t/ /      __truediv__  __rtruediv__  /)
		    (py-//  //     __floordiv__ __rfloordiv__)
		    (py-/   /      __div__      __rdiv__)
		    (py-%   %      __mod__      __rmod__)
		    (py-<<  <<     __lshift__   __rlshift__)
		    (py->>  >>     __rshift__   __rrshift__)
		    (py-&   &      __and__      __rand__)
		    (py-\|  \|     __or__       __ror__)
		    (py-^   ^      __xor__      __rxor__)
		    )
		append `((make-binary-op-method ,fname ,op ,rop ,cl-op ,py-syntax) 
			 (push (cons ',py-syntax #',fname)
			       *math-binary-op-assoc*)))
	  
	  ;; Because operators are roughly in order of frequency, reverse to keep original order
	  (setf *math-binary-op-assoc* (nreverse *math-binary-op-assoc*))))

(generate-binary-methods)


;; a**b (to-the-power) is a special case:
;;  - method __pow__ takes an optional third argument:
;;    that argument can be supplied using the built-in function POW but not
;;     syntactically (using **)
;;  - there is no corresponding __rpow__ magic method
;;  - so, function py-** always gets 2 arguments when called for the a**b syntax,
;;    but may have a third argument when called for the built-in function POW.

(defgeneric py-** (x y &optional z))

(defmethod __pow__ :around (x y &optional z)
  (declare (ignore x y z))
  (if (next-method-p)
       (call-next-method)
    *NotImplemented*))
  
(defmethod py-** (x y &optional z) ;; doesn't support third arg (but built-in function POW does) (?)
  (multiple-value-bind (meth found)
      (internal-get-attribute (__class__ x) '__pow__)
    (let ((result (if found
		      (py-call meth (list x y z))
		    *NotImplemented*)))
      (if (eq result *NotImplemented*)
	  (py-raise 'TypeError "Unsupported operands for ** (got: ~A, ~A, ~A)" x y z)
	result))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defvar *math-binary-cmp-assoc* ())

(defmacro do-def-python-comparisons (data)
  `(progn ,@(loop for (syntax func-name func-body) in data
		append `((defun ,func-name (x y)
			   ,func-body)
			 (push (cons ',syntax #',func-name)
			       *math-binary-cmp-assoc*)))
	  (setf *math-binary-cmp-assoc* (nreverse *math-binary-cmp-assoc*))))

(do-def-python-comparisons
   
    ;; pyb:cmp returns -1, 0 or 1 (or TypeError if user-supplied
    ;; method returns bogus comparison result; that TypeError is not
    ;; catched here but goes to user code.)
    
    ((<   py-<   (=  (pyb:cmp x y) -1))
     (>   py->   (=  (pyb:cmp x y)  1))
     (==  py-==  (=  (pyb:cmp x y)  0))
     (!=  py-!=  (/= (pyb:cmp x y)  0)) ;; parser replaced <> by != already
     (<=  py-<=  (<= (pyb:cmp x y)  0))
     (>=  py->=  (>= (pyb:cmp x y)  0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unary operators
;; 
;; abs() , __abs__ is handled in function py-abs in builtin-funcs.cl
;; (there is no syntax for it)

(defvar *math-unary-op-assoc* ())

(defmacro make-unary-op-method (fname op py-syntax)
  `(progn (defgeneric ,fname (x))
	  
	  (defmethod ,fname (x)
	    (let ((class (__class__ x)))
	      (multiple-value-bind (meth found)
		  (internal-get-attribute class ',op)
		(let ((result (if found
				  (py-call meth (list x))
				*NotImplemented*)))
		  (if (eq result *NotImplemented*)
		      (py-raise 'TypeError "Unsupported operand for ~A (got: ~A)"
				',py-syntax x)
		    result)))))))
	    
(defmacro make-unary-op-methods ()
  `(progn ,@(loop for (fname op py-syntax) in
		  '((py-unary-~ __invert__ ~)
		    (py-unary-+ __pos__    +)
		    (py-unary-- __neg__    -))
		append `((make-unary-op-method ,fname ,op ,py-syntax)
			 (push (cons ',py-syntax #',fname)
			       *math-unary-op-assoc*)))
	  (setf *math-unary-op-assoc* (nreverse *math-unary-op-assoc*))))

(make-unary-op-methods)
