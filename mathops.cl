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
	    (if (next-method-p) (call-next-method) *NotImplemented*))
	  
	  (defmethod ,rop :around (x y)
	    (declare (ignore x y))
	    (if (next-method-p) (call-next-method) *NotImplemented*))
	  
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
			 (if (and found (not (eq meth *NotImplemented*)))
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
		      (py-raise 'TypeError
				"Operands don't support ~A (got: ~A and ~A, resulted in NotImplemented)"
				',err-py-op x y)
		    result)))))))

;; This mapping is used by the internal-get-attribute, in EVAL-BINARY
;; (pyeval.cl).

(defvar *math-binary-op-assoc* ())

(defmacro generate-binary-methods ()
  `(progn ,@(loop for (fname py-syntax op       rop           cl-op) in
		  '((py-+   +      __add__      __radd__      +)
		    (py--   -      __sub__      __rsub__      -)
		    (py-*   *      __mul__      __rmul__      *)
		    (py-/t/ /t/    __truediv__  __rtruediv__  /) ;; /t/ a little hack; pyeval.cl
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
	  
	  ;; Because operators are roughly in order of frequency, reverse to keep
	  ;; original order.
	  
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

(push (cons '** #'py-**) *math-binary-op-assoc*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In-place operations
;; 
;;  x += y   -->        x.__iadd__(y)
;;            fallback: x = x.__add__(y) - where place x evaluated only once
;; 
;; The fall-back is not part of the function, because it operates on
;; _places_, while the function only tries the in-place operation on a
;; _value_.
;; 
;; As these are statements, the value returned by the in-place
;; operation is irrelevant, but whether or not the in-place method is
;; found _is_ relevant.

(defvar *math-inplace-op-assoc* ())

(defmacro create-inplace-func (syntax py-@= meth-name py-@)
  `(progn (defmethod ,py-@= (x val)
	    (multiple-value-bind (res found)
		(call-attribute-via-class x ',meth-name (list val))
	      (declare (ignore res))
	      found))
	  (push (cons ',syntax (cons #',py-@= #',py-@)) *math-inplace-op-assoc*)))

(defmacro generate-inplace-funcs ()
  `(progn ,@(loop for (syntax py-@= meth-name py-@) in
		  '((+=  py-+= __iadd__ py-+)
		    (-=  py--= __isub__ py--)
		    (*=  py-*= __imul__ py-*)
		    (/=  py-/= __idiv__ py-/)
		    (//= py-//= __ifloordiv__ py-//)
		    (%=  py-%=  __imod__ py-%)
		    (**= py-**= __ipow__ py-**)
		    (<<= py-<<= __ilshift__ py-<<)
		    (>>= py->>= __irshift__ py->>)
		    (&=  py-&=  __iand__ py-&)
		    (^=  py-^=  __ixor__ py-^)
		    (\|= py-\|= __ior__ py-\|))
		collect `(create-inplace-func ,syntax ,py-@= ,meth-name ,py-@))))

(generate-inplace-funcs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; in, not in, is, is not

(defmethod py-in (x seq)
  ;; either using __contains__, or using iterator
  
  (multiple-value-bind (res meth-found)
      (call-attribute-via-class seq '__contains__ (list x))
    (when meth-found
      (return-from py-in res)))
  
  (loop with f = (get-py-iterate-fun seq)
      with seq-item = (funcall f)
      while seq-item do
	(when (py-== x seq-item)
	  (return-from py-in *True*))
	(setf seq-item (funcall f))
      finally (return *False*)))

(defmethod py-not-in (x seq)
  (if (eq (py-in x seq) *True*)
      *False*
    *True*))

(defmethod py-is (x y)
  (if (eq x y) *True* *False*))

(defmethod py-is-not (x y)
  (if (eq x y) *False* *True*))
  
(loop for (name func) in `((in ,#'py-in)
			   (not-in ,#'py-not-in)
			   (is ,#'py-is)
			   (is-not ,#'py-is-not))
    do (push (cons name func) *math-binary-op-assoc*))




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


(defmethod py-unary-not (x)
  (if (py-val->lisp-bool x) *False* *True*))

(push (cons 'not #'py-unary-not) *math-unary-op-assoc*)
