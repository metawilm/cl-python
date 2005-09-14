(in-package :python)

;;; Optimizations:
;;; 
;;; Decrease execution speed by means of compiler macros and
;;; specialized methods.


;;; Iteration

(defmethod py-iterate->lisp-list ((v vector))
  (loop for x across v collect x))

(defmethod py-iterate->lisp-list ((s string))
  (loop for c across s collect (string c)))

(defmethod py-iterate->lisp-list ((x list))
  x)

(defmethod map-over-py-object ((func function) (object t))
  (warn "map over ~A" object)
  (loop with it-fun = (get-py-iterate-fun object)
      for val = (funcall it-fun)
      while val do (funcall func val)))

(defmethod map-over-py-object ((f function) (fi py-func-iterator))
  (with-slots (stopped-yet func) fi
    (unless stopped-yet
      (loop
	(multiple-value-bind (val err)
	    (ignore-errors (values (funcall func)))
	  (cond ((and err (not val))
		 (if (typep err 'StopIteration)
		     (return-from map-over-py-object)
		   (error err)))
		(val (funcall f val))
		(t   (return-from map-over-py-object))))))))

(defmethod map-over-py-object ((f function) (v string))
  (loop for ch across v
      for str = (py-string-from-char ch)
      do (funcall f str)))

(defmethod map-over-py-object ((f function) (v vector))
  #+(or)(warn "begin calling ~A on vector ~A" f v)
  (loop for x across v do (funcall f x))
  #+(or)(warn "end   calling ~A on vector ~A" f v))

(defmethod map-over-py-object ((f function) (x list))
  (mapc f x))

(defmethod map-over-py-object ((f function) (x hash-table))
  (loop for key being the hash-key in x
      do (funcall f key)))

;; Membership test

(defmethod py-in (x (ht hash-table))
  (multiple-value-bind (val found)
      (gethash x ht)
    (declare (ignore val))
    (py-bool found)))

(defmethod py-in ((item string) (seq string))
  ;; XXX 'ab' in 'abc' -> ??
  (if (= (length item) 1)
      
      (let ((ch (char item 0)))
	(py-bool (and (position ch seq) t)))
    
    (progn (warn "py-in on two non-char strings: ~S ~S" item seq)
	   (py-bool nil))))

(defmethod py-in (item (x vector))
  (if (stringp x)
      (call-next-method)
    (loop for d across x
	when (py-==->lisp-val item d) return (py-bool t)
	finally (return (py-bool nil)))))

(defmethod py-in (item (seq list))
  (py-bool (member item seq :test #'py-==->lisp-val)))


;; getitem

(defmethod py-subs ((x vector) (item fixnum))
  (let* ((x.len (length x))
	 (i2 (if (< item 0) (+ item x.len) item)))
    (if (<= 0 i2 (1- x.len))
	(aref x i2)
      (call-next-method))))

(defmethod py-subs ((x string) (item fixnum))
  (let* ((x.len (length x))
	 (i2 (if (< item 0) (+ item x.len) item)))
    (if (<= 0 i2 (1- x.len))
	(string (schar x i2))
      (call-next-method))))

(defmethod (setf py-subs) (val (x vector) (item fixnum))
  (let* ((x.len (length x))
	 (i2 (if (< item 0) (+ item x.len) item)))
    (if (<= 0 i2 (1- x.len))
	(setf (aref x i2) val)
      (call-next-method))))

(defmethod py-subs ((x hash-table) item)
  (or (gethash item x)
      (call-next-method)))

(defmethod (setf py-subs) (val (x hash-table) item)
  (setf (gethash item x) val))

;;; Comparison: ==
#+(or)
(define-compiler-macro py-== (&whole whole x y)
  #+(or)(warn "comp mac for == ~A ~A" x y)
  (cond ((comp-opt-p :inline-number-math)
	 `(let ((left ,x) (right ,y))
	    (if (and (excl:fixnump left) (excl:fixnump right))
		(fast (py-bool (= (the fixnum left) (the fixnum right))))
	      (locally (declare (notinline py-==))
		(py-== left right)))))
	
	(t whole)))

(defmethod py-== ((x fixnum) (y fixnum)) (py-bool (= x y)))
(defmethod py-== ((x string) (y string)) (py-bool (string= x y)))

(defmethod py-!= ((x fixnum) (y fixnum)) (py-bool (/= x y)))
(defmethod py-!= ((x string) (y string)) (py-bool (string/= x y)))

(defmethod py-== ((x vector) (y vector)) (py-list.__eq__ x y))
(defmethod py-== ((x list)   (y list))   (py-tuple.__eq__ x y))

(defmethod py-<  ((x fixnum) (y fixnum)) (py-bool (<  x y)))
(defmethod py-<= ((x fixnum) (y fixnum)) (py-bool (<= x y)))
(defmethod py->  ((x fixnum) (y fixnum)) (py-bool (>  x y)))
(defmethod py->= ((x fixnum) (y fixnum)) (py-bool (>= x y)))

;;; Arithmetic: + * // etc

#+(or)
(define-compiler-macro py-+ (&whole whole x y)
  #+(or)(warn "comp mac for + ~A ~A" x y)
  (cond ((comp-opt-p :inline-number-math)
	 `(let ((left ,x) (right ,y))
	    (if (and (excl:fixnump left) (excl:fixnump right))
		(+ (the fixnum left) (the fixnum right))
	      (locally (declare (notinline py-+))
		(py-+ left right)))))
	
	(t whole)))

#+(or)
(define-compiler-macro py-* (&whole whole x y)
  #+(or)(warn "comp mac for * ~A ~A" x y)
  (cond ((comp-opt-p :inline-number-math)
	 `(let ((left ,x) (right ,y))
	    (if (and (integerp left) (integerp right))
		(fast (* (the integer left) (the integer right)))
	      (locally (declare (notinline py-*))
		(py-* left right)))))
	
	(t whole)))

#+(or)
(define-compiler-macro py-// (&whole whole x y)
  #+(or)(warn "comp mac for // ~A ~A" x y)
  (cond ((comp-opt-p :inline-number-math)
	 `(let ((left ,x) (right ,y))
	    (if (and (integerp left) (integerp right))
		(fast (floor (the integer left) (the integer right)))
	      (locally (declare (notinline py-//))
		(py-// left right)))))
	
	(t whole)))


#+(or)
(define-compiler-macro py-% (&whole whole x y)
  #+(or)(warn "comp mac for % ~A ~A" x y)
  (cond ((comp-opt-p :inline-number-math)
	 `(let ((left ,x) (right ,y))
	    
	    (cond ((and (integerp left) (integerp right))
		   (fast (mod (the integer left) (the integer right))))
		  
		  ((stringp left)
		   (py-string.__mod__ left right))
		  
		  (t 
		   (locally (declare (notinline py-%))
		     (py-% left right))))))
	
	(t whole)))


(defmethod py-% ((x fixnum) (y fixnum)) (mod x y))
(defmethod py-% ((x string) y) (py-string.__mod__ x y))

(defmethod py-* ((x number) (y number)) (* x y))
(defmethod py-^ ((x integer) (y integer)) (logxor x y))
(defmethod py-& ((x integer) (y integer)) (logand x y))

#+(or) ;; XXX ignores softspace
(define-compiler-macro py-print (&whole whole dest items comma?)
  (if (and (null dest)
	   (listp items)
	   (eq (car items) 'list))
      
      `(excl::fast ,@(loop 
			 with num-items = (length items)
			 for x in (cdr items)
			 for i from 0
			 collect `(let ((x ,x))
				    (if (excl:fixnump x)
					(if (<= 0 (the fixnum x) 9)
					    (write-char
					     (char "0123456789" (the (integer 0 9) x)))
					  (write (the fixnum x) :pretty nil :base 10))
				      (write-string (py-str-string x)))
				    ,(when (< i (- num-items 2))
				       `(write-char #\Space t))))
		   ,(unless comma?
		      `(write-char #\Newline)))
    whole))


;;; Calling objects

(define-compiler-macro py-call (x &rest args)
  #+(or)(when (and (listp x) (eq (car x) 'py-attr))
	  (warn "py-call-attr ~A" x))
  `(let ((.x ,x))
     (if (functionp .x)
	 (excl::fast (funcall (the function .x) ,@args))
       (locally (declare (notinline py-call))
	 (py-call .x ,@args)))))

(defmethod py-hash ((x string)) (py-string.__hash__ x))
(defmethod py-hash ((x number)) (py-number.__hash__ x))


;;; String representation

(defmethod py-str ((x string)) x)
(defmethod py-str ((x vector)) (py-list.__str__ x))
(defmethod py-str ((x list))   (py-tuple.__str__ x))
(defmethod py-str ((x fixnum)) (format nil "~D" x))

(defmethod py-repr ((x string))
  ;; A rough filter for now
  (if (every #'alphanumericp x) (format nil "'~A'" x) (py-string.__repr__ x)))

(defmethod py-repr ((x vector)) (py-list.__repr__ x))
(defmethod py-repr ((x list))   (py-tuple.__repr__ x))
(defmethod py-repr ((x fixnum)) (format nil "~D" x))


;;; Length

(defmethod py-len ((x string)) (length x))
(defmethod py-len ((x vector)) (length x))
(defmethod py-len ((x list))   (length x))


