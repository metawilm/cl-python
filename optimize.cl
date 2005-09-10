(in-package :python)

;;; Optimizations: compiler macros, specialized methods 

;;; tests for ==, in

(defmethod py-in (x (ht hash-table))
  (py-bool (gethash x ht)))

(defmethod py-in ((item string) (seq string))
  ;; XXX 'ab' in 'abc' -> ??
  (if (= (length item) 1)
      (let ((ch (char item 0)))
	(py-bool (position ch seq :test #'py-==->lisp-val) ))
    (py-bool nil)))
		 
(defmethod py-in (item (seq list))
  (py-bool (member item seq :test #'py-==->lisp-val)))


;;; Iteration

(defmethod py-iterate->lisp-list ((v vector))
  (loop for x across v collect x))

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
      for str = (string ch) ;; XXX wasteful
      do (funcall f str)))

(defmethod map-over-py-object ((f function) (v vector))
  #+(or)(warn "begin calling ~A on vector ~A" f v)
  (loop for x across v do (funcall f x))
  #+(or)(warn "end   calling ~A on vector ~A" f v))

(defmethod map-over-py-object ((f function) (x list))
  ;; X is a tuple
  (mapc f x))

;;; Arithmetic: + * // etc

(define-compiler-macro py-+ (&whole whole x y)
  #+(or)(warn "comp mac for + ~A ~A" x y)
  (cond ((comp-opt-p :inline-number-math)
	 `(let ((left ,x) (right ,y))
	    (if (and (excl:fixnump left) (excl:fixnump right))
		(+ (the fixnum left) (the fixnum right))
	      (locally (declare (notinline py-+))
		(py-+ left right)))))
	
	(t whole)))

(define-compiler-macro py-* (&whole whole x y)
  #+(or)(warn "comp mac for * ~A ~A" x y)
  (cond ((comp-opt-p :inline-number-math)
	 `(let ((left ,x) (right ,y))
	    (if (and (integerp left) (integerp right))
		(fast (* (the integer left) (the integer right)))
	      (locally (declare (notinline py-*))
		(py-* left right)))))
	
	(t whole)))

(define-compiler-macro py-// (&whole whole x y)
  #+(or)(warn "comp mac for // ~A ~A" x y)
  (cond ((comp-opt-p :inline-number-math)
	 `(let ((left ,x) (right ,y))
	    (if (and (integerp left) (integerp right))
		(fast (floor (the integer left) (the integer right)))
	      (locally (declare (notinline py-//))
		(py-// left right)))))
	
	(t whole)))

(define-compiler-macro py-== (&whole whole x y)
  #+(or)(warn "comp mac for == ~A ~A" x y)
  (cond ((comp-opt-p :inline-number-math)
	 `(let ((left ,x) (right ,y))
	    (if (and (excl:fixnump left) (excl:fixnump right))
		(fast (py-bool (= (the fixnum left) (the fixnum right))))
	      (locally (declare (notinline py-==))
		(py-== left right)))))
	
	(t whole)))

(define-compiler-macro py-% (&whole whole x y)
  #+(or)(warn "comp mac for % ~A ~A" x y)
  (cond ((comp-opt-p :inline-number-math)
	 `(let ((left ,x) (right ,y))
	    (if (and (integerp left) (integerp right))
		(fast (mod (the integer left) (the integer right)))
	      (locally (declare (notinline py-%))
		(py-% left right)))))
	
	(t whole)))


#+(or) ;; methods on py-+
((defmethod py-+  ((x integer) (y integer)) (+ x y))
 (defmethod py-// ((x integer) (y integer)) (floor x y))
 (defmethod py-*  ((x integer) (y integer)) (* x y)))



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

#+(or) ;; not for now: regular #'py-call contains debug stuff
(define-compiler-macro py-call (x &rest args)
  `(let ((.x ,x))
     (if (functionp .x)
	 (excl::fast (funcall (the function .x) ,@args))
       (locally (declare (notinline py-call))
	 (py-call .x ,@args)))))




