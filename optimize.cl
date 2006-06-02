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

#+(or) ;; already in classes.cl
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

(defmethod map-over-py-object ((f function) (x py-xrange))
  (with-slots (start stop step) x
    (cond ((and (<= start stop) (> step 0))
	   (loop for i from start below stop by step do (funcall f i)))
	  ((and (>= start stop) (< step 0))
	   (loop for i from stop downto start by (- step) do (funcall f i)))
	  (t (call-next-method)))))


(defmethod get-py-iterate-fun ((x py-func-iterator))
  (slot-value x 'func))

(defmethod get-py-iterate-fun ((x string))
  (let ((i 0))
    (lambda ()
      (when (< i (length x))
	(prog1 (string (aref x i))
	  (incf i))))))

(defmethod get-py-iterate-fun ((x vector))
  (let ((i 0))
    (lambda ()
      (when (< i (length x))
	(prog1 (aref x i)
	  (incf i))))))
	  

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

(defmethod py-subs ((x list) (item fixnum)) ;; tuple
  (when (< item 0)
    (setf x    (nthcdr (- item) x)
	  item 0))
  (or (nth item x)
      (call-next-method)))

(defmethod (setf py-subs) (val (x vector) (item fixnum))
  (let* ((x.len (length x))
	 (i2 (if (< item 0) (+ item x.len) item)))
    (if (<= 0 i2 (1- x.len))
	(if (null val)
	    (progn (loop for i from (1+ i2) below x.len
		       do (setf (aref x (1- i)) (aref x i)))
		   (decf (fill-pointer x)))
	  (setf (aref x i2) val))
      (call-next-method))))

(defmethod py-subs ((x hash-table) item)
  (or (gethash item x)
      (call-next-method)))

(defmethod (setf py-subs) (val (x hash-table) item)
  (if (null val)
      (or (remhash item x)
	  (call-next-method))
    (setf (gethash item x) val)))

;;; Comparison: ==

(define-compiler-macro py-== (x y)
  `(let ((.x ,x)
	 (.y ,y))
     (if (and (integerp .x) (integerp .y))
	 (py-bool (= .x .y))
       (locally (declare (notinline py-==))
	 (py-== .x .y)))))

(define-compiler-macro py-!= (x y)
  `(let ((.x ,x)
	 (.y ,y))
     (if (and (integerp .x) (integerp .y))
	 (py-bool (/= .x .y))
       (locally (declare (notinline py-!=))
	 (py-!= .x .y)))))

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

(define-compiler-macro py-+ (x y)
  `(let ((.x ,x)
	 (.y ,y))
     (cond ((and (excl::fixnump .x) (excl::fixnump .y))
	    (+ (the fixnum .x) (the fixnum .y)))
	   ((and (stringp .x) (stringp .y))
	    (concatenate 'string .x .y))
	   (t (locally (declare (notinline py-+))
		(py-+ .x .y))))))

(define-compiler-macro py-* (x y)
  `(let ((.x ,x)
	 (.y ,y))
     (if (and (excl::fixnump .x) (excl::fixnump .y))
	 (* (the fixnum .x) (the fixnum .y))
       (locally (declare (notinline py-*))
	 (py-* .x .y)))))

(define-compiler-macro py-- (x y)
  `(let ((.x ,x)
	 (.y ,y))
     (if (and (excl::fixnump .x) (excl::fixnump .y))
	 (- (the fixnum .x) (the fixnum .y))
       (locally (declare (notinline py--))
	 (py-- .x .y)))))

(define-compiler-macro py-// (x y)
  `(let ((.x ,x)
	 (.y ,y))
     (if (and (excl::fixnump .x) (excl::fixnump .y))
	 (floor (the fixnum .x) (the fixnum .y))
       (locally (declare (notinline py-//))
	 (py-// .x .y)))))

(define-compiler-macro py-% (x y)
  (if (stringp x)
    
      `(py-string.__mod__ ,x ,y)
    
    `(let ((.x ,x)
	   (.y ,y))
       (if (and (excl::fixnump .x) (excl::fixnump .y))
	   (mod (the fixnum .x) (the fixnum .y))
	 (locally (declare (notinline py-%))
	   (py-% .x .y))))))

(defmethod py-% ((x fixnum) (y fixnum)) (mod x y))
(defmethod py-% ((x string) y) (py-string.__mod__ x y))
(defmethod py-% ((x integer) (y integer)) (mod x y))

(defmethod py-* ((x number) (y number)) (* x y))
(defmethod py-^ ((x integer) (y integer)) (logxor x y))
(defmethod py-& ((x integer) (y integer)) (logand x y))

(defmethod py-+  ((x number) (y number))   (+ x y))
(defmethod py--  ((x integer) (y integer)) (- x y))
(defmethod py-// ((x integer) (y integer)) (floor x y))

;; Augmented assignment

(defmethod py-+= ((x string) y) 
  (declare (ignore y))
  nil)

(defmethod py-+= ((x number) y) 
  (declare (ignore y))
  nil)

(defmethod py--= ((x number) y) 
  (declare (ignore y))
  nil)

(defmethod py-*= ((x number) y) 
  (declare (ignore y))
  nil)

(defmethod py-/= ((x number) y)
  (declare (ignore y))
  nil)

(defmethod py-//= ((x number) y)
  (declare (ignore y))
  nil)

(defun py-print-cmhelper (x stream)
  ;; Quickly prints obj to stdout
  ;; If X is a string, returns last char of it; otherwise returns NIL
  (excl::fast
   (typecase x
     
     (fixnum (excl::print-fixnum stream 10 x)
	     nil)
     
     (string (excl::write-string-nokey x stream)
	     (let ((x.len (length x)))
	       (and (> x.len 0)
		    (aref x (1- (length x))))))
     
     (t      (excl::write-string-nokey (py-str-string x) stream)
	     nil))))

(define-compiler-macro py-print (&whole whole dest items comma?)
  (if (and (null dest)
	   (listp items)
	   (eq (car items) 'list))
      
      `(let ((stdout (excl::fast *standard-output*))) ;; XXX use sys.stdout (add check?)
	 (declare (ignorable stdout))
	 ,@(loop 
	       with num-items = (length (cdr items))
	       for x in (cdr items)
	       for i from 0
	       collect `(progn 
			  ;; Spaces before first item (perhaps), always between items
			  ,(if (= i 0)
			       `(when (py-val->lisp-bool (excl::fast *stdout-softspace*))
				  (excl::fast-write-char #\Space stdout))
			     `(excl::fast-write-char #\Space stdout))
			  
			  ;; Print item
			  ,(if (< i (1- num-items))
			       `(py-print-cmhelper ,x stdout)
			     
			     `(progn 
				;; Set new softspace value
				(let ((last-char-written (py-print-cmhelper ,x stdout)))
				  (declare (ignorable last-char-written))
				  (let ((printed-newline-already
					 ,(cond ((not comma?) t)
						(t `(and last-char-written
							 (char= last-char-written
								#\Newline))))))
				    (setf *stdout-softspace*
				      (py-bool (not printed-newline-already)))))))))
	 ;; Newline after last item
	 ,(unless comma?
	    `(progn (excl::fast-write-char #\Newline stdout)
		    (force-output stdout)))
	 
	 ;; Return value:
	 nil)
    
    whole))


;;; Calling objects

#|| ;; too risky for now
(define-compiler-macro py-call (x &rest args)
  #+(or)(when (and (listp x) (eq (car x) 'py-attr))
	  (warn "py-call-attr ~A" x))
  `(let ((.x ,x))
     (if (functionp .x)
	 (excl::fast (funcall (the function .x) ,@args))
       (locally (declare (notinline py-call))
	 (py-call .x ,@args)))))
||#

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


;;; Tests

(define-compiler-macro py-val->lisp-bool (&whole whole x)
  (typecase x
    ((or string number) (py-val->lisp-bool x))
    (t                  whole)))
