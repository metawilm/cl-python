;; -*- package: clpython; readtable: py-user-readtable -*-
;; 
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython)
(in-syntax *user-readtable*)

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

(defmethod map-over-py-object ((f function) (fi py-func-iterator))
  (with-slots (stopped-yet func) fi
    (unless stopped-yet
      (loop
	(multiple-value-bind (val err)
	    (ignore-errors (values (funcall func)))
	  (cond ((and err (not val))
		 (if (typep err '{StopIteration})
		     (return-from map-over-py-object)
		   (progn
		     #+(or)(warn "MAP-OVER-PY-OBJECT error (func ~S, py-func-iterator ~S): ~S" f fi err)
		     (error err))))
		(val (funcall f val))
		(t   (return-from map-over-py-object))))))))

(defmethod map-over-py-object ((f function) (v string))
  (loop for ch across v
      for str = (py-string-from-char ch)
      do (funcall f str)))

(defmethod map-over-py-object ((f function) (v vector))
  (loop for x across v do (funcall f x)))

(defmethod map-over-py-object ((f function) (x list))
  (mapc f x))

(defmethod map-over-py-object ((f function) (x py-dict))
  (let ((map-func (lambda (k v)
                    (declare (ignore v))
                    (funcall f k))))
    (declare (dynamic-extent map-func))
    (dict-map x map-func)))

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
    (declare (type (integer 0 #.most-positive-fixnum) i))
    (lambda ()
      (fast
       (when (< i (length x))
	 (prog1 (string (aref x i))
	   (incf i)))))))

(defmethod get-py-iterate-fun ((x vector))
  (let ((i 0))
    (declare (type (integer 0 #.most-positive-fixnum) i))
    (lambda ()
      (fast
       (when (< i (length x))
	 (prog1 (aref x i)
	   (incf i)))))))
	  

;; Membership test

(defmethod py-in (x (d py-dict))
  (if (eq (class-of d) (ltv-find-class 'py-dict))
      (py-bool (sub/dict-get d x))
    (call-next-method)))
    
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
	(string (char x i2))
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
	(progn
	  (when (stringp x)
	    (call-next-method)) ;; error
	  (if (null val)
	      (progn (loop for i from (1+ i2) below x.len
			 do (setf (aref x (1- i)) (aref x i)))
		     (decf (fill-pointer x)))
	    (setf (aref x i2) val)))
      (call-next-method)))) ;; error

(defmethod py-subs ((x py-dict) item)
  (if (eq (class-of x) (ltv-find-class 'py-dict))
      (py-dict.__getitem__ x item)
    (call-next-method)))

(defmethod (setf py-subs) (val (x py-dict) item)
  (if (eq (class-of x) (ltv-find-class 'py-dict))
      (if val
          (py-dict.__setitem__ x item val)
        (py-dict.__delitem__ x item))
    (call-next-method)))

;;; Comparison: ==

(defun maybe-number-p (expr)
  (etypecase expr
    (null   nil)
    (list   t)
    (string nil)
    (number t)
    (symbol t)))

(define-compiler-macro py-== (&whole whole x y)
  (if (and *inline-fixnum-arithmetic*
           (maybe-number-p x)
           (maybe-number-p y))
      `(let ((.x ,x)
	     (.y ,y))
	 (if (and (integerp .x) (integerp .y))
	     (py-bool (= .x .y))
	   (locally (declare (notinline py-==))
	     (py-== .x .y))))
    whole))

(define-compiler-macro py-!= (&whole whole x y)
  (if (and *inline-fixnum-arithmetic*
           (maybe-number-p x)
           (maybe-number-p y))
      `(let ((.x ,x)
	     (.y ,y))
	 (if (and (integerp .x) (integerp .y))
	     (py-bool (/= .x .y))
	   (locally (declare (notinline py-!=))
	     (py-!= .x .y))))
    whole))



(defmethod py-== ((x fixnum) (y fixnum)) (py-bool (= x y)))
(defmethod py-== ((x string) (y string)) (py-bool (string= x y)))
;; (py-== string symbol) and (py-== symbol string) are defined in classes.lisp already

(defmethod py-== ((x vector) (y vector)) (py-list.__eq__ x y))
(defmethod py-== ((x list)   (y list))   (py-tuple.__eq__ x y))

(defmethod py-!= ((x fixnum) (y fixnum)) (py-bool (/= x y)))
(defmethod py-!= ((x string) (y string)) (py-bool (string/= x y)))

(defmethod py-<  ((x fixnum) (y fixnum)) (py-bool (<  x y)))
(defmethod py-<= ((x fixnum) (y fixnum)) (py-bool (<= x y)))
(defmethod py-<= ((x string) (y string)) (py-bool (string<= x y)))

(defmethod py->  ((x fixnum) (y fixnum)) (py-bool (>  x y)))
(defmethod py->= ((x fixnum) (y fixnum)) (py-bool (>= x y)))

(defmacro generate-cmp-cm (op)
  `(define-compiler-macro ,op (&whole whole x y)
     (if (and (numberp x) (numberp y))
         (let ((val (py-bool (,op x y))))
           `,val)
       whole)))

(generate-cmp-cm py-<)
(generate-cmp-cm py-<=)
(generate-cmp-cm py->)
(generate-cmp-cm py->=)

;(py-<= 1 2)
    
;;; Arithmetic: + * // etc

(defmacro fixnump (x)
  `(typep ,x 'fixnum))

(define-compiler-macro fixnump (&whole whole x)
  (if (numberp x)
      (typep x 'fixnum)
    whole))

(define-compiler-macro py-+ (&whole whole x y)
  (if (and *inline-fixnum-arithmetic*
           (maybe-number-p x)
           (maybe-number-p y))
      `(let ((.x ,x)
	     (.y ,y))
	 (cond ((and (fixnump .x) (fixnump .y))
		(+ (the fixnum .x) (the fixnum .y)))
	       (t (locally (declare (notinline py-+))
		    (py-+ .x .y)))))
    whole))

(define-compiler-macro py-* (&whole whole x y)
  (if (and *inline-fixnum-arithmetic*
           (maybe-number-p x)
           (maybe-number-p y))
      `(let ((.x ,x)
	     (.y ,y))
	 (if (and (fixnump .x) (fixnump .y))
	     (* (the fixnum .x) (the fixnum .y))
	   (locally (declare (notinline py-*))
	     (py-* .x .y))))
    whole))

(define-compiler-macro py-- (&whole whole x y)
  (if (and *inline-fixnum-arithmetic*
           (maybe-number-p x)
           (maybe-number-p y))
      `(let ((.x ,x)
	     (.y ,y))
	 (if (and (fixnump .x) (fixnump .y))
	     (- (the fixnum .x) (the fixnum .y))
	   (locally (declare (notinline py--))
	     (py-- .x .y))))
    whole))

(define-compiler-macro py-// (&whole whole x y)
  (if (and *inline-fixnum-arithmetic*
           (maybe-number-p x)
           (maybe-number-p y))
      `(let ((.x ,x)
	     (.y ,y))
	 (if (and (fixnump .x) (fixnump .y))
	     (floor (the fixnum .x) (the fixnum .y))
	   (locally (declare (notinline py-//))
	     (py-// .x .y))))
    whole))

(define-compiler-macro py-% (&whole whole x y)
  (cond ((stringp x)
         `(py-string.__mod__ ,x ,y))
        ((and  *inline-fixnum-arithmetic*
               (maybe-number-p x)
               (maybe-number-p y))
         `(let ((.x ,x)
                (.y ,y))
            (if (and (fixnump .x) (fixnump .y))
                (mod (the fixnum .x) (the fixnum .y))
              (locally (declare (notinline py-%))
                (py-% .x .y)))))
        (t whole)))

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
  (typecase x
    (fixnum (when (< x 0)
              (write-char #\- stream)
              (setf x (- x)))
            #+allegro (excl::print-fixnum stream 10 x) ;; only handles positive numbers!
            #-allegro (format stream "~D" x)
            nil)
    (string #+allegro (excl::write-string-nokey x stream)
            #-allegro (write-string x stream)
            (let ((x.len (length x)))
              (and (> x.len 0)
                   (aref x (1- (length x))))))
    (t      #+allegro (excl::write-string-nokey (py-str-string x) stream)
            #-allegro (write-string (py-str-string x) stream)
            nil)))

(defmacro fast-write-char (char stream)
  ;; #+allegro `(excl::fast-write-char ,char ,stream)
  `(write-char ,char ,stream))

(define-compiler-macro py-print (&whole whole dest items comma?)
  (if (and *inline-print*
           (null dest)
	   (listp items)
	   (eq (car items) 'list))
      
      `(let ((stdout (fast *standard-output*))) ;; XXX use sys.stdout (add check?)
	 (declare (ignorable stdout))
	 ,@(loop 
	       with num-items = (length (cdr items))
	       for x in (cdr items)
	       for i from 0
	       collect `(progn 
			  ;; Spaces before first item (perhaps), always between items
			  ,(if (= i 0)
			       `(when (py-val->lisp-bool (fast *stdout-softspace*))
				  (fast-write-char #\Space stdout))
                             `(fast-write-char #\Space stdout))
                          
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
	    `(progn (fast-write-char #\Newline stdout)
		    (force-output stdout)))
	 
	 ;; Return value:
	 nil)
    
    whole))


(defmethod py-hash ((x string)) (py-string.__hash__ x))
(defmethod py-hash ((x number)) (py-number.__hash__ x))


;;; String representation

(defmethod py-str ((x string)) x)
(defmethod py-str ((x vector)) (py-list.__str__ x))
(defmethod py-str ((x list))   (py-tuple.__str__ x))
(defmethod py-str ((x fixnum)) (if (<= 0 x 100)
				   (svref (load-time-value
					   (coerce (loop for i from 0 to 100
						       collect (format nil "~D" i)) 'simple-vector))
					  x)
				 (format nil "~D" x)))

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
