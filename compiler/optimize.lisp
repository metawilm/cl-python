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

#+(or)
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

#+(or)
(defmethod map-over-py-object ((f function) (v string))
  (loop for ch across v
      for str = (py-string-from-char ch)
      do (funcall f str)))

#+(or)
(defmethod map-over-py-object ((f function) (v vector))
  (loop for x across v do (funcall f x)))

#+(or)
(defmethod map-over-py-object ((f function) (x list))
  (mapc f x))

#+(or)
(defmethod map-over-py-object ((f function) (x dict))
  (let ((map-func (lambda (k v)
                    (declare (ignore v))
                    (funcall f k))))
    (declare (dynamic-extent map-func))
    (dict-map x map-func)))

#+(or)
(defmethod map-over-py-object ((f function) (x py-xrange))
  (with-slots (start stop step) x
    (cond ((and (<= start stop) (> step 0))
	   (loop for i from start below stop by step do (funcall f i)))
	  ((and (>= start stop) (< step 0))
	   (loop for i from stop downto start by (- step) do (funcall f i)))
	  (t (call-next-method)))))

(defmethod get-py-iterate-fun ((x py-xrange))
  (xrange-iter-func x))

(defmethod get-py-iterate-fun ((x list)) ;; tuple
  (lambda () (pop x)))
  
(defmethod get-py-iterate-fun ((x py-func-iterator))
  (lambda ()
    (handler-case (funcall (slot-value x 'func))
      ({StopIteration} () nil))))

(defmethod get-py-iterate-fun ((x string))
  (let ((i 0))
    (declare (type (integer 0 #.most-positive-fixnum) i))
    (lambda ()
      (fastest
       (when (< i (length x))
	 (prog1 (string (aref x i))
	   (incf i)))))))

(defmethod get-py-iterate-fun ((x vector))
  (let ((i 0))
    (declare (type (integer 0 #.most-positive-fixnum) i))
    (lambda ()
      (fastest
       (when (< i (length x))
	 (prog1 (aref x i)
	   (incf i)))))))
	  

;; Membership test

(defmethod py-in (x (d dict))
  (if (eq (class-of d) (ltv-find-class 'dict))
      (py-bool (gethash x (deproxy d)))
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +fixnum-is-a-class+ (not (null (find-class 'fixnum nil))))
  (register-feature :clpython-fixnum-is-a-class +fixnum-is-a-class+))

(defmethod py-subs ((x vector) (item #+clpython-fixnum-is-a-class fixnum #-clpython-fixnum-is-a-class integer))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((x.len (length x))
         (item2 (if (minusp item)
                    (+ item x.len)
                  item)))
    (if (<= 0 item2 (1- x.len))
	(aref x item2)
      (py-list.__getitem__ x item))))

(defmethod py-subs ((x vector) (item py-slice))
  ;; inline x[:]
  (with-slots (start stop step) item
    (when (and (none-p start) (none-p stop) (none-p step))
      (return-from py-subs
        (if (stringp x)
            x ;; using property that strings are immutable
          (make-array (length x) :adjustable t :fill-pointer (length x) :initial-contents x)))))
  (if (stringp x)
      (py-string.__getitem__ x item)
    (py-list.__getitem__ x item)))
      
(defmethod py-subs ((x string) (item #+clpython-fixnum-is-a-class fixnum #-clpython-fixnum-is-a-class integer))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((x.len (length x))
	 (i2 (if (< item 0) (+ item x.len) item)))
    (if (<= 0 i2 (1- x.len))
	(py-string-from-char (char x i2))
      (py-string.__getitem__ x item))))

(defmethod py-subs ((x list) (item #+clpython-fixnum-is-a-class fixnum #-clpython-fixnum-is-a-class integer)) ;; tuple
  (when (< item 0)
    (setf x    (nthcdr (- item) x)
	  item 0))
  (or (nth item x)
      (call-next-method)))

(defmethod (setf py-subs) ((val vector) (x vector) (item py-slice))
  ;; Inline cases like "x[:4] = [1,2,3,4]"
  (destructuring-bind (kind &rest args)
      (multiple-value-list (slice-indices item (length x)))
    (when (eq kind :nonempty-slice)
      (destructuring-bind (start-incl stop-incl num) args
        (when (= num (length val))
          (loop for x-i from start-incl to stop-incl
              for val-i from 0
              do (setf (aref x x-i) (aref val val-i)))
          (return-from py-subs *the-none*)))))
  (call-next-method))
               
(defmethod (setf py-subs) (val (x vector) (item #+clpython-fixnum-is-a-class fixnum #-clpython-fixnum-is-a-class integer))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (when (stringp x)
    (py-raise '{TypeError} "Cannot assign to string items."))
  (let* ((x.len (length x))
	 (i2 (if (minusp item) (+ item x.len) item)))
    (if (<= 0 i2 (1- x.len))
        (if (null val)
            (progn (loop for i from (1+ i2) below x.len
                       do (setf (aref x (1- i)) (aref x i)))
                   (decf (fill-pointer x)))
          (setf (aref x i2) val))
      (if val
          (py-list.__setitem__ x item val) ;; error
        (py-list.__delitem__ x item))))) ;; error

(defmethod py-subs ((x dict) item)
  (if (eq (class-of x) (ltv-find-class 'dict))
      (dict.__getitem__ x item)
    (call-next-method)))

(defmethod (setf py-subs) (val (x dict) item)
  (if (eq (class-of x) (ltv-find-class 'dict))
      (if val
          (dict.__setitem__ x item val)
        (dict.__delitem__ x item))
    (call-next-method)))

(defmethod py-subs ((x hash-table) item)
  (let ((res (gethash item x)))
    (or res
        (call-next-method)))) ;; deal with key error

;;; Comparison: ==

(defun maybe-number-p (expr)
  (etypecase expr
    (null   nil)
    (list   t)
    (string nil)
    (number t)
    (symbol t)))

(define-compiler-macro py-== (&whole whole x y)
  (if (and *inline-number-arithmetic*
           (maybe-number-p x)
           (maybe-number-p y))
      `(let ((.x ,x)
             (.y ,y))
         (if (and (numberp .x) (numberp .y))
             (py-bool (= (the number .x) (the number .y)))
           (locally (declare (notinline py-==))
             (py-== .x .y))))
    whole))

(define-compiler-macro py-!= (&whole whole x y)
  (if (and *inline-number-arithmetic*
           (maybe-number-p x)
           (maybe-number-p y))
      `(let ((.x ,x)
	     (.y ,y))
	 (if (and (numberp .x) (numberp .y))
	     (py-bool (/= .x .y))
	   (locally (declare (notinline py-!=))
	     (py-!= .x .y))))
    whole))

#+clpython-fixnum-is-a-class
(defmethod py-== ((x fixnum) (y fixnum)) (py-bool (= x y)))
(defmethod py-== ((x string) (y string)) (py-bool (string= x y)))
;; (py-== string symbol) and (py-== symbol string) are defined in classes.lisp already

(defmethod py-== ((x vector) (y vector)) (py-list.__eq__ x y))
(defmethod py-== ((x list)   (y list))   (py-tuple.__eq__ x y))

#+clpython-fixnum-is-a-class
(defmethod py-!= ((x fixnum) (y fixnum)) (py-bool (/= x y)))
(defmethod py-!= ((x string) (y string)) (py-bool (string/= x y)))

#+clpython-fixnum-is-a-class
(progn (defmethod py-<  ((x fixnum) (y fixnum)) (py-bool (<  x y)))
       (defmethod py-<= ((x fixnum) (y fixnum)) (py-bool (<= x y)))
       (defmethod py->  ((x fixnum) (y fixnum)) (py-bool (>  x y)))
       (defmethod py->= ((x fixnum) (y fixnum)) (py-bool (>= x y))))

(defmethod py-<  ((x float) (y float)) (declare #.+optimize-fastest+) (py-bool (<  x y)))
(defmethod py-<= ((x float) (y float)) (declare #.+optimize-fastest+) (py-bool (<= x y)))
(defmethod py->  ((x float) (y float)) (declare #.+optimize-fastest+) (py-bool (>  x y)))
(defmethod py->= ((x float) (y float)) (declare #.+optimize-fastest+) (py-bool (>= x y)))

#+clpython-fixnum-is-a-class
(progn (defmethod py-<= ((x single-float) (y fixnum)) (declare #.+optimize-fastest+) (py-bool (<= x y)))
       (defmethod py-<= ((x double-float) (y fixnum)) (declare #.+optimize-fastest+) (py-bool (<= x y)))
       (defmethod py-<= ((x fixnum) (y single-float)) (declare #.+optimize-fastest+) (py-bool (<= x y)))
       (defmethod py-<= ((x fixnum) (y double-float)) (declare #.+optimize-fastest+) (py-bool (<= x y)))

       (defmethod py->= ((x single-float) (y fixnum)) (declare #.+optimize-fastest+) (py-bool (>= x y)))
       (defmethod py->= ((x double-float) (y fixnum)) (declare #.+optimize-fastest+) (py-bool (>= x y)))
       (defmethod py->= ((x fixnum) (y single-float)) (declare #.+optimize-fastest+) (py-bool (>= x y)))
       (defmethod py->= ((x fixnum) (y double-float)) (declare #.+optimize-fastest+) (py-bool (>= x y)))

       (defmethod py-< ((x single-float) (y fixnum)) (declare #.+optimize-fastest+) (py-bool (< x y)))
       (defmethod py-< ((x double-float) (y fixnum)) (declare #.+optimize-fastest+) (py-bool (< x y)))
       (defmethod py-< ((x fixnum) (y single-float)) (declare #.+optimize-fastest+) (py-bool (< x y)))
       (defmethod py-< ((x fixnum) (y double-float)) (declare #.+optimize-fastest+) (py-bool (< x y)))

       (defmethod py-> ((x single-float) (y fixnum))
         (declare #.+optimize-fastest+ #+(or)(:explain :boxing))
         ;; Prevent Allegro from converting a float into ratio if comparison can be done without.
         ;; XXX check this optimization for other methods and in other implementations.
         (let ((xf (floor x)))
           (py-bool (cond ((> xf y) t)
                          ((< (1+ xf) y) nil)
                          (t (> x y))))))

       (defmethod py-> ((x double-float) (y fixnum)) (declare #.+optimize-fastest+) (py-bool (> x y)))
       (defmethod py-> ((x fixnum) (y single-float)) (declare #.+optimize-fastest+) (py-bool (> x y)))
       (defmethod py-> ((x fixnum) (y double-float)) (declare #.+optimize-fastest+) (py-bool (> x y))))

(defmethod py-<  ((x number) (y number)) (py-bool (<  x y)))
(defmethod py-<= ((x number) (y number)) (py-bool (<= x y)))
(defmethod py->  ((x number) (y number)) (py-bool (>  x y)))
(defmethod py->= ((x number) (y number)) (py-bool (>= x y)))

(defmethod py->  ((x string) (y string)) (py-bool (string>  x y)))
(defmethod py->= ((x string) (y string)) (py-bool (string>= x y)))
(defmethod py-<= ((x string) (y string)) (py-bool (string<= x y)))
(defmethod py-<  ((x string) (y string)) (py-bool (string<  x y)))

(defmacro generate-cmp-cm (op)
  `(define-compiler-macro ,op (&whole whole x y)
     (if (and (numberp x) (numberp y))
         (let ((val (,op x y)))
           (assert (member val (list +the-true+ +the-false+)) ()
             "Constant comparison of two numbers should result in True or False (got ~A ~A ~A => ~S)"
             x ',op y val)
           `,val)
       whole)))

(generate-cmp-cm py-<)
(generate-cmp-cm py-<=)
(generate-cmp-cm py->)
(generate-cmp-cm py->=)

;(py-<= 1 2)
    
;;; Arithmetic: + * // etc

(define-compiler-macro py-+ (&whole whole x y)
  (if (and *inline-number-arithmetic*
           (maybe-number-p x)
           (maybe-number-p y))
      `(let ((.x ,x)
	     (.y ,y))
	 (if (and (numberp .x) (numberp .y))
             (+ .x .y)
           (locally (declare (notinline py-+))
             (py-+ .x .y))))
    whole))

(define-compiler-macro py-* (&whole whole x y)
  (if (and *inline-number-arithmetic*
           (maybe-number-p x)
           (maybe-number-p y))
      `(let ((.x ,x)
	     (.y ,y))
	 (if (and (numberp .x) (numberp .y))
	     (* .x .y)
	   (locally (declare (notinline py-*))
	     (py-* .x .y))))
    whole))

(define-compiler-macro py-- (&whole whole x y)
  (if (and *inline-number-arithmetic*
           (maybe-number-p x)
           (maybe-number-p y))
      `(let ((.x ,x)
	     (.y ,y))
	 (if (and (numberp .x) (numberp .y))
	     (- .x .y)
	   (locally (declare (notinline py--))
	     (py-- .x .y))))
    whole))

(define-compiler-macro py-// (&whole whole x y)
  (if (and *inline-number-arithmetic*
           (maybe-number-p x)
           (maybe-number-p y))
      `(let ((.x ,x)
	     (.y ,y))
	 (if (and (integerp .x) (integerp .y)) ;; integer, not number
	     (careful-floor-1ret .x .y)
	   (locally (declare (notinline py-//))
	     (py-// .x .y))))
    whole))

(define-compiler-macro py-% (&whole whole x y)
  (cond ((stringp x)
         `(py-string.__mod__ ,x ,y))
        ((and  *inline-number-arithmetic*
               (maybe-number-p x)
               (maybe-number-p y))
         `(let ((.x ,x)
                (.y ,y))
            (if (and (integerp .x) (integerp .y)) ;; integer, not number
                (mod .x .y)
              (locally (declare (notinline py-%))
                (py-% .x .y)))))
        (t whole)))

#+clpython-fixnum-is-a-class
(defmethod py-% ((x fixnum) (y fixnum)) (mod x y))
(defmethod py-% ((x string) y) (py-string.__mod__ x y))
(defmethod py-% ((x integer) (y integer)) (mod x y))

(defmethod py-* ((x number) (y number)) (* x y))
(defmethod py-^ ((x integer) (y integer)) (logxor x y))
(defmethod py-& ((x integer) (y integer)) (logand x y))

(defmethod py-+  ((x number) (y number))   (+ x y))
(defmethod py--  ((x integer) (y integer)) (- x y))

(defmethod py--  ((x float) (y float)) (- x y))
(defmethod py-+  ((x float) (y float)) (+ x y))

(defmethod py-// ((x integer) (y integer)) (careful-floor-1ret x y))

(defmethod py-/ ((x integer) (y integer)) (careful-floor-1ret x y))
(defmethod py-/ ((x float) (y float)) (/ x y))
                                           
;; Augmented assignment

(defmethod py-+= ((x string) y) 
  (declare (ignorable x) (ignore y))
  nil)

(defmethod py-+= ((x number) y) 
  (declare (ignorable x) (ignore y))
  nil)

(defmethod py--= ((x number) y) 
  (declare (ignorable x) (ignore y))
  nil)

(defmethod py-*= ((x number) y) 
  (declare (ignorable x) (ignore y))
  nil)

(defmethod py-/= ((x number) y)
  (declare (ignorable x) (ignore y))
  nil)

(defmethod py-//= ((x number) y)
  (declare (ignorable x) (ignore y))
  nil)

(defmethod py-** ((x float) (y float) &optional z)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (null z)
      (coerce (expt x y) '#.clpython.parser:*normal-float-representation-type*)
    (py-raise '{TypeError} "Not implemented: x**y % z with x,y float and z provided.")))

(defmethod py-** ((x number) (y number) &optional z)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if z
      (progn (check-type x integer)
             (check-type y integer)
             (check-type z integer)
             (mod (expt x y) z))
    (let ((res (expt x y)))
      (if (rationalp res)
          (coerce res '#.clpython.parser:*normal-float-representation-type*)
        res))))

(define-compiler-macro py-** (x y &optional z)
  `(let ((.x ,x)
         (.y ,y)
         (.z ,z))
     (if (and (null .z)
              (numberp .x)
              (numberp .y))
         (expt .x .y)
       (locally (declare (notinline py-**))
         (py-** .x .y .z)))))

;; Unary

(defmethod py-unary-- ((x number))
  (- x))

#+(or)
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

#+(or)
(defmacro fast-write-char (char stream)
  ;; #+allegro `(excl::fast-write-char ,char ,stream)
  `(write-char ,char ,stream))

#+(or)
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
		    #+(or)(force-output stdout) ;; hurts performance
                    ))
	 
	 ;; Return value:
	 nil)
    
    whole))


(defmethod py-hash ((x string)) (py-string.__hash__ x))
(defmethod py-hash ((x number)) (py-number.__hash__ x))

;; Not

#+clpython-fixnum-is-a-class
(defmethod py-not ((x fixnum))
  (py-bool (zerop x)))

(defmethod py-not ((x py-none))
  (declare (ignorable x))
  +the-true+)

(defmethod py-not ((x py-tuple))
  (py-bool (eq x *the-empty-tuple*)))
(defmethod py-not ((x list)) ;; a tuple
  (py-bool (null x)))

;;; String representation

(defmethod py-str ((x string)) x)
(defmethod py-str ((x vector)) (py-list.__str__ x))
(defmethod py-str ((x list))   (py-tuple.__str__ x))
#+clpython-fixnum-is-a-class
(defmethod py-str ((x fixnum)) (if (<= 0 x 100)
				   (svref (load-time-value
					   (coerce (loop for i from 0 to 100
						       collect (format nil "~D" i)) 'simple-vector))
					  x)
				 (format nil "~D" x)))

(defmethod py-repr ((x string))
  ;; A rough filter for now; this lets unicode characters outside ASCII through.
  (if (every #'alphanumericp x) (format nil "'~A'" x) (py-string.__repr__ x)))

(defmethod py-repr ((x vector)) (py-list.__repr__ x))
(defmethod py-repr ((x list))   (py-tuple.__repr__ x))
#+clpython-fixnum-is-a-class
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


;;; Function/object calling

(define-compiler-macro py-call (&whole whole prim &rest args)
  (with-perhaps-matching (prim ([attributeref-expr] ?x ([identifier-expr] ?attr-sym)))
    ;; Optimize "obj.attr(..args..)" = (py-call (py-attr obj attr) ..args..)
    ;; so the allocation of a bound method object is skipped.
    (let ((res `(let* ((.x ,?x)
                       (.val (class.attr-no-magic (py-class-of .x) ',?attr-sym)))
                  (if (and (functionp .val) (not (instance.attr-no-magic .x ',?attr-sym)))
                      (funcall .val .x ,@args)
                    (locally (declare (notinline py-call))
                      (py-call (attr .x ',?attr-sym) ,@args))))))
      #+(or)(warn "py-call cm res: ~A" res)
      (return-from py-call res)))
  whole)
