;;; Test suite

(in-package :python)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lexer

(defmacro do-asserts (&rest suite)
  `(progn
     ,@(loop for (eqlfun . cases) in suite
	   append (loop for (what expected) in cases
		      collect
			`(progn (format t "trying: ~S ...~%" ',what)
				(format t "  result: ~S   expected: ~S~%"
					,what ',expected)
				(assert (,eqlfun ,what ,expected)))))))

(defmacro with-io (string &body body)
  `(with-input-from-string (*standard-input* ,string)
     ,@body))

(defun lexer-test ()
  (do-asserts 
      (string=  ;; identifier
       ((with-io " x" (read-identifier #\A)) "A")
       ((with-io "x y" (read-identifier #\A)) "Ax")
       ((with-io "xd" (read-identifier #\A)) "Axd")
       ((with-io "123" (read-identifier #\_)) "_123")
       ((with-io "_foo__(sdf" (read-identifier #\_)) "__foo__")
       
       ;; string
       ((with-io "'" (read-string #\')) "") ; empty single quote
       ((with-io "'''''" (read-string #\')) "") ; three quotes
       ((with-io "''abcdefghijk''' " (read-string #\')) "abcdefghijk")
       ((with-io "\"\"xs\"\"\"" (read-string #\")) "xs")
       
       ((with-io "'" (read-string #\')) "") ; nonempty single quote
       ((with-io "abcd'" (read-string #\')) "abcd")
       ((with-io "abcdabcd'xyz" (read-string #\')) "abcdabcd"))
    
    (=  ;; number
     ((with-io "1 " (read-number #\2)) 21) ; decimal
     ((with-io " " (read-number #\2)) 2)
     ((with-io "" (read-number #\0)) 0)
     
     ((with-io "" (read-number #\2)) 2)
     ((with-io "45" (read-number #\2)) 245)
     ((with-io "4 5 6" (read-number #\2)) 24)
     ((with-io " 45" (read-number #\2)) 2)
     
     ((with-io "45a" (read-number #\2)) 245)
     ((with-io "45[" (read-number #\2)) 245)
     
     ((with-io "x " (read-number #\0)) 0) ; hexadecimal
     ((with-io "x0 " (read-number #\0)) 0)
     ((with-io "X0 " (read-number #\0)) 0)
     ((with-io "xf " (read-number #\0)) 15)
     ((with-io "Xaa" (read-number #\0)) 170)
     ((with-io "Xaax" (read-number #\0)) 170)
     ((with-io "x0ff " (read-number #\0)) 255)
     ((with-io "x123[" (read-number #\0)) 291)
      
     ((with-io "0" (read-number #\0)) 0) ; otcal
     ((with-io "0[" (read-number #\0)) 0) ; otcal
     ((with-io "123[" (read-number #\0)) 83)
     ((with-io "123 " (read-number #\0)) 83)
     ((with-io "7" (read-number #\0)) 7)
     
     ((with-io "." (read-number #\2)) 2.) ; decimal + fraction
     ((with-io ".3" (read-number #\1)) 1.3)
     ((with-io "0." (read-number #\1)) 10.)
     ((with-io "0.1" (read-number #\1)) 10.1)
     ((with-io "0.23" (read-number #\1)) 10.23)
     ((with-io "23." (read-number #\1)) 123.)
     ((with-io "23.45" (read-number #\1)) 123.45))
    
    (string=  ;; punctuation
     ((with-io "" (read-punctuation #\])) "]")
     ((with-io "a" (read-punctuation #\])) "]")
     ((with-io "]" (read-punctuation #\])) "]")
     ((with-io "=" (read-punctuation #\])) "]")
     ((with-io "]" (read-punctuation #\*)) "*")
     ((with-io "=[" (read-punctuation #\*)) "*=")
     ((with-io "===" (read-punctuation #\*)) "*=")
     ((with-io "*" (read-punctuation #\+)) "+")
     ((with-io "+" (read-punctuation #\+)) "+")
     ((with-io "= " (read-punctuation #\+)) "+=")
     ((with-io "asdf)" (read-punctuation #\()) "(")
     ((with-io "<== " (read-punctuation #\<)) "<<=")
     ((with-io "==" (read-punctuation #\!)) "!=")
     ((with-io "> 4" (read-punctuation #\<)) "<>")
     ((with-io "..x" (read-punctuation #\.)) "...")
     ((with-io "a" (read-punctuation #\.)) ".")
     ((with-io "a" (read-punctuation #\`)) "`")
     )))

(lexer-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser (AST)

(defmacro parser-test (data)
  `(progn ,@(loop for (string expected) in data
		collect `(progn
			   (format t "trying:  ~A ...~%" ,string)
			   (let ((actual (parse-python-string ,string)))
			     (assert (eq (car actual) 'file-input))
			     (setf actual (cadr actual))
			     (assert (equal actual ',expected) ()
			       "Parsing wrong for ~A:~%got: ~A~%wanted: ~A"
			       ,string actual ',expected))))))

(parser-test
 (("0" 0)
  ("3" 3)
  ("3+2" (binary + 3 2))
  ("a" (identifier a))
  ("a+b+c" (binary + (binary + (identifier a) (identifier b)) (identifier c)))
  ("a+b*c+d*e" (binary +
		       (binary + (identifier a) (binary * (identifier b) (identifier c)))
		       (binary * (identifier d) (identifier e))))
  ("x[0]" (trailers (identifier x) ((subscription ((0) nil)))))
  ("x[0,1]" (trailers (identifier x) ((subscription ((0 1) nil)))))
  ("x(0)" (trailers (identifier x) ((call (0)))))
  ("x(0,1)" (trailers (identifier x) ((call (0 1))))) 
  ("x(*args)" (trailers (identifier x) ((call ((* (identifier args)))))))
  ("x(**kw)" (trailers (identifier x) ((call ((** (identifier kw)))))))
  ("x(*args, **kwargs)" (trailers (identifier x)
				  ((call ((* (identifier args)) (** (identifier kwargs)))))))
  ("x(1,a=5)" (trailers (identifier x) ((call (1 (= (identifier a) 5))))))
  ("f(a,*a, **w)" (trailers (identifier f)
			    ((call ((identifier a) (* (identifier a)) (** (identifier w)))))))
  ;; dict
  ("{}" (dict nil))
  ("{1:3}" (dict ((1 . 3))))
  ("{1:3,}" (dict ((1 . 3))))
  ("{1:3, 2:4}" (dict ((1 . 3) (2 . 4))))

  ;; list
  ("[]" (list nil))
  ("[1]" (list (1)))
  ("[1,]" (list (1)))
  ("[1,2,3,4]" (list (1 2 3 4)))
  ("[1,2,3,4,]" (list (1 2 3 4)))
  
  ;; decimal numbers
  ("3.2" 3.2)
  
  ;; strings
  ("'x' 'y' 'z'" "xyz")
  
  ;; for-in
  ("for i in x: print i" (for-in ((identifier i) nil) (identifier x)
				 (suite (print ((identifier i)) nil)) nil))
  ("for i in x: print i, i," (for-in ((identifier i) nil) (identifier x)
				     (suite (print ((identifier i) (identifier i)) t)) nil))
 
  ))




;; End of parser test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro assert-raises (form exc)
  `(handler-case ,form
     (,exc () 'ok)
     (:no-error ()
	 (error "test failed!"))))

(defmacro assert-all (&rest clauses)
  `(progn ,@(loop for c in clauses
		collect `(assert ,c))
	  'ok))

(defmacro suppress-warnings (&rest body)
  `(handler-bind
       ((warning (lambda (c) 
		   (declare (ignore c))
		   (invoke-restart (find-restart 'muffle-warning)))))
    ,@body))


;; `None', `Ellipsis', `NotImplemented'
(assert-raises (make-instance 'py-none) TypeError)
(assert-raises (make-instance 'py-ellipsis) TypeError)
(assert-raises (make-instance 'py-notimplemented) TypeError)

;; Integer
(let ((i0 (make-int))
      (i0-2 (make-int 0))
      (i3 (make-int 3))
      (i-3 (make-int -3)))
  (assert-all
   ;; __eq__, __ne__
   (__eq__ 0 0)
   (__eq__ 12 12)
   (__eq__ i0 i0)
   (__eq__ 0 i0)
   (__eq__ i0 i0-2)
   (__ne__ 0 1)
   (__ne__ i0 1)
   ;; __lt__ etc
   (__lt__ 1 2)
   (__lt__ 1 (make-int 2))
   (__le__ 1 1)
   (__le__ 1 2)
   (__gt__ 2 1)
   (__ge__ 2 2)
   ;; abs
   (__eq__ (__abs__ i-3) i3)
   (__eq__ (__abs__ 3) (__abs__ -3))
   ;; designator
   (py-int-designator-p i0)
   (py-int-designator-p i-3)
   (py-int-designator-p 0)
   (py-int-designator-p 3)
   (not (py-int-designator-p "asdf"))
   (not (py-int-designator-p 'sadf))
   (not (py-int-designator-p 3.1))
   ;; __add__, __sub__, __mul__, __mod__, and their reverse
   (__eq__ 5 (__add__ 2 3))
   (__eq__ 5 (__radd__ 2 3))
   (__eq__ 5 (__add__ (make-int 2) 3))
   (__eq__ (make-int 5) (__add__ 2 (make-int 3)))
   (__eq__ (make-int 5) (__add__ (make-int 2) (make-int 3)))
   (__eq__ 15 (__mul__ 3 5))
   (__eq__ 15 (__rmul__ 5 3))
   (__eq__ 3 (__sub__ 4 (make-int 1)))
   (__eq__ 3 (__rsub__ 1 4))
   (__eq__ 1 (__mod__ 21 5))
   (__eq__ 0 (__mod__ 15 5))
   (__eq__ 1 (__rmod__ 5 21))
   (__eq__ 0 (__rmod__ 5 15))
   ;; and, or
   (__eq__ 0 (__and__ 1 2))
   (__eq__ 4 (__and__ 4 4))
   (__eq__ 3 (__or__ 1 2))
   (__eq__ 3 (__or__ 3 3))
   ;; pow
   (__eq__ 8 (__pow__ 2 3))
   (__eq__ 1 (__pow__ 2 3 7))
   ;; cmp
   (__eq__ (__cmp__ 3 3) 0)
   (__eq__ (__cmp__ 1 3) -1)
   (__eq__ (__cmp__ 3 1) 1)
   ;; div
   (__eq__ (__div__ 1 3) 0)
   (__eq__ (__div__ 2 3) 0)
   (__eq__ (__floordiv__ 1 3) 0)
   
   ;; float don't work right yet
   (__eq__ (__truediv__ 1 3) (make-float (/ 1 3)))
   
   ;; tuples
   (=  (__cmp__ (make-tuple 1 2 3) (make-tuple 1 2 3)) 0)
   (=  (__cmp__ (make-tuple 1 2 2) (make-tuple 1 2 3)) -1)
   ))



;; Test div/mod equivalence to Python results:
;;
;; These should return the same values:
;;  (floor x y) -- CL
;;  divmod(x,y) -- Python
;;
;; for x in (-3,-2,-1,1,2,3):
;;     for y in (-3,-2,-1,1,2,3):
;;         print divmod(x,y),
;;     print
;;
(let ((res ())
      (expected '((1 . 0) (1 . -1) (3 . 0) (-3 . 0) (-2 . 1) (-1 . 0)
		  (0 . -2) (1 . 0) (2 . 0) (-2 . 0) (-1 . 0) (-1 . 1)
		  (0 . -1) (0 . -1) (1 . 0) (-1 . 0) (-1 . 1) (-1 . 2)
		  (-1 . -2) (-1 . -1) (-1 . 0) (1 . 0) (0 . 1) (0 . 1)
		  (-1 . -1) (-1 . 0) (-2 . 0) (2 . 0) (1 . 0) (0 . 2)
		  (-1 . 0) (-2 . -1) (-3 . 0) (3 . 0) (1 . 1) (1 . 0))))
  (dolist (x '(-3 -2 -1 1 2 3))
    (dolist (y '(-3 -2 -1 1 2 3))
      (multiple-value-bind (div rem)
	  (floor x y)
	(push (cons div rem) res))))
  (setf res (nreverse res))
  (assert (equalp res expected) () 
    "Got~%~{~A ~A ~A ~A ~A ~A~^~%~}~@
    but wanted~%~{~A ~A ~A ~A ~A ~A~^~%~}" res expected)
  'ok)

;; string designators
(progn (assert (= (__hash__ 'as)
		  (__hash__ "as")
		  (__hash__ (make-string "as"))))
       'ok)

;; attributes of built-in classes
(suppress-warnings
 (assert-all
  (unbound-method-p
   (internal-get-attribute (find-class 'py-complex) '__abs__))
  (eq nil (internal-get-attribute (find-class 'py-complex) 'real))
  (= 1 (complex-real (make-complex #C(1 3))))
  (= 1 (internal-get-attribute (make-complex #C(1 3)) 'real))
  (bound-method-p (internal-get-attribute (make-complex #C(1 3)) '__abs__))
  (bound-method-p (internal-get-attribute (make-int 3) '__abs__))
  (eq nil (internal-get-attribute (find-class 'py-complex) '__len__))
 
  (eq nil (internal-get-attribute (make-dict) '__abs__)) ; dicts have no __abs__ meth
  (bound-method-p (internal-get-attribute (make-dict) '__len__))
  (bound-method-p (internal-get-attribute (make-dict) 'clear))))
 
(let ((d (make-dict '((key1 . val1)))))
  (__call__ (internal-get-attribute d 'clear))
  (assert (= 0 (__len__ d)))
  'ok)


;; user-defined classes
(suppress-warnings
 (values ;; make sure they all can be defined
  (make-python-class :name 'test1)
  (make-python-class :name 'test2 :slots '())
  (make-python-class :name 'test3 :slots '(slot1))
  (make-python-class :name 'test4 :slots '(slot1 slot2))
  (make-python-class :name 'test5 :slots '(__dict__))
  (make-python-class :name 'test6 :slots '(slot1 __dict__))))

;; setting attributes with __dict__
(suppress-warnings
 (let ((c (make-python-class :name 'test1)))
   (assert (typep c 'user-defined-class))
   (assert (typep c 'user-defined-object))
   (assert (= 0 (__len__ (slot-value c '__dict__))))
   (eq nil (internal-get-attribute c 'foo))))

(suppress-warnings
 (let* ((c (make-python-class :name 'test2))
        (x (make-instance c)))
   (internal-set-attribute x 'foo 'bar)
   (assert (= 1 (__len__ (internal-get-attribute x '__dict__))))
   (assert (eq (internal-get-attribute x 'foo) 'bar))))


;; setting attributes with __slots__
(suppress-warnings
 (let* ((c (make-python-class :name 'test3 :slots '(slot1)))
	(x (make-instance c)))
   (assert (not (slot-exists-p x '__dict__)))
   (assert (slot-exists-p x 'slot1))
   (internal-set-attribute x 'slot1 'bar)
   (assert (eq (internal-get-attribute x 'slot1) 'bar))
   'ok))

;; setting attributes with both __dict__ and __slots__
(suppress-warnings
 (let* ((c (make-python-class :name 'test4 :slots '(slot1 __dict__)))
	(x (make-instance c)))
   
   (assert (slot-exists-p x 'slot1))
   (internal-set-attribute x 'slot1 'bar)
   (assert (eq (slot-value x 'slot1) 'bar))
   (assert (eq (internal-get-attribute x 'slot1) 'bar))
   
   (assert (slot-exists-p x '__dict__))
   (internal-set-attribute x 'dict-ent 'baz)
   (let ((d (internal-get-attribute x '__dict__)))
     (assert (= (__len__ d) 1) () "len of ~A is: ~A" d (__len__ d)))
   (let ((a (internal-get-attribute x 'dict-ent)))
     (assert (eq a 'baz) () "att is ~A, not ~A" a 'baz))
   
   (assert (eq (internal-get-attribute x 'slot1) 'bar))
   'ok))

;; setting attributes on built-in classes, instances should fail
(progn
  (assert-raises (internal-set-attribute (find-class 'py-dict) 'foo 'bar)
		 TypeError)
  (assert-raises (internal-set-attribute (make-dict) 'foo 'bar)
		 TypeError)
  'ok)

;; built-in namespace
(progn
  (assert (= (__call__ (namespace-lookup *builtins* 'abs) '(-42) ())
	     42))
  'ok)


;; operations on lists
(let ((x (make-py-list 1 2 3)))
  (__setitem__ x 0 42)
  (assert (= (__getitem__ x 0) 42))
  'ok)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; py-eval examples

(progn
  (py-eval (parse-python-string "a + 2") '(("a" . 3))) ; -> 5
  (py-eval (parse-python-string "def foo(): return 42")) ; -> #<function foo>
  (py-eval (parse-python-string "def foo(): return 42~
                                 foo()")) ; -> 42
  (py-eval (parse-python-string "def foo(a): return a+3~
                                 foo(39)")) ; -> 42
  (py-eval (parse-python-string "a = 3,
                                 a")))	; -> 3

;;;; simple parsing

(assert-all
 (__eq__ (user-py-eval '(number . 42)) 42)
 (__eq__ (user-py-eval '(string . "hello world"))
	 "hello world")
 (py-== (user-py-eval '(list . (1 2 3)))
	(make-py-list-from-list '(1 2 3)))
 (py-== (make-tuple 1 2 3)
	(user-py-eval '(tuple . (1 2 3))))
 (py-== (user-py-eval '(binary + (number . 3) (number . 4)))
	7))

;;; defining, calling functions

#|
def bar():
  print 42
  return 42
bar()
|#
(assert (__eq__ (user-py-eval '(suite
				(funcdef bar (() () nil nil)
				 (suite
				  (print (number . 42))
				  (return (number . 42))))
				(call (identifier . bar) () () nil nil)))
		42))

#|
def bar():
  print 42
  return 13
bar()
|#
(assert (__eq__ (user-py-eval '(suite
				(funcdef bar (() () nil nil)
				 (suite
				  (print (number . 42))
				  (return (number . 13))))
				(call (identifier . bar) () () nil nil)))
		13))
#|
def bar(x):
  return x + 42
bar(3)
|#  
(assert (__eq__ (user-py-eval '(suite
				(funcdef bar ((x) () nil nil)
				 (return (binary + (identifier . x) (number . 42))))
				(call (identifier . bar) ((number . 3)) () nil nil)))
		(+ 3 42)))


#|
def bar(a):
  return a
bar(42)
|#
(assert (__eq__ (user-py-eval
		 '(suite
		   (funcdef bar ((a) () nil nil)
		    (return (identifier . a)))
		   (call (identifier . bar) (42) () nil nil)))
		42))

;; Default arguments
#|
def bar(a, c=3):
  return a + c
bar(2, 2) + bar(10) + bar(1,1) + bar(15)
|#
(assert (__eq__ (user-py-eval
		 '(suite
		   (funcdef bar ((a) ((c . 3)) nil nil)
		    (return (binary + (identifier . a) (identifier . c))))
		   (binary +
		    (call (identifier . bar) (2 2) () nil nil) ;; 39 3
		    (binary +
		     (call (identifier . bar) (10) () nil nil) ;; 22 20
		     (binary +
		      (call (identifier . bar) (1 1) () nil nil)
		      (call (identifier . bar) (15) () nil nil))))))
		(+ 2 2 10 3 1 1 15 3)))

;; *arg, **kwd
#|
def bar(a, c=3, *arg, **kwd
  print a, c, arg, kwd
bar(42, 17, 12, q=2)
|#
(assert (py-== (user-py-eval
		'(suite
		  (funcdef bar ((a) ((c . 3)) arg kwd)
		   (suite
		    ;;(print "a = " (identifier . a))
		    ;;(print "c = " (identifier . c))
		    ;;(print "arg = " (identifier . arg))
		    ;;(print "kwd = " (identifier . kwd))
		    (return (list
			     (identifier . a) (identifier . c) ;; 42 17
			     (identifier . arg) (identifier . kwd))))) ;; (12) ((q . 2))
		  (call (identifier . bar) (42 17 12) ((q . 2)) nil nil)))
	       (user-py-eval
		'(list 42 17 (list 12) (lisp (make-dict '((q . 2))))))))
  
#|
def outer(a):
  print 'outer: a=', a
  def inner():
    print 'inner: a=', a
  print 'outer: a=', a
  return inner
print 'before'
print outer(12)
print 'middle'
print outer(7)()
print 'after'
|#
#+(or) ;; check prints
(user-py-eval
 '(suite
   (funcdef outer ((a) () nil nil)
    (suite
     (print "outer: a = " (identifier . a))
     (funcdef inner (() () nil nil)
      (print "inner: a = " (identifier . a)))
     (print "outer: a = " (identifier . a))
     (return (identifier . inner))))
   (print "before")
   (identifier . outer)
   (call (identifier . outer) (12) () nil nil)
   (print "middle")
   (call
    (call (identifier . outer) (7) () nil nil)
    () () nil nil)
   (print "after"))) ;; 12 12  7 7 7


#|
def outer(a):
  def inner():
    print a, 33
  return inner
outer(12)()
|#  
#+(or)
(assert (py-== (user-py-eval
		'(suite
		  (funcdef outer ((a) () nil nil)
		   (suite
		    (funcdef inner (() () nil nil)
		     (suite
		      (return (binary + (identifier . a) 33))))
		    (return (identifier . inner))))
		  (call
		   (call (identifier . outer) (32) () nil nil)
		   () () nil nil)))
	       (+ 32 33)))

#|
def outer(a):
  def inner(b):
    print a, b
  return inner
outer(12)(42)
|#  
(assert (py-== (user-py-eval
		'(suite
		  (funcdef outer ((a) () nil nil)
		   (suite
		    (funcdef inner ((b) () nil nil)
		     (suite
		      (return (binary + (identifier . a) (identifier . b))))) ;; 12 42
		    (return (identifier . inner))))
		  (call
		   (call (identifier . outer) (12) () nil nil)
		   (42) () nil nil)))
	       (+ 12 42)))

;;; for x in y
#+(or) ;; check print
(user-py-eval 
 '(for-stmt ((identifier . a)) (tuple (list 1 2 3))
   (suite (print (identifier . a))) ;; print 1; print 2; print 3
   ()))


#|
sum_a = 0
sum_b = 0
for a,b in [(1,10), (2,20), (3,30)]:
  sum_a = sum_a + a
  sum_b = sum_b + b
sum_a * sum_b  =?= 360
|#
(assert (py-== (user-py-eval
		'(suite
		  (assign-expr ((identifier . sum_a)) (tuple (number . 0)))
		  (assign-expr ((identifier . sum_b)) (tuple (number . 0)))
		  (for-stmt ((identifier . a) (identifier . b))
		   (tuple (list (tuple 1 10) (tuple 2 20) (tuple 3 30)))
		   (suite
		    (assign-expr
		     ((identifier . sum_a))
		     (tuple (binary + (identifier . sum_a) (identifier . a))))
		    (assign-expr
		     ((identifier . sum_b))
		     (tuple (binary + (identifier . sum_b) (identifier . b)))))
		   ())
		  (binary * (identifier . sum_a) (identifier . sum_b))))
	       (* (+ 1 2 3) (+ 10 20 30))))
		
			  
#|
res= 0
for a,b in ((1,10) (2,20) (3,30)):
  res = a*b + res
res =?= (+ (* 1 10) (* 2 20) (* 3 30))
|#
#+(or)
(assert (py-== (user-py-eval 
		'(suite
		  (assign-expr ((identifier . res)) (tuple (number . 0)))
		  (for-stmt ((identifier . a) (identifier . b))
		   (tuple (list (tuple 1 10) (tuple 2 20) (tuple 3 30)))
		   (suite (assign-expr ((identifier . res))
			   (tuple (binary + 
					  (binary * (identifier . a) (identifier . b))
					  (identifier . res)))))
		   ())
		  (identifier . res)))
	       (+ (* 1 10) (* 2 20) (* 3 30))))

#|
res = 0
for a in ((1,10), (2,20), (3,30))
  res = res + a[0]
res =?= (+ 1 2)
|#
(assert (py-== (user-py-eval 
		'(suite
		  (assign-expr ((identifier . res)) (tuple (number . 0)))
		  (for-stmt ((identifier . a))
		   (tuple (list (tuple 1 10) (tuple 2 20) (tuple 3 30)))
		   (suite (assign-expr ((identifier . res))
			   (tuple (binary +
					  (identifier . res)
					  (subscription (identifier . a)
							(tuple (number . 0)))))))
		   ())
		  (identifier . res)))
	       (+ 1 2 3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; List comprehension

#+(or) ;; this one is only to check the macroexpansion is correct
(eval-listcompr-m-1
 (binary + (identifier . x) (identifier . y))
 ((for-in (identifier . x) (list (number . 1) (number . 2) (number . 3)
				 (number . 4) (number . 5)))
  (for-in (identifier . y) (tuple (number . 10) 
				  (number . 20)
				  (number . 30)))
  (for-in (identifier . a) (list (number . 3)))) ;; dummy to introduce other var
 ((list-if (comparison < (identifier . x) (identifier . a)))
  (list-if (comparison == (identifier . y) (number . 20)))))

#|
[ (x+y) for x in [1,2,3,4,5]
        for y in (10, 20, 30)
        for a in [3]
     if x < a
     if y == 20 ]
-> [21, 22]
|#

#+(or)
(assert 
    (py-== (make-py-list 21 22)
	   (user-py-eval
	    '(listcompr
	      (binary + (identifier . x) (identifier . y))
	      ((for-in (identifier . x) (list (number . 1) (number . 2) (number . 3)
					 (number . 4) (number . 5)))
	       (for-in (identifier . y) (tuple (number . 10) 
					 (number . 20)
					 (number . 30)))
	       (for-in (identifier . a) (list (number . 3)))) ;; dummy to introduce other var
	      ((list-if (comparison < (identifier . x) (identifier . a)))
	       (list-if (comparison == (identifier . y) (number . 20))))))))

;;;; setting variables

;; a = 42
(assert (py-== (user-py-eval
		'(suite
		  (assign-expr ((identifier . a)) (tuple (number . 42)))
		  (identifier . a)))
	      42))

;; a,b = 42, 3
(assert (py-== (user-py-eval
		'(suite
		  (assign-expr ((identifier . a) (identifier . b))
		   (tuple (number . 42) (number . 3)))
		  (binary + (identifier . a) (unary - (identifier . b)))))
	       (- 42 3)))

;; a = 42,3
(assert (py-== (user-py-eval
		'(suite
		  (assign-expr ((identifier . a)) (tuple (number . 42) (number . 3)))
		  (identifier . a)))
	      (make-tuple 42 3)))

;; a = ((t1,t2), (v1,v2))
(assert (py-== (user-py-eval
		'(suite
		  (assign-expr ((identifier . a))
		   (tuple (tuple "t1" "t2") (tuple "v1" "v2")))
		  (identifier . a)))
	       (make-tuple (make-tuple "t1" "t2")
			   (make-tuple "v1" "v2"))))

;; a,b = ((t1,t2), (v1,v2))
(assert (py-== (user-py-eval
		'(suite
		  (assign-expr ((identifier . a)(identifier . b))
		   (tuple (tuple "t1" "t2") (tuple "v1" "v2")))
		  (list (identifier . a) (identifier . b))))
	       (make-py-list (make-tuple "t1" "t2")
			     (make-tuple "v1" "v2"))))
	      

;; a,b = "ab"
(assert (py-== (user-py-eval
		'(suite
		  (assign-expr ((identifier . a)(identifier . b)) (tuple (string . "ab")))
		  (list
		   (binary + (identifier . a) (identifier . b))
		   (identifier . a))))
	       (make-py-list "ab" "a")))

;; (a,b) = (1,2)
(assert (py-== (user-py-eval
		'(suite
		  (assign-expr
		   ((tuple (identifier . a)(identifier . b))) 
		   (tuple (number . 1) (number . 2)))
		  (binary + (identifier . a) (identifier . b))))
	       3))

;; ((a1,a2), b) = ((1,2), 3)
(assert (py-== (user-py-eval
		'(suite
		  (assign-expr ((tuple
				 (tuple (identifier . a1) (identifier . a2))
				 (identifier . b)))
		   (tuple
		    (tuple (number . 1) (number . 2))
		    (number . 3))) ;; a1 = 1, a2 = 2, b = 3
		  
		  (binary + (identifier . a1) (identifier . b))))
	       (+ 1 3)))

;; [0,10,2,3,4][1] =?= 10
(assert (= (eval-subscription '(list 0 10 2 3 4) '(tuple 1))
	   10))

#|
x = [1,2,3,4]
x[1+0] = 42
x =?= [1, 42, 3, 4]
|#
(assert (py-== (user-py-eval
		'(suite
		  (assign-expr ((identifier . x)) (tuple (list 1 2 3 4)))
		  (assign-expr
		   ((subscription
		     (identifier . x)
		     (tuple (binary + (number . 1) (number . 0)))))
		   (tuple (number . 42)))
		  (list
		   (subscription (identifier . x) (tuple (number . 1)))
		   (identifier . x))))
	       (make-py-list 42
			     (make-py-list 1 42 3 4))))

#|
x = [0,1,2,3,4]
x[ x[1] ] = 42
x
==> [0,42,2,3,4]
|#
(assert (py-== (user-py-eval
		'(suite
		  (assign-expr ((identifier . x)) (tuple (list 0 1 2 3 4)))
		  (assign-expr
		   ((subscription
		     (identifier . x)
		     (tuple (subscription
			     (identifier . x)
			     (tuple (number . 1))))))
		   (tuple (number . 42)))
		  (subscription (identifier . x) (tuple (number . 1)))))
	      42))

#|
class C():
  pass
C().__class__ =?= C
|#


(defun crash ()
  ;; (trace py-eval eval-attributeref eval-comparison pyb:cmp eval-call __call__)
  (format t "start code..~%")
  (flet ((crash-code () 
	   (user-py-eval '(suite
			   (classdef C ()
			    (suite (pass)))
			   (comparison ==
			    (attributeref
			     (call (identifier . C) () () nil nil)
			     (identifier . __class__))
			    (identifier . C))))))
    (crash-code)))

#+(or) ;; removes no-app-meth
(defun crash ()
  (trace py-eval eval-attributeref eval-comparison pyb:cmp eval-call __call__)
  (format t "start code..~%")
  (flet ((crash-code () 
	   (user-py-eval '(suite
			   (classdef C ()
			    (suite (pass)))
			   (comparison ==
			    (attributeref
			     (call (identifier . C) () () nil nil)
			     (identifier . __class__))
			    (identifier . C))))))
    (crash-code)
    (format t "~%~%~%--- now for the second time: ---~%~%~%")
    (crash-code))
  (format t "~%end code...~%"))


#+(or) ;; Strange: `no-applicable-method' gets deleted without reason
(defun crash ()
  
  ;; When this tracing is disabled, it works fine
  (trace py-eval eval-attributeref eval-comparison pyb:cmp eval-call __call__)
  
  (format t "start code..~%")
  (flet ((crash-code () 
	   (format t "### 1~%")
	   (handler-case 
	       (__cmp__ (list 1) (list 2))
	     (TypeError ()
	       (format t "ok: TypeError~%")))
	   (format t "### 2~%")
	   
	   (user-py-eval '(suite
			   (classdef C ()
			    ())
			   (lisp
			    ;;(format t "~%### inside lisp~%")
			    ;;(describe (find-class 'C))
			    ;;(format t "~%### creating instance:~%")
			    ;;(trace make-instance)
			    (make-instance 'C)
			    )))

	   (format t "### 3~%")
	   (handler-case 
		     (__cmp__ (list 1) (list 2))
		   (TypeError ()
		     (format t "ok: TypeError~%")))
	   (format t "### 4~%")))
    (crash-code)
    (format t "~%end code...~%")))


#+(or) ;; no tracing: this works fine (2 times TypeError)
(defun crash ()
  
  (format t "start code..~%")
  (flet ((crash-code () 
	   (format t "### 1~%")
	   (handler-case 
	       (__cmp__ (list 1) (list 2))
	     (TypeError ()
	       (format t "ok: TypeError~%")))
	   (format t "### 2~%")
	   
	   (user-py-eval '(suite
			   (classdef C ()
			    ())
			   (lisp
			    (make-instance 'C)
			    )))

	   (format t "### 3~%")
	   (handler-case 
		     (__cmp__ (list 1) (list 2))
		   (TypeError ()
		     (format t "ok: TypeError~%")))
	   (format t "### 4~%")))
    (crash-code)
    (format t "~%end code...~%")))


#+(or) ;; just tracing make-instance: this one removes the (no-applicable-method (eql #'__cmp))
(defun crash ()
  
  (format t "start code..~%")
  (flet ((crash-code () 
	   (format t "### 1~%")
	   (handler-case 
	       (__cmp__ (list 1) (list 2))
	     (TypeError ()
	       (format t "ok: TypeError~%")))
	   (format t "### 2~%")
	   
	   (user-py-eval '(suite
			   (classdef C ()
			    ())
			   (lisp
			    ;;(format t "~%### inside lisp~%")
			    ;;(describe (find-class 'C))
			    ;;(format t "~%### creating instance:~%")
			    (trace make-instance)
			    (make-instance 'C)
			    )))

	   (format t "### 3~%")
	   (handler-case 
		     (__cmp__ (list 1) (list 2))
		   (TypeError ()
		     (format t "ok: TypeError~%")))
	   (format t "### 4~%")))
    (crash-code)
    (format t "~%end code...~%")))


#+(or) ;; Without tracing make-instance: works fine (2 times TypeError)
(defun crash ()
  
  (format t "start code..~%")
  (flet ((crash-code () 
	   (format t "### 1~%")
	   (handler-case 
	       (__cmp__ (list 1) (list 2))
	     (TypeError ()
	       (format t "ok: TypeError~%")))
	   (format t "### 2~%")
	   
	   (user-py-eval '(suite
			   (classdef C ()
			    ())
			   (lisp
			    ;;(format t "~%### inside lisp~%")
			    ;;(describe (find-class 'C))
			    ;;(format t "~%### creating instance:~%")
			    ;;(trace make-instance)
			    (make-instance 'C)
			    )))

	   (format t "### 3~%")
	   (handler-case 
		     (__cmp__ (list 1) (list 2))
		   (TypeError ()
		     (format t "ok: TypeError~%")))
	   (format t "### 4~%")))
    (crash-code)
    (format t "~%end code...~%")))


#+(or)
(user-py-eval
 '(suite
   (classdef C ()
    (suite (pass)))
   (identifier . C)))

#+(or)
(user-py-eval
 '(suite
   (classdef C ()
    (suite (pass)))
   (attributeref
    (call (identifier . C) () () nil nil)
    (identifier . __class__))))



#|
class C:
  def foo(self):
    print "foo", self
c = C()
c.foo()    
|#
#+(or)
(user-py-eval
 '(suite
   (classdef C ()
    (funcdef foo ((self) () nil nil)
     (print "foo" (identifier . self))))
   (assign-expr
    ((identifier . c))
    (tuple (call (identifier . C) () () nil nil)))
   (call
    (attributeref 
     (identifier . c)
     (identifier . foo))
    () () nil nil))) ;; "foo #<C @ ..>"


#|
class C:
  def foo(self, a):
    return self.bar(a)
  def bar(self, x):
    return x + 1
c = C()
print c.foo(41)
|#
#+(or)
(user-py-eval
 '(suite
   
   (classdef C ()
    (suite
     (funcdef foo ((self a) () nil nil)
      (return (call (attributeref (identifier . self)
				  (identifier . bar))
		    ((identifier . a)) () nil nil)))
     (funcdef bar ((self x) () nil nil)
      (return (binary + (identifier . x) 1)))))
   
   (assign-expr
    ((identifier . c))
    (tuple (call (identifier . C) () () nil nil)))
   
   (print (call (attributeref
		 (identifier . c)
		 (identifier . foo))
	   (41) () nil nil)))) ;; 42

#|
class C:
  def foo(self):
    return 42
class D(C):
  pass    
d = D()
print d.foo()
|#
#+(or) ;; inheritance of class method
(user-py-eval
 '(suite
   
   (classdef C ()
    (suite
     (funcdef foo ((self) () nil nil)
      (return 42))))
   
   (classdef D (tuple (identifier . C))
    (suite (pass)))

   (assign-expr
    ((identifier . d))
    (tuple (call (identifier . D) () () nil nil)))
   
   (assign-expr
    ((identifier . meth))
    (tuple (attributeref (identifier . d) (identifier . foo))))
   
   (call (identifier . meth) () () nil nil)))


#|
class C:
  foo = 42
class D(C):
  pass    
d = D()
print d.foo
|#
#+(or) ;; inhericante of class attribute
(user-py-eval
 '(suite
   
   (classdef C ()
    (suite
     (assign-expr ((identifier . foo)) (tuple (number . 42)))))
   
   (print (attributeref (identifier . C) (identifier . foo)))
   
   (classdef D (tuple (identifier . C))
    (suite (pass)))
   
   (print (attributeref (identifier . D) (identifier . foo)))
   
   (assign-expr
    ((identifier . d))
    (tuple (call (identifier . D) () () nil nil)))
   
   (print (attributeref (identifier . d) (identifier . foo))))) ;; 42 42 42




#|
c = 3 + 4j
print c.real, c.imag
tp = type(c)
print type.real
|#
#+(or) ;; XXX complex numbers are not supported in the parser yet
(user-py-eval
 '(suite
   (assign-expr ((identifier . c)) (tuple (number . #C(3 4))))
   (print
    (attributeref (identifier . c) (identifier . real))
    (attributeref (identifier . c) (identifier . imag)))
   (assign-expr
    ((identifier . tp))
    (tuple (call (identifier . type) ((identifier . c)) () nil nil)))
   (print
    (identifier . tp)
    (attributeref (identifier . tp) (identifier . real)))))


#|
class C:
  foo = 42
class D(C):
  pass    
d = D()
print d.foo
d.foo = 13
print d.foo
|#
#+(or) ;; inhericante of class attribute
(user-py-eval
 '(suite
   
   (classdef C ()
    (suite
     (assign-expr ((identifier . foo)) (tuple (number . 42)))))
   
   (classdef D (tuple (identifier . C))
    (suite (pass)))
   
   (assign-expr
    ((identifier . d))
    (tuple (call (identifier . D) () () nil nil)))
   
   (print (attributeref (identifier . d) (identifier . foo))))) ;; 42 42 42

