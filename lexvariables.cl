
#+(or)
(defparameter py 
    '(def foo ("a" "b" ("c" . 3))
      (if (> a zz)
	  (progn (assign "a" "b" 3 4)
		 (return (+ "a" "b")))
	(if (== "a" "b")
	    (progn (assign "d" (- "a" "b"))
		   (return "d"))))
      (assign "c" 5)
      (return (+ "c" (+ "a" 42)))))


;; The naive approach: look up every var all the time

#+(or)
(lambda (a b c :key (locals (make-hash-table :test #'string=)
			    (globals (make-hash-table :test #'string=))))

  ;; "a", "b" and "c" can only be locals in this scope:
  ;; a `global' declaration for them is forbidden.

  (ns-set "a" a locals)
  (ns-set "b" b locals)
  (ns-set "c" c locals)
  
  (if (oper>
       (ns-get "a" locals globals)
       (ns-get "zz" locals globals))
      (progn (ns-assign "a" "b" 3 4 locals globals)
	     (return (oper+ 
		      (ns-get "a" locals globals)
		      (ns-get "b" locals globals))))
    (if (oper== (ns-get "a" locals globals)
		(ns-get "b" locals globals))
	(progn (ns-set "d"
		       (oper- (ns-get "a" locals globals)
			      (ns-get "b" locals globals)))
	       (return (ns-get "d")))))
  (ns-assign "c" 5 locals globals)
  (return (oper+
	   (ns-get "c" locals globals)
	   (oper+
	    (ns-get "a" locals globals)
	    42))))


;; A bit smarter approach: the names of the parameters are known beforehand.

#+(or)
(lambda (a b c :key (locals (make-hash-table :test #'string=)
			    (globals (make-hash-table :test #'string=))))

  ;; "a", "b" and "c" can only be locals in this scope:
  ;; a `global' declaration for them is forbidden.

  (if (oper>
       a
       (ns-get "zz" locals globals))
      (progn (ns-assign "a" "b" 3 4 locals globals)
	     (return (oper+ 
		      (ns-get "a" locals globals)
		      (ns-get "b" locals globals))))
    (if (oper== (ns-get "a" locals globals)
		(ns-get "b" locals globals))
	(progn (ns-set "d"
		       (oper- (ns-get "a" locals globals)
			      (ns-get "b" locals globals)))
	       (return (ns-get "d")))))
  (ns-assign "c" 5 locals globals)
  (return (oper+
	   (ns-get "c" locals globals)
	   (oper+
	    (ns-get "a" locals globals)
	    42))))


#+(or)
(make-python-function :name 'foo
		      :params '(a b (c . 10))
		      :func (lambda (a b c)
			      (if (> a b)
				  (progn (assign "a" "b" 3 4)
					 (return (+ a b)))
				(if (= a b)
				    (return (- a b))))
			      (assign "c" 5)
			      (return (+ c (+ a 42)))))

