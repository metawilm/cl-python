(in-package :python)

;; Generator function rewriting


;; (defconstant *StopIteration* '|*stop-iteration*|)

(defun strings->symbol (&rest strings)
  (make-symbol (apply #'concatenate 'string strings)))

(defun form->symbol (form &optional (prefix ""))
  (make-symbol (format nil "~A ~A" prefix form)))

(defun apply-splits (suite)
  (let ((res ())
	(found-split nil))
    (dolist (x suite)
      (if (atom x)
	  (push x res)
	(if (eq (car x) :split)
	    (progn (setf found-split t)
		   (dolist (y (cdr x))
		     (push y res)))
	  (push (apply-splits x) res))))
    (setf res (nreverse res))
    (if found-split
	(apply-splits res)
      res)))

(defun rewrite-yield (form env)
  (let ((counters (copy-tree '((:yield . 0) (:if . 0) (:for . 0) (:while . 0))))
	(var-list ()))
    (labels ((new-tag (kind) (let ((res (assoc kind counters)))
			       (incf (cdr res))))
	     (new-strtag (kind) (format nil "~A~A" kind (new-tag kind))))
      (values 
       (labels
	   ((walk (form stack env)
	      (excl::walk-form
	       form
	       env
	       (lambda (form context env)
		 (declare (ignore context))
		 ;; (format t "form: ~A~%" form)
		 (cond 
		  ((consp form)
		   (cond ((eq (car form) 'suite)
			  ;;(setf (car form) ':split)
			  (cons ':split (cdr form)))
			 ;;form)
			 
			 ((eq (car form) 'yield)
			  (let ((tag (new-tag :yield)))
			    `(:split (ret ,(cadr form) ,tag) ,tag)))
			 
			 ((eq (car form) 'if)
			  (destructuring-bind
			      (if-test then-part else-part) (cdr form)
			    (let* ((tag (new-strtag :if))
				   (then-tag (strings->symbol tag "-then"))
				   (else-tag (strings->symbol tag "-else"))
				   (after-tag (strings->symbol tag "-after")))
			      (let* ((wthen `(:split ,(walk then-part stack env)));; (go ,after-tag)))
				     (welse `(:split ,(walk else-part stack env))));; (go ,after-tag))))
				(setf form
				  `(:split ,(form->symbol form tag)
					   (if (py-if ,if-test) (go ,then-tag) (go ,else-tag))
					   ,then-tag ,wthen (go ,after-tag)
					   ,else-tag ,welse
					   ,after-tag))
				(values form t)))))
			 
			 
			 ((eq (car form) 'for-else)
			  (destructuring-bind 
			      (var list suite else-suite) (cdr form)
			    (let* ((tag (new-strtag :for))
				   (repeat-tag (strings->symbol tag "-repeat"))
				   (else-tag (strings->symbol tag "-else"))
				   (end-tag (strings->symbol tag "-end/break-target"))
				   (continue-tag (strings->symbol tag "-continue-target"))
				   (fvar (strings->symbol tag "-generator"))
				   (fval (strings->symbol tag "-loop-var")))
			      (push fvar var-list)
			      (push fval var-list)
			      (values
			       `(:split ,(form->symbol form tag)
					(setf ,fvar (iter ,list))
					(setf ,fval (generator-next ,fvar))
					(when (eq ,fval *StopIteration*)
					  (go ,else-tag))
					,repeat-tag
					(assign ,var ,fval)
					,(walk suite (cons (cons continue-tag end-tag) stack) env)
					,continue-tag
					(setf ,fval (generator-next ,fvar))
					(if (eq ,fval *StopIteration*)
					    (go ,end-tag)
					  (go ,repeat-tag))
					,else-tag
					,(walk else-suite stack env)
					,end-tag
					(setf ,fvar nil
					      ,fval nil))
			       t))))
			 
			 ((eq (car form) 'while-else)
			  (destructuring-bind
			      (condition suite else-suite) (cdr form)
			    (let* ((tag (new-strtag :while))
				   (repeat-tag (strings->symbol tag "-repeat"))
				   (else-tag (strings->symbol tag "-else"))
				   (continue-tag (strings->symbol tag "-continue-target"))
				   (end-tag (strings->symbol tag "-end/break-target")))
			      (values
			      `(:split
				,(form->symbol form tag)
				(if (py-if ,condition) (go ,repeat-tag) (go ,else-tag))
				,repeat-tag
				,(walk suite (cons (cons continue-tag end-tag) stack) env)
				,continue-tag
				(if (py-if ,condition) (go ,repeat-tag) (go ,end-tag))
				,else-tag
				,(walk else-suite stack env)
				,end-tag)
			      t))))
			  
			 ((eq (car form) 'continue)
			  (let ((k (car stack)))
			    (unless k
			      (error "CONTINUE outside loop"))
			    (let ((repeat-tag (car k)))
			      (values `(:split
					,(form->symbol form)
					(go ,repeat-tag))
				      t))))
			 
			 ((eq (car form) 'break)
			  (let ((k (car stack)))
			    (unless k
			      (error "BREAK outside loop"))
			    (let ((end-tag (cdr k)))
			      (values `(:split
					,(form->symbol form)
					(go ,end-tag))
				      t))))
			 
			 ((eq (car form) 'return)
			  ;; The generator will return no more
			  ;; values. (This can be used at another than
			  ;; the top-level of the function, like in an
			  ;; IF.
			  (when (cdr form)
			    (error "inside generator, RETURN may have no argument (got: ~A)"
				   form))
			  (let ((tag (new-tag :yield)))
			    (values
			     `(:split ,tag
				      (ret *StopIteration* ,tag))
			     t)))
			 
			 (t form)))
		  
		  (t form))))))
	 
	 (let ((walked (walk form () env))
	       (final-tag (new-tag :yield)))
	   
	   `(lambda ()
	      (let ((state 0)
		    ,@(nreverse var-list))
		(lambda ()
		  (block inside
		    (flet
			((ret (val new-state)
			   (setf state new-state)
			   (return-from inside val)))
		      (tagbody
			(case state
			  ,@(loop for i from 0 to (cdr (assoc :yield counters))
				collect `(,i (go ,i))))
		       0
			,walked
			,final-tag
			(ret *StopIteration* ,final-tag)))))))))))))

(defmacro def-generator (code)
  `(apply-splits (rewrite-yield ',code nil)))

(let* ((make-generator (def-generator (suite
				       (yield 1)
				       (yield 2))))
       (generator (funcall make-generator))
       (val (funcall generator))
       (val2 (funcall generator))
       (val3 (funcall generator)))
  (values 
   make-generator
   generator
   val
   val2
   val3))

;; new:
(let ((c (rewrite-yield
	  '(suite
	    ((assign-expr (testlist ((identifier x)) nil) (testlist (0) nil))
	     (while (comparison < (identifier x) (identifier i))
	       (suite
		((yield (testlist ((identifier x)) nil))
		 (assign-expr (testlist ((identifier x)) nil)
			      (testlist ((binary + (identifier x) 1)) nil))))
	       nil)))
	  
	  nil)))
  (apply-splits c))


#+(or)
(let ((c (rewrite-yield
	  '(suite
	    (foo------------------------------------------------)
	    (for-else x some-list (do-something with x) (suite))
	    (foo-10------------------------------------------------)
	    (for-else x some-list 
	     (suite
	      (bar)
	      (yield (+ x 2))
	      (baz))
	     (suite))
	    (foo-15-----------------------------------------------)
	    (for-else x somelist
	     (suite (thenclause))
	     (suite (else-if-nothing-in-list-clause)))
	    (foo-20-----------------------------------------------)
	    (if (test) 
		(suite (foo) (yield x) (bar) (yield x2))
	      (suite (else part) (yield a) (yield b)))
	    (foo-30-----------------------------------------------)
	    (if (foo)
		(suite (return))
	      (suite))
	    (foo-40-----------------------------------------------)
	    (if (test)
		(suite
		 (a)
		 (if (test2)
		     (suite (b)
			    (yield a)
			    (c))
		   (suite
		    (yield b1)
		    (yield b2))))
	      (suite
	       (x)
	       (yield c)))
	    (foo-50-----------------------------------------------)
	    (for-else x somelist
	     (suite
	      (aaa)
	      (if (test)
		  (suite (break))
		(suite))
	      (bbb))
	     (suite))
	    (foo-55-----------------------------------------------)
	    (for-else x somelist
	     (suite
	      (aaa)
	      (if (test)
		  (suite (continue))
		(suite))
	      (bbb))
	     (suite))
	    (foo-60-----------------------------------------------)
	    (for-else x somelist
	     (suite
	      (aaa)
	      (if (test)
		  (suite (break))
		(suite))
	      (bbb))
	     (suite
	      (xxx)))
	    (foo-65-----------------------------------------------)
	    (for-else x somelist
	     (suite
	      (aaa)
	      (if (test)
		  (suite (yield a)
			 (continue)
			 (yield never-comes-here))
		(suite
		 (yield x)
		 (b)))
	      (bbb))
	     (suite
	      (xxx)
	      (yield x)
	      (yyy)))
	    (foo-70-----------------------------------------------)
	    (while-else (test)
	     (suite
	      (do-a)
	      (do-b))
	     (suite
	      (else-a)
	      (else-b)))
	    (foo-75-----------------------------------------------)
	    (while-else (test)
	     (suite
	      (do-a)
	      (if (test)
		  (suite (break))
		(suite))
	      (do-b))
	     (suite
	      (else-a)))
	    (foo-75-----------------------------------------------)
	    (while-else (test)
	     (suite
	      (do-a)
	      (if (test2)
		  (suite (continue))
		(suite))
	      (do-b))
	     (suite
	      (else-a)))
	    (foo-80-----------------------------------------------)
	    (for-else x outer-list
	     (suite
	      (aaa)
	      (if (t1)
		  (suite (break))
		(suite (continue)))
	      (aaa)
	      (for-else x inner-list
	       (suite
		(bbb)
		(if (t2)
		    (suite (break))
		  (suite (continue))))
	       (suite
		(if (t3)
		    (suite (break))
		  (suite (continue))))))
	     (suite))
	    )
	  nil)))
  (apply-splits c))




   
#+(or)
(values
 (apply-splits '(suite a b (:split aa bb) c d))
 (apply-splits '(a (b (:split c d))))
 (apply-splits '(a (:split b c)))
 (apply-splits '(a (:split (b) (c))))
 (apply-splits '(progn (:split
			(:split
			 (lisp-if (py-if (test))
			  (:split
			   (:split (foo) (:split (ret x 3) 3) (bar) (:split (ret x2 4) 4)))
			  (go 1))
			 1
			 (:split
			  (:split (else part) (:split (ret a 5) 5) (:split (ret b 6) 6))
			  (go 2))
			 2)))))

#+(or) ;; only do FOR-ELSE
((eq (car form) 'for-in)
 (destructuring-bind 
     (var list suite) (cdr form)
   (let* ((tag (new-strtag :for))
	  (repeat-tag (strings->symbol tag "-repeat"))
	  (end-tag (strings->symbol tag "-end"))
	  (fvar (strings->symbol tag "-generator"))
	  (fval (strings->symbol tag "-next")))
     (push fvar var-list)
     (push fval var-list)
     (values
      `(:split ,(form->symbol form tag)
	       (setf ,fvar (iter ,list))
	       ,repeat-tag
	       (setf ,fval (generator-next ,fvar))
	       (when (eq ,fval *StopIteration*)
		 (go ,end-tag))
	       (assign ,var ,fval)
	       ,(walk suite (cons (cons repeat-tag end-tag) stack) env)
	       (go ,repeat-tag)
	       ,end-tag
	       (setf ,fvar nil
		     ,fval nil))
      t))))
			 