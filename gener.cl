(in-package :python)

(defun funcdef-is-generator-p (ast)
  (assert (eq (car ast) 'funcdef-stmt))
  (destructuring-bind (decorators fname (pos-args key-args *-arg **-arg) suite)
      (cdr ast)
    (declare (ignore decorators fname pos-args key-args *-arg **-arg))
    (generator-ast-p suite)))
   
(defun generator-ast-p (ast)
  "Is AST a function definition for a generator?"
  
  (assert (not (eq (car ast) 'module-stmt)) ()
    "GENERATOR-AST-P called with a MODULE ast.")
  
  (catch 'is-generator
    (walk-py-ast ast 
		 (lambda (x &key value target)
		   (declare (ignore value target))
		   (case (car x)
		     
		     (yield-stmt (throw 'is-generator t))
		     
		     ;; don't look for 'yield' in inner functions and classes
		     ((classdef-stmt funcdef-stmt) (values nil t))
		     
		     (t x))))
    nil))

#+(or) ;; unused
(defun ast-has-yield-p (ast)
  "yield-stmt of return-stmt in AST?"
  (catch 'result
    (walk-py-ast ast 
		 (lambda (x &key value target)
		   (declare (ignore value target))
		   (case (car x)
		     
		     ((yield-stmt return-stmt) (throw 'result t))
		     
		     ;; don't look for 'yield' in inner functions and classes
		     ((classdef funcdef) (values nil t))
		     
		     (t x))))
    nil))


(defun suite->generator (fname suite)
  ;; Lisp generator function that returns one of:
  ;;  VAL              -- value explicitly `yield'-ed
  ;;  :implicit-return -- nore more statements in the function body
  ;;  :explicit-return -- explicit return from function
  (assert (eq (car suite) 'suite-stmt))
  (assert (generator-ast-p suite))
  
  `(funcdef-stmt
    nil (identifier-expr ,fname) (nil nil nil nil)
    (suite-stmt
     ,@(mapcar (lambda (x)
		 (walk-py-ast x
			      (lambda (form &rest context)
				(declare (ignore context))
				(case (car form)
				     
				  ((funcdef-stmt classdef-stmt)
				   (values form t))
				     
				  (return-stmt 
				   (when (second form)
				     (error "SyntaxError: Inside generator, RETURN ~
                                                statement may not have an argument ~
                                                (got: ~S)" form))
				   (values `(return-from :function-body :explicit-return)
					   t))
				     
				  (t form)))))
	       (second suite))
     (return-from :function-body :implicit-return))))

(defun rewrite-generator-expr-ast (ast)
  ;; rewrite:  (x*y for x in bar if y)
  ;; into:     def f(src):  for x in src:  if y:  yield x*y
  ;;           f(bar)
  ;; values: (FUNCDEF ...)  bar
  (assert (eq (car ast) 'generator-expr))
  (destructuring-bind (item for-in/if-clauses) (cdr ast)
  
    (let ((first-for (pop for-in/if-clauses))
	  (func-name '#:generator-expr-helper)
	  (first-source '#:first-source))
      
      (assert (eq (car first-for) 'gen-for-in))
      
      (let ((stuff (loop with res = `(yield-stmt ,item)
		       for clause in (reverse for-in/if-clauses)
		       do (setf res
			    (ecase (car clause)
			      (gen-for-in `(for-in-stmt
					    ,(second clause) ,(third clause) ,res nil))
			      (gen-if     `(if-stmt ((,(second clause) ,res)) nil))))
		       finally (return res))))
	
	(values (rewrite-generator-funcdef-ast-w/o-params
		 `(funcdef-stmt nil ,func-name
				(((identifier-expr ,first-source)) nil nil nil)
				(suite-stmt
				 ((for-in-stmt ,(second first-for) ,first-source
					       ,stuff nil))))
		 :gen-maker-lambda-args `(,first-source))
		
		(third first-for))))))

(defun rewrite-generator-funcdef-ast (ast)
  (assert (eq (car ast) 'funcdef-stmt))
  (assert (funcdef-is-generator-p ast))
  (destructuring-bind
      (decorators fname args suite) (cdr ast)
    (declare (ignore decorators fname suite))
    (if (some #'identity args)
	(rewrite-generator-funcdef-ast-w/params ast)
      (rewrite-generator-funcdef-ast-w/o-params ast))))

(defun rewrite-generator-funcdef-ast-w/params (ast)
  
  ;; Rewrite so that the generator takes no arguments:
  ;;
  ;;   def f(a,b):         def f(a,b):
  ;;     ..yield..   ==>     def g():
  ;;                           ..yield..
  ;;                         return g()
  ;; 
  ;; XXX this is maybe wrong: when assignment to formal arg (here: a
  ;; or b) happens in ..yield..
  
  (assert (eq (car ast) 'funcdef-stmt))
  (assert (funcdef-is-generator-p ast))

  (destructuring-bind
      (decorators fname args suite) (cdr ast)
    (declare (ignore suite))
    
    (assert (some #'identity args))
    
    `(funcdef ,decorators ,fname ,args
	      (suite-stmt (funcall ,(rewrite-generator-funcdef-ast-w/o-params ast))))))
  

(defun rewrite-generator-funcdef-ast-w/o-params (ast &key (gen-maker-lambda-args nil))
  (assert (eq (car ast) 'funcdef-stmt))
  (assert (funcdef-is-generator-p ast))

  #+(or)
  (destructuring-bind
      (decorators fname args suite) (cdr ast)
    (declare (ignore decorators fname suite))
    (assert (not (some #'identity args))))
  
  (let ((yield-counter 0)
	(other-counter 0)
	(vars ()))
    
    (flet ((new-tag (kind)
	     (if (eq kind :yield)
		 (incf yield-counter)
	       (make-symbol (format nil "~A~A" kind (incf other-counter))))))
      
      (labels
	  ((walk (form stack)
	     (walk-py-ast
	      form
	      (lambda (form &rest context)
		(declare (ignore context))
		(case (first form)
		  
		  (yield-stmt
		   (let ((tag (new-tag :yield)))
		     (values `(:split (setf .state. ,tag)
				      (return-from :function-body ,(second form)) 
				      ,tag)
			     t)))
		  
		  (while-stmt
		   (destructuring-bind (test suite else-suite) (cdr form)
		     (let ((repeat-tag (new-tag :repeat))
			   (else-tag   (new-tag :else))
			   (after-tag  (new-tag :end+break-target)))
		       (values `(:split
				 (unless (py-val->lisp-bool ,test)
				   (go ,else-tag))

				 ,repeat-tag
				 (:split
				  ,(walk suite
					 (cons (cons repeat-tag after-tag)
					       stack)))
				 (if (py-val->lisp-bool ,test)
				     (go ,repeat-tag)
				   (go ,after-tag))
				 
				 ,else-tag
				 ,@(when else-suite
				     `((:split ,(walk else-suite stack))))
				 
				 ,after-tag)
			       t))))
		  
		  (if-stmt
		   (if (not (generator-ast-p form))
		       (values form t)
		     (destructuring-bind (clauses else-suite) (cdr form)
		       (loop
			   with else-tag = (new-tag :else) and after-tag = (new-tag :after)
									   
			   for (expr suite) in clauses
			   for then-tag = (new-tag :then)
					  
			   collect `((py-val->lisp-bool ,expr) (go ,then-tag)) into tests
			   collect `(:split ,then-tag
					    (:split ,(walk suite stack))
					    (go ,after-tag)) into suites
			   finally
			     (return
			       (values `(:split (cond ,@tests
						      (t (go ,else-tag)))
						(:split ,@suites)
						,else-tag
						,@(when else-suite
						    `((:split ,(walk else-suite stack))))
						,after-tag)
				       t))))))
		  
		  (for-in-stmt
		   (destructuring-bind (target source suite else-suite) (cdr form)
		     (let* ((repeat-tag (new-tag :repeat))
			    (else-tag   (new-tag :else))
			    (end-tag    (new-tag :end+break-target))
			    (continue-tag (new-tag :continue-target))
			    (generator  (new-tag :generator))
			    (loop-var   (new-tag :loop-var))
			    (stack2     (cons (cons continue-tag end-tag)
					      stack)))
		       (push loop-var vars)
		       (push generator vars)
		       
		       (values
			`(:split 
			  (setf ,generator (get-py-iterate-fun ,source)
				,loop-var  (funcall ,generator))
			  (unless ,loop-var (go ,else-tag))
			  
			  ,repeat-tag
			  (assign-stmt ,loop-var (,target))
			  (:split ,(walk suite stack2))
			  
			  ,continue-tag
			  (setf ,loop-var (funcall ,generator))
			  (if ,loop-var (go ,repeat-tag) (go ,end-tag))
			  
			  ,else-tag
			  ,@(when else-suite
			      `((:split ,(walk else-suite stack2))))
			  
			  ,end-tag
			  (setf ,loop-var nil
				,generator nil))
			t))))
		  
		  (break-stmt
		   (unless stack (error "BREAK outside loop"))
		   (values `(go ,(cdr (car stack)))
			   t))
		    
		  (continue-stmt
		   (unless stack (error "CONTINUE outside loop"))
		   (values `(go ,(car (car stack)))
			   t))
		    
		  (return-stmt
		   (when (second form)
		     (error "SyntaxError: Inside generator, RETURN statement may not have ~
                             an argument (got: ~S)" form))
		    
		   ;; from now on, we will always return to this state
		   (values `(generator-finished)
			   t))

		  (suite-stmt
		   (if (not (generator-ast-p form))
		       (values form t)
		     (values `(:split ,@(loop for stmt in (second form)
					    collect (walk stmt stack)))
			     t)))
		    
		  (try-except-stmt
		   (if (not (generator-ast-p form))
		       (values form t)
		     
		     ;; Three possibilities:
		     ;;  1. YIELD-STMT or RETURN-STMT in TRY-SUITE 
		     ;;  2. YIELD-STMT or RETURN-STMT in some EXCEPT-CLAUSES
		     ;;  3. YIELD-STMT or RETURN-STMT in ELSE-SUITE
		     ;; 
		     ;; We rewrite it such that all cases are covered,
		     ;; so maybe there is more rewritten than strictly
		     ;; needed.
		     
		     (destructuring-bind (try-suite except-clauses else-suite) (cdr form)
		       (loop
			   with try-tag = (new-tag :yield)
			   with else-tag = (new-tag :else)
			   with after-tag = (new-tag :after)
			   with gen-maker = '#:helper-gen-maker and gen = '#:helper-gen
				      				      
			   initially (push gen vars)
			     
			   for (exc var suite) in except-clauses
			   for tag = (new-tag :exc-suite)
				     
			   collect `(,exc ,var (go ,tag)) into jumps
			   nconc `(,tag ,(walk suite stack) (go ,after-tag)) into exc-bodies
									      
			   finally
			     (return
			       (values
				`(:split
				  (setf ,gen (py-iterate->lisp-fun
					      (funcall ,(suite->generator gen-maker try-suite))))
				  (setf .state. ,try-tag)
				  
				  ;; yield all values returned by helpder function .gen.
				  ,try-tag
				  (try-except-stmt
				   (let ((val (funcall ,gen)))
				     (case val
				       (:explicit-return (generator-finished))
				       (:implicit-return (go ,else-tag))
				       (t (return-from :function-body val))))
				   
				   ,@jumps)
				  
				  ,@exc-bodies
				  
				  ,else-tag
				  ,@(when else-suite
				      `((:split ,(walk else-suite stack))))
				  
				  ,after-tag
				  (setf ,gen nil))
				t))))))
		  
		  (try-finally
		   (destructuring-bind (try-suite finally-suite) (cdr form)
		     (when (generator-ast-p try-suite)
		       (error "SyntaxError: YIELD is not allowed in the TRY suite of ~
                               a TRY/FINALLY statement (got: ~S)" form))
		     
		     (if (not (generator-ast-p finally-suite))
			 (values form t)

		       (let ((fin-catched-exp '#:fin-catched-exc))
			 
			 (pushnew fin-catched-exp vars)
			 (values
			  `(:split
			    (multiple-value-bind (val cond)
				(ignore-errors ,try-suite ;; no need to walk
					       (values))
			      (setf ,fin-catched-exp cond))
			    
			    ,(walk finally-suite stack)
			    
			    (when ,fin-catched-exp
			      (error ,fin-catched-exp)))
			  
			  t)))))
		  
		  (t (values form
			     t)))))))

	(destructuring-bind (decorators fname (pos-args key-args *-arg **-arg) suite)
	    (cdr ast)
	  (declare (ignore decorators fname pos-args key-args *-arg **-arg))
	  
	  (let ((walked-as-list (multiple-value-list (apply-splits (walk suite ()))))
		(final-tag -1))
	  
	    `(lambda ,gen-maker-lambda-args ;; This is the function that returns a generator
	       (let ((.state. 0)
		     ,@(nreverse vars))
		 
		 (lambda ()
		   ;; This is the function that will repeatedly be
		   ;; called to return the values
		   
		   (macrolet ((generator-finished ()
				'(progn (setf .state. ,final-tag)
					(go ,final-tag))))
		   
		     (block :function-body
		       (tagbody
			 (case .state.
			   ,@(loop for i from 0 to yield-counter
				 collect `(,i (go ,i))))
			0
			 ,@walked-as-list

			 (generator-finished)
			 
			 ,final-tag
			 (py-raise 'StopIteration "The generator has finished.")))))))))))))


(defun apply-splits (form)
  (cond ((atom form)
	 (values form))
	
	((eq (car form) :split)
	 (values-list (loop for elm in (cdr form)
			  append (multiple-value-list (apply-splits elm)))))
	
	(t (loop for elm in form
	       append (multiple-value-list (apply-splits elm))))))


#| ;; test
(apply-splits '(1 2 3))
(apply-splits '(1 (:split 2 3)))
(apply-splits '(:split 1 2))
(apply-splits '(1 (:split 2 (:split 3 4))))
(apply-splits '((:split (:split 1 2 3 4))))
|#		     


#+(or) ;; test
(let ((ast '(suite ((yield (testlist (1) nil)) 
		    (yield (testlist (2) nil))))))
  (let* ((generator (funcall (eval (create-generator-function ast)))))
    (format t "~A~%" (funcall generator))
    (format t "~A~%" (funcall generator))
    (funcall generator)))

#+(or) ;; for testing
(let ((ast '(suite
         ((for-in (exprlist ((identifier i)) nil) (testlist ((call (identifier range) (3))) nil)
           (suite ((yield (testlist ((identifier i)) nil)))) nil)))))
  (create-generator-function ast))

#+(or) ;; for testing
(defun generate-n (ast n &key show-ast)
  (let ((ast (create-generator-function ast)))
    (when show-ast
      (format t "ast: ~A~%" ast))
    (let ((generator (funcall (eval ast))))
      (dotimes (i n)
	(format t "value ~A: ~A~%" i (funcall generator))))))

#+(or) ;; test
(generate-n '(suite
	      ((for-in (exprlist ((identifier i)) nil) (testlist ((call (identifier range) (3))) nil)
		(suite ((yield (testlist ((identifier i)) nil)))) nil)))
	    4
	    :show-ast t)

#+(or) ;; test
(let* ((generator (funcall (eval (create-generator-function ast)))))
    (format t "~A~%" (funcall generator))
    (format t "~A~%" (funcall generator))
    (funcall generator))


#+(or)
(defun test ()
  (let ((ast (parse-python-string "
def f():
  x = 3
  yield x
  x += 1
  yield x")))
    (generator-ast-p ast)))

#+(or)
(defun test ()
  (let* ((ast (parse-python-string "
def f():
  yield 2
  try:
    yield 3
    return
  except:
    pass"))
	 (funcdef (car (second (second ast)))))
    (rewrite-generator-funcdef-ast funcdef)))