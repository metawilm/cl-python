(in-package :python)

(defun generator-ast-p (ast)
  "Is AST a function definition for a generator?"
  (catch 'is-generator
    (walk-py-form ast 
		  (lambda (x)
		    (when (eq (car x) 'yield)
		      (throw 'is-generator t))
		    x))
    nil))

(defun strings->symbol (&rest strings)
  (make-symbol (apply #'concatenate 'string strings)))



(defun create-generator-function (ast)
  (let ((counters (copy-tree '((:yield . 0) (:if . 0) (:for . 0) (:while . 0) (:iterator . 0))))
	(vars ()))
        
    (labels ((new-tag (kind) (let ((res (assoc kind counters)))
			       (incf (cdr res))))
	     (new-strtag (kind) (format nil "~A~A" kind (new-tag kind))))
      
      (macrolet ((with-derived-tags (tag var-strings &body body)
		   (let ((g '#:tag))
		     `(let* ((,g ,tag)
			     ,@(loop for (var string) in var-strings
				   collect `(,var (strings->symbol ,g ,string))))
			,@body))))
	
	(labels
	    ((walk (form stack)
	       #+(or)(break "walking: ~A" form)
	       (walk-py-form
		form
		(lambda (form)
		  (case (car form)
		     
		    (yield (let ((tag (new-tag :yield)))
			     (values `(:split (ret (py-eval ',(second form)) ,tag)
					      ,tag)
				     t)))

		    (while (destructuring-bind
			       (test suite else-suite) (cdr form)
			     (let ((tag (new-strtag :while)))
			       (with-derived-tags
				   tag ((repeat-tag "-repeat")(else-tag "-else")(after-tag "-end/break-target"))
				   (values `(:split (unless (py-val->lisp-bool (py-eval ',test))
						      (go ,else-tag))
						    ,repeat-tag
						    (:split ,(walk suite (cons (cons repeat-tag after-tag) stack)))
						    (if (py-val->lisp-bool (py-eval ',test))
							 (go ,repeat-tag)
						       (go ,after-tag))
						    ,else-tag
						    (:split ,(when else-suite (walk else-suite stack)))
						    ,after-tag)
					   t)))))
				 
		    (if (destructuring-bind 
			    (clauses else-suite) (cdr form)
			  (let ((tag (new-strtag :if)))
			    (with-derived-tags
				tag ((else-tag "-else") (after-tag "-after"))
				(let ((indexed-clauses
				       (loop for (expr suite) in clauses
					   for i from 1
					   collect `(,(strings->symbol tag "-then" (format nil "~A" i))
						     ,expr ,suite))))
			  
				  (multiple-value-bind (tests suites)
				      (loop for (then-tag expr suite) in indexed-clauses
					  collect `((py-val->lisp-bool (py-eval ',expr))
						    (go ,then-tag)) into tests
					  collect `(:split ,then-tag
							   (:split ,(walk suite stack))
							   (go ,after-tag)) into suites
					  finally (return (values tests suites)))
			    
				    (values `(:split (cond ,@tests (t (go ,else-tag)))
						     (:split ,@suites)
						     ,else-tag
						     (:split ,(when else-suite (walk else-suite stack)))
						     ,after-tag)
					    t)))))))
				     
		    (for-in (destructuring-bind
				(targets sources suite else-suite) (cdr form)
			      (with-derived-tags (new-strtag :for)
				((repeat-tag "-repeat") (else-tag "-else") (end-tag "-end/break-target")
							(continue-tag "-continue-target") (generator "-generator")
							(loop-var "-loop-var"))
	
				(let ((stack2 (cons (cons continue-tag end-tag) stack)))
				  (push loop-var vars)
				  (push generator vars)
				  (values `(:split (setf ,generator (get-py-iterate-fun (py-eval ',sources))
							 ,loop-var (funcall ,generator))
						   (unless ,loop-var (go ,else-tag))
						   ,repeat-tag
						   (eval-assign-expr-1 ',targets ,loop-var)
						   (:split ,(walk suite stack2))
						   ,continue-tag
						   (setf ,loop-var (funcall ,generator))
						   (if ,loop-var (go ,repeat-tag) (go ,end-tag))
						   ,else-tag
						   ,(when else-suite `(:split ,(walk else-suite stack2)))
						   ,end-tag
						   (setf ,loop-var nil  ;; enable garbage collection
							 ,generator nil))
					  t)))))
		    (break
		     (unless stack
		       (error "BREAK outside loop"))
		     (values `(go ,(cdr (car stack)))
			     t))
		    
		    (continue
		     (unless stack
		       (error "CONTINUE outside loop"))
		     (values `(go ,(car (car stack)))
			     t))
		    
		    (return
		      (when (second form)
			(error "Inside generator, RETURN may nothave an argument (got: ~A)" form))
		      (let ((tag (new-tag :yield))) ;; from now on, we will always return to this state
			(values `(:split ,tag
					 (py-raise 'StopIteration "Generator has called RETURN"))
				t)))

		    (suite
		     (values `(:split ,@(loop for stmt in (second form)
					    collect (walk stmt stack)))
			     t))
		    
		    (t (values `(py-eval ',form)
			       t)))))))
	  
	  (let ((walked-as-list (multiple-value-list (apply-splits (walk ast ()))))
		(final-tag (new-tag :yield)))
	    
	    `(lambda ()  ;; This is the function that returns a generator
	       (let ((state 0)
		     ,@(nreverse vars))
		   
		 (lambda ()  ;; This is the function that will repeatedly be called to return the values
		   (block inside
		     (macrolet ((ret (val new-state)
				  `(progn (setf state ,new-state)
					  (return-from inside ,val))))
		       (tagbody
			 (case state
			   ,@(loop for i from 0 to (cdr (assoc :yield counters))
				 collect `(,i (go ,i))))
			0
			 ,@walked-as-list
			 (setf state ,final-tag)
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

	     

#|
TODO: rewriting `yield' when it occurs in `try', `except', `finally' or `else'.

try:
  -a-
  yield x
  -b-
except E:
  -c-
else:
  -d-

 ==>

$yield-val$ = nil
$do-yield$ = nil
$continue-try$ = t
$do-else$ = t

try:
  -a-
  $yield-val$ = x
  $do-yield$ = t
except E:
  $continue-try$ = nil
  $do-else$ = nil
  -c-

if $do-yield$:
  yield $yield-val$

if $continue-try$:
  try:
    -b-
  except E:
    $do-else$ = nil
    -c-

if $do-else$:
  -d-
|#