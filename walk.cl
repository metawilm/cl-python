(in-package :python)

;;; Walking the Python AST
;;; 
;;; Determining whether a function is a generator is determined by
;;; walking it's AST,looking for a YIELD node.

(defun walk (ast f)
  "Walk recursively through AST, calling F on each statement. ~@
   When F returns T, T, the current form is not walked into."
  ;; XXX for now it only walks statements, not expressions

  (multiple-value-bind 
      (form final-p) (funcall f ast)
    (when final-p
      (return-from walk form)))

  (macrolet ((do-walk (form)
	       `(walk ,form f))
	     (map-walk (forms)
	       `(loop for form in ,forms
		   collect (walk form f))))
   
    (ccase (car ast)
    
      ((file-input suite) (format t "second ast = ~A" (second ast))
       `(suite ,(map-walk (second ast))))
        
      (try-except
       (destructuring-bind
	   (suite except-clauses else-clause) (cdr ast)
	 `(try-except ,(do-walk suite)
		      ,(loop for ((nil nil) handler-form) in except-clauses
			   collect (do-walk handler-form))
		      ,(when else-clause
			 (do-walk else-clause)))))
    
      (for-in
       (destructuring-bind
	   (targets sources suite else-suite) (cdr ast)
	 `(for-in ,targets
		  ,sources
		  ,(do-walk suite)
		  ,(when else-suite
		     (do-walk else-suite)))))

      (funcdef
       (destructuring-bind
	   (fname params suite) (cdr ast)
	 `(funcdef ,fname ,params ,(do-walk suite))))
    
      (classdef
       (destructuring-bind
	   (cname inheritance suite) (cdr ast)
	 `(classdef ,cname ,inheritance ,(do-walk suite))))
    
      (if
	  (destructuring-bind
	      (clauses else-suite) (cdr ast)
	    `(if ,(loop for (nil suite) in clauses
		      collect (do-walk suite))
		 ,(when else-suite
		    (do-walk else-suite)))))

      (while 
	  (destructuring-bind 
	      (test suite else-suite) (cdr ast)
	    `(while
		 ,test
	       ,(do-walk suite)
	       ,(when else-suite
		  (do-walk else-suite)))))
    
      ((inline-lisp raise assert del list testlist identifier return yield dict
	attributeref subscription slice call assign-expr import
	comparison unary binary tuple slice print print>> lambda
	continue break pass) 
       ast))))



(defun ast-generator-p (ast)
  (catch 'is-generator
    (walk ast 
	  (lambda (x) (when (eq (car x) 'yield)
			(throw 'is-generator t))))))
