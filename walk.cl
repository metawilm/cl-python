(in-package :python)

;;; Walking the Python AST
;;; 
;;; Determining whether a function is a generator is determined by
;;; walking it's AST,looking for a YIELD node.

(defun walk-py-form (ast f)
  "Walk recursively through AST, calling F on each statement. ~@
   F should return the form to walk through, derived from the form it's given. ~@
   When F returns two values and the second value is T, the form returned in ~@
   the first value is considered the final form and it is not walked into."
  ;; XXX for now it only walks statements, not expressions

  (multiple-value-bind 
      (form final-p) (funcall f ast)
    (if final-p
	
	(return-from walk-py-form form)
      
      (macrolet ((do-walk (form)
		   `(walk-py-form ,form f))
		 (map-walk (forms)
		   `(loop for form in ,forms
			collect (walk-py-form form f))))
   
	(ccase (car form)
    
	  ((file-input suite) 
	   `(suite ,(map-walk (second form))))
        
	  (try-except
	   (destructuring-bind
	       (suite except-clauses else-clause) (cdr form)
	     `(try-except ,(do-walk suite)
			  ,(loop for ((nil nil) handler-form) in except-clauses
			       collect (do-walk handler-form))
			  ,(when else-clause
			     (do-walk else-clause)))))
    
	  (for-in
	   (destructuring-bind
	       (targets sources suite else-suite) (cdr form)
	     `(for-in ,targets
		      ,sources
		      ,(do-walk suite)
		      ,(when else-suite
			 (do-walk else-suite)))))

	  (funcdef
	   (destructuring-bind
	       (fname params suite) (cdr form)
	     `(funcdef ,fname ,params ,(do-walk suite))))
    
	  (classdef
	   (destructuring-bind
	       (cname inheritance suite) (cdr form)
	     `(classdef ,cname ,inheritance ,(do-walk suite))))
    
	  (if
	      (destructuring-bind
		  (clauses else-suite) (cdr form)
		`(if ,(loop for (nil suite) in clauses
			  collect (do-walk suite))
		     ,(when else-suite
			(do-walk else-suite)))))

	  (while 
	      (destructuring-bind 
		  (test suite else-suite) (cdr form)
		`(while
		     ,test
		   ,(do-walk suite)
		   ,(when else-suite
		      (do-walk else-suite)))))
    
	  ((inline-lisp raise assert del list testlist identifier return yield dict
	    attributeref subscription slice call assign-expr import
	    comparison unary binary tuple slice print print>> lambda
	    continue break pass) 
	   form))))))
