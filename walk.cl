(in-package :python)

;;; Walking the Python AST
;;; 
;;; Determining whether a function is a generator is determined by
;;; walking it's AST,looking for a YIELD node.

(defun walk-py-form (ast f &key (walk-lists-only t))
  "Walk recursively through AST, calling F on each statement. ~@
   F should return a (possibly new) form to walk through, derived from ~@
   the form it's given). ~@
   The collected results are returned as a new AST. ~@
   When F returns two values and the second value is T, the form returned in ~@
   the first value is considered the final form and it is not walked into."
  
  (unless ast
    ;; unused optional element of a syntactical construct
    (return-from walk-py-form nil))
  
  (when (and (not (listp ast))
	     walk-lists-only)
    (return-from walk-py-form ast))
  
  (multiple-value-bind 
      (form final-p) (funcall f ast)
    
    (when final-p
      (return-from walk-py-form form))
    
    (macrolet ((do-walk  (form)  `(walk-py-form ,form f))
	       (map-walk (forms) `(loop for form in ,forms
				      collect (walk-py-form form f))))
      
      (unless (listp form)
	;; Can't walk into numbers and strings.
	(return-from walk-py-form form))
      
      (ecase (car form)
	      
	(assert `(assert ,@(map-walk (cdr form))))
	
	(assign-expr
	 ;; XXX L-value...
	 `(assign-expr ,(do-walk (second form)) ,(map-walk (third form))))
	
	(attributeref `(attributeref ,@(map-walk (cdr form))))
	
	(augassign-expr `(augassign-expr ,(second form) ,(third form)
					 ,(do-walk (fourth form)))) 
	
	(backticks `(backticks ,(map-walk (second form))))
	(binary `(binary ,(second form) 
			 ,(do-walk (third form)) ,(do-walk (fourth form))))
	(binary-lazy `(binary-lazy ,(second form)
				   ,(do-walk (third form))
				   ,(do-walk (fourth form))))
	(break form)
	
	(call `(call ,(do-walk (second form))
		     ,(third form)))
	;; XXX todo: pos arg values; key arg default values
	
	(classdef
	 (destructuring-bind (cname inheritance suite) (cdr form)
	   `(classdef ,cname ,(map-walk inheritance) ,(do-walk suite))))
	(comparison `(comparison ,(second form) ,(do-walk (third form))
				 ,(do-walk (fourth form))))
	(continue form)
	(del        `(del ,(do-walk (second form))))
	(dict `(dict ,(loop for (k . v) in (second form)
			  collect (cons (do-walk k) (do-walk v)))))
	(file-input `(file-input ,(map-walk (second form))))

	(for-in (destructuring-bind
		    (targets sources suite else-suite) (cdr form)
		  `(for-in ,targets
			   ,sources
			   ,(do-walk suite)
			   ,(when else-suite
			      (do-walk else-suite)))))
	
	(funcdef (destructuring-bind
		     (fname params suite) (cdr form)
		   `(funcdef ,fname ,params ,(do-walk suite))))
	(global form)
	(identifier form)
	
	(if (destructuring-bind
		(clauses else-suite) (cdr form)
	      `(if ,(loop for (nil suite) in clauses
			collect (do-walk suite))
		   ,(when else-suite
		      (do-walk else-suite)))))
	(import form)
	(import-from form)
	(lambda `(lambda ,(second form) ,(do-walk (third form))))
	      
	(list  `(list ,(map-walk (second form))))
	(list-compr `(list-compr
		      ,(do-walk (second form))
		      ,(loop for for/if in (third form)
			   collect
			     (ecase (car for/if)
			       (list-for-in
				`(list-for-in ,(do-walk (second for/if))
					      ,(do-walk (third  for/if))))
			       (list-if
				`(list-if ,(do-walk (second for/if))))))))
	
	(pass  form)
	(print `(print ,(map-walk (second form)) ,(third form)))
	(print->> `(print->> ,(do-walk (second form))
			     ,(map-walk (third form))
			     ,(fourth form)))
	      
	(raise `(raise ,@(map-walk (cdr form))))

	(return `(return ,(do-walk (second form))))
	(slice `(slice ,@(do-walk (cdr form))))
	(subscription ;; XXX: TODO: make third item testlist?
	 `(subscription ,(do-walk (second form)) ,(third form)))
	      
	(suite  `(suite ,(map-walk (second form))))
	(testlist `(testlist ,(map-walk (second form)) ,(third form)))
	      
	(try-except
	 (destructuring-bind
	     (suite except-clauses else-clause) (cdr form)
	   `(try-except ,(do-walk suite)
			,(loop for ((nil nil) handler-form) in except-clauses
			     collect (do-walk handler-form))
			,(when else-clause
			   (do-walk else-clause)))))
	
	(try-finally
	 (destructuring-bind (try-suite finally-suite) (cdr form)
	   `(try-finally ,(do-walk try-suite) ,(do-walk finally-suite))))
	
	(unary `(unary ,(second form) ,(do-walk (third form))))
	
	(while (destructuring-bind 
		   (test suite else-suite) (cdr form)
		 `(while ,(do-walk test)
		    ,(do-walk suite)
		    ,(do-walk else-suite))))
	
	(yield `(yield ,(do-walk (second form))))))))

	
	

