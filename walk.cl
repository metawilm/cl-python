(in-package :python)

;;; Walking the Python AST
;;; 
;;; Determining whether a function is a generator is determined by
;;; walking it's AST,looking for a YIELD node.

(defvar *walk-lists-only* nil)

(defun walk-py-form (ast f &key (walk-lists-only t))
  (walk-py-ast ast f :walk-lists-only walk-lists-only))

(defun walk-py-ast (ast f &key (walk-lists-only t))
  (declare (optimize (debug 3))
	   (notinline walk-py-form))
  
  "Walk recursively through AST, calling F on each statement. During the call, ~
   *py-ast-lvalue* will have value T iff the form is an l-value (assignment ~
   target), ~@
   F should return a (possibly new) form to walk through, derived in some way ~
   from the form it's given). ~@
   The collected results are returned as a new AST. ~@
   When F returns two values and the second value is T, the form returned in ~@
   the first value is considered the final form and it is not walked into."

  (assert (listp ast))
  
  ;; `file-input' is evaluated in a context where its value is not
  ;; used, and it is not a target for assignment either.

  (let ((*walk-lists-only* walk-lists-only))
    (walk-py-ast-1 ast f :value nil :target nil)))


(defun ast-recurse-fun (ast)
  (declare (optimize (debug 3)))
  (assert (listp ast))
  (ecase (car ast)
    
    (assert         (lambda (f form &key value target)
		      (assert (not (or value target)))
		      `(assert ,(funcall f (second form) t nil) ,(funcall f (third form)))))
    
    (assign-expr    (lambda (f form &key value target)
		      (assert (not (or value target)))
		      `(assign-expr ,(funcall f (second form) :value t)
				    ,(mapcar (lambda (x) (funcall f x :target t)) (third form)))))
    
    (attributeref   (lambda (f form &key value target)
		      (declare (ignore value target))
		      `(attributeref ,(funcall f (second form))
				     ,(funcall f (third form)))))
      
    (augassign-expr (lambda (f form &key value target)
		      (assert (not (or value target)))
		      `(augassign-expr ,(second form)
				       ,(funcall f (third form) :value t :target t) ;; both val and tg
				       ,(funcall f (fourth form)))))
      
    (backticks      (lambda (f form &key value target)
		      (declare (ignore value))
		      (assert (not target))
		      `(backticks ,(funcall f (second form) :value t))))
    
    ((binary binary-lazy)  (lambda (f form &key value target)
			     (declare (ignore value))
			     (assert (not target))
			     `(,(first form) ,(second form)
					     ,(funcall f (third form) :value t)
					     ,(funcall f (fourth form) :value t))))
      
    ((break continue global identifier pass)  (lambda (f form &key value target)
						(declare (ignore f value target))
						form))
      
    (call        (lambda (f form &key value target)
		   (declare (ignore value))
		   (assert (not target))
		   `(call ,(funcall f (second form) :value t)
			  ,(loop for arg in (third form)
			       collect (ecase (first arg)
					 ((pos * **) (list (first arg) (funcall f (second arg) :value t)))
					 ;; in key args, the key name is not a target 
					 (key (list 'key (second arg) (funcall f (third arg) :value t))))))))
    
    (classdef    (lambda (f form &key value target)
		   (assert (not (or target value)))
		   (destructuring-bind (cname inheritance suite) (cdr form)
		     `(classdef ,(funcall f cname :target t)
				,(funcall f inheritance :value t) ;; a tuple
				,(funcall f suite)))))
    
    (comparison  (lambda (f form &key value target)
		   (declare (ignore value))
		   (assert (not target))
		   `(comparison ,(second form)
				,(funcall f (third form) :value t)
				,(funcall f (fourth form) :value t))))
      
    (del         (lambda (f form &key value target)
		   (assert (not (or target value)))
		   `(del ,(funcall f (second form) :target t))))
      
    (dict        (lambda (f form &key value target)
		   (declare (ignore value))
		   (assert (not target))
		   `(dict ,(loop for (k . v) in (second form)
			       collect (cons (funcall f k :value t)
					     (funcall f v :value t))))))
      
    (file-input  (lambda (f form &key value target)
		   (assert (not (or value target)))
		   `(file-input ,(mapcar (lambda (x) (funcall f x))
					 (second form)))))
      
    (for-in      (lambda (f form &key value target)
		   (assert (not (or value target)))
		   (destructuring-bind
		       (targets sources suite else-suite) (cdr form)
		     `(for-in ,(funcall f targets :target t)
			      ,(funcall f sources :value t)
			      ,(funcall f suite)
			      ,(when else-suite
				 (funcall f else-suite))))))
      
    (funcdef     (lambda (f form &key value target)
		   (assert (not (or target value)))
		   (destructuring-bind
		       (fname (pos-key-params *-param **-param) suite) (cdr form)
		     `(funcdef ,(funcall f fname :target t)
			       (,(loop for p in pos-key-params
				     if (consp p) collect (cons (car p) (funcall f (cdr p) :value t))) ;; keyword
				   else collect p ;; positional
					,*-param
					,**-param)
			       ,(funcall f suite)))))
      
    (if          (lambda (f form &key value target)
		   (assert (not (or value target)))
		   (destructuring-bind
		       (clauses else-suite) (cdr form)
		     `(if ,(loop for (test suite) in clauses
			       collect (list (funcall f test :value t)
					     (funcall f suite)))
			  ,(when else-suite
			     (funcall f else-suite))))))
      
    (import      (lambda (f form &key value target)
		   (assert (not (or value target)))
		   `(import ,(loop for clause in (second form)
				 collect (ecase (first clause)
					   (as `(as ,(second clause)
						    ,(funcall f (third clause) :target t)))
					   (not-as (error "import walk not-as: todo")))))))  ;; TODO: attributeref
      
    (import-from  (lambda (f form &key value target)
		    (assert (not (or value target)))
		    ;; Because of the special machinery behind `import', don't treat
		    ;; the source package name as value. XXX
		    `(import-from ,(second form)
				  ,(loop for (as src dest) in (third form)
				       do (assert (eq as 'as))
					  (assert (eq (first dest) 'identifier))
				       collect `(as ,src ,(funcall f dest :target t))))))
      
    (lambda       (lambda (f form &key value target)
		    (declare (ignore value))
		    (assert (not target))
		    (destructuring-bind
			((pos-key-params *-param **-param) suite) (cdr form)
		      `(lambda (,(loop for p in pos-key-params
				     if (consp p) collect (cons (car p) (funcall f (cdr p) :value t)))
				   else collect p ;; positional
					,*-param
					,**-param)
			 ,(funcall f suite :value t)))))
      
    ((list tuple)   (lambda (f form &key value target)
		      ;; the contained items are a target iff the list/tuple itself is
		      `(,(first form)
			,(loop for x in (second form)
			     collect (funcall f x :value value :target target)))))
      
    (list-compr     (lambda (f form &key value target)
		      (declare (ignore value))
		      (assert (not target))
		      `(list-compr ,(funcall f (second form) :value t) ;; XXX value ok...?
				   ,(loop for for/if in (third form)
					collect (ecase (car for/if)
						  (list-for-in `(list-for-in ,(funcall f (second for/if) :target t)
									     ,(funcall f (third  for/if) :value t)))
						  (list-if `(list-if ,(funcall (second for/if) :value t))))))))
      
    (print          (lambda (f form &key value target)
		      (assert (not (or target value)))
		      `(print ,(loop for x in (second form)
				   collect (funcall f x :value t))
			      ,(third form))))
      
    (print->>       (lambda (f form &key value target)
		      (assert (not (or target value)))
		      `(print->> ,(funcall f (second form) :value t)
				 ,(loop for x in (third form)
				      collect (funcall f x :value t))
				 ,(fourth form))))
      
    (raise          (lambda (f form &key value target)
		      (assert (not (or target value)))
		      (destructuring-bind (exc var tb) (cdr form)
			`(raise ,(when exc (funcall f exc :value t))
				,(when var (funcall f var :value t))
				,(when tb  (funcall f var :value t))))))
      
    (return         (lambda (f form &key value target)
		      (assert (not (or target value)))
		      `(return ,(when (second form)
				  (funcall f (second form) :value t)))))
      
    (slice          (lambda (f form &key value target)
		      (declare (ignore value))
		      (assert (not target))
		      (destructuring-bind (start stop step) (cdr form)
			`(slice ,(when start (funcall f start :value t))
				,(when stop  (funcall f stop :value t))
				,(when step  (funcall f step :value t))))))
							  
    (subscription   (lambda (f form &key value target)
		      (declare (ignore value target))
		      `(subscription ,(funcall f (second form) :value t)
				     ,(funcall f (third form)  :value t))))
      
    (suite          (lambda (f form &key value target)
		      (assert (not (or value target)))
		      `(suite ,(loop for x in (second form)
				   collect (funcall f x)))))
           
    (try-except     (lambda (f form &key value target)
		      (assert (not (or value target)))
		      (destructuring-bind (suite except-clauses else-clause) (cdr form)
			`(try-except ,(funcall f suite)
				     ,(loop for ((exc var) handler-form) in except-clauses
					  collect (list (when exc (funcall f exc :value t))
							(when var (funcall f var :target t))
							(funcall f handler-form)))
				     ,(when else-clause
					(funcall f else-clause))))))
      
    (try-finally    (lambda (f form &key value target)
		      (assert (not (or value target)))
		      (destructuring-bind (try-suite finally-suite) (cdr form)
			`(try-finally ,(funcall f try-suite) ,(funcall f finally-suite)))))
      
    (unary          (lambda (f form &key value target)
		      (declare (ignore value))
		      (assert (not target))
		      `(unary ,(second form) ,(funcall f (third form) :value t))))
      
    (while          (lambda (f form &key value target)
		      (assert (not (or value target)))
		      (destructuring-bind (test suite else-suite) (cdr form)
			`(while ,(funcall f test :value t)
			   ,(funcall f suite)
			   ,(when else-suite (funcall f else-suite))))))
      
    (yield          (lambda (f form &key value target)
		      (assert (not (or value target)))
		      `(yield ,(funcall f (second form) :value t))))))


(defun walk-py-ast-1 (ast user-fun &rest context &key value target)
  (declare (ignore value target)
	   (optimize (debug 3)))
  
  (when (and *walk-lists-only* (not (listp ast)))
    (return-from walk-py-ast-1 ast))

  ;; Call user function on whole form. The returned values imply our
  ;; next action.
  
  (multiple-value-bind (ret-ast final-p) 
      (apply user-fun ast context)
      
    (cond (final-p         (return-from walk-py-ast-1 ret-ast)) 
	  
	  ((null ret-ast)  (error "Walking AST: got NIL as return value for ast:~%~
                                   ~S~%with user-fun:~% ~A" ast user-fun))
	  
	  ((listp ret-ast) (let ((recurse-fun (or (ast-recurse-fun ast)
						  (error "no recurse fun for AST ~A" (car ret-ast))))
				 (lam (lambda (ast &rest context &key value target)
					(declare (ignore value target))
					(assert ast)
					(apply #'walk-py-ast-1 ast user-fun context))))
			     (return-from walk-py-ast-1
			       (apply recurse-fun lam ret-ast context))))
	  
	  (t               (return-from walk-py-ast-1 ret-ast)))))


#+(or) ;; old
(defun walk-py-form (ast f &key (walk-lists-only t))
  (declare (optimize (debug 3))
	   (notinline walk-py-form))
  
  "Walk recursively through AST, calling F on each statement. During the call, ~
   *py-ast-lvalue* will have value T iff the form is an l-value (assignment ~
   target), ~@
   F should return a (possibly new) form to walk through, derived in some way ~
   from the form it's given). ~@
   The collected results are returned as a new AST. ~@
   When F returns two values and the second value is T, the form returned in ~@
   the first value is considered the final form and it is not walked into."
  
  (assert ast)

  (when (and walk-lists-only (not (listp ast)))
    (return-from walk-py-form ast))
  
  (multiple-value-bind 
      (form final-p) (funcall f ast)
    
    (cond (final-p (return-from walk-py-form form))
	  ((null form) (error "Walking AST: got NIL as return value for ast:~% ~S~%with f:~% ~A"
			      ast f)))
    
    (macrolet ((do-walk  (form)  `(walk-py-form ,form f :walk-lists-only walk-lists-only))
	       (map-walk (forms) `(loop for form in ,forms
				      collect (walk-py-form form f :walk-lists-only walk-lists-only)))
	       (as-lvalue (&body body) `(let ((*py-ast-lvalue* t))
					  ,@body)))
      
      (unless (listp form)
	;; Can't walk into numbers and strings.
	(return-from walk-py-form form))
      
      (ecase (car form)
	      
	(assert `(assert ,(do-walk (second form)) ,(do-walk (third form))))
	
	(assign-expr
	 ;; XXX L-value...
	 `(assign-expr ,(do-walk (second form)) ,(map-walk (third form))))
	
	(attributeref `(attributeref ,(do-walk (second form)) ,(do-walk (third form))))
	
	(augassign-expr `(augassign-expr ,(second form)
					 ,(do-walk (third form))
					 ,(do-walk (fourth form)))) 
	
	(backticks `(backticks ,(do-walk (second form))))
	((binary binary-lazy) `(,(first form) ,(second form) 
					      ,(do-walk (third form))
					      ,(do-walk (fourth form))))
	((break continue) form)
	
	(call `(call ,(do-walk (second form))
		     ,(loop for arg in (third form)
			  collect (ecase (first arg)
				    ((pos * **) (list (first arg) (do-walk (second arg))))
				    (key #+(or)(list 'key (do-walk (second arg)) (do-walk (third arg)))
					 (list 'key (second arg) (do-walk (third arg))))))))
	
	;; XXX todo: pos arg values; key arg default values
	
	(classdef
	 (destructuring-bind ((identifier cname) inheritance suite) (cdr form)
	   (assert (eq identifier 'identifier))
	   `(classdef ,cname ,(map-walk inheritance) ,(do-walk suite))))
	(comparison `(comparison ,(second form) ,(do-walk (third form))
				 ,(do-walk (fourth form))))

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
	      `(if ,(loop for (test suite) in clauses
			collect (do-walk test) (do-walk suite))
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
	 `(subscription ,(do-walk (second form)) ,(do-walk (third form))))
	      
	(suite  `(suite ,(map-walk (second form))))
	(tuple `(tuple ,(map-walk (second form))))
	      
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


;;;

;; Derive from the AST the likely attributes of class instances, by
;; looking for assignments of the form `self.x = y' in the body of
;; methods that take `self' as first arg, inside classes.

#+(or)
(defun foo ()
  (walk-py-ast
   (parse-python-string (read-file "b0.py"))
   (lambda (form &key value target)
     (declare (ignore value target))
     (when (eq (first form) 'classdef)
       (let ((class-name (second (second form))))
	 #+(or)(warn "classdef: ~A" class-name)
	 
	 (walk-py-ast
	  form
	  (lambda (form &key value target)
	    (declare (ignore value target))
	    (when (eq (first form) 'funcdef) ;; method definition XXX nested classes handled wrong
	      (let ((meth-name (second (second form)))
		    (self-param (caar (third form))))
		(when self-param
		  #+(or)(warn "  method with param: ~A" meth-name self-param)
		  
		  (walk-py-ast
		   form
		   (lambda (form &key value target)
		     (declare (ignore value target))
		     (when (eq (first form) 'assign-expr)
		       (let ((assign-expr form))
			 #+(or)(warn "    assign-expr: ~A" assign-expr)
			 
			 (walk-py-ast
			  form
			  (lambda (form &key value target)
			    (declare (ignore value))
			    (when (and (eq (first form) 'attributeref)
				       (listp (second form))
				       (eq (second (second form)) self-param)
				       target)
			      (format t "in method ~A.~A(~A, ...): ~A~%"
				      class-name meth-name self-param (ast->python-code assign-expr)))
			    form))))
		     form)))))
	    form))))
     form)))

