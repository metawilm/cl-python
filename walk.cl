(in-package :python)

;;; Python AST code walker
;;; 
;;; This module does not depend on other modules, as ASTs are
;;; represented by plain lists.


(defun walk-py-ast (ast f &key (value nil) (target nil)
			       (walk-lists-only t) 
			       (build-result nil)
			       (into-nested-namespaces nil))
  (declare (optimize (debug 3)))
  "Walk recursively through AST, calling F on each statement. F should have
lambda list: (ast &key value target). F should return a new form to walk
through, derived in some way from the form it's given. The collected results
are returned as a new AST.

When F returns two values and the second value is T, the form returned in
the first value is considered the final form and it is not walked into, but
it is inserted as-is in the result.

The initial form AST is considered an expressions used for its value iff 
VALUE; it is considered an assignment target iff TARGET.

Unless WALK-LISTS-ONLY, F will also be called on numbers and strings."

  ;; As `file-input', the default top-level AST node representing a
  ;; module, is evaluated in a non-value non-target context, that
  ;; context seems a reasonable default for the keyword arguments.

  (when (and walk-lists-only (not (listp ast)))
    (return-from walk-py-ast ast))
	     
  #+(or)(assert (or (listp ast) (null walk-lists-only)))
  
  (labels ((walk-py-ast-1 (ast &rest context)
	     (declare (optimize (debug 3)))
	     (assert ast)
	     (if (and walk-lists-only (not (consp ast)))
		 
		 ;; Don't call user function on AST
		 ast
	       
	       ;; Call user function on whole form. The returned values
	       ;; control how we proceed.
	       (multiple-value-bind (ret-ast final-p) 
		   (apply f ast context)
		 
		 (cond (final-p         ret-ast)
		       
		       ((null ret-ast) (break "User AST func returned NIL (for AST: ~S)"
					      ast))
		       
		       ((or (consp ret-ast) (not walk-lists-only))
			(apply #'ast-recurse-fun
			       (lambda (ast &key value target)
				 (declare (optimize (debug 3)))
				 (walk-py-ast-1 ast :value value :target target))
			       ret-ast
			       build-result
			       into-nested-namespaces
			       context))
		       
		       (t (break "Walking AST: invalid return value ~S (AST: ~S, F: ~S)"
				 ret-ast ast f)))))))
    
    (walk-py-ast-1 ast :value value :target target)))


(defun ast-recurse-fun (f form build-result into-nested-namespaces
			  &rest context &key value target)
  "Given function F and a FORM (AST), walks into subforms of FORM in the given
VALUE and TARGET context."
  (declare (optimize (debug 3)))
  (assert (and form (listp form)))
  (case (car form)
      
    (assert-stmt
     (if build-result
	 `(assert-stmt ,(funcall f (second form) :value t) ,(funcall f (third form)))
       (progn (funcall f (second form) :value t)
	      (funcall f (third form)))))
      
    (assign-stmt
     (if build-result
	 `(assign-stmt ,(funcall f (second form) :value t)
		       ,(mapcar (lambda (x) (funcall f x :target t)) (third form)))
       (progn (funcall f (second form) :value t)
	      (mapc (lambda (x) (funcall f x :target t)) (third form)))))
    
    (attributeref-expr
     ;; don't recurse on attr name
     (if build-result
	 `(attributeref-expr ,(funcall f (second form)) ,(third form))
       (funcall f (second form))))
    
    (augassign-stmt
     (if build-result
	 `(augassign-stmt ,(second form)
			  ,(funcall f (third form) :value t :target t) ;; is both v and tg
			  ,(funcall f (fourth form)))
       (progn (funcall f (third form) :value t :target t)
	      (funcall f (fourth form)))))
    
    (backticks-expr
     (if build-result
	 `(backticks ,(funcall f (second form) :value t))
       (funcall f (second form) :value t))) 
      
    ((binary-expr binary-lazy-expr)
     (if build-result
	 `(,(first form) ,(second form)
			 ,(funcall f (third form) :value t)
			 ,(funcall f (fourth form) :value t))
       (progn (funcall f (third form) :value t)
	      (funcall f (fourth form) :value t))))
      
    ((break-stmt continue-stmt global-stmt identifier-expr pass-stmt) 
     ;; Note that an identifier-expr may be for None, Ellipsis, True, False
     form)
      
    (call-expr
     (destructuring-bind (primary (p-a k-a *-a **-a)) (cdr form)
       (if build-result
	   `(call-expr ,(funcall f primary :value t)
		       (,(mapcar (lambda (pos-arg) (funcall f pos-arg :value t))
				 p-a)
			,(mapcar (lambda (kv)
				   (list (first kv) (funcall f (second kv) :value t)))
				 k-a)
			,(when *-a (funcall f *-a :value t))
			,(when **-a (funcall f **-a :value t))))
	 
	 (progn (mapc (lambda (pos-arg) (funcall f pos-arg :value t)) p-a)
		(mapc (lambda (kv) (funcall f (second kv) :value t)) k-a)
		(when *-a (funcall f *-a :value t))
		(when **-a (funcall f **-a :value t))))))
    
    (classdef-stmt 
     (warn "walking classdef-stmt")
     (destructuring-bind (cname inheritance suite) (cdr form)
       (assert (eq (car inheritance) 'tuple-expr))
       (if build-result
	   `(classdef-stmt ,(funcall f cname :target t)
			   ,(funcall f inheritance :value t)
			   ,(if into-nested-namespaces
				(funcall f suite)
			      suite))
	 (progn (funcall f cname :target t)
		(funcall f inheritance :value t)
		(when into-nested-namespaces
		  (funcall f suite))))))
    
    (comparison-expr 
     (destructuring-bind (op left right) (cdr form)
       (if build-result
	   `(comparison-expr ,op
			     ,(funcall f left :value t)
			     ,(funcall f right :value t))
	 
	 (progn (funcall f left :value t)
		(funcall f right :value t)))))
    
    (del-stmt
     (if build-result
	 `(del-stmt ,(funcall f (second form) :target t)) ;; ":target" hmm
       (funcall f (second form) :target t)))
      
    (dict-expr
     (if build-result
	 `(dict ,(loop for (k . v) in (second form)
		     collect (cons (funcall f k :value t)
				   (funcall f v :value t))))
       (loop for (k . v) in (second form)
	   do (funcall f k :value t)
	      (funcall f v :value t))))
    
    (exec-stmt
     (destructuring-bind (code globals locals) (cdr form)
       (if build-result
	   `(exec-stmt ,(funcall f code :value t)
		       ,(when globals (funcall f globals :value t))
		       ,(when locals (funcall f locals  :value t)))
	 (progn (funcall f code :value t)
		(when globals (funcall f globals :value t))
		(when locals (funcall f locals  :value t))))))
    
    (for-in-stmt
     (destructuring-bind (targets sources suite else-suite)
	 (cdr form)
       (if build-result
	   `(for-in-stmt ,(funcall f targets :target t)
			 ,(funcall f sources :value t)
			 ,(funcall f suite)
			 ,(when else-suite (funcall f else-suite)))
	 (progn (funcall f targets :target t)
		(funcall f sources :value t)
		(funcall f suite)
		(when else-suite (funcall f else-suite))))))
    
    (funcdef-stmt
     (warn "walking funcdef-stmt")
     (destructuring-bind (decorators fname (pos-args key-args *-arg **-arg) suite)
	 (cdr form)
       (if build-result
	   `(funcdef-stmt ,(loop for deco in decorators
			       collect (funcall f deco :value t))
			  ,(funcall f fname :target t)
			  (,pos-args
			   ,(mapcar (lambda (kv) (list (first kv)
						       (funcall f (second kv) :value t)))
				    key-args)
			   ,*-arg
			   ,**-arg)
			  (if into-nested-namespaces
			      (funcall suite)
			    suite))
	 
	 (progn (loop for deco in decorators
		    do (funcall f deco :value t))
		(funcall f fname :target t)
		(mapc (lambda (kv) (funcall f (second kv) :value t)) key-args)
		(when into-nested-namespaces
		  (funcall suite))))))
    
    ((generator-expr listcompr-expr)
     (destructuring-bind (item for-in/if-clauses) (cdr form)
       
       ;; The order of walking subforms is important: if we started
       ;; recursing on ITEM now, then the compiler might raise a
       ;; SyntaxError ("local variable used before assignment")
       ;; because the variable is bound in a `for-in' or `if' clause.
       
       (if build-result
	   
	   (let* ((rec-clauses
		   (loop for for/if in for-in/if-clauses
		       collect (ecase (car for/if)
				 (for-in (destructuring-bind (expr iterable) (cdr for/if)
					   (let* ((rec-iterable
						   (funcall f iterable :value t))
						  (rec-expr
						   (funcall f expr :target t)))
					     `(for-in ,rec-expr ,rec-iterable))))
				 (if (let* ((test (second for/if)))
				       `(if ,(funcall f test :value t)))))))
		  
		  (rec-item (funcall f item :value t)))
	     
	     `(generator-expr ,rec-item ,rec-clauses))
	 
	 (progn (loop for for/if in for-in/if-clauses
		    do (ecase (car for/if)
			 (for-in (destructuring-bind (expr iterable) (cdr for/if)
				   (funcall f iterable :value t)
				   (funcall f expr     :target t)))
			 (if (let ((test (second for/if)))
			       (funcall f test :value t)))))
		(funcall f item :value t)))))
    
    (if-stmt
     (destructuring-bind
	 (clauses else-suite) (cdr form)
       (if build-result
	
	   `(if-stmt ,(loop for (test suite) in clauses
			  collect (list (funcall f test :value t)
					(funcall f suite)))
		     ,(when else-suite (funcall f else-suite)))
	 
	 (progn (loop for (test suite) in clauses
		    do (funcall f test :value t)
		       (funcall f suite))
		(when else-suite (funcall f else-suite))))))
    
    (import-stmt
     ;; XXX http://www.python.org/doc/essays/packages.html
     (warn "walking import-stmt: TODO")
     #+(or)(assert (not (or value target)))
     (values form t)
    
     #+(or) ;; todo
     `(import-stmt ,(loop for (as/not-as src-item &optional here-name) in (second form)
			do (ecase as/not-as
			     (as (assert here-name)
				 (ecase (first src-item)
				   (attributeref-expr  :todo)
				   (identifier-expr    (funcall src-item :value t)))
				 
				 (funcall f (third clause)
					  :target t)))
			   (not-as  ;; TODO: attributeref
			    (error
			     "import walk not-as: todo")))))
      
    (import-from-stmt
     (warn "walking import-from-stmt: TODO")
     #+(or)(assert (not (or value target)))
     (values form t)
     
     #+(or) ;; todo
     (destructuring-bind (source items) (cdr form)
       (assert (member (car source) '(identifier-expr attributeref-expr)))
       
       `(import-from-stmt ,(ecase (car source)
			     (identifier-expr   (funcall f source :value t))
			     (attributeref-expr (progn 
						  ;; let's recurse only on the 'x' in
						  ;; 'from x.y.z import ...'
						  (funcall (first (second source)) :value t)
						  source)))
			  ,(if (eq items '*)
			       items
			     (loop for x in items
				 do (ecase (car x)
				      (as     (destructuring-bind (src-name here-name) (cdr x)
						;; let's recurse only on the here-name
						(declare (ignore src-name))
						(funcall here-name :target t)))
				      
				      (not-as (let ((item (second x)))
						(assert (eq (car item) 'identifier-expr))
						;; from .. import x -> x becomes bound
						(funcall f item :target t))))
				  
				    ;; for now don't use returns from funcall
				 finally (return items))))))
    
    (lambda-expr
     (warn "walking lambda-expr")
     #+(or)(assert (not target))
     (destructuring-bind
	 ((pos-a key-a *-a **-a) expr) (cdr form)
       (if build-result
	   `(lambda-expr (,pos-a
			  ,(mapcar (lambda (kv) (funcall f (second kv) :value t)) key-a)
			  ,*-a
			  ,**-a)
			 ,(if into-nested-namespaces
			      (funcall f expr :value t)
			    expr))
	 (progn (mapc (lambda (kv) (funcall f (second kv) :value t)) key-a)
		(when into-nested-namespaces
		  (funcall f expr :value t))))))
			 
    ((list-expr tuple-expr)
     ;; the items within are a target if the list itself is
     (if build-result
	 `(,(first form)
	   ,(loop for x in (second form)
		collect (funcall f x :value value :target target)))
       (loop for x in (second form)
	   do (funcall f x :value value :target target))))
    
    (module-stmt
     (if build-result
	 `(module-stmt ,(funcall f (second form) :value t))
       (funcall f (second form) :value t)))
      
    (print-stmt
     (destructuring-bind (dest items comma?) (cdr form)
       (if build-result
	   `(print-stmt
	     ,(when dest (funcall f dest :value t))
	     ,(mapcar (lambda (x) (funcall f x :value t)) items)
	     ,comma?)
	 (progn (when dest (funcall f dest :value t))
		(mapc (lambda (x) (funcall f x :value t)) items)))))
    
    (raise-stmt
     (destructuring-bind (exc var tb) (cdr form)
       (if build-result
	   `(raise-stmt ,(when exc (funcall f exc :value t))
			,(when var (funcall f var :value t))
			,(when tb  (funcall f var :value t)))
	 (progn (when exc (funcall f exc :value t)
		(when var (funcall f var :value t)
		(when tb  (funcall f var :value t))))))))
    
    (return-stmt
     (if build-result
	 `(return-stmt ,(when (second form)
			  (funcall f (second form) :value t)))
       (when (second form)
	 (funcall f (second form) :value t))))
      
    (slice-expr
     (destructuring-bind (start stop step) (cdr form)
       (if build-result
	   `(slice-expr ,(when start (funcall f start :value t))
			,(when stop  (funcall f stop  :value t))
			,(when step  (funcall f step  :value t)))
	 (progn (when start (funcall f start :value t))
		(when stop  (funcall f stop  :value t))
		(when step  (funcall f step  :value t))))))
      
    (subscription-expr
     (if build-result
	 `(subscription-expr ,(funcall f (second form) :value t)
			     ,(funcall f (third form)  :value t))
       (progn (funcall f (second form) :value t)
	      (funcall f (third form)  :value t))))
      
    (suite-stmt
     (if build-result
	 `(suite-stmt ,(loop for x in (second form) collect (funcall f x)))
       (loop for x in (second form) do (funcall f x))))
      
    (try-except-stmt
     (destructuring-bind (suite except-clauses else-suite) (cdr form)
       (if build-result
	   
	   `(try-except-stmt
	     ,(funcall f suite)
	     (,@(loop for (exc var handler-form) in except-clauses 
		    collect `(,(when exc (funcall f exc :value t))
				 ,(when var (funcall f var :target t))
			       ,(funcall f handler-form))))
	     ,(when else-suite
		(funcall f else-suite)))
	 
	 (progn (funcall f suite)
		(loop for (exc var handler-form) in except-clauses 
		    do (when exc (funcall f exc :value t))
		       (when var (funcall f var :target t))
		       (funcall f handler-form))
		(when else-suite
		  (funcall f else-suite))))))
      
    (try-finally-stmt
     (destructuring-bind (try-suite finally-suite) (cdr form)
       (if build-result
	   `(try-finally-stmt
	     ,(funcall f try-suite) ,(funcall f finally-suite))
	 (progn (funcall f try-suite)
		(funcall f finally-suite)))))
    
    (unary-expr
     (if build-result
	 `(unary-expr ,(second form) ,(funcall f (third form) :value t))
       (funcall f (third form) :value t)))
      
    (while-stmt
     (destructuring-bind (test suite else-suite) (cdr form)
       (if build-result
	   `(while-stmt ,(funcall f test :value t)
			,(funcall f suite)
			,(when else-suite (funcall f else-suite)))
	 (progn (funcall f test :value t)
		(funcall f suite)
		(when else-suite (funcall f else-suite))))))
      
    (yield-stmt
     (if build-result
	 `(yield-stmt ,(funcall f (second form) :value t))
       (funcall f (second form) :value t)))
    
    (t
     (warn "WALK: assuming ~S is a Lisp form: not walked into." form)
     form)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Handy functions for dealing with ASTs

(defmacro with-py-ast ((subform ast &rest options)
					  ;; key value target walk-lists-only 
					  ;; into-nested-namespaces
		       &body body)
  ;; (with-sub-ast ((form &key value target) ast)
  ;;    ... form ... value ... target ...)
  ;; 
  ;; (with-sub-ast (form ast)
  ;;   ... form...)
  
  (let ((context '#:context))
    
    (when (symbolp subform)
      (setf subform `(,subform &rest ,context)))
  
    `(walk-py-ast ,ast
		  (excl:named-function :with-py-ast-function
		    (lambda ,subform
		      (declare (optimize (debug 3))
			       (ignore ,context))
		      ,@body))
		  
		  ,@options ;; user-defined options take precedence...
		  
		  :build-result nil;; but these are the defaults
		  :walk-lists-only t)))


#+(or)
(defmacro match-ast-nodes (ast &rest clauses)
  ;; (match-ast-nodes ast
  ;;	   (x   yield-stmt)           (... x...))
  ;;	   (x   (if-stmt while-stmt)) (... x ..))
  ;;       (nil (print-stmt))         (.....)))
  
  (let ((sub-ast '#:sub-ast))
    
    (flet ((make-cond-clause (c)
	     (destructuring-bind (var nodenames &rest body) c
	       `(,nodenames (let (,(when var
				     `(,var ,sub-ast)))
			      ,@body)))))
      
      `(walk-py-ast ,ast
		    (lambda (,sub-ast)
		      (case (car ,sub-ast)
			,@(mapcar #'make-cond-clause clauses)
			(t (values ,sub-ast))))
		    :walk-lists-only t))))
		    
		    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Walker applications

#+(or)
(defun walk-print (ast &rest walk-options)
  (apply #'walk-py-ast
	 ast
	 (lambda (ast &key value target)
	   (format t "> ~A ~@[[value]~] ~@[[target]~]~%" ast value target)
	   ast)
	 walk-options))

