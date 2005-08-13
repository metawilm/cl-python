(in-package :python)

;;; Python AST code walker
;;; 
;;; This module does not depend on other modules, as ASTs are
;;; represented by plain lists.


(defun walk-py-ast (ast f &key (value nil) (target nil) (walk-lists-only t))
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

  (assert (or (listp ast) (null walk-lists-only)))
  
  (labels ((walk-py-ast-1 (ast &rest context)
	     (assert ast)
	     (if (and walk-lists-only (not (listp ast)))
		 
		 ;; Don't call user function on AST
		 ast
	       
	       ;; Call user function on whole form. The returned values
	       ;; control how we proceed.
	       (multiple-value-bind (ret-ast final-p) 
		   (apply f ast context)
		 
		 (cond (final-p         ret-ast)
		       
		       ((or (listp ret-ast) (not walk-lists-only))
			(apply #'ast-recurse-fun
			       (lambda (ast &key value target)
				 (walk-py-ast-1 ast :value value :target target))
			       ret-ast
			       context))
		       
		       (t (break "Walking AST: invalid return value ~A for ~A with ~A"
				 ret-ast ast f)))))))
    
    (walk-py-ast-1 ast :value value :target target)))


(defun ast-recurse-fun (f form &rest context &key value target)
  "Given function F and a FORM (AST), walks into subforms of FORM in the given
VALUE and TARGET context."
  (declare (optimize (debug 3)))
  (assert (and form (listp form)))
  (case (car form)
      
    (assert-stmt
     (assert (not (or value target)))
     `(assert-stmt ,(funcall f (second form) :value t) ,(funcall f (third form))))
      
    (assign-stmt
     (assert (not (or value target)))
     `(assign-stmt ,(funcall f (second form) :value t)
		   ,(mapcar (lambda (x) (funcall f x :target t)) (third form))))
      
    (attributeref-expr
     `(attributeref-expr ,(funcall f (second form)) ,(third form))) ;; don't recurse on attr name
      
    (augassign-stmt
     (assert (not (or value target)))
     `(augassign-stmt ,(second form)
		      ,(funcall f (third form) :value t :target t) ;; is both v and tg
		      ,(funcall f (fourth form))))
      
    (backticks-expr
     (assert (not target))
     `(backticks ,(funcall f (second form) :value t)))
      
    ((binary-expr binary-lazy-expr)
     (assert (not target))
     `(,(first form) ,(second form)
		     ,(funcall f (third form) :value t)
		     ,(funcall f (fourth form) :value t)))
      
    ((break-stmt continue-stmt global-stmt identifier-expr pass-stmt) 
     form)
      
    (call-expr
     (assert (not target))
     (destructuring-bind (primary (p-a k-a *-a **-a)) (cdr form)
       `(call-expr ,(funcall f primary :value t)
		   ,(mapcar (lambda (pos-arg) (funcall f pos-arg :value t)) p-a)
		   ,(mapcar (lambda (kv) (list (first kv) (funcall f (second kv) :value t)))
			    k-a)
		   ,(when *-a (funcall f *-a :value t))
		   ,(when **-a (funcall f **-a :value t)))))
      
    (classdef-stmt 
     (break "classdef-stmt ~A" form)
     (assert (not (or target value)))
     (destructuring-bind (cname inheritance suite) (cdr form)
       (assert (eq (car inheritance) 'tuple-expr))
       `(classdef-stmt ,(funcall f cname :target t)
		       ,(funcall f inheritance :value t)
		       ,(funcall f suite))))
      
    (comparison-expr  
     (assert (not target))
     `(comparison-expr ,(second form)
		       ,(funcall f (third form) :value t)
		       ,(funcall f (fourth form) :value t)))
      
    (del-stmt
     (assert (not (or target value)))
     `(del-stmt ,(funcall f (second form) :target t)))
      
    (dict-expr
     (assert (not target))
     `(dict ,(loop for (k . v) in (second form)
		 collect (cons (funcall f k :value t)
			       (funcall f v :value t)))))
    
    (exec-stmt
     (destructuring-bind (code globals locals) (cdr form)
       `(exec-stmt ,(funcall f code    :value t)
		   ,(when globals (funcall f globals :value t))
		   ,(when locals (funcall f locals  :value t)))))
    
    (for-in-stmt
     (assert (not (or value target)))
     (destructuring-bind (targets sources suite else-suite)
	 (cdr form)
       `(for-in-stmt ,(funcall f targets :target t)
		     ,(funcall f sources :value t)
		     ,(funcall f suite)
		     ,(when else-suite (funcall f else-suite)))))
      
    (funcdef-stmt
     (assert (not (or target value)))
     (destructuring-bind (decorators fname (pos-args key-args *-arg **-arg) suite)
	 (cdr form)
       (break "WALK-PY-AST recursing into funcdef, allright?  ~A" form)
       ;; XXX check compiler, that always recursing here is intended
       `(funcdef-stmt ,(loop for deco in decorators
			   collect (funcall f deco :value t))
		      ,(funcall f fname :target t)
		      (,pos-args
		       ,(mapcar (lambda (kv) (funcall f (cdr kv) :value t)) key-args)
		       ,*-arg
		       ,**-arg)
		      ,suite))) ;; was: funcall on suite 2005.08.10
    
    (generator-expr `(generator-expr
		      ,(funcall f (second form) :value t) ;; XXX value ok...?
		      ,(loop for for/if in (third form) collect
			     (ecase (car for/if)
			       (for-in `(for-in
					     ,(funcall f (second for/if) :target t)
					     ,(funcall f (third  for/if) :value t)))
			       (if `(if ,(funcall (second for/if) :value t)))))))
      
    (if-stmt
     (assert (not (or value target)))
     (destructuring-bind
	 (clauses else-suite) (cdr form)
       `(if-stmt ,(loop for (test suite) in clauses
		      collect (list (funcall f test :value t)
				    (funcall f suite)))
		 ,(when else-suite (funcall f else-suite)))))
      
    (import-stmt
     (assert (not (or value target)))
     `(import-stmt ,(loop for clause in (second form) collect
			  (ecase (first clause)
			    (as `(as ,(second clause)
				     ,(funcall f (third clause)
					       :target t)))
			    (not-as  ;; TODO: attributeref
			     (error
			      "import walk not-as: todo"))))))
      
    (import-from-stmt
     (assert (not (or value target)))
     ;; Because of the special machinery behind `import',
     ;; don't treat the source package name as value. XXX
     `(import-from-stmt ,(second form)
			,(loop for (as src dest) in (third form)
			     do (assert (eq as 'as))
				(assert (eq (first dest) 'identifier))
			     collect `(as ,src ,(funcall f dest :target t)))))
      
    (lambda-expr
     (assert (not target))
     (destructuring-bind
	 ((pos-a key-a *-a **-a) expr) (cdr form)
       `(lambda-expr (,pos-a
		      ,(mapcar (lambda (kv) (funcall f (cdr kv) :value t)) key-a)
		      ,*-a
		      ,**-a)
		     ,(funcall f expr :value t))))
      
    ((list-expr tuple-expr)
     ;; the items within are a target if the list itself is
     `(,(first form)
       ,(loop for x in (second form) collect
	      (funcall f x :value value :target target))))
      
    (listcompr-expr
     (assert (not target))
     `(listcompr-expr
       ,(funcall f (second form) :value t) ;; XXX value ok...?
       ,(loop for for/if in (third form) collect
	      (ecase (car for/if)
		(for-in
		 `(for-in
		   ,(funcall f (second for/if) :target t)
		   ,(funcall f (third  for/if) :value t)))
		(if
		 `(if
		   ,(funcall (second for/if) :value t)))))))
    
    (module-stmt
     (assert (not (or value target)))
     `(module-stmt ,(funcall (second form))))
      
    (print-stmt
     (assert (not (or target value)))
     (destructuring-bind (dest items comma?) (cdr form)
       `(print-stmt 
	 ,(when dest (funcall f dest :value t))
	 ,(mapcar (lambda (x) (funcall f x :value t)) items)
	 ,comma?)))
    
    (raise-stmt
     (assert (not (or target value)))
     (destructuring-bind (exc var tb) (cdr form)
       `(raise-stmt ,(when exc (funcall f exc :value t))
		    ,(when var (funcall f var :value t))
		    ,(when tb  (funcall f var :value t)))))
      
    (return-stmt
     (assert (not (or target value)))
     `(return-stmt ,(when (second form)
		      (funcall f (second form) :value t))))
      
    (slice-expr
     (assert (not target))
     (destructuring-bind (start stop step) (cdr form)
       `(slice-expr ,(when start (funcall f start :value t))
		    ,(when stop  (funcall f stop  :value t))
		    ,(when step  (funcall f step  :value t)))))
      
    (subscription-expr
     `(subscription-expr ,(funcall f (second form) :value t)
			 ,(funcall f (third form)  :value t)))
      
    (suite-stmt
     (assert (not (or value target)))
     `(suite-stmt ,(loop for x in (second form)
		       collect (funcall f x))))
      
    (try-except-stmt
     (assert (not (or value target)))
     (destructuring-bind
	 (suite except-clauses else-suite) (cdr form)
       `(try-except-stmt
	 ,(funcall f suite)
	 (,@(loop for (exc var handler-form) 
		in except-clauses collect
		  `(,(when exc (funcall f exc :value t))
		       ,(when var (funcall f var :target t))
		     ,(funcall f handler-form))))
	 ,(when else-suite
	    (funcall f else-suite)))))
      
    (try-finally-stmt
     (assert (not (or value target)))
     (destructuring-bind (try-suite finally-suite) (cdr form)
       `(try-finally-stmt
	 ,(funcall f try-suite) ,(funcall f finally-suite))))
      
    (unary-expr
     (assert (not target))
     `(unary-expr ,(second form) ,(funcall f (third form) :value t)))
      
    (while-stmt
     (assert (not (or value target)))
     (destructuring-bind (test suite else-suite) (cdr form)
       `(while-stmt ,(funcall f test :value t)
		    ,(funcall f suite)
		    ,(when else-suite (funcall f else-suite)))))
      
    (yield-stmt
     (assert (not (or value target)))
     `(yield-stmt ,(funcall f (second form) :value t)))
    
    (t
     (warn "WALK: assuming ~S is a Lisp form: not walked into." form)
     form)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Handy functions for dealing with ASTs

(defmacro with-py-ast ((target ast) &body body)
  "Example: (with-sub-ast ((form &key value target) ast t)"
  `(walk-py-ast ,ast
		(excl:named-function :with-py-ast-function
		  (lambda ,target
		    ,@body))))
		  


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

	       

#+(or)
(defun guess-annotate-class-attributes (ast)
  (with-sub-ast (classdef-ast 'classdef ast)
    (with-sub-ast (funcdef-ast 'funcdef classdef-ast (:same-namespace t))
      (when (has_>=1_args)
        (let ((first-arg-name (first xxx)))
          (with-sub-ast (attr-ref-ast 'attribute-ref funcdef-ast (:same-namespace t))
            (when (eq (cadr attr-ref-ast) first-arg-name)
              (register-attribute (cadr attr-ref-ast) classdef-ast))))))))

;;;
;; Derive from the AST the likely attributes of class instances, by
;; looking for assignments of the form `self.x = y' in the body of
;; methods that take `self' as first arg, inside classes.

#+(or)
(defun likely-class-attribs (ast)
  (with-sub-ast (cls-ast class ast (:recurse-results t :check-self nil))
    (with-sub-ast ((expr-ast &key value target) (binary unary) ast (:same-namespace t))
      (do-something cls-ast meth-ast))))
		   
#+(or)
(defun likely-class-attribs (ast)
  (with-sub-ast (cls-ast class ast (:same-namespace t :recurse-results t :check-self nil))
    (do-something cls-ast)))


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
		     (when (eq (first form) 'assign-stmt)
		       (let ((assign-stmt form))
			 #+(or)(warn "    assign-stmt: ~A" assign-stmt)
			 
			 (walk-py-ast
			  form
			  (lambda (form &key value target)
			    (declare (ignore value))
			    (when (and (eq (first form) 'attributeref)
				       (listp (second form))
				       (eq (second (second form)) self-param)
				       target)
			      (format t "in method ~A.~A(~A, ...): ~A~%"
				      class-name meth-name self-param (ast->python-code assign-stmt)))
			    form))))
		     form)))))
	    form))))
     form)))
