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
the first value is considered the final form and it is not walked into; it
is inserted as-is in the result.

The initial form is considered an expressions used for its value iff VALUE;
it is considered an assignment target iff TARGET.

If WALK-LISTS-ONLY is false, F will also be called on numbers and strings.
Normally, they are included as-is in the result."

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
      
    (assign-expr
     (assert (not (or value target)))
     `(assign-expr ,(funcall f (second form) :value t)
		   ,(mapcar (lambda (x) (funcall f x :target t)) (third form))))
      
    (attributeref-expr
     `(attributeref-expr ,(funcall f (second form)) ,(third form))) ;; don't recurse on attr name
      
    (augassign-expr
     (assert (not (or value target)))
     `(augassign-expr ,(second form)
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
		   ,(mapcar (lambda (kv) (funcall f (cdr kv) :value t)) k-a)
		   ,(when *-a (funcall f *-a :value t))
		   ,(when **-a (funcall f **-a :value t)))))
      
    (classdef-stmt 
     (assert (not (or target value)))
     (destructuring-bind (cname inheritance suite) (cdr form)
       (assert (eq (car inheritance) 'tuple))
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
       `(funcdef-stmt ,(loop for deco in decorators
			   collect (funcall f deco :value t))
		      ,(funcall f fname :target t)
		      (,pos-args
		       ,(mapcar (lambda (kv) (funcall f (cdr kv) :value t)) key-args)
		       ,*-arg
		       ,**-arg)
		      ,(funcall f suite))))
    
    (generator-expr `(generator-expr
		      ,(funcall f (second form) :value t) ;; XXX value ok...?
		      ,(loop for for/if in (third form) collect
			     (ecase (car for/if)
			       (gen-for-in `(gen-for-in
					     ,(funcall f (second for/if) :target t)
					     ,(funcall f (third  for/if) :value t)))
			       (gen-if `(gen-if ,(funcall (second for/if) :value t)))))))
      
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
		(list-for-in
		 `(list-for-in
		   ,(funcall f (second for/if) :target t)
		   ,(funcall f (third  for/if) :value t)))
		(list-if
		 `(list-if
		   ,(funcall (second for/if) :value t)))))))
    
    (module-stmt
     (assert (not (or value target)))
     `(module-stmt ,(mapcar (lambda (x) (funcall f x)) (second form))))
      
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
;; 
;; Handy functions for dealing with ASTs

(defun map-sub-ast (func ast &key (nodes t) not-inside (recurse t)
				  (same-namespace nil) (check-self t))
  
  ;;  FUNC         : function of three args: (AST &key VALUE TARGET)
  ;;  AST          : Python AST, which is a list
  ;;  
  ;; :NODES        : a symbol or a list of symbols, or T for all
  ;; :NOT-INSIDE   : designator for a list of symbols
  ;;                 (initial AST arg is not checked against this)
  ;; :RECURSE      : recurse into returned forms?
  ;; :SAME-NAMESPACE T <->  :not-inside '(classdef funcdef lambda) [eval]
  ;; :CHECK-SELF : return initial AST arg if it matches?
  
  (check-type ast list) ;; number, string?
  
  (if (symbolp nodes)
      (setf nodes (list nodes))
    (check-type nodes list))
  
  (when same-namespace
    (setf not-inside (append `(list 'classdef 'funcdef 'lambda) not-inside)))

  (let ((initial-ast ast))
    (walk-py-ast ast
		 (lambda (ast &rest context)
		   (let* ((is-match (and (or check-self (not (eq initial-ast ast)))
					 (or (eq nodes t) (member (car ast) nodes))))
			  (do-recurse (and (or (not is-match) recurse)
					   (not (member (car ast) not-inside)))))
		     (when is-match
		       (apply #'funcall func ast context))
		     
		     (if do-recurse
			 ast
		       (values nil t)))) ;; nil is the final value for the walker
		 :walk-lists-only t)))


(defmacro with-sub-ast ((target nodes ast &optional options-list) &body body)
  "Examples:
     (with-sub-ast (cls-ast 'classdef ast (:same-namespace t ...)) ...)
     (with-sub-ast ((expr-ast &key value target) '(binary unary) ast
                       (:same-namespace t :recurse t)) ...)"
  ;; XXX maybe TARGET should be structured differently?
  (let* ((dummy '#:dummy)
	 (target (cond ((symbolp target)          `(,target &rest ,dummy))
		       ((= (length target) 1)     `(,(car target) &rest ,dummy))
		       ((>= (length target) 1)
			(assert (eq (second target) '&key) ()
			  "form: (var) or (var &key ...) where possible keywords are ~
                           :value and :target  (got: ~A)" target)
			(assert (every (lambda (kw)
					 (member kw '(:value :target &allow-other-keys)))
				       (cddr target))
			    () "Got invalid keyword (only :value and :target are supported) ~
                                (got: ~A)" (cddr target))
			`(,(car target) &key ,@(cddr target)
					,@(unless (member '&allow-other-keys (cddr target))
					    '(&allow-other-keys))))
		       (t (error "Malformed with-sub-ast target variable(s) (got: ~A)"
				 target)))))
    
    `(map-sub-ast (lambda ,target
		    ,@(when (member dummy target) 
			`((declare (ignore ,dummy))))
		    ,@body)
		  ,ast
		  :nodes ,nodes ,@options-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Walker applications

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
