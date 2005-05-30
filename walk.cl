(in-package :python)

;;; Walking the Python AST
;;; 
;;; This module does not depend on other modules; it's working on ASTs
;;; represented by plain lists.

(defun walk-py-ast (ast f &key (value nil) (target nil) (walk-lists-only t))
  "Walk recursively through AST, calling F on each statement. F should
return a (possibly new) form to walk through, derived in some way
from the form it's given). The collected results are returned as a new AST.

When F returns two values and the second value is T, the form returned in
the first value is considered the final form and it is not walked into; it
is collected as-is.

The initial form is considered an expressions used for its value iff VALUE;
it is considered an assignment target iff TARGET.

If WALK-LISTS-ONLY, F is not called on numbers and strings."

  ;; `file-input', the default top-level AST node representing a
  ;; module, is evaluated in a non-value, non-target context. That
  ;; seems a reasonable default choice for the keyword arguments.

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
			(apply #'ast-recurse-fun (lambda (ast &rest context)
						   (walk-py-ast-1 ast f context))
			       ret-ast
			       context))
		       
		       (t (break "Walking AST: invalid return value ~A for ~A with ~A"
				 ret-ast ast f)))))))
    
    (walk-py-ast-1 ast :value value :target target)))


(defun ast-recurse-fun (f form &rest context &key value target)
  "Given function F and ast FORM, walks into subforms of FORM in the given
VALUE and TARGET context."
  (declare (optimize (debug 3)))
  (assert (listp form))
  (ecase (car form)
      
    (assert
	(assert (not (or value target)))
	`(assert ,(funcall f (second form) :value t) ,(funcall f (third form))))
      
    (assign-expr
     (assert (not (or value target)))
     `(assign-expr ,(funcall f (second form) :value t)
		   ,(mapcar (lambda (x) (funcall f x :target t)) (third form))))
      
    (attributeref
     `(attributeref ,(funcall f (second form)) ,(funcall f (third form))))
      
    (augassign-expr
     (assert (not (or value target)))
     `(augassign-expr ,(second form)
		      ,(funcall f (third form) :value t :target t) ;; both v,tg
		      ,(funcall f (fourth form))))
      
    (backticks
     (assert (not target))
     `(backticks ,(funcall f (second form) :value t)))
      
    ((binary binary-lazy)
     (assert (not target))
     `(,(first form) ,(second form)
		     ,(funcall f (third form) :value t)
		     ,(funcall f (fourth form) :value t)))
      
    ((break continue global identifier pass) 
     form)
      
    (call  
     (assert (not target))
     `(call ,(funcall f (second form) :value t)
	    ,(loop for arg in (third form)
		 collect (ecase (first arg)
			   ((pos * **)
			    `(,(first arg) ,(funcall f (second arg) :value t)))
			     
			   (key  ;; in a call with key args, the key names are not targets
			    `(key ,(second arg)
				  ,(funcall f (third arg) :value t)))))))
      
    (classdef 
     (assert (not (or target value)))
     (destructuring-bind (cname inheritance suite) (cdr form)
       (assert (eq (car inheritance) 'tuple))
       `(classdef ,(funcall f cname :target t)
		  ,(funcall f inheritance :value t)
		  ,(funcall f suite))))
      
    (comparison  
     (assert (not target))
     `(comparison ,(second form)
		  ,(funcall f (third form) :value t)
		  ,(funcall f (fourth form) :value t)))
      
    (del
     (assert (not (or target value)))
     `(del ,(funcall f (second form) :target t)))
      
    (dict
     (assert (not target))
     `(dict ,(loop for (k . v) in (second form)
		 collect (cons (funcall f k :value t)
			       (funcall f v :value t)))))
      
    (file-input
     (assert (not (or value target)))
     `(file-input ,(mapcar (lambda (x) (funcall f x)) (second form))))
      
    (for-in
     (assert (not (or value target)))
     (destructuring-bind (targets sources suite else-suite)
	 (cdr form)
       `(for-in ,(funcall f targets :target t)
		,(funcall f sources :value t)
		,(funcall f suite)
		,(when else-suite (funcall f else-suite)))))
      
    (funcdef
     (assert (not (or target value)))
     (destructuring-bind (fname (pos-key-params *-param **-param) suite)
	 (cdr form)
       `(funcdef ,(funcall f fname :target t)
		 (,(loop for p in pos-key-params
		       if (consp p) ;; keyword
		       collect (cons (car p) (funcall f (cdr p) :value t))
		       else collect p) ;; positional
		   ,*-param
		   ,**-param)
		 ,(funcall f suite))))
      
    (if
	(assert (not (or value target)))
	(destructuring-bind
	    (clauses else-suite) (cdr form)
	  `(if ,(loop for (test suite) in clauses
		    collect (list (funcall f test :value t)
				  (funcall f suite)))
	       ,(when else-suite (funcall f else-suite)))))
      
    (import
     (assert (not (or value target)))
     `(import ,(loop for clause in (second form) collect
		     (ecase (first clause)
		       (as `(as ,(second clause)
				,(funcall f (third clause)
					  :target t)))
		       (not-as  ;; TODO: attributeref
			(error
			 "import walk not-as: todo"))))))
      
    (import-from
     (assert (not (or value target)))
     ;; Because of the special machinery behind `import',
     ;; don't treat the source package name as value. XXX
     `(import-from ,(second form)
		   ,(loop for (as src dest) in (third form)
			do (assert (eq as 'as))
			   (assert (eq (first dest) 'identifier))
			collect `(as ,src ,(funcall f dest :target t)))))
      
    (lambda
	(assert (not target))
      (destructuring-bind
	  ((pos-key-params *-param **-param) suite) (cdr form)
	`(lambda (,(loop for p in pos-key-params
		       if (consp p) collect
			 (cons (car p)
			       (funcall f (cdr p) :value t))
		       else collect p) ;; positional
		   ,*-param
		   ,**-param)
	   ,(funcall f suite :value t))))
      
    ((list tuple)
     ;; the items within are target if the list itself is
     `(,(first form)
       ,(loop for x in (second form) collect
	      (funcall f x :value value :target target))))
      
    (list-compr
     (assert (not target))
     `(list-compr
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
      
    (print
     (assert (not (or target value)))
     `(print ,(loop for x in (second form)
		  collect (funcall f x :value t))
	     ,(third form)))
      
    (print->>      
     (assert (not (or target value)))
     `(print->> ,(funcall f (second form) :value t)
		,(loop for x in (third form)
		     collect (funcall f x :value t))
		,(fourth form)))
      
    (raise
     (assert (not (or target value)))
     (destructuring-bind (exc var tb) (cdr form)
       `(raise ,(when exc (funcall f exc :value t))
	       ,(when var (funcall f var :value t))
	       ,(when tb  (funcall f var :value t)))))
      
    (return
      (assert (not (or target value)))
      `(return ,(when (second form)
		  (funcall f (second form) :value t))))
      
    (slice
     (assert (not target))
     (destructuring-bind (start stop step) (cdr form)
       `(slice ,(when start (funcall f start :value t))
	       ,(when stop  (funcall f stop :value t))
	       ,(when step  (funcall f step :value t)))))
      
    (subscription
     `(subscription ,(funcall f (second form) :value t)
		    ,(funcall f (third form)  :value t)))
      
    (suite
     (assert (not (or value target)))
     `(suite ,(loop for x in (second form)
		  collect (funcall f x))))
      
    (try-except
     (assert (not (or value target)))
     (destructuring-bind
	 (suite except-clauses else-clause) (cdr form)
       `(try-except
	 ,(funcall f suite)
	 (,@(loop for (exc var handler-form) 
		in except-clauses collect
		  `(,(when exc (funcall f exc :value t))
		       ,(when var (funcall f var :target t))
		     ,(funcall f handler-form))))
	 ,(when else-clause
	    (funcall f else-clause)))))
      
    (try-finally
     (assert (not (or value target)))
     (destructuring-bind (try-suite finally-suite) (cdr form)
       `(try-finally
	 ,(funcall f try-suite) ,(funcall f finally-suite))))
      
    (unary
     (assert (not target))
     `(unary ,(second form) ,(funcall f (third form) :value t)))
      
    (while
	(assert (not (or value target)))
      (destructuring-bind (test suite else-suite) (cdr form)
	`(while ,(funcall f test :value t)
	   ,(funcall f suite)
	   ,(when else-suite (funcall f else-suite)))))
      
    (yield
     (assert (not (or value target)))
     `(yield ,(funcall f (second form) :value t)))))



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
  ;; XXX maybe there are better ways to deal with the various target forms
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
		       (t (error "Malformed with-sub-ast target variable(s) (got: ~A)" target)))))
    
    `(map-sub-ast (lambda ,target
		    ,@(when (member dummy target) 
			`((declare (ignore ,dummy))))
		    ,@body)
		  ,ast
		  :nodes ,nodes ,@options-list)))


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


