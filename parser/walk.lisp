;; -*- package: clpython.parser; readtable: py-ast-readtable -*-
;; 
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Python AST code walker

(in-package :clpython.parser)
(in-syntax *ast-readtable*)

(defvar *walk-lists-only*)
(defvar *walk-build-result*)
(defvar *walk-into-nested-namespaces*)
(defvar *walk-warn-unknown-form* t)

;; These constants are used to indicate the "value" and/or "target" context of a
;; part of the AST. You can rely on them having a "false" value (nil) when the AST
;; is not used in that context, and a "true" value (not necessarily 't) if it is.

(defconstant +normal-target+      t            "A place that is assigned to.")
(defconstant +delete-target+      :delete      "A place that is a deletion target.")
(defconstant +augassign-target+   :augassign   "A place that is an augmented assignment value and target.")
(defconstant +global-decl-target+ :global-decl "A place that is declared global.")
(defconstant +no-target+           nil         "Not an assignment or deletion target.")

(defconstant +normal-value+    t          "An expression that is used for its value.")
(defconstant +augassign-value+ :augassign "A place that is an augmented assignment value and target.")
(defconstant +no-value+        nil        "An expression that is not used for its value.")

(defvar *walk-debug* nil
  "Print every walk step")

(defun walk-py-ast (ast f &key (value +no-value+)
			       (target +no-target+)
			       (lists-only t) 
			       (build-result nil)
			       (into-nested-namespaces nil)
			       (warn-unknown-form *walk-warn-unknown-form*))
  (declare (optimize (debug 3)))
  "Walk recursively through AST, calling F on each statement. The values
returned by F are (optionally) collected, and eventually returned (as AST).

F should have lambda list: (ast &key value target). VALUE indicates the
expression is evaluated for its value. TARGET is T if this is a place to
which a value will be assigned, and it is +WALK-DELETE-TARGET+ when
it is a place that is deleted.

F should return a new form to walk into, derived in some way from the form
it is given. (If F always returns what it got as first argument, the whole
AST will be visited.)

If F returns two values and the second value is true, the form returned as
first value is considered final: it will not be walked into, but will be
included as-is in the new AST to be returned. (When F return NIL as first
value, the second value must be true.)

The initial form AST is considered an expression used for its value iff 
VALUE is true; it is considered an assignment target iff TARGET is true.

When LISTS-ONLY is false, F will also be called on numbers and strings.

When build-result is false, no new AST will be returned, so F is only
called for side effects.

When INTO-NESTED-NAMESPACES is false, walking stops when encountering
CLASSDEF, FUNCDEF or LAMBDA."

  ;; As `module-stmt', the default top-level AST node representing a
  ;; module, is evaluated in a non-value non-target context, that
  ;; context seems a reasonable default for the keyword arguments.

  (when (and lists-only (not (listp ast)))
    (return-from walk-py-ast ast))
	     
  (labels ((walk-py-ast-1 (ast &key value target)
	     (declare (optimize (debug 3)))
	     
	     (when *walk-debug*
               (warn "w> ~A" ast))
             
	     (assert ast () "Attempt to WALK-PY-AST into an AST that is NIL")
	     
	     (when (and lists-only (not (consp ast)))
	       ;; Don't call user function on AST
	       (return-from walk-py-ast-1 ast))
	     
	     ;; Call user function on whole form. The returned values
	     ;; control how we proceed.
	     (multiple-value-bind (ret-ast final-p) 
		 (funcall f ast :value value :target target)
	       
	       (when final-p
		 (return-from walk-py-ast-1 ret-ast))
	       
	       (assert ret-ast ()
		 "User AST func returned NIL (for AST: ~S); that is only allowed ~
                when second value (final-p) is True, but second value was: ~S" ast final-p)
	       
	       (when (or (consp ret-ast) (not lists-only))
		 (return-from walk-py-ast-1
		   (ast-recurse-fun (lambda (ast &key value target)
				      (walk-py-ast-1 ast :value value :target target))
				    ret-ast
				    :value value
				    :target target)))
	       
	       (break "Walking AST: invalid return value ~S (AST: ~S, F: ~S)"
		      ret-ast ast f))))
    
    (let ((*walk-lists-only*   lists-only)
	  (*walk-build-result* build-result)
	  (*walk-into-nested-namespaces* into-nested-namespaces)
	  (*walk-warn-unknown-form* warn-unknown-form))
      
      (walk-py-ast-1 ast :value value :target target))))


(defun ast-recurse-fun (f form &rest context &key value target)
  "Given function F and a FORM (AST), walks into subforms of FORM in the given
VALUE and TARGET context."
  (declare (optimize (debug 3)))
  (assert form () "AST-RECURSE-FUN: got NIL as AST")
  
  (unless (consp form)
    (return-from ast-recurse-fun form))
  
  (macrolet ((make (form)
	       `(if *walk-build-result*
		    ,form
		  (let ((y ,form))
		    (declare (dynamic-extent y)
			     (ignore y)) ;; This way Allegro will optimize away most consing
		    nil))))
    
    (case (car form)
      
      ([assert-stmt]
       (make `([assert-stmt] ,(funcall f (second form) :value +normal-value+)
			     ,(when (third form) (funcall f (third form) :value +normal-value+)))))
      
      ([assign-stmt]
       (make `([assign-stmt] ,(funcall f (second form) :value +normal-value+ :target +no-target+)
			     ,(mapcar (lambda (x) (funcall f x :target +normal-target+))
				      (third form)))))
      
      ([attributeref-expr]
       ;; Don't recurse on attr name
       (make `([attributeref-expr] ,(funcall f (second form) :value +normal-value+) ,(third form))))
    
      ([augassign-stmt]
       (make `(augassign-stmt ,(second form)
			      ;; the place is used as both value and target
			      ,(funcall f (third form)
					:value +augassign-value+
					:target +augassign-target+)
			      ,(funcall f (fourth form) :value +normal-value+))))
      
      ([backticks-expr]
       (make `(backticks-expr ,(funcall f (second form) :value +normal-value+))))
      
      (([binary-expr] [binary-lazy-expr])
       (make `(,(first form) ,(second form)
			     ,(funcall f (third form)  :value +normal-value+)
			     ,(funcall f (fourth form) :value +normal-value+))))
            
      ([break-stmt]
       form) 
      
      ([call-expr]
       (destructuring-bind (primary (p-a k-a *-a **-a)) (cdr form)
	 (make `([call-expr] ,(funcall f primary :value +normal-value+)
			     (,(mapcar (lambda (pos-arg) (funcall f pos-arg :value +normal-value+)) p-a)
			      ,(mapcar (lambda (kv)
					 (list (first kv) (funcall f (second kv) :value +normal-value+)))
				       k-a)
			      ,(when *-a  (funcall f *-a  :value +normal-value+))
			      ,(when **-a (funcall f **-a :value +normal-value+)))))))
      
      ([classdef-stmt] 
       (destructuring-bind (cname inheritance suite) (cdr form)
	 (assert (eq (car inheritance) '[tuple-expr]))
	 (make `([classdef-stmt] ,(funcall f cname :target +normal-target+)
				 ,(funcall f inheritance :value +normal-value+)
				 ,(if *walk-into-nested-namespaces*
				      (funcall f suite)
				    suite)))))
    
      ([comparison-expr] 
       (destructuring-bind (op left right) (cdr form)
	 (make `([comparison-expr] ,op
				   ,(funcall f left  :value +normal-value+)
				   ,(funcall f right :value +normal-value+)))))

      ([continue-stmt]
       form)
      
      ([del-stmt]
       ;; Note the special (true) value for :target
       (make `([del-stmt] ,(funcall f (second form) :target +delete-target+))))
      
      ([dict-expr]
       (make `([dict-expr] ,(loop for (k . v) in (second form)
				collect (cons (funcall f k :value +normal-value+)
					      (funcall f v :value +normal-value+))))))
    
      ([exec-stmt]
       (destructuring-bind (code globals locals) (cdr form)
	 (make `([exec-stmt] ,(funcall f code :value +normal-value+)
			     ,(when globals (funcall f globals :value +normal-value+))
			     ,(when locals  (funcall f locals  :value +normal-value+))))))
    
      ([for-in-stmt]
       (destructuring-bind (targets sources suite else-suite)
	   (cdr form)
	 (make `([for-in-stmt] ,(funcall f targets :target +normal-target+)
			       ,(funcall f sources :value +normal-value+)
			       ,(funcall f suite)
			       ,(when else-suite (funcall f else-suite))))))
    
      ([funcdef-stmt]
       (destructuring-bind (decorators fname (pos-args key-args *-arg **-arg) suite)
	   (cdr form)
	 (make `([funcdef-stmt] ,(loop for deco in decorators
				     collect (funcall f deco :value +normal-value+))
				,(funcall f fname :target +normal-target+)
				(,pos-args
				 ,(mapcar (lambda (kv)
					    (list (first kv)
						  (funcall f (second kv) :value +normal-value+)))
					  key-args)
				 ,*-arg
				 ,**-arg)
				,(if *walk-into-nested-namespaces*
				     (funcall f suite)
				   suite)))))
    
      (([generator-expr] [listcompr-expr])
       (destructuring-bind (item for-in/if-clauses) (cdr form)
	 ;; The order of walking subforms is important: if we started
	 ;; recursing on ITEM now, then the compiler might raise a
	 ;; SyntaxError ("local variable used before assignment")
	 ;; because the variable is bound in a `for-in' or `if' clause.
	 (flet ((clause-handler (for/if)
		  (ecase (car for/if)
		    ([for-in-clause] (destructuring-bind (expr iterable) (cdr for/if)
				       (let* ((rec-iterable (funcall f iterable :value +normal-value+))
					      (rec-expr (funcall f expr :target +normal-target+)))
					 `(:for-in ,rec-expr ,rec-iterable))))
		    ([if-clause] (let* ((test (second for/if)))
				   `(:if ,(funcall f test :value +normal-value+)))))))
	   (let* ((rec-clauses (mapcar #'clause-handler for-in/if-clauses))
		  (rec-item (funcall f item :value +normal-value+)))
	     `(,(car form) ,rec-item ,rec-clauses)))))

      ([global-stmt]
       (let ((names (second form)))
         `(,(car form) ,(funcall f names :target +global-decl-target+))))
      
      ([identifier-expr]
       form)
      
      ([if-stmt]
       (destructuring-bind
	   (clauses else-suite) (cdr form)
	 (make `([if-stmt] ,(loop for (test suite) in clauses
				collect (list (funcall f test :value +normal-value+)
					      (funcall f suite)))
			   ,(when else-suite (funcall f else-suite))))))
    
      ([import-stmt]
       ;; IMPORT statement is not walked into; name handling is a bit tricky.
       (values form t))
    
      ([import-from-stmt]
       ;; IMPORT-FROM statement is not walked into; name handling is a bit tricky.
       (values form t))
     
      ([lambda-expr]
       (destructuring-bind
	   ((pos-a key-a *-a **-a) expr) (cdr form)
	 (make `([lambda-expr] (,pos-a
				,(mapcar (lambda (kv) 
					   (funcall f (second kv) :value +normal-value+))
					 key-a)
				,*-a
				,**-a)
			       ,(if *walk-into-nested-namespaces*
				    (funcall f expr :value +normal-value+)
				  expr)))))
    
      (([list-expr] [tuple-expr])
       ;; The items inside are a target iff the list itself is
       (make `(,(first form)
	       ,(loop for x in (second form)
		    collect (funcall f x :value value :target target)))))
    
      ([module-stmt]
       (make `([module-stmt] ,(funcall f (second form) :value +normal-value+))))
      
      ([pass-stmt]
       form)
      
      ([print-stmt]
       (destructuring-bind (dest items comma?) (cdr form)
	 (make `([print-stmt]
		 ,(when dest (funcall f dest :value +normal-value+))
		 ,(mapcar (lambda (x) (funcall f x :value +normal-value+)) items)
		 ,comma?))))
    
      ([raise-stmt]
       (destructuring-bind (exc var tb) (cdr form)
	 (make `([raise-stmt] ,(when exc (funcall f exc :value +normal-value+))
			      ,(when var (funcall f var :value +normal-value+))
			      ,(when tb  (funcall f var :value +normal-value+))))))
      ([return-stmt]
       (make `([return-stmt] ,(when (second form)
				(funcall f (second form) :value +normal-value+)))))
      
      ([slice-expr]
       (destructuring-bind (start stop step) (cdr form)
	 (make `([slice-expr] ,(when start (funcall f start :value +normal-value+))
			      ,(when stop  (funcall f stop  :value +normal-value+))
			      ,(when step  (funcall f step  :value +normal-value+))))))
      
      ([subscription-expr]
       `([subscription-expr] ,(funcall f (second form) :value +normal-value+)
			     ,(funcall f (third form)  :value +normal-value+)))
      ([suite-stmt]
       `([suite-stmt] ,(loop for x in (second form) collect (funcall f x))))
      
      ([try-except-stmt]
       (destructuring-bind (suite except-clauses else-suite) (cdr form)
	 (make `([try-except-stmt]
		 ,(funcall f suite)
		 (,@(loop for (exc var handler-form) in except-clauses 
			collect `(,(when exc (funcall f exc :value +normal-value+))
				     ,(when var (funcall f var :target +normal-target+))
				   ,(funcall f handler-form))))
		 ,(when else-suite
		    (funcall f else-suite))))))
      
      ([try-finally-stmt]
       (destructuring-bind (try-suite finally-suite) (cdr form)
	 (make `([try-finally-stmt]
		 ,(funcall f try-suite) ,(funcall f finally-suite)))))
    
      ([unary-expr]
       (make `(unary-expr ,(second form) ,(funcall f (third form) :value +normal-value+))))
      
      ([while-stmt]
       (destructuring-bind (test suite else-suite) (cdr form)
	 (make `([while-stmt] ,(funcall f test :value +normal-value+)
			      ,(funcall f suite)
			      ,(when else-suite (funcall f else-suite))))))

      ([with-stmt]
       (destructuring-bind (test var suite) (cdr form)
         (make `([with-stmt] ,(funcall f test :value +normal-value+)
                             ,(when var (funcall f var :target +normal-target+))
                             ,(funcall f suite)))))
       
      ([yield-stmt]
       (make `([yield-stmt] ,(when (second form)
                               (funcall f (second form) :value +normal-value+)))))
    
      (t
       (when (and (symbolp (car form))
                  (not (fboundp (car form)))
		  *walk-warn-unknown-form*)
	 (warn "WALK: assuming ~S is a Lisp form: not walked into." form))
       form))))

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
		  
		  ;; user-supplied options take precedence...
		  ,@options
		  ;; but these are the defaults
		  :build-result nil
		  :lists-only t)))


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
		    

;;;; Printing walker, for debugging

#+(or)
(defun walk-print (ast &rest walk-options)
  (apply #'walk-py-ast
	 ast
	 (lambda (ast &key value target)
	   (format t "> ~A ~@[:value(~S) ~]~@[:target(~S)~]~%" ast value target)
	   ast)
	 walk-options))
