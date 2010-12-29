;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.PARSER; Readtable: PY-AST-READTABLE -*-
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
(defconstant-once +all-values-names+ '(+normal-value+ +augassign-value+ +no-value+))

(defvar *walk-debug* nil
  "Print every walk step")

(defun walk-py-ast (ast f &key (value +no-value+)
			       (target +no-target+)
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

When build-result is false, no new AST will be returned, so F is only
called for side effects.

When INTO-NESTED-NAMESPACES is false, walking stops when encountering
CLASSDEF, FUNCDEF or LAMBDA."

  ;; As `module-stmt', the default top-level AST node representing a
  ;; module, is evaluated in a non-value non-target context, that
  ;; context seems a reasonable default for the keyword arguments.

  (labels ((walk-py-ast-1 (ast &key value target)
	     (declare (optimize (debug 3)))
             
             (when *walk-debug*
               (warn "w> ~A" ast))
             (unless ast
               (break "Attempt to WALK-PY-AST into an AST that is NIL"))
             
             ;; Call user function on whole form. The returned values
	     ;; control how we proceed.
             (multiple-value-bind (ret-ast final-p) 
		 (funcall f ast :value value :target target)
               
               (when final-p
		 (return-from walk-py-ast-1 ret-ast))
               
               (assert ret-ast ()
		 "User AST func returned NIL (for AST: ~S); that is only allowed ~
                when second value (final-p) is True, but second value was: ~S" ast final-p)
	       
               (return-from walk-py-ast-1
                 (ast-recurse-fun (lambda (ast &key value target)
                                    (walk-py-ast-1 ast :value value :target target))
                                  ret-ast
                                  :value value
                                  :target target)))))
    
    (let ((*walk-build-result* build-result)
	  (*walk-into-nested-namespaces* into-nested-namespaces)
	  (*walk-warn-unknown-form* warn-unknown-form))
      
      (walk-py-ast-1 ast :value value :target target))))

(defparameter *ast-node-walk-structs* (make-hash-table :test 'eq))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (ast-walker-node (:conc-name awn-))
    arg-structs targetable subtargetable)
  
  (defmethod make-load-form ((x ast-walker-node) &optional env)
    (make-load-form-saving-slots x :environment env))
  
  (defstruct (ast-walker-node-arg (:conc-name awna-))
    var ix prio walk optional-p rest-p key tg/val)
  
  (defmethod make-load-form ((x ast-walker-node-arg) &optional env)
    (make-load-form-saving-slots x :environment env))
  
  (defparameter *allowed-node-tg/val* '(+normal-value+ +normal-target+ +suite+
                                        +namespace-suite+ +global-decl-target+
                                        +augassign-target/value+ +delete-target+))
) ;; eval-when

(defmacro def-ast-node (node args &rest options)
  (flet ((parse-clause (x ix)
           (flet ((malformed (&optional msg) 
                    (error "Malformed clause for ~A~@[ (~A)~]: ~S." node msg args)))
             (let ((prio (+ 1000 ix))
                   (walk t)
                   (optional-p nil)
                   (rest-p nil)
                   var key tg/val)
               (if (symbolp x)
                   (setf walk nil)
                 (progn (loop (if (null x) ;; prefix, varname
                                  (malformed)
                                (case (car x)
                                  (&optional (setf optional-p t) (pop x))
                                  (&rest     (setf rest-p t
                                                   optional-p t)
                                             (pop x))
                                  (t         (setf var (pop x)) (return)))))
                        (when (eq (car x) '&key) ;; key
                          (pop x)
                          (setf key (or (pop x) (malformed)))
                          (unless (eq key 'second)
                            (malformed (format nil "key ~A not allowed; should be: ~S" key 'second))))
                        (unless (= (length x) 1) ;; target/value
                          (malformed))
                        (if (member (car x) *allowed-node-tg/val*)
                            (setf tg/val (car x))
                          (malformed (format nil "unknown target/value ~S; should be one of ~A"
                                             (car x) *allowed-node-tg/val*)))))
               ;; Args are normally evaluated from left to right, but this can be overruled
               ;; by indicating order in arg name suffixes: "foo=1", "bar=2", etc.
               (whereas ((pos (position #\= (symbol-name var) :test 'char=)))
                 (setf prio (parse-integer (symbol-name var) :start (1+ pos))))
               (make-ast-walker-node-arg :var var
                                         :ix ix
                                         :prio prio
                                         :optional-p optional-p
                                         :rest-p rest-p
                                         :key key
                                         :tg/val tg/val
                                         :walk walk)))))
    (let ((structs (loop for a in args and i from 0
                       collect (parse-clause a i) into s
                       finally (return (sort s #'< :key #'awna-prio))))
          (targetable (second (assoc :targetable options)))
          (subtargetable (second (assoc :subtargetable options))))
      `(setf (gethash ',node *ast-node-walk-structs*)
         ,(make-ast-walker-node :arg-structs structs
                                :targetable targetable
                                :subtargetable subtargetable)))))


;;; AST node definitions

(def-ast-node [assert-stmt] ((test +normal-value+) (&optional raise-arg +normal-value+)))
(def-ast-node [assign-stmt] ((value +normal-value+) (&rest targets +normal-target+)))
(def-ast-node [attributeref-expr] ((item +normal-value+) attr) (:targetable t) (:subtargetable nil))
(def-ast-node [augassign-stmt] (op (place +augassign-target/value+) (val +normal-value+)))
(def-ast-node [backticks-expr] ((val +normal-value+)))
(def-ast-node [binary-expr]      (op (left +normal-value+) (right +normal-value+)))
(def-ast-node [binary-lazy-expr] (op (left +normal-value+) (right +normal-value+)))
(def-ast-node [break-stmt] ())
(def-ast-node [bracketed-expr] ((item +normal-value+)))

(def-ast-node [call-expr] ((primary +normal-value+) (&rest pos-arg +normal-value+)
                                                    (&rest kv &key second +normal-value+)
                                                    (&optional *-a +normal-value+)
                                                    (&optional **-a +normal-value+)))

#+(or) ;; nested namespace
(def-ast-node [classdef-stmt] (name +normal-target+) (inheritance +normal-value+) (suite +namespace-suite+))

(def-ast-node [comparison-expr] (op (left +normal-value+) (right +normal-value+) brackets))
(def-ast-node [continue-stmt] ())
(def-ast-node [del-stmt] ((item +delete-target+)))
(def-ast-node [dict-expr] ((&rest vk-list +normal-value+)))
(def-ast-node [exec-stmt] ((code +normal-value+) (&optional globals +normal-value+) (&optional locals +normal-value+)))
(def-ast-node [for-in-stmt] ((target=2 +normal-target+) (sources=1 +normal-value+) (suite +suite+)
                                                        (&optional else-suite +suite+)))

#+(or) ;; hairy: keyword default args
(def-ast-node [funcdef-stmt] (&rest decorators=2 +normal-value+) (fname=3 +normal-target+)
              (pos-args (&rest key-args=1 &key second +normal-value+) *-arg **-arg)
              +namespace-suite+)

;; [generator-expr] item for-in/if-clauses

(def-ast-node [global-stmt] ((names +global-decl-target+)))
(def-ast-node [identifier-expr] (name) (:targetable t))
(def-ast-node [if-expr] ((condition +normal-value+) (then +normal-value+) (else +normal-value+)))

;; [if-stmt] (clauses else-suite)
;; [import-stmt] ...
;; [import-from-stmt] ...
;; [lambda-expr] ...

(def-ast-node [list-expr]  ((&rest names +normal-value+)) (:targetable t) (:subtargetable t))

;; Literal value itself (number, string) is not walked into.
(def-ast-node [literal-expr]  (kind value))

;; listcompr-expr : hairy

(def-ast-node [tuple-expr] ((&rest names +normal-value+)) (:targetable t) (:subtargetable t))

(def-ast-node [module-stmt] ((body +normal-value+)))

(def-ast-node [pass-stmt] ())
(def-ast-node [print-stmt] ((&optional dest +normal-value+) (&rest items +normal-value+) comma?))
(def-ast-node [raise-stmt] ((&optional exc +normal-value+) (&optional var +normal-value+) (&optional tb +normal-value+)))
(def-ast-node [return-stmt] ((&optional value +normal-value+)))
(def-ast-node [slice-expr] ((&optional start +normal-value+) (&optional stop +normal-value+) (&optional step +normal-value+)))

(def-ast-node [subscription-expr] ((prim +normal-value+) (sub +normal-value+)) (:targetable t) (:subtargetable nil))
(def-ast-node [suite-stmt] ((&rest suite-stmts +normal-value+)))

;; [try-except-stmt]

(def-ast-node [try-finally-stmt] ((try-suite +suite+) (finally-suite +suite+)))

(def-ast-node [unary-expr] (op (val +normal-value+)))
(def-ast-node [while-stmt] ((test +normal-value+) (suite +suite+) (&optional else-suite +suite+)))
(def-ast-node [with-stmt]  ((test +normal-value+) (&optional var +normal-target+) (suite +suite+)))
(def-ast-node [yield-expr] ((val +normal-value+)))
(def-ast-node [yield-stmt] ((&optional val +normal-value+)))


(defun ast-recurse-fun (f ast &key value target)
  (when (null ast) 
    (break "AST-RECURSE-FUN got NIL"))
  
  (unless (consp ast)
    (return-from ast-recurse-fun ast))
  
  (case (car ast)
    
    ([classdef-stmt] 
     (destructuring-bind (cname inheritance suite) (cdr ast)
       (assert (eq (car inheritance) '[tuple-expr]))
       `([classdef-stmt] ,(funcall f cname :target +normal-target+)
                         ,(funcall f inheritance :value +normal-value+)
                         ,(if *walk-into-nested-namespaces* (funcall f suite) suite))))
    
    ([funcdef-stmt] (destructuring-bind (decorators fname (pos-args key-args *-arg **-arg) suite)
                        (cdr ast)
                      `([funcdef-stmt] ,(loop for deco in decorators
                                            collect (funcall f deco :value +normal-value+))
                                       ,(funcall f fname :target +normal-target+)
                                       (,pos-args
                                        ,(mapcar (lambda (kv)
                                                   (list (first kv)
                                                         (funcall f (second kv) :value +normal-value+)))
                                                 key-args)
                                        ,*-arg
                                        ,**-arg)
                                       ,(if *walk-into-nested-namespaces* (funcall f suite) suite))))
    
    (([generator-expr] [listcompr-expr])
     (destructuring-bind (item for-in/if-clauses) (cdr ast)
       ;; The order of walking subforms is important: if we started
       ;; recursing on ITEM now, then the compiler might raise a
       ;; SyntaxError ("local variable used before assignment")
       ;; because the variable is bound in a `for-in' or `if' clause.
       (flet ((clause-handler (for/if)
                (ecase (car for/if)
                  ([for-in-clause] (destructuring-bind (expr iterable) (cdr for/if)
                                     (let* ((rec-iterable (funcall f iterable :value +normal-value+))
                                            (rec-expr (funcall f expr :target +normal-target+)))
                                       `(,(car for/if) ,rec-expr ,rec-iterable))))
                  ([if-clause] (let* ((test (second for/if)))
                                 `(,(car for/if) ,(funcall f test :value +normal-value+)))))))
         (let* ((rec-clauses (mapcar #'clause-handler for-in/if-clauses))
                (rec-item (funcall f item :value +normal-value+)))
           `(,(car ast) ,rec-item ,rec-clauses)))))
    
    ([if-stmt] (destructuring-bind (clauses else-suite) (cdr ast)
                 `([if-stmt] ,(loop for (test suite) in clauses
                                  collect (list (funcall f test :value +normal-value+)
                                                (funcall f suite)))
                             ,(when else-suite (funcall f else-suite)))))
    
    ([import-stmt]
     ;; A bit tricky: this statement binds names, but does not contain identifier-expr. (Maybe it should contain.)
     ;;   "import ... as foo" binds "foo"
     ;;   "import x.y.z" binds "x"
     (loop with items = (second ast)
         for (mod-name-as-list bind-name) in items
         for id-sym = (or bind-name (car mod-name-as-list))
         for id-expr = `([identifier-expr] ,id-sym)
         do (funcall f id-expr :target +normal-target+))
     (values ast t)) ;; No possibility to modify AST: original is returned.
    
    ([import-from-stmt]
     ;; A bit tricky: the module from which items are imported is not a variable reference.
     ;; Also, "from foo import *" imports all items of foo that exist at runtime... which we don't know now.
     (destructuring-bind (mod-name-as-list items) (cdr ast)
       (declare (ignore mod-name-as-list))
       (cond ((eq items '[*])
              ;; Some unknown identifiers will be set... can't do much with F here.
              )
             (t (loop for (item bindname) in items
                    for id-sym = (or bindname item)
                    for id-expr = `([identifier-expr] ,id-sym)
                    do (funcall f id-expr :target +normal-target+)))))
     ;; No possibility to modify AST: original is returned.
     (values ast t))
    
    ([lambda-expr]
     (destructuring-bind ((pos-a key-a *-a **-a) expr) (cdr ast)
       `([lambda-expr] (,pos-a
                        ,(mapcar (lambda (kv) 
                                   (funcall f (second kv) :value +normal-value+))
                                 key-a)
                        ,*-a
                        ,**-a)
                       ,(if *walk-into-nested-namespaces*
                            (funcall f expr :value +normal-value+)
                          expr))))
    
    ([literal-expr] 
     (values ast t))
    
    ([try-except-stmt]
     (destructuring-bind (suite except-clauses else-suite) (cdr ast)
       `([try-except-stmt]
         ,(funcall f suite)
         (,@(loop for (exc var handler-form) in except-clauses 
                collect `(,(when exc (funcall f exc :value +normal-value+))
                             ,(when var (funcall f var :target +normal-target+))
                           ,(funcall f handler-form))))
         ,(when else-suite
            (funcall f else-suite)))))
    
    (t 
     (cond ((and (symbolp (car ast))
                 (eq (car ast) (find-symbol (symbol-name (car ast)) :clpython.ast.node)))
            (ast-recurse-fun-with-structs f ast :value value :target target))
           ((and (symbolp (car ast))
                 (not (eq (symbol-package (car ast)) (find-package :clpython.ast.node)))
                 (not (fboundp (car ast)))
                 *walk-warn-unknown-form*)
            (warn "WALK: assuming ~S is a Lisp form: not walked into." ast))
           (t
            ast)))))

(defun ast-recurse-fun-with-structs (f ast &key value target)
  (destructuring-bind (node . args) ast
    (multiple-value-bind (walker-node present-p)
        (gethash node *ast-node-walk-structs*)
      (unless present-p
        (error "No walker structs defined for node ~S." node))
      
      (flet ((walk-arg (ast awna subtargetable)
               #+(or)(warn "walk-arg: ~A  rest=~A key=~A" ast (awna-rest-p awna) (awna-key awna))
               #+(or)(setf ast (copy-tree ast))
               (unless (awna-walk awna)
                 (return-from walk-arg ast))
               (when (null ast)
                 (if (awna-optional-p awna)
                     (return-from walk-arg nil)
                   (error "Required argument ~A for node ~A is NIL" (awna-var awna) node)))
               (when (eq (awna-tg/val awna) '+suite+)
                 (return-from walk-arg (funcall f ast)))
               (multiple-value-bind (value target)
                   (if (and subtargetable target)
                       (values nil target) ;; overrule +normal-value+ (e.g. list expr) 
                     (case (awna-tg/val awna)
                       (+normal-value+              (values +normal-value+ nil))
                       (+normal-target+             (values nil +normal-target+))
                       (+namespace-suite+           (values nil nil))
                       (+global-decl-target+        (values nil +global-decl-target+))
                       (+augassign-target/value+    (values +augassign-value+ +augassign-target+))
                       (+delete-target+             (values nil +delete-target+))
                       (t                           (values value target)))) ;; as passed to ast-recurse-fun
                 
                 (setf ast (cond ((and (awna-rest-p awna) (awna-key awna))
                                  (check-type (awna-key awna) (eql second))
                                  (loop for x in ast do (assert (= (length x) 2)))
                                  (loop for (a b) in ast
                                      collect (list a (funcall f b :value value :target target))))
                                 ((awna-rest-p awna)
                                  (mapcar (lambda (x) (funcall f x :value value :target target))
                                          ast))
                                 ((eq (awna-tg/val awna) '+suite+)
                                  (mapcar (lambda (x) (funcall f x :value value :target target))
                                          (second ast)))
                                 (t
                                  (funcall f ast :value value :target target)))))
               ast))
        
        (loop with ast-copy = (copy-tree ast)
            for s in (awn-arg-structs walker-node)
            for ix = (awna-ix s)
            for repl = (walk-arg (nth ix args) s (awn-subtargetable walker-node))
            do (unless (eq (nth (1+ ix) ast-copy) repl) ;; catches setting nil as last cdr when it is already set
                 (setf (nth (1+ ix) ast-copy) repl))
            finally (progn #+(or)(unless (equalp ast ast-copy)
                                   (warn "Replacing ast ~S by ~S" ast ast-copy))
                           (return ast-copy)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Handy functions for dealing with ASTs

(defmacro with-py-ast ((subform ast &rest options)
					  ;; key value target 
					  ;; into-nested-namespaces
		       &body body)
  ;; (with-sub-ast ((form &key value target) ast)
  ;;    ... form ... value ... target ...)
  ;; 
  ;; (with-sub-ast (form ast)
  ;;   ... form...)
  (let (context)
    (when (symbolp subform)
      (setf context '#:contect
            subform `(,subform &rest ,context)))
    `(walk-py-ast ,ast
		  (named-function :with-py-ast-function
		    (lambda ,subform
		      (declare (optimize (debug 3))
			       ,@(when context `((ignore ,context))))
                      ,@body))
		  ;; user-supplied options take precedence...
		  ,@options
		  ;; but these are the defaults
		  :build-result nil)))

;;; Printing walker, for debugging

(defgeneric walk-print (ast &rest walk-options)
  (:method ((ast list) &rest walk-options)
           (prog1 (apply #'walk-py-ast
                         ast
                         (lambda (ast &key value target)
                           (format t "> ~A ~@[:value(~S) ~]~@[:target(~S)~]~%" ast value target)
                           ast)
                         walk-options)
             (terpri)))
  (:method ((ast string) &rest walk-options)
           (apply #'walk-print (parse ast) walk-options)))
