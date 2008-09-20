;; -*- package: clpython; readtable: py-ast-user-readtable -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython)
(in-syntax *ast-user-readtable*)


(defclass generator ()
  ((k :initarg :k :accessor generator-k)))
;; A few CPS macros expand into a (setf generator-k) call.

(defmacro with-generator-context (suite &key sub-generator)
  "Returns the GENERATOR object whose function K executes SUITE, pausing at each
YIELD expression to wait for new input. K changes during the lifetime of the generator,
as it is always set to the current continuation (i.e. the part of SUITE that remains to
be done once the generator is resumed).
 
K is always a function of one list argument, either (:VALUE V) or (:EXCEPTION E TB).
K is called in these two ways by GEN.SEND and GEN.THROW, respectively.

K either returns the values :YIELD V or raises an exception. In particular it raises
StopIteration when exhausted."
  `(let ((.g (make-instance 'generator :k nil)))
     ;; Set the initial value for K. Upon execution K will replace "itself" (the
     ;; slot value) with a new continuation function.
     (setf (generator-k .g)
       (lambda (.x)
         (assert (and (listp .x) (member (car .x) '(:value :exception)))
             (.x) "Invalid Yield expression input value: ~S." .x)
         (ecase (car .x)
           (:value (assert (eq (second .x) *the-none*) ()
                     "Generator expected None, but got ~A, as initial input value."
                     (second .x))
                   (with-pydecl ((:context :function)
                                 (:in-sub-generator ,sub-generator))
                     (cps-convert ,suite #'identity)))
           (:exception
            (warn "Generator got immediate command to raise exception! ~A" val)
            (apply #'raise-stmt-1 (cdr .x))))))
     .g))

(defmacro suite->generator-fun (suite)
  "Returns a function to be called multiple times, taking the same list arg as
WITH-GENERATOR-CONTEXT. A call either:
 - returns the values :YIELD V
 - returns the special token :IMPLICIT-RETURN or :EXPLICIT-RETURN
 - raises an exception"
  ;; To return :IMPLICIT-RETURN at the end of the body, add it to the suite.
  ;; The outer continuation of WITH-GENERATOR-CONTEXT is #'IDENTITY so this symbol
  ;; will simply be returned at the end.
  (with-matching (suite ([suite-stmt] ?items))
    (setf suite `([suite-stmt] (,@?items :implicit-return))))
  
  `(let* ((gener-maker (lambda () (with-generator-context ,suite :sub-generator t)))
          (gener (funcall gener-maker))
          (i 1))
     (declare (ignorable i))
     (lambda (.val-for-generator)
       (check-type .val-for-generator list)
       (let ((yield-val (multiple-value-list (apply #'gen.send-or-throw gener .val-for-generator))))
         #+(or)(progn (warn "suite->generator ~A, yield ~A: values ~A" ',suite i yield-val)
                      (incf i))
         (ecase (first yield-val)
           (:yield (apply #'values yield-val)))))))

;; PEP 342

(defun gen.next (g)
  (gen.send g *the-none*))

(defun gen.send (g value)
  (funcall (generator-k g) (list :value value)))

(defun gen.send-or-throw (g &rest items)
  (ecase (car items)
    (:value (gen.send g (second items)))
    (:exception (apply #'gen.throw g (cdr items))))) 

(defun gen.throw (g &optional (value *the-none*) (traceback *the-none*))
  (funcall (generator-k g) (list :exception value traceback)))

(defun g.__del__ (g)
  (gen.close g))

(defun gen.close (g)
  (warn "gen.close ~A" g)
  (handler-case (gen.throw g (find-class '{GeneratorExit}))
    ((or {GeneratorExit} {StopIteration}) () *the-none*)
    ({Exception} () (py-raise '{RuntimeError} "Generator ~A ignored GeneratorExit."))))

(defun generator-finished (&optional arg) 
  (declare (ignore arg))
  (warn "raising StopIteration")
  (py-raise '{StopIteration} "Generator is finished."))

(defvar *cps-macro-functions* (make-hash-table :test 'eq)
  "Mapping from node to CPS macro function, e.g. [ASSERT-STMT] -> #'cps-convert-assert-stmt")

(defvar *in-cps-compiler* nil)

(defvar *cps-compiler-warn-undefined-macros* t)

(defun make-cps-macroexpand-hook (orig-hook)
  (flet ((new-hook (mfunc mform env)
           ;; Initially the forms are macroexpanded by the CPS macros.
           ;; But as soon as we enter a function or class namespace,
           ;; only CPS-convert if really necessary.
           (when (and env (not (get-pydecl :in-generator-toplevel env)))
             #+(or)(warn "The form ~A is in non-cps context." mform)
             (return-from new-hook (funcall orig-hook mfunc mform env)))

           (whereas ((node (when (listp mform) (car mform)))
                     (cps-mfunc (cond ((gethash node *cps-macro-functions*))
                                      ((and *cps-compiler-warn-undefined-macros*
                                            (member node clpython.parser::*expr-stmt-nodes*))
                                       (warn "No CPS macro function defined for node `~A' ~@
                                         therefore standard macro function used." node)
                                       nil))))
             ;; Found CPS overrule. I think it does not make sense to also call ORIG-HOOK in any way now.
             (return-from new-hook (funcall orig-hook cps-mfunc mform env)))
           
           (funcall orig-hook mfunc mform env)))
    #'new-hook))

(defmacro with-cps-compiler (&body body)
  "Execute BODY in a CPS compilation context. For every ast node (i.e. every ..-STMT or ..-EXPR symbol in
package CLPYTHON.AST.NODE) the macro function is overruled by the corresponding CPS macro function.
Note that CPS macros take the continuation as additional first parameter."
  `(progn (assert (not *in-cps-compiler*) () "Recursive call to WITH-CPS-COMPILER")
          (let ((*macroexpand-hook* (make-cps-macroexpand-hook *macroexpand-hook*)))
            (let ((*in-cps-compiler* t))
              ,@body))))

(defun optimize-funcall (form)
  #+(or)
  (when (= 3 (length form))
    (destructuring-bind (fc lm val)
        form
      (when (and (eq fc 'funcall)
                 (eq (car lm) 'lambda)
                 (= (length (second lm)) 1))
        (return-from optimize-funcall
          `(let ((,(car (second lm)) ,val))
             ,@(cddr lm))))))
  form)
  
(defmacro cps-convert (ast k)
  (etypecase ast
    (null 
     (error "CPS-CONVERT of NIL (k=~S)" k))
    ((or string number symbol)
     ;; symbol e.g. :implicit-return
     (optimize-funcall `(funcall ,k ,ast)))
    (list
     `(,(car ast) ,k ,@(cdr ast)))))

(defmacro def-cps-macro (node args &body body)
  "Define a CPS transformation for AST node. In the BODY use
 (CALL-CONTINUATION) for statements, and
 (CALL-CONTINUATION <VALUE>) for expressions."
  (check-type node symbol)
  (let ((node-type (let* ((node.s (symbol-name node))
                          (suffix (subseq node.s (- (length node.s) 5))))
                     (cond ((string= suffix "-stmt") :stmt)
                           ((string= suffix "-expr") :expr)
                           (t (error "Unexpected node name: ~A" node)))))
        (fname (intern (format nil "~A-cps-convert" node) #.*package*)))
    `(progn (defmacro ,fname (%k ,@args) ;; K is the first arg
              (flet ((store-continuation (f) `(setf (generator-k .g) ,f)) ;; ,%k))
                     (generator-finished () `(funcall (setf (generator-k .g) 'generator-finished)))
                     (current-continuation () %k)
                     ,(ecase node-type
                        (:stmt `(call-continuation () `(funcall ,%k nil))) ;; enfore no value
                        (:expr `(call-continuation (val) `(funcall ,%k ,val))))) ;; enforce value
                (declare (ignorable #'store-continuation #'generator-finished #'call-continuation #'current-continuation))
                ,@body))
            (setf (gethash ',node *cps-macro-functions*) (macro-function ',fname)))))

(def-cps-macro [assert-stmt] (test raise-arg)
  `(when *__debug__*
     (cps-convert ,test (lambda (test-val)
                          ,(if raise-arg
                               `(cps-convert ,raise-arg (lambda (raise-arg) (assert-stmt-1 test-val ',test raise-arg)))
                             `(assert-stmt-1 test-val ',test raise-arg))
                          ,(call-continuation)))))

(def-cps-macro [assign-stmt] (value targets &environment e)
    ;; CPS form is built from the back, so everything is in reversed order.
    (let ((res '(values)))
      (dolist (tg (reverse targets))
        (multiple-value-bind (temps values stores store-form read-form del-form)
            (get-setf-expansion tg e)
          (declare (ignore del-form read-form))
          (assert (= (length stores) 1))
          (setf res `(let ((,(car stores) .assign-value))
                       ,store-form
                       ,res))
          (loop for var in (reverse temps) for val in (reverse values)
              do (setf res `(cps-convert ,val (lambda (,var) ,res))))))
      `(cps-convert ,value (lambda (.assign-value)
                             ,res
                             ,(call-continuation)))))

(def-cps-macro [attributeref-expr] (item attr)
  `(cps-convert ,item (lambda (item-val)
                        ,(call-continuation `(py-attr item-val ',(second attr))))))

(def-cps-macro [augassign-stmt] (op place val &environment e)
  (let ((py-@= (get-binary-iop-func-name op))
        (py-@  (get-binary-op-func-name-from-iop op)))
    (multiple-value-bind (vars vals stores writer reader)
        (get-setf-expansion place e)
      (assert (null (cdr stores)))
      (with-gensyms (place-val-now op-val)
        (let ((res `(cps-convert ,val (lambda (,op-val)
                                        (let ((,place-val-now ,reader))
                                          (or (,py-@= ,place-val-now ,op-val)
                                              (let ((,(car stores) (,py-@ ,place-val-now ,op-val)))
                                                ,writer)))))))
          (loop for var in (reverse vars)
              for val in (reverse vals)
              do (setf res `(cps-convert ,val (lambda (,var) ,res))))
        res)))))

(def-cps-macro [backticks-expr] (item)
  `(cps-convert ,item #'py-repr))

(def-cps-macro [binary-expr] (op x y)
  `(cps-convert ,x (lambda (x-val)
                     (cps-convert ,y (lambda (y-val)
                                       ,(call-continuation `(,(get-binary-op-func-name op) x-val y-val)))))))

(def-cps-macro [binary-lazy-expr] (op left right)
  (ecase op
    ([or] `(cps-convert ,left (lambda (x) (if (py-val->lisp-bool x)
                                              x 
                                            (cps-convert ,right ,(current-continuation))))))
    ([and] `(cps-convert ,left (lambda (x) (if (py-val->lisp-bool x)
                                               (cps-convert ,right ,(current-continuation))
                                             x))))))

(def-cps-macro [break-stmt] ()
  `(.break-cont)) ;; see [for-in-stmt] and [while-stmt]

;; [exec-stmt] : keep
(def-cps-macro [exec-stmt] (name inheritance suite &environment e)
  (declare (ignore name inheritance suite e))
  ())

(def-cps-macro [call-expr] (primary pos-args kwd-args *-arg **-arg)
  (let* ((primary-gensym (gensym "primary"))
         (pos-arg-gensyms (loop repeat (length pos-args) collect (gensym "parg")))
         (kwd-arg-gensyms (loop repeat (length kwd-args) collect (gensym "kwarg")))
         (new-kwd-args (loop for gensym in kwd-arg-gensyms and (name nil) in kwd-args
                           collect (list name gensym)))
         (*-arg-gensym (when *-arg (gensym "*-arg")))
         (**-arg-gensym (when **-arg (gensym "**-arg")))
         (res (call-continuation `(call-expr-1 ,primary-gensym ,pos-arg-gensyms ,new-kwd-args ,*-arg-gensym ,**-arg-gensym))))
    
    (when **-arg (setf res `(cps-convert ,**-arg (lambda (,**-arg-gensym) ,res))))
    (when *-arg (setf res `(cps-convert ,*-arg (lambda (,*-arg-gensym)  ,res))))
    (loop for (nil val) in (reverse kwd-args) for gensym in (reverse kwd-arg-gensyms)
        do (setf res `(cps-convert ,val (lambda (,gensym) ,res))))
    (loop for p in (reverse pos-args) for gensym in (reverse pos-arg-gensyms)
        do (setf res `(cps-convert ,p (lambda (,gensym) ,res))))
    (setf res `(cps-convert ,primary (lambda (,primary-gensym) ,res)))
    res))

;; [classdef-stmt] : keep
(def-cps-macro [classdef-stmt] (name inheritance suite)
  (assert ([tuple-expr-p] inheritance)) 
  (let* ((inheritance-gensyms (loop repeat (length (second inheritance)) collect (gensym "superclass")))
         (res `(progn (with-pydecl ((:in-generator-toplevel nil))
                        (classdef-stmt-1 ,name ([tuple-expr] ,inheritance-gensyms) ,suite))
                      ,(call-continuation))))
    (loop for sup in (second inheritance) for gensym in inheritance-gensyms
        do (setf res `(cps-convert ,sup (lambda (,gensym) ,res))))
    res))

;; [clpython-stmt] : not external; keep

(def-cps-macro [comparison-expr] (cmp left right)
  (let ((py-@ (get-binary-comparison-func-name cmp)))
    `(cps-convert ,left (lambda (left)
                          (cps-convert ,right (lambda (right)
                                                (,py-@ left right)))))))

(def-cps-macro [continue-stmt] ()
  `(.continue-cont)) ;; see [for-in-stmt]

(def-cps-macro [del-stmt] (item &environment e)
  ;; yield can occur in subscripts and attributes:
  ;;   del x[(yield 2)]
  ;;   del (yield 2).a
  ;; but can be together with other items, like:
  ;;   del [a, b[(yield 2), c.a]
  (multiple-value-bind (temps values stores store-form read-form del-form)
      (get-setf-expansion item e)
    (declare (ignore stores store-form read-form))
    (let ((res `(progn ,del-form ,(call-continuation))))
      (loop for v in (reverse values)
          for tmp in (reverse temps)
          do (setf res `(cps-convert ,v (lambda (tmp) ,res))))
      res)))

(def-cps-macro [dict-expr] (vk-list)
  (let ((res (call-continuation '.dict)))
    (loop for (k v) on (reverse vk-list) by #'cddr
        do (setf res `(cps-convert ,v (lambda (v)
                                        (sub/dict-set .dict ,k v)
                                        ,res))))
    (setf res `(let ((.dict (make-dict)))
                 ,res))
    res))

(def-cps-macro [for-in-stmt] (target source suite else-suite)
  `(cps-convert ,source (lambda (source)
                          (let* ((.it-fun (get-py-iterate-fun source)))
                            (labels ((.break-cont (val)
                                       (declare (ignore val))
                                       (call-continuation))
                                     (.else-cont (val)
                                       (declare (ignore val))
                                       (cps-convert ,else-suite (lambda (.val) ;; may be nil
                                                                  (declare (ignore .val))
                                                                  (.break-cont nil))))
                                     (.continue-cont (.val)
                                       (declare (ignore .val))
                                       (let ((.x (funcall .it-fun)))
                                         (unless .x
                                           (.else-cont nil))
                                         ([assign-stmt] .x (,target))
                                         (cps-convert ,suite (lambda (val)
                                                               (declare (ignore val))
                                                               (.continue-cont nil))))))
                              (.continue-cont))))))

(def-cps-macro [funcdef-stmt] (decorators
                               fname (pos-args key-args *-arg **-arg)
                               suite)
  ;; Deal with yield in two places: in decorators, and in keyword argument default values,
  ;; in that evaluation order. 
  (let* ((decorator-gensyms (loop repeat (length decorators) collect (gensym "deco")))
         (kwd-val-gensyms (loop repeat (length key-args) collect (gensym "kwarg-default")))
         (new-key-args (loop for (name nil) in key-args and gensym in kwd-val-gensyms
                           collect (list name gensym)))
         (res `(progn (funcdef-stmt-1 ,decorator-gensyms ,fname (,pos-args ,new-key-args ,*-arg ,**-arg) ,suite)
                      ,(call-continuation))))
    (loop for (nil val) in (reverse key-args) for gensym in (reverse kwd-val-gensyms)
        do (setf res `(cps-convert ,val (lambda (,gensym) ,res))))
    (loop for d in (reverse decorators) for gensym in (reverse decorator-gensyms)
        do (setf res `(cps-convert ,d (lambda (,gensym) ,res))))
    res))

;; [global-stmt] : keep

(def-cps-macro [identifier-expr] (name &environment e)
  ;; Copied from (defmacro [identifier-expr] (name &environment e) ..)
  (check-type name symbol)
  (let ((res (ecase (variable-level name e)
               (:module-level `(%module-get ',name))
               (:function-level (if (member name (get-pydecl :safe-lex-visible-vars e))
                                    (progn (comp-msg "Safe lexical var `~A' in context `~A': skipped boundness check."
                                                     name (format nil "~{~A~^.~}" (reverse (get-pydecl :context-stack e))))
                                           name)
                                  `(or ,name (unbound-variable-error ',name :debug-info (format nil "(identifier-expr func-level; names=~A)" (funcall (mgh-names mgh)))))))
               (:class-level `(or (sub/dict-get +cls-namespace+ ',(symbol-name name))
                                  ,(if (member name (get-pydecl :lexically-visible-vars e))
                                       name
                                     `(%module-get ',name)))))))
    (call-continuation res)))


(def-cps-macro [if-expr] (condition then else)
  `(cps-convert ,condition (lambda (.c)
                             (if (py-val->lisp-bool .c)
                                 (cps-convert ,then ,(current-continuation))
                               (cps-convert ,else ,(current-continuation))))))

(def-cps-macro [if-stmt] (if-clauses else-clause)
  (let ((res (if else-clause 
                 `(cps-convert ,else-clause ,(current-continuation))
               (call-continuation))))
    (loop for (cond body) in (reverse if-clauses)
        do (setf res `(cps-convert ,cond (lambda (.cond)
                                           (if (py-val->lisp-bool .cond)
                                               (cps-convert ,body ,(current-continuation))
                                             ,res)))))
    res))

;; [import-stmt] : keep
;; [import-from-stmt] : keep
;; [lambda-expr] : keep -- goes to our [funcdef-stmt]

(def-cps-macro [listcompr-expr] (item for-in/if-clauses)
  ;; Convert into a suite like this:
  ;;   list = []
  ;;   for a in b:
  ;;     if k:
  ;;       x.append(item)
  ;;   list  # implicitly returned from the suite
  (let ((list (gensym "list")))
    `([suite-stmt] (;; list = []
                    ([assign-stmt] ([list-expr] nil) (([identifier-expr] ,list)))
                    ;; list.append(item)
                    ,@(loop with res = `(real-py-list.append ,list ,item)
                          for clause in (reverse for-in/if-clauses)
                          do (setf res (ecase (car clause) ;; These subexpr may yield, therefore expand into AST
                                         ;; for a in b:
                                         ([for-in-clause] (destructuring-bind (target source) (cdr clause)
                                                            (setf res `([for-in-stmt] ,target ,source ,res nil))))
                                         ;; if k:
                                         ([if-clause] (let ((test (second clause)))
                                                        (setf res `([if-stmt] ((,test ,res)) ())))))))
                    ([identifier-expr] ,list))))) ;; list

(def-cps-macro [list-expr] (items)
  (let ((build-func 'make-py-list-unevaled-list))
    (loop with list = (gensym "list")
        with item-gensyms = (loop repeat (length items) collect (gensym "item"))
        with res = (call-continuation `(,build-func ,item-gensyms))
        for x in (reverse items) and gensym in (reverse item-gensyms)
        do (setf res `(cps-convert ,(pop items) (lambda (,gensym) ,res)))
        finally (return `(let (,list) ,res)))))

(def-cps-macro [module-stmt] (&rest args)
  (declare (ignore args))
  (error "Should not happen: module is not a generator."))

(def-cps-macro [pass-stmt] ()
  (call-continuation))

(def-cps-macro [print-stmt] (dest items comma?)
  (let* ((item-gensyms (loop repeat (length items) collect (gensym "item")))
         (dest-gensym (gensym "dest"))
         (res `(progn (py-print ,dest-gensym (list ,@item-gensyms) ,comma?)
                      ,(call-continuation))))
    (loop for it in (reverse items) and gensym in (reverse item-gensyms)
        do (setf res `(cps-convert ,it (lambda (,gensym) ,res))))
    (setf res (if dest 
                  `(cps-convert ,dest (lambda (,dest-gensym) ,res))
                `(let ((,dest-gensym ,dest))
                   ,res)))
    res))

(def-cps-macro [raise-stmt] (exc var tb)
  `(cps-convert ,exc (lambda (.exc)
                       ,(if tb
                            `(cps-convert ,tb (lambda (.tb)
                                                (raise-stmt-1 ,exc ,var .tb)))
                          `(raise-stmt-1 ,exc ,var ,tb)))))
  
(def-cps-macro [return-stmt] (&optional value &environment e)
    "todo: check if value allowed in generator"
    (assert (not value))
    (if (get-pydecl :in-sub-generator e)
        (call-continuation :explicit-return)
      (generator-finished)))

;; [slice-expr] : keep

(def-cps-macro [subscription-expr] (item subs)
  `(cps-convert ,item (lambda (.item)
                        (cps-convert ,subs (lambda (.subs)
                                             ,(call-continuation `(py-subs .item .subs)))))))

(def-cps-macro [suite-stmt] (stmts)
    (cond ((null stmts)
           (error "Empty SUITE: should be disallowed by grammar."))
          ((null (cadr stmts))
           `(cps-convert ,(car stmts) ,(current-continuation)))
          (t 
           `(cps-convert ,(car stmts) (lambda (val)
                                        (declare (ignore val))
                                        (cps-convert ([suite-stmt] ,(cdr stmts))
                                                     ,(current-continuation)))))))

(def-cps-macro [try-except-stmt] (suite except-clauses else-suite)
  `(flet ((.attempt-handle-error (.c)
            ,(loop with res = `(warn "Error ~A not handled in try/except; passing on..." .c)
                 for (exc var handler-suite) in (reverse except-clauses)
                 if exc
                 do (setf res `(cps-convert
                                ,exc
                                (lambda (.exc)
                                  (warn "testing ~A of class ~A?" .c .exc)
                                  (let ((applicable (typecase .exc 
                                                      ;; exception class, or typle of exception classes
                                                      (list (some (lambda (x) (typep .c x)) .exc))
                                                      (t    (typep .c .exc)))))
                                    (if applicable
                                        (progn (warn "applicable")
                                               ,@(when var `(([assign-stmt] .c (,var))))
                                               (cps-convert ,handler-suite ,(current-continuation)))
                                      ,res)))))
                 else ;; blank "except:" catches all
                 do (setf res `(progn ,@(when var `(([assign-stmt] .c (,var))))
                                      (warn "in bare except")
                                      ;(return-from try-except-block
                                        (cps-convert ,handler-suite ,(current-continuation)))) ;)
                 finally (return res)))
          (.do-else ()
            ,(if else-suite
                 `(cps-convert ,else-suite ,(current-continuation))
               (call-continuation))))
     (let ((iterfunc (suite->generator-fun ,suite)))
       (labels ((.next-try-value (.val-for-gener)
                  (warn ".next-try-value ~A" .val-for-gener)
                  (handler-case
                    (let ((val (with-py-errors ()
                                 (multiple-value-list (funcall iterfunc .val-for-gener)))))
                      (case (car val)
                        (:implicit-return (warn "implicit return")
                                          (.do-else))
                        (:explicit-return (warn "explicit return")
                                          ,(generator-finished))
                        (:yield           (warn ".next-try: yield-val = ~A" val)
                                          ,(store-continuation `(lambda (.val-for-gener) 
                                                                  (.next-try-value .val-for-gener)))
                                          (apply #'values val))))
                    ({Exception} (c) (.attempt-handle-error c)))))
         (.next-try-value (list :value *the-none*)))))) ;; start up the generator

(def-cps-macro [try-finally-stmt] (try-suite finally-suite)
  `(let ((stmt-k ,(current-continuation)))
     (block try-finally-block
       (let ((iterfunc (suite->generator-fun ,try-suite))
             (finalization-scheduled nil))
         (labels ((.run-finally (generator-finished-p)
                    (warn "running Finally ~A" generator-finished-p)
                    (cps-convert ,finally-suite
                                 (lambda (.val)
                                   ;; Whole finally-body executed -> remove finalization
                                   (when finalization-scheduled (unschedule-finalization finalization-scheduled))
                                   (if generator-finished-p
                                       ,(generator-finished)
                                     (funcall stmt-k nil) #+(or)(return-from try-finally-block)))))
                  (.next-val (.val-for-generator)
                    ;; Get new val from TRY:
                    ;;  - if this results in an error, run the FINALLY (which may raise another
                    ;;    exception itself) and raise the error again;
                    ;;  - if this does not result in an error, then FINALLY is not yet run.
                    (let ((val (handler-case (multiple-value-list
                                              (with-py-errors ()
                                                (funcall iterfunc .val-for-generator)))
                                 ({Exception} (c)
                                   (list :exception)))))
                      (case (car val)
                        (:exception 
                         (.run-finally t))
                        ((:implicit-return :explicit-return)
                         ;; :implicit = normal end of TRY block.
                         ;; :explicit = TRY block did a RETURN, which means generator is finished.
                         ;; In both cases we must go to the FINALLY block.
                         (.run-finally (eq (car val) :explicit-return)))
                        (:yield
                         ;; The TRY block yields a value. Maybe the generator will never be run
                         ;; further. To ensure the FINALLY block is run some time, schedule
                         ;; finalization function.
                         (unless finalization-scheduled
                           (setf finalization-scheduled (schedule-finalization .g 'gen.close)))
                         ,(store-continuation `(lambda (v) (.next-val v)))
                         (apply #'values val))))))
           (.next-val (list :value *the-none*)))))
     
     #+(or)(funcall stmt-k nil)))

#||
;; If only the FINALLY suite contains YIELD, the rewrite could simply be:
`(unwind-protect (cps-convert ,try-suite)
   (cps-convert ,finally-suite))
||#

(defun schedule-finalization (object function)
  "Schedule FUNCTION to be run when OBJECT is garbage collected."
  (declare (ignorable object function))
  #+allegro (excl:schedule-finalization object function)
  #-allegro (error "Don't know how to SCHEDULE-FINALIZATION in this lisp."))

(defun unschedule-finalization (sf)
  "Removes a finalization function. SF is a value returned by SCHEDULE-FINALIZATION."
  (declare (ignorable sf))
  #+allegro (excl:unschedule-finalization sf)
  #-allegro (error "Don't know how to UNSCHEDULE-FINALIZATION in this lisp."))

(def-cps-macro [tuple-expr] (items)
  (let ((build-func 'make-tuple-unevaled-list))
    (loop with list = (gensym "list")
        with item-gensyms = (loop repeat (length items) collect (gensym "item"))
        with res = (call-continuation `(,build-func ,item-gensyms))
        for x in (reverse items) and gensym in (reverse item-gensyms)
        do (setf res `(cps-convert ,(pop items) (lambda (,gensym) ,res)))
        finally (return `(let (,list) ,res)))))

(def-cps-macro [unary-expr] (op val)
  (let ((py-op-func (get-unary-op-func-name op)))
    `(cps-convert ,val (lambda (.val)
                         ,(call-continuation `(funcall (function ,py-op-func) .val))))))

(def-cps-macro [while-stmt] (test suite else-suite)
  `(labels ((.break-cont () ;; see [break-stmt]
              (call-continuation))
            (.loop-again ()
              (cps-convert ,test (lambda (val)
                                   (if (py-val->lisp-bool val)
                                       (cps-convert ,suite (lambda (v) (declare (ignore v) (.loop-again))))
                                     (cps-convert ,else-suite (lambda (v) ,(call-continuation))))))))
     (.loop-again)))

;; [with-stmt]: keep?

(def-cps-macro [yield-expr] (val)
  `(cps-convert ,val (lambda (val)
                       ,(store-continuation `(lambda (.x)
                                               (assert (and (listp .x) (member (car .x) '(:value :exception)))
                                                   (.x) "Invalid Yield expression input value: ~S." .x)
                                               (ecase (car .x)
                                                 (:value ,(call-continuation `(second .x)))
                                                 (:exception (apply #'raise-stmt-1 (cdr .x))))))
                       (warn "Yielding ~A" val)
                       (values :yield val))))

(def-cps-macro [yield-stmt] (&optional value)
  ;; A yield statement is a yield expression whose value is not used.
  `(cps-convert ([yield-expr] ,(or value '*the-none*)) ,(current-continuation)))

  
(defmacro view-cps (string)
  (format t "Code: ~A~%" string)
  (let ((ast (parse string :one-expr t)))
    (format t "AST: ~A~%" ast)
    (let ((*cps-compiler-warn-undefined-macros* nil))
      (with-cps-compiler
          ;(with-assuming-function-variables
              (let ((me (excl::compiler-macroexpand `(cps-convert ,ast 'k))))
                (format t "CPS: ~S~%" me)))))
  ;)
  (values))

(defmacro with-mgh (&body body)
  `(let ((mgh (make-dict-mgh "module-name" "module-path")))
     (macrolet ((%module-get (name)      `(funcall (mgh-get mgh) ,name))
                (%module-set (name val)  `(funcall (mgh-set mgh) ,name ,val))
                (%module-del (name)      `(funcall (mgh-del mgh) ,name)))
       ,@body)))

(defun make-gen (string &key debug (max 10))
  "Returns yielded values. The values send as the result of each yield expression
are increasing integers starting from 1, up to MAX.
MAX is a safey limit, in case the code erroneously goes into an endless loop."
  (flet ((wformat (&rest args)
           (when debug (apply #'format t args)))) 
    (with-cps-compiler
        (let* ((ast `([suite-stmt] ,(nconc (parse string :incl-module nil) (list :implicit-return))))
               (gener-maker (eval `(with-mgh (lambda ()
                                               (with-pydecl ((:in-generator-toplevel t))
                                                 (with-generator-context ,ast))))))
               (gener (funcall gener-maker))
               (yielded ()))
          (block iterate
            (wformat "GM: ~A~%" gener-maker)
            (wformat "G: ~A~%" gener)
            (wformat "Starting G with next()~%")
            (let ((values (multiple-value-list (handler-case (gen.next gener)
                                                 ({StopIteration} ()
                                                   (wformat " exhausted~%")
                                                   :exhausted)))))
              (wformat "  yield 1: ~S~%" (second values))
              (ecase (car values)
                (:exhausted
                 (return-from iterate))
                (:implicit-return
                 (return-from iterate))
                (:yield
                 (push (second values) yielded)
                 (loop for val = (multiple-value-list (handler-case (gen.send gener *the-none*)
                                                        ({StopIteration} ()
                                                          (wformat " exhausted~%")
                                                          :exhausted))
                                                      #+(or)(gen.send gener *the-none*))
                     for i from 1 below (1+ max)
                     while val do (ecase (car val)
                                    (:exhausted
                                     (return-from iterate))
                                    (:yield
                                     (wformat "  yield ~A: ~A~%" (1+ i) (second val))
                                     (push (second val) yielded))
                                    (:implicit-return
                                     (return-from iterate)))
                     finally (when (> i max)
                               (warn "Generator manually stopped (but perhaps not exhausted).~%")))))))
          (nreverse yielded)))))

(defparameter *cps-tests*
    `(("pass" ())
      ("yield 1" (1))
      ("yield" ,(list *the-none*))
      ("
if 0:
  yield 1
elif []:
  yield 2
else:
  yield 3
  if 1:
    yield 4
  else:
    yield 5" (3 4))
      ("
try:
  1/0
  print 'a'
  yield 2
except:
  print 'x'
  print 'y'
  yield 3
  print 'z'
print 'below'" (3))
      ("
yield 1
class C: pass
class D:
  pass
yield 2" (1 2))
      ("
yield 1
class C:
  pass
try:
  print 'aap'
  yield 2
  1/0
except C, e:
  print 'never'
  yield 20
except:
  yield 3
  yield 4" (1 2 3 4))
      ("
try:
  print 'start try'
  yield 1
  yield 2
  1/0
  yield 3
  print 'end try'
finally:
  print 'start finally'
  yield 40
  yield 41
  print 'end finally'
print 'after try/finally'
yield 5
print 'still after try/finally'" (1 2 40 41))))

(defun cps-test (&key debug)
  (loop for (src expected) in *cps-tests*
      for actual = (make-gen src :debug debug)
      if (equal expected actual)
      do (format t "Ok. ~A~%" expected)
      else do (break "Failure:  expected ~A, got ~A, for:~_~A~%" expected actual src)))
