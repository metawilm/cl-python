;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON; READTABLE: PY-AST-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython)
(in-syntax *ast-user-readtable*)

;;; Generators

;;; Generators containing only yield statements are rewritten into
;;; state machines, while generators containing yield expressions
;;; are made into processes.

(defun rewrite-generator-funcdef-suite (fname suite)
  ;; Returns the function body
  (assert (or (symbolp fname) (listp fname)))
  (assert (match-p suite '([suite-stmt] ?_)) ()
    "CAR of SUITE must be SUITE-STMT, but got: ~S" (car suite))
  (assert (generator-ast-p suite))
  `(make-generator ,suite))

(defun rewrite-generator-expr-ast (ast)
  ;; rewrite:  (x*y for x in bar if y)
  ;; into:     def f(src):  for x in src:  if y:  yield x*y
  ;;           f(bar)
  ;; values: (FUNCDEF ...)  bar
  (assert (eq (car ast) '[generator-expr]))
  (destructuring-bind (item for-in/if-clauses) (cdr ast)

    (let ((first-for (pop for-in/if-clauses))
	  (first-source (gensym "first-source")))
      (assert (eq (car first-for) '[for-in-clause]))
      (let ((iteration-stuff (loop with res = `([yield-stmt] ,item)
				 for clause in (reverse for-in/if-clauses)
				 do (setf res
				      (ecase (car clause)
					([for-in-clause] `([for-in-stmt]
							   ,(second clause) ,(third clause) ,res nil))
					([if-clause]     `([if-stmt] ((,(second clause) ,res)) nil))))
				 finally (return res))))
        `([call-expr] 
	  ([funcdef-stmt] nil ([identifier-expr] :generator-expr-helper-func)
			  ((([identifier-expr] ,first-source)) nil nil nil)
			  ([suite-stmt]
			   (([for-in-stmt] ,(second first-for) ([identifier-expr] ,first-source)
					   ,iteration-stuff nil))))
          (,(third first-for)) nil nil nil)))))


;;; Conversion into Continuation Passing Style
;;; PEP 342

(defun generator-state.function (gs)
  (car gs))

(defun generator-state.input-value (gs val)
  (funcall (generator-state.function gs) val))

(defun parse-generator-input (x &key initial)
  "Either calls RAISE-STMT-1, or returns the input value (or None if X is NIL)."
  (assert x)
  (cond ((and x (listp x) (eq (car x) :exception))
         (apply #'raise-stmt-1 (cdr x)))
        ((and initial (not (none-p x)))
         (py-raise '{ValueError} "Generator must have `None' as initial input value (got: ~A)." x))
        (t x)))

(defun error-fall-through (arg)
  (cerror "Continue"
          "CPS failed to take control: ~A" arg))

(defmacro yield-value (val)
  `(throw '.generator-body ,val))

(defun %generator.close (g)
  #+(or)(warn "generator.close ~A" g)
  (generator-state.close (generator-state g)))

(defun generator-state.close (gs)
  (handler-case (generator-state.input-value gs (list :exception (find-class '{GeneratorExit}))) ;; XXX no traceback yet
    ((or {GeneratorExit} {StopIteration}) () *the-none*)
    ({Exception} () (py-raise '{RuntimeError} "Generator ~A ignored GeneratorExit sent by generator.close()."))))

(defmacro receive-yielded-value (&body body)
  `(let ((val (catch '.generator-body
                (progn ,@body
                       (error-fall-through ',body)))))
     (or val (error "No yielded value received."))))

(defun %generator.send (g value wait)
  (flet ((insert ()
           (generator-state.input-value (generator-state g) value)))
    (declare (dynamic-extent #'insert))
    (if wait
        (receive-yielded-value (insert))
      (insert))))

(defun %store-continuation (f)
  `(setf (car %stored-k-cons) ,f))

(defun %mark-generator-finished ()
  `(progn (%store-continuation 'raise-stop-iteration)
          (raise-stop-iteration)))

(defvar *cps-macro-functions* (make-hash-table :test 'eq)
  "Mapping from node to CPS macro function, e.g. [ASSERT-STMT] -> #'cps-convert-assert-stmt")

(defmacro def-cps-macro (node args &body body)
  "Define a CPS transformation for AST node. In the BODY are defined:
  - (%CALL-CONTINUATION) for statements, (%CALL-CONTINUATION <VALUE>) for expressions;
  - %CURRENT-CONTINUATION
  - (%MARK-GENERATOR-FINISHED)"
  (check-type node symbol)
  (let ((node-type (let* ((node.s (symbol-name node))
                          (suffix (subseq node.s (- (length node.s) 5))))
                     (cond ((string= suffix "-stmt") :stmt)
                           ((string= suffix "-expr") :expr)
                           (t (warn "Unexpected node name: ~A" node)))))
        (fname (intern (format nil "~A/cps" node) #.*package*)))
    `(progn (defmacro ,fname (%k ,@args) ;; Continuation becomes the first arg
              (flet (,(ecase node-type
                        (:stmt `(%call-continuation () `(funcall ,%k nil))) ;; enforce no value
                        (:expr `(%call-continuation (val) `(funcall ,%k ,val))))) ;; enforce value
                (declare (ignorable #'%call-continuation))
                (let ((%current-continuation %k))
                  (declare (ignorable %current-continuation))
                  ,@body)))
            (setf (gethash ',node *cps-macro-functions*) ',fname))))

(defun raise-stop-iteration () 
  (py-raise '{StopIteration} "Generator is finished."))

#+(or) ;; Should be done by implementations already.
(progn (defun optimize-funcall (form)
         "Replace (FUNCALL (LAMBDA (X) ..) VAL) by: (LET ((X VAL)) ..)"
         (when (and (listp form)
                    (= 3 (length form)))
           (destructuring-bind (fc lm val)
               form
             (when (and (eq fc 'funcall)
                        (listp lm)
                        (eq (car lm) 'lambda)
                        (= (length (second lm)) 1))
               #+(or)(format t "Optimizing ~A~%" form)
               (return-from optimize-funcall
                 `(let ((,(car (second lm)) ,val))
                    ,@(cddr lm))))))
         form)

       (defmacro my-funcall (func &rest args)
         (optimize-funcall `(funcall ,func ,@args))))

(defmacro %cps-convert (ast k &key nil-allowed)
  (cond ((and (null ast) (not nil-allowed))
         (error "%CPS-CONVERT of NIL (k=~S)" k))
        ((null ast)
         `(funcall ,k ,ast))
        ((listp ast)
         (let ((f (or (gethash (car ast) *cps-macro-functions*)
                               (error "CPS-conversion of ~A not defined (form: ~A)." (car ast) ast))))
                    `(,f ,k ,@(cdr ast))))
        (t
         `(progn (funcall ,k ,ast)
                 (error "fallthrough (cps convert)")))))

(defmacro with-cps-conversion ((ast value &key nil-allowed) &body body)
  `(with-pydecl ((:inside-cps-conversion t)) ;; for non-cps macros for YIELD-{EXPR,STMT}
     (%cps-convert ,ast (lambda (,value) ,@body) :nil-allowed ,nil-allowed)))

(defmacro make-generator-state (suite &key sub-generator &environment e)
  (assert (match-p suite '([suite-stmt] ?stmts)))
  `(let ((%stored-k-cons (cons nil nil)))
     ,(%store-continuation `(lambda (&optional x)
                              (parse-generator-input x :initial t)
                              (with-pydecl ((:context-type-stack
                                             ,(cons :function (get-pydecl :context-type-stack e)))
                                            (:in-sub-generator ,sub-generator))
                                (with-cps-conversion (,suite val)
                                  (declare (ignore val))
                                  ,(if sub-generator
                                       `(yield-value :implicit-return)
                                     (%mark-generator-finished))))))
     %stored-k-cons))

(defmacro make-generator (suite &key sub-generator)
  "Returns a generator for given SUITE. SUB-GENERATOR means that execution of SUITE ends
with an implicit or explicit return.
K either YIELD-VALUE or raises an exception (like StopIteration upon exhaustion). The
former requires that this form is executed within RECEIVE-YIELDED-VALUE."
  `(make-instance 'generator :state (make-generator-state ,suite :sub-generator ,sub-generator)))

;;; AST CPS macros

(def-cps-macro [assert-stmt] (test raise-arg)
  (with-gensyms (assert-k)
    `(progn
       (let ((,assert-k ,%current-continuation))
         (when *__debug__*
           (with-cps-conversion (,test .test-val)
             (if (py-val->lisp-bool .test-val)
                 #1=(funcall ,assert-k nil)
                 (with-cps-conversion (,raise-arg .raise-arg :nil-allowed t)
                   (assert-stmt-1 .test-val ',test .raise-arg)
                   #1#))))
         #1#))))

(def-cps-macro [assign-stmt] (value targets &environment e)
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
            do (setf res `(with-cps-conversion (,val ,var) ,res)))))
    `(with-cps-conversion (,value .assign-value)
       ,res
       ,(%call-continuation))))

(def-cps-macro [attributeref-expr] (item attr)
  (with-gensyms (e-item)
    `(with-cps-conversion (,item ,e-item)
       ,(%call-continuation `(attr ,e-item ',(second attr))))))

(def-cps-macro [augassign-stmt] (op place val &environment e)
  (let ((py-@= (get-binary-iop-func-name op))
        (py-@  (get-binary-op-func-name-from-iop op)))
    (multiple-value-bind (vars vals stores writer reader)
        (get-setf-expansion place e)
      (assert (null (cdr stores)))
      (with-gensyms (place-val-now op-val)
        (let ((res `(with-cps-conversion (,val ,op-val)
                      (let ((,place-val-now ,reader))
                        (or (,py-@= ,place-val-now ,op-val)
                            (let ((,(car stores) (,py-@ ,place-val-now ,op-val)))
                              ,writer)))
                      ,(%call-continuation))))
          (loop for var in (reverse vars)
              for val in (reverse vals)
              do (setf res `(with-cps-conversion (,val ,var) ,res)))
          res)))))

(def-cps-macro [backticks-expr] (item)
  (with-gensyms (e-item)
    `(with-cps-conversion (,item ,e-item)
       ,(%call-continuation `(py-repr ,e-item)))))

(def-cps-macro [binary-expr] (op x y)
  (with-gensyms (e-x e-y)
    `(with-cps-conversion (,x ,e-x)
       (with-cps-conversion (,y ,e-y)
         ,(%call-continuation `(,(get-binary-op-func-name op) ,e-x ,e-y))))))

(def-cps-macro [binary-lazy-expr] (op left right)
  (with-gensyms (binary-lazy-k e-left)
    `(let ((,binary-lazy-k ,%current-continuation))
       (with-cps-conversion (,left ,e-left)
         (if (py-val->lisp-bool ,e-left)
             ,(if (eq op '[or])
                  `(funcall ,binary-lazy-k ,e-left)
                `(%cps-convert ,right ,binary-lazy-k))
           ,(if (eq op '[or])
                `(%cps-convert ,right ,binary-lazy-k)
              `(funcall ,binary-lazy-k ,left)))))))

(def-cps-macro [bracketed-expr] (expr)
  `(%cps-convert ,expr ,%current-continuation))

(def-cps-macro [break-stmt] ()
  `(%break-cont))

(def-cps-macro [call-expr] (primary pos-args kwd-args *-arg **-arg)
  (with-gensyms (call-k)
    (let* ((primary-gensym (gensym "primary"))
           (pos-arg-gensyms (loop repeat (length pos-args) collect (gensym "parg")))
           (kwd-arg-gensyms (loop repeat (length kwd-args) collect (gensym "kwarg")))
           (new-kwd-args (loop for gensym in kwd-arg-gensyms and (name nil) in kwd-args
                             collect (list name gensym)))
           (*-arg-gensym (when *-arg (gensym "*-arg")))
           (**-arg-gensym (when **-arg (gensym "**-arg")))
           (res `(funcall ,call-k
                          (call-expr-1 ,primary-gensym ,pos-arg-gensyms ,new-kwd-args ,*-arg-gensym ,**-arg-gensym))))
      (when **-arg (setf res `(with-cps-conversion (,**-arg ,**-arg-gensym) ,res)))
      (when *-arg (setf res `(with-cps-conversion (,*-arg ,*-arg-gensym) ,res)))
      (loop for (nil val) in (reverse kwd-args) for gensym in (reverse kwd-arg-gensyms)
          do (setf res `(with-cps-conversion (,val ,gensym) ,res)))
      (loop for p in (reverse pos-args) for gensym in (reverse pos-arg-gensyms)
          do (setf res `(with-cps-conversion (,p ,gensym) ,res)))
      `(let ((,call-k ,%current-continuation))
         (with-cps-conversion (,primary ,primary-gensym) ,res)))))

(def-cps-macro [classdef-stmt] (name inheritance suite)
  (assert (ast-p inheritance '[tuple-expr]))
  (with-gensyms (classdef-k)
    (let* ((inheritance-gensyms (loop for i from 1 repeat (length (second inheritance)) collect
                                      (gensym (format nil "superclass-~A-" i))))
           (res `(progn (with-pydecl ((:inside-cps-conversion nil))
                          (classdef-stmt-1 ,name ([tuple-expr] ,inheritance-gensyms) ,suite))
                        (funcall ,classdef-k nil))))
      (loop for sup in (reverse (second inheritance)) for gensym in (reverse inheritance-gensyms)
          do (setf res `(with-cps-conversion (,sup ,gensym) ,res)))
      `(let ((,classdef-k ,%current-continuation))
         ,res))))

(defun cps-comparison-expr-eval (left right comp-func comparison-k last-p)
  (let ((cmp-res (funcall comp-func left right)))
    (when (or last-p (not (py-val->lisp-bool cmp-res)))
      (funcall comparison-k cmp-res))))
         
(def-cps-macro [comparison-expr] (cmp left right)
  (multiple-value-bind (args cmp-func-names)
      (apply-comparison-brackets `([comparison-expr] ,cmp ,left ,right))
    (with-gensyms (comparison-k e-left e-right)
      `(let ((,comparison-k ,%current-continuation))
         (with-cps-conversion (,(pop args) ,e-left)
           ,@(loop while cmp-func-names
                 collect `(with-cps-conversion (,(pop args) ,e-right)
                            (cps-comparison-expr-eval ,e-left ,e-right ',(pop cmp-func-names) ,comparison-k ,(null cmp-func-names))
                            ,@(unless (null cmp-func-names)
                                `((setf ,e-left ,e-right))))))))))

(def-cps-macro [continue-stmt] ()
  `(%continue-cont))

(def-cps-macro [del-stmt] (item &environment e)
  ;; yield can occur in subscripts and attributes:
  ;;   del x[(yield 2)]
  ;;   del (yield 2).a
  ;; but can be together with other items, like:
  ;;   del [a, b[(yield 2), c.a]
  (multiple-value-bind (temps values stores del-form read-form)
      (let ((*want-DEL-setf-expansion* t))
        (get-setf-expansion item e))
    (declare (ignore stores read-form))
    (let ((res `(progn ,del-form
                       ,(%call-continuation))))
      (loop for v in (reverse values)
          for tmp in (reverse temps)
          do (setf res `(with-cps-conversion (,v ,tmp) ,res)))
      res)))

(def-cps-macro [dict-expr] (vk-list)
  (let ((res (%call-continuation '.dict)))
    (loop for (k v) on (reverse vk-list) by #'cddr
        do (setf res (with-gensyms (key val)
                       ;; First 1st val is evaluated, then 1st key, then 2nd, etc.
                       `(with-cps-conversion (,v ,val)
                          (with-cps-conversion (,k ,key)
                            (with-py-dict
                                (setf (gethash ,key .dict) ,val))
                            ,res)))))
    `(let ((.dict (make-py-hash-table)))
       ,res)))

(def-cps-macro [exec-stmt] (&rest args)
  `(progn ([exec-stmt] ,@args) ;; No CPS
          ,(%call-continuation)))

(def-cps-macro [for-in-stmt] (target source suite else-suite &environment e)
  (with-gensyms (e-source it-fun for-cont for-break-cont else-cont ignore for-in-stmt-k)
    `(let ((,for-in-stmt-k ,%current-continuation)) 
       (with-cps-conversion (,source ,e-source)
         (let* ((,it-fun (get-py-iterate-fun ,e-source)))
           (labels (,@(when (contains-continue-stmt-p suite e)
                        `((%continue-cont () (,for-cont))))
                      ,@(when (contains-break-stmt-p suite e)
                          `((%break-cont () (,for-break-cont))))
                      (,for-break-cont ()
                        (funcall ,for-in-stmt-k nil))
                      (,else-cont (val)
                        (declare (ignore val))
                        ,(if else-suite
                             `(with-cps-conversion (,else-suite val)
                                (declare (ignore val))
                                (,for-break-cont))
                           `(,for-break-cont)))
                      (,for-cont ()
                        ,(with-gensyms (x)
                           `(let ((,x (funcall ,it-fun)))
                              (if ,x
                                  (with-cps-conversion (([assign-stmt] ,x (,target)) ,ignore)
                                    (declare (ignore ,ignore))
                                    (with-cps-conversion (,suite ,ignore)
                                      (declare (ignore ,ignore))
                                      (,for-cont)))
                                (,else-cont nil))))))
             (,for-cont)))))))

(def-cps-macro [funcdef-stmt] (decorators
                               fname (pos-args key-args *-arg **-arg)
                               suite)
  ;; Deal with yield in two places: in decorators, and in keyword argument default values,
  ;; in that evaluation order. 
  (let* ((decorator-gensyms (loop repeat (length decorators) collect (gensym "deco")))
         (kwd-val-gensyms (loop repeat (length key-args) collect (gensym "kwarg-default")))
         (new-key-args (loop for (name nil) in key-args and gensym in kwd-val-gensyms
                           collect (list name gensym)))
         (res `(progn (with-pydecl ((:in-generator-toplevel nil))
                        (funcdef-stmt-1 ,decorator-gensyms ,fname (,pos-args ,new-key-args ,*-arg ,**-arg) ,suite))
                      ,(%call-continuation))))
    (loop for (nil val) in (reverse key-args) for gensym in (reverse kwd-val-gensyms)
        do (setf res `(with-cps-conversion (,val ,gensym) ,res)))
    (loop for d in (reverse decorators) for gensym in (reverse decorator-gensyms)
        do (setf res `(with-cps-conversion (,d ,gensym) ,res)))
    res))

(def-cps-macro [global-stmt] (names &environment e)
  (assert (member :function (get-pydecl :context-type-stack e)))
  (or names) ;; suppress warning
  (%call-continuation))

(def-cps-macro [identifier-expr] (name)
  (check-type name symbol)
  (%call-continuation `([identifier-expr] ,name))) ;; No CPS

(def-cps-macro [if-expr] (condition then else)
  (with-gensyms (c)
    `(with-cps-conversion (,condition ,c)
       (if (py-val->lisp-bool ,c)
           (%cps-convert ,then ,%current-continuation)
         (%cps-convert ,else ,%current-continuation)))))

(def-cps-macro [if-stmt] (if-clauses else-clause)
  (with-gensyms (if-stmt-k)
    `(let ((,if-stmt-k ,%current-continuation))
       (declare (ignorable ,if-stmt-k))
       ,(let ((res (if else-clause 
                       `(%cps-convert ,else-clause ,if-stmt-k)
                     `(funcall ,if-stmt-k nil))))
          (loop for (cond body) in (reverse if-clauses)
              do (setf res (with-gensyms (e-cond)
                             `(with-cps-conversion (,cond ,e-cond)
                                (if (py-val->lisp-bool ,e-cond)
                                    (%cps-convert ,body ,if-stmt-k)
                                  ,res)))))
          res))))

(def-cps-macro [import-stmt] (&rest args)
  `(progn ([import-stmt] ,@args) ;; No CPS
          ,(%call-continuation)))

(def-cps-macro [import-from-stmt] (&rest args)
  `(progn ([import-from-stmt] ,@args) ;; No CPS
          ,(%call-continuation)))

(def-cps-macro [lambda-expr] ((pos-args key-args *-arg **-arg) expr)
  (let* ((kwd-val-gensyms (loop repeat (length key-args) collect (gensym "kwarg-default")))
         (new-key-args (loop for (name nil) in key-args and gensym in kwd-val-gensyms
                           collect (list name gensym)))
         (res (%call-continuation `([lambda-expr] ;; No CPS
                                    (,pos-args ,new-key-args ,*-arg ,**-arg) ,expr))))
    (loop for (nil val) in (reverse key-args) for gensym in (reverse kwd-val-gensyms)
        do (setf res `(with-cps-conversion (,val ,gensym) ,res)))
    res))

(def-cps-macro [listcompr-expr] (item for-in/if-clauses)
  (with-gensyms (result-list ignore)
    `(let ((,result-list (make-py-list-from-list ())))
       (with-pydecl ((:inside-cps-list-comprehension t))
       ,(loop with builder = `([call-expr] ([attributeref-expr] ,result-list ([identifier-expr] {append}))
                                           (,item) nil nil nil)
            for clause in (reverse for-in/if-clauses)
              do (setf builder
                   (ecase (car clause)
                     ([for-in-clause] (destructuring-bind (target source) (cdr clause)
                                        `([for-in-stmt] ,target ,source ,builder nil)))
                     ([if-clause] (let ((test (second clause)))
                                    `([if-stmt] ((,test ,builder)) nil)))))
              finally (return `(with-cps-conversion (,builder ,ignore)
                                 (declare (ignore ,ignore))
                                 ,(%call-continuation result-list))))))))

(def-cps-macro [list-expr] (items)
  ;; Relies on tuples being Lisp lists.
  (with-gensyms (e-tuple)
    `(with-cps-conversion (([tuple-expr] ,items) ,e-tuple)
       ,(%call-continuation `(make-py-list-from-tuple ,e-tuple)))))

(def-cps-macro [literal-expr] (kind value)
  (%call-continuation `([literal-expr] ,kind ,value))) ;; No CPS

(def-cps-macro [module-stmt] (&rest args)
  (progn args) ;; suppress unused warning
  (error "Should not happen: module is not a generator."))

(def-cps-macro [pass-stmt] ()
  (%call-continuation))

(def-cps-macro [print-stmt] (dest items comma?)
  (let* ((item-gensyms (loop repeat (length items) collect (gensym "item")))
         (dest-gensym (gensym "dest"))
         (res `(progn (py-print ,dest-gensym (list ,@item-gensyms) ,comma?)
                      ,(%call-continuation))))
    (loop for it in (reverse items) and gensym in (reverse item-gensyms)
        do (setf res `(with-cps-conversion (,it ,gensym) ,res)))
    (setf res (if dest 
                  `(with-cps-conversion (,dest ,dest-gensym) ,res)
                `(let ((,dest-gensym ,dest))
                   ,res)))
    res))

(def-cps-macro [raise-stmt] (exc var tb)
  (with-gensyms (e-exc e-tb)
    `(with-cps-conversion (,exc ,e-exc :nil-allowed t)
       (with-cps-conversion (,tb ,e-tb :nil-allowed t)
         (raise-stmt-1 ,e-exc ,var ,e-tb)))))
  
(def-cps-macro [return-stmt] (&optional value &environment e)
  ;; VALUE can be given, if this is inside a generator.
  (cond (value
         (raise-syntax-error 
          "Generator ~A may not `return' with a value."
          (format nil "~{~A~^.~}" (reverse (get-pydecl :context-name-stack e)))))
        ((get-pydecl :in-sub-generator e)
         `(yield-value :explicit-return))
        (t 
         (%mark-generator-finished))))

(def-cps-macro [slice-expr] (start stop step)
  (with-gensyms (e-start e-stop e-step)
    `(with-cps-conversion (,start ,e-start :nil-allowed t)
       (with-cps-conversion (,stop ,e-stop :nil-allowed t)
         (with-cps-conversion (,step ,e-step :nil-allowed t)
           ,(%call-continuation
             `([slice-expr] ,e-start ,e-stop ,e-step))))))) ;; No CPS
    
(def-cps-macro [subscription-expr] (item subs)
  (with-gensyms (e-subs e-item)
    `(with-cps-conversion (,item ,e-item)
       (with-cps-conversion (,subs ,e-subs)
         ,(%call-continuation `(py-subs ,e-item ,e-subs))))))

(def-cps-macro [suite-stmt] (stmts)
  (assert stmts () "Empty SUITE: should be disallowed by grammar.")
  (cond ((null (cadr stmts))
         `(%cps-convert ,(car stmts) ,%current-continuation))
        (t
         `(%cps-convert ,(car stmts)
                        ,(let ((flet-name (intern (format nil "suite \"~A; ...\"" (clpython.parser:py-pprint (cadr stmts)))
                                                  #.*package*)))
                           `(flet ((,flet-name (e-val)
                                     (declare (ignore e-val))
                                     (%cps-convert ([suite-stmt] ,(cdr stmts))
                                                   ,%current-continuation)))
                              #',flet-name))))))

(def-cps-macro [try-except-stmt] (suite except-clauses else-suite)
  (with-gensyms (e-exc)
    `(flet ((.attempt-handle-error (.c)
              (declare (ignorable .c))
              ,(loop with res = `(error .c)
                   for (exc var handler-suite) in (reverse except-clauses)
                   do (setf res `(with-cps-conversion (,exc ,e-exc :nil-allowed t)
                                   (let ((applicable (typecase ,e-exc 
                                                       ;; exception class, or typle of exception classes
                                                       (null t) ;; blank "except:" catches all
                                                       (list (loop for x in ,e-exc thereis (typep .c x)))
                                                       (t    (typep .c ,e-exc)))))
                                     (if applicable
                                         (progn ,@(when var `(([assign-stmt] .c (,var))))
                                                (with-last-raised-exception (.c)
                                                  (%cps-convert ,handler-suite ,%current-continuation)))
                                       ,res))))
                   finally (return res)))
            (.do-else ()
              ,(if else-suite
                   `(%cps-convert ,else-suite ,%current-continuation)
                 `(funcall ,%current-continuation nil))))
       (let ((gen-state (make-generator-state ,suite :sub-generator t)))
         (labels ((next-try-value (val-for-gener)
                    (handler-case
                        (receive-yielded-value (with-py-errors ()
                                                 (generator-state.input-value gen-state val-for-gener)))
                      ({Exception} (c) (.attempt-handle-error c))
                      (:no-error (val)
                        (case val
                          (:implicit-return (.do-else))
                          (:explicit-return ,(%mark-generator-finished))
                          (t ,(%store-continuation `(lambda (val-for-gener) (next-try-value val-for-gener)))
                             (yield-value val)))))))
           (next-try-value *the-none*))))))

(def-cps-macro [try-finally-stmt] (try-suite finally-suite)
  (with-gensyms (try-finally-stmt-k finalization-scheduled ignore)
    `(let ((,try-finally-stmt-k ,%current-continuation))
       (block try-finally-block
         (let ((gen-state (make-generator-state ,try-suite :sub-generator t))
               (,finalization-scheduled nil))
           (labels ((run-finally (k)
                      (with-cps-conversion (,finally-suite ,ignore)
                        (declare (ignore ,ignore))
                        ;; Whole finally-body executed -> remove finalization
                        (when ,finalization-scheduled
                          (unschedule-finalization ,finalization-scheduled))
                        (funcall k)))
                    (next-val (val-for-generator)
                      ;; Get new val from TRY:
                      ;;  - if this results in an error, run the FINALLY (which may raise another
                      ;;    exception itself) and raise the error again;
                      ;;  - if this does not result in an error, then FINALLY is not yet run.
                      (handler-case (receive-yielded-value
                                     (with-py-errors ()
                                       (generator-state.input-value gen-state val-for-generator)))
                        ({Exception} (c)
                          (run-finally (lambda () (error c))))
                        (:no-error (val)
                          (case val
                            ((:implicit-return :explicit-return)
                             ;; :implicit = normal end of TRY block.
                             ;; :explicit = TRY block did a RETURN, which means generator is finished.
                             (run-finally (if (eq val :explicit-return)
                                              (lambda () (%mark-generator-finished))
                                            (lambda () (funcall ,try-finally-stmt-k nil)))))
                            (t
                             ;; The TRY block yields a value. Maybe the generator will never be run
                             ;; further. To ensure the FINALLY block is run some time, schedule
                             ;; finalization function.
                             (unless ,finalization-scheduled
                               (setf ,finalization-scheduled (schedule-finalization gen-state 'generator-state.close)))
                             ,(%store-continuation `(lambda (v) (next-val v)))
                             (yield-value val)))))))
             (next-val *the-none*)))))))

(def-cps-macro [tuple-expr] (items)
  (with-gensyms (tuple-k)
    `(let ((,tuple-k ,%current-continuation))
       ,(loop
            with item-gensyms = (loop for i from 0 repeat (length items) collect (gensym (format nil "item-~A-" i)))
            with res = `(funcall ,tuple-k (make-tuple-unevaled-list ,item-gensyms))
            for x in (reverse items) and gensym in (reverse item-gensyms)
            do (setf res `(with-cps-conversion (,x ,gensym) ,res))
            finally (return res)))))

(def-cps-macro [unary-expr] (op val)
  (with-gensyms (e-val)
    (let ((py-op-func (get-unary-op-func-name op)))
      `(with-cps-conversion (,val ,e-val)
         ,(%call-continuation `(funcall (function ,py-op-func) ,e-val))))))

(defun contains-break-stmt-p (suite env)
  "Whether SUITE contains BREAK stmt (not within inner loop)."
  (when (get-pydecl :inside-cps-list-comprehension env)
    ;; List comprehension expansion is unwalkable; this is a hack to make the walker not crash.
    (return-from contains-break-stmt-p nil))
  (with-py-ast (form suite)
    (case (car form)
      (([for-in-stmt] [while-stmt]) (values nil t))
      (([funcdef-stmt] [classdef-stmt]) (values nil t))
      (([break-stmt]) (return-from contains-break-stmt-p t))
      (t form)))
  nil)

(defun contains-continue-stmt-p (suite env)
  "Whether SUITE contains BREAK stmt (not within inner loop)."
  (when (get-pydecl :inside-cps-list-comprehension env)
    ;; List comprehension expansion is unwalkable; this is a hack to make the walker not crash.
    (return-from contains-continue-stmt-p nil))
  (with-py-ast (form suite)
    (case (car form)
      (([for-in-stmt] [while-stmt]) (values nil t))
      (([funcdef-stmt] [classdef-stmt]) (values nil t))
      (([continue-stmt]) (return-from contains-continue-stmt-p t))
      (t form)))
  nil)

(def-cps-macro [while-stmt] (test suite else-suite &environment e)
  (let ((stmt-k %current-continuation))
    (with-gensyms (while-cont val ignore)
      `(labels (,@(when (contains-break-stmt-p suite e)
                    `((%break-cont () (funcall ,stmt-k nil))))
                  ,@(when (contains-continue-stmt-p suite e)
                      `((%continue-cont () (,while-cont))))
                  (,while-cont ()
                    (with-cps-conversion (,test ,val)
                      (if (py-val->lisp-bool ,val)
                          (with-cps-conversion (,suite ,ignore)
                            (declare (ignore ,ignore))
                            (,while-cont))
                        ,(if else-suite
                             `(%cps-convert ,else-suite ,stmt-k)
                           `(funcall ,stmt-k nil))))))
         (,while-cont)))))

(def-cps-macro [with-stmt] (expr var block &environment e)
  ;; Dissect the normal expansion, apply conversion to the Python part within.
  (multiple-value-bind (expansion exp-p)
      (macroexpand-1 `([with-stmt] ,expr ,var ,block) e)
    (assert exp-p)
    (destructuring-bind (with-namespace namespace suite)
        expansion
      (assert (eq with-namespace 'with-namespace))
      `(,with-namespace ,namespace
         (%cps-convert ,suite ,%current-continuation)))))

#||  
  (with-gensyms (mgr exit value no-exc exc-details c)
    `(with-cps-conversion (,expr ,mgr)
       (let* ((,exit (attr ,mgr '{__exit__}))
              (,value (py-call (attr ,mgr '{__enter__})))
              (,no-exc t))
         (declare (ignorable ,value))
         ;; VAR is a identifier-expr, so no CPS conversion.
         ,@(when var `((setf ,var ,value)))
         (let ((gen-state (make-generator-state ,suite :sub-generator t)))
           (labels ((next-suite-value (val-for-gener)
                      (handler-case
                          (receive-yielded-value (with-py-errors ()
                                                   (generator-state.input-value gen-state val-for-gener)))
                        (error (c)
                          (setf ,no-exc nil)
                          ...)
                        (:no-error (val)
                        (case val
                          (:implicit-return (.do-else))
                          (:explicit-return ,(%mark-generator-finished))
                          (t ,(%store-continuation `(lambda (val-for-gener) (next-try-value val-for-gener)))
                             (yield-value val)))))))
           (next-suite-value *the-none*)))
||#
         
(def-cps-macro [yield-expr] (val &environment e)
  (unless (eq :function (car (get-pydecl :context-type-stack e)))
    (raise-syntax-error "Expression `yield' was found outside function."))
  (with-gensyms (after-yield-k e-val)
    `(let ((,after-yield-k ,%current-continuation))
       (with-cps-conversion (,val ,e-val)
         ,(%store-continuation `(lambda (x) (funcall ,after-yield-k (parse-generator-input x))))
         
         ;; XXX There seems to be a bug in LW 6.0.1 on linux-x86, where E-VAL is suddenly unbound
         ;; in the YIELD-VALUE call. This seems to work around the issue for now:
         #+lispworks
         (funcall (lambda (x) (yield-value (car x))) (list ,e-val))
         #-lispworks
         (yield-value ,e-val)))))
    
(def-cps-macro [yield-stmt] (&optional value &environment e)
  ;; A yield statement is a yield expression whose value is not used.
  (unless (eq :function (car (get-pydecl :context-type-stack e)))
    (raise-syntax-error "Statement `yield' was found outside function."))
  `(%cps-convert ([yield-expr] ,(or value '*the-none*)) ,%current-continuation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debugging

#+allegro
(defmacro view-cps (string)
  (format t "Code: ~A~%" string)
  (let ((ast (parse string :one-expr t)))
    (format t "AST: ~A~%" ast)
    (let ((*debug-dummy-outer-namespace* t))
      (declare (special *debug-dummy-outer-namespace*))
      (eval `(macrolet ((m (&environment e)
                          (let ((me (excl::compiler-macroexpand '(%cps-convert ,ast 'k) e)))
                            (format t "CPS: ~S~%" me))))
               (m))))))
