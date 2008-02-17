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

;;; Generators

;;; Generators containing only yield statements are rewritten into
;;; state machines, while generators containing yield expressions
;;; are made into processes.

(defun rewrite-generator-funcdef-suite (fname suite)
  ;; Returns the function body
  (assert (or (symbolp fname) (listp fname)))
  (assert (match-p suite '([suite-stmt] ?_)) ()
    "CAR of SUITE must be SUITE-STMT, but got: ~S" (car suite))
  (let ((yield-found (generator-ast-p suite)))
    (assert yield-found)
    (let ((f (if (member '[yield-expr] yield-found)
                 #'rewrite-generator-funcdef-also-yield-expr
               #'rewrite-generator-funcdef-only-yield-stmt)))
      (funcall f fname suite))))
    
(defun rewrite-generator-funcdef-also-yield-expr (fname suite)
  (declare (ignorable fname suite))
  (error "todo")
  #+(or)
  `(make-generator-process
      ',fname
      (excl:named-function (:iterator-from-function ,fname :expr)
        (lambda ()
          (macrolet (([yield-stmt] (val) `(gen.yield ,val))
                     ([yield-expr] (val) `(gen.yield ,val)))
            ,suite)))))

(defun rewrite-generator-funcdef-only-yield-stmt (fname suite)
  ;; Returns the function body
  (assert (or (symbolp fname) (listp fname)))
  (assert (match-p suite '([suite-stmt] ?_)) ()
    "CAR of SUITE must be SUITE-STMT, but got: ~S" (car suite))
  (assert (generator-ast-p suite))

  (with-py-ast (form suite)
    (case (car form)
      (([yield-expr])    (error "`Yield' expression not supported yet."))
      (([classdef-stmt] [funcdef-stmt]) (values nil t))
      (t                                form)))
  
  (let ((yield-counter 0)
	(other-counter 0)
	(vars ()))
    (flet ((new-tag (kind) (if (eq kind :yield)
			       (incf yield-counter)
			     (make-symbol (format nil "~A~A" kind (incf other-counter)))))
           
	   (grouped-into-suites (list)
             ;; Group sequences of expressions into [suite] statements,
             ;; the goal of which is triggering the :safe-lex-visible-vars optimization.
	     (let ((res      ())
		   (non-tags ()))
               (dolist (x list)
		 (if (listp x)
		     (push x non-tags)
		   (progn
		     (when non-tags
		       (push `([suite-stmt] ,(nreverse non-tags)) res)
		       (setf non-tags nil))
		     (push x res))))
               (when non-tags
		 (push `([suite-stmt] ,(nreverse non-tags)) res))
               (nreverse res)))
           
           (suite->generator (fname suite)
             ;; Wrap `suite' inside a function. Upon execution, besides `yield'-ing the
             ;; values generated in the suite, the function will return :explicit-return or
             ;; :implicit-return depending on whether the suite executed a `return' statement."
             (flet ((suite-walker (form &rest context)
                      (declare (ignore context))
                      (case (car form)
                        (([funcdef-stmt] [classdef-stmt]) (values form t))
                        ([return-stmt] (when (second form)
                                         (py-raise '{SyntaxError}
                                                   "Inside generator, `return' statement may ~
				         not have an argument (got: ~S)" form))
                                       (values `(return-from function-body :explicit-return)
                                               t))
                        (t form))))
               `(excl:named-function ,fname
                  (lambda ()
                    ,(rewrite-generator-funcdef-suite
                      fname
                      `([suite-stmt] (,(walk-py-ast suite #'suite-walker :build-result t)
                                      (return-from function-body :implicit-return)))))))))
      (labels
	  ((walk (form stack)
	     (walk-py-ast
	      form
	      (lambda (form &rest context)
		(declare (ignore context))
		(case (first form)
		  
		  ([break-stmt]
                   (if stack
                       (values `(go ,(cdr (car stack))) t)
                     (py-raise '{SyntaxError} "Statement `break' was found outside loop.")))
		    
		  ([continue-stmt]
                   (if stack
                       (values `(go ,(car (car stack))) t)
                     (py-raise '{SyntaxError} "Statement `continue' was found outside loop.")))
		  
		  ([for-in-stmt]
		   (destructuring-bind (target source suite else-suite) (cdr form)
		     (let* ((repeat-tag (new-tag :repeat))
			    (else-tag   (new-tag :else))
			    (end-tag    (new-tag :end+break-target))
			    (continue-tag (new-tag :continue-target))
			    (generator  (new-tag :generator))
			    (loop-var   (new-tag :loop-var))
			    (stack2     (cons (cons continue-tag end-tag)
					      stack)))
		       (push loop-var vars)
		       (push generator vars)
		       
		       (values
			`(:split
                          ;; create generator; request first value
                          (setf ,generator (get-py-iterate-fun ,source)
				,loop-var  (funcall ,generator))
			  (unless ,loop-var (go ,else-tag))
                          ;; handle a value
			  ,repeat-tag
			  ([assign-stmt] ,loop-var (,target))
			  (:split ,(walk suite stack2))
                          (go ,continue-tag) ;; prevent warnings
			  ,continue-tag
                          ;; request new value
			  (setf ,loop-var (funcall ,generator))
			  (if ,loop-var (go ,repeat-tag) (go ,end-tag))
			  
			  ,else-tag
			  ,@(when else-suite
			      `((:split ,(walk else-suite stack2))))
			  
			  ,end-tag
			  (setf ,loop-var nil
				,generator nil))
			t))))
		  
		  ([if-stmt]
                   ;; E.g. in this example, the 'continue' must be rewritten
                   ;; correspondingly to the rewritten 'while':
                   ;; 
                   ;;  def f():
                   ;;    while test:
                   ;;      yield 1
                   ;;      if foo:
                   ;;        continue
		   (destructuring-bind (clauses else-suite) (cdr form)
		     (loop with else-tag = (new-tag :else) and after-tag = (new-tag :after)
									 
			 for (expr suite) in clauses
			 for then-tag = (new-tag :then)
					
			 collect `((py-val->lisp-bool ,expr) (go ,then-tag)) into tests
			 collect `(:split ,then-tag
					  (:split ,(walk suite stack))
					  (go ,after-tag)) into suites
			 finally
			   (return
			     (values `(:split (cond ,@tests
						    (t (go ,else-tag)))
					      (:split ,@suites)
					      ,else-tag
					      ,@(when else-suite
						  `((:split ,(walk else-suite stack))))
					      ,after-tag)
				     t)))))
		    
		  ([return-stmt]
		   (when (second form)
		     (py-raise '{SyntaxError}
			       "Inside generator, `return' statement may not have ~
                                an argument (got: ~S)" form))
                   ;; From now on, we will always return to this state
		   (values `(generator-finished) t))

		  ([suite-stmt]
		   (values `(:split ,@(loop for stmt in (second form)
					  collect (walk stmt stack)))
			   t))

                  ([try-except-stmt]
                   ;; Three possibilities:
                   ;;  1. YIELD-STMT or RETURN-STMT in TRY-SUITE 
                   ;;  2. YIELD-STMT or RETURN-STMT in some EXCEPT-CLAUSES
                   ;;  3. YIELD-STMT or RETURN-STMT in ELSE-SUITE
                   ;; 
                   ;; We rewrite it once completely, such that all
                   ;; cases are covered. Maybe there is more rewriting
                   ;; going on than needed, but it doesn't hurt.
                   (destructuring-bind (try-suite except-clauses else-suite) (cdr form)
		     (loop
			 with try-tag = (new-tag :yield)
			 with else-tag = (new-tag :else)
			 with after-tag = (new-tag :after)
			 with gen = (gensym "helper-gen")
									
			 initially (push gen vars)
				   
			 for (exc var suite) in except-clauses
			 for tag = (new-tag :exc-suite)
				   
			 collect `(,exc ,var (go ,tag)) into jumps
			 nconc `(,tag ,(walk suite stack) (go ,after-tag)) into exc-bodies
										
			 finally
			   (return
			     (values
                              ;; By creating a dummy function for the suite, and iterating over
                              ;; the values that function yields, the suite itself does not have
                              ;; to be analyzed and rewritten.
			      `(:split
				(setf ,gen (get-py-iterate-fun
					    (funcall ,(suite->generator `(,fname :try-suite) try-suite))))
				(setf .state. ,try-tag)
				
                                ;; yield all values returned by helper function .gen.
				,try-tag
				([try-except-stmt]
                                 (let ((val (funcall ,gen))) ;; try-suite
				   (case val
				     (:explicit-return (generator-finished))
				     (:implicit-return (go ,else-tag))
				     (t (return-from function-body val))))
                                 ,jumps ;; exception handlers
                                 nil) ;; else-suite
				
				,@exc-bodies ;; handler bodies
				
				,else-tag
				,@(when else-suite `((:split ,(walk else-suite stack))))
				
				,after-tag
				(setf ,gen nil))
			      t)))))
		  
		  ([try-finally-stmt]
		   (destructuring-bind (try-suite finally-suite) (cdr form)
		     (when (generator-ast-p try-suite)
		       (py-raise '{SyntaxError}
				 "YIELD is not allowed in the TRY suite of ~
                                  a TRY/FINALLY statement (got: ~S)" form))
                     (let ((fin-catched-exp (gensym "fin-catched-exc")))
                       (pushnew fin-catched-exp vars)
		       (values
			`(:split
			  (multiple-value-bind (val cond)
			      (ignore-errors ,try-suite ;; no need to walk
					     (values))
			    (setf ,fin-catched-exp cond))
                          ,(walk finally-suite stack)
                          (when ,fin-catched-exp
			    (error ,fin-catched-exp)))
                        t))))
		  
		  ([while-stmt]
		   (destructuring-bind (test suite else-suite) (cdr form)
		     (let ((repeat-tag (new-tag :repeat))
			   (else-tag   (new-tag :else))
			   (after-tag  (new-tag :end+break-target)))
		       (values `(:split
				 (unless (py-val->lisp-bool ,test)
				   (go ,else-tag))

				 ,repeat-tag
				 (:split
				  ,(walk suite
					 (cons (cons repeat-tag after-tag)
					       stack)))
				 (if (py-val->lisp-bool ,test)
				     (go ,repeat-tag)
				   (go ,else-tag))
				 
				 ,else-tag
				 ,@(when else-suite
				     `((:split ,(walk else-suite stack))))
				 
                                 (go ,after-tag)
				 ,after-tag)
			       t))))
		  
		  ([yield-stmt]
		   (let ((tag (new-tag :yield)))
		     (values `(:split (setf .state. ,tag)
				      (return-from function-body ,(or (second form) '*the-none*))
				      ,tag)
			     t)))
                  
                  (t (values form t))))
	      :build-result t)))

	(let* ((walked-as-list (multiple-value-list (apply-splits (walk suite ()))))
               (walked-list-with-suites (grouped-into-suites walked-as-list))
               (final-tag -1))
	  
	  `(let ((.state. 0) ,@(nreverse vars))
             (make-iterator-from-function 
	      :name '(,fname generator)
	      :func
	      (excl:named-function (,fname generator-internal-lambda)
		(lambda ()
                  ;; This is the function that will repeatedly be
                  ;; called to return the values
                  (macrolet ((generator-finished ()
			       '(progn (setf .state. ,final-tag)
				 (go ,final-tag))))
                    (block function-body
		      (tagbody
			(case .state.
			  ,@(loop for i from -1 to yield-counter
				collect `(,i (go ,i))))
		       0
			,@walked-list-with-suites
                        (generator-finished)
                        ,final-tag
			(return-from function-body nil)))))))))))))

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
          ((,(third first-for)) nil nil nil))))))

;;; Processes

#||
(eval-when (:compile-top-level :load-top-level :execute) 
  (require :process))

(defvar *debug* t)

(define-condition generator-yield (condition)
  ((value :initarg :value :accessor generator-yield-value))
  (:report (lambda (c s)
             (format s "Generator has yielded value ~A."
                     (generator-yield-value c)))))

(defstruct (generator-interface (:conc-name "gi-") (:constructor make-gi))
  "Generator interface state"
  process c->g g->c c-proc g-proc started-p)

(defun make-generator-process (fname f)
  (check-type f function)
  (let* ((gen-if (gen.start f))
         (f (lambda (&optional val)
              (gen.send gen-if val)))
         (x (make-iterator-from-function :name `(:generator-process ,fname)
                                         :func f)))
    (change-class x 'py-func-iterator-sendable)
    x))

(defun gen.start (func)
  "Start a new process for FUN, which will be gen.wrap'ed"
  (check-type func function)
  (let* ((gen-if (make-gi :c-proc mp:*current-process*)))
    (setf (gi-g-proc gen-if)
      (mp:process-run-function (format nil "Generator ~A" func)
        (gen.wrap func gen-if)))
    gen-if))

(defun gen.send (if val)
  "Send value (for yield expr) to generator, waiting for next value."
  (check-type if generator-interface)
  (when (and val (not (gi-started-p if)))
    (py-raise '{TypeError} 
              "Generator must be initialized with `next', not `send' (got value: ~A)" val))
  (setf (gi-c->g if) (or val *the-none*))
  (mp:without-scheduling
    (when *debug*
      (format t "[gen.send] Enabling generator, disabling caller~%"))
    (mp:process-enable (gi-g-proc if))
    (mp:process-disable (gi-c-proc if)))
  ;; Wait until we are enabled by the generator...
  (when *debug*
    (format t "[gen.send] Caller enabled; got from generator: ~A~%" (gi-g->c if)))
  (setf (gi-started-p if) t)
  (gi-g->c if))

(defun gen.wrap (f if)
  "Returns a lambda that will call F, invoking `send-value' on subsequent calls."
  (flet ((yield-wait-restart (c)
           (let ((r (find-restart 'gen.send-value-restart)))
             (assert r () "Generator wrapper misses `gen.send-value-restart' restart.")
             (when *debug* (format t "[gen.wrap] Wrapper receiving yielded value: ~A.~%"
                                   (generator-yield-value c)))
             (setf (gi-g->c if) (generator-yield-value c))
             (setf (gi-c->g if) :bla)
             
             (mp:without-scheduling
               (when *debug*
                 (format t "[gen.wrap] Caller enabled, generator disabled.~%"))
               (mp:process-enable (gi-c-proc if))
               (mp:process-disable (gi-g-proc if)))
             
             (when *debug*
               (format t "[gen.wrap] Resuming generator with value ~A.~%" (gi-c->g if)))
             (assert (not (eq (gi-c->g if) :bla)))
             (invoke-restart r (gi-c->g if)))))
    
    (lambda ()
      (handler-bind ((generator-yield #'yield-wait-restart))
        (funcall f)))))

(defun gen.yield (yield-val)
  "Signal a Yield condition. Execution is resumed by invoking the `send-value' restart."
  (let ((c (load-time-value (make-condition 'generator-yield))))
    (restart-bind ((gen.send-value-restart (lambda (sent-val) (return-from gen.yield sent-val))))
      (setf (generator-yield-value c) yield-val)
      (error c))))
||#
