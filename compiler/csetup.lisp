;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON; Readtable: PY-AST-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython)
(in-syntax *ast-user-readtable*)

;;; Raising exceptions

(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *exceptions-are-python-objects*
    #+allegro t
    #+lispworks t
    #+cmu nil    ;; CMUCL does not allow arbitrary meta/superclasses in conditions
    #-(or allegro lispworks cmu) nil)

(register-feature :clpython-exceptions-are-python-objects *exceptions-are-python-objects*)
)

(define-compiler-macro py-call (&whole whole prim &rest args)
  (with-perhaps-matching (prim ([attributeref-expr] ?x ([identifier-expr] ?attr-sym)))
    ;; Optimize "obj.attr(..args..)" = (py-call (py-attr obj attr) ..args..)
    ;; so the allocation of a bound method object is skipped.
    (let ((res `(let* ((.x ,?x)
                       (.val (class.attr-no-magic (py-class-of .x) ',?attr-sym)))
                  (if (and (functionp .val) (not (instance.attr-no-magic .x ',?attr-sym)))
                      (funcall .val .x ,@args)
                    (locally (declare (notinline py-call))
                      (py-call (attr .x ',?attr-sym) ,@args))))))
      #+(or)(warn "py-call cm res: ~A" res)
      (return-from py-call res)))
  whole)



;; If a macro raises an ERROR, it is undefined whether the compiler
;; catches that or passes it on outwards. CLHS on COMPILE: "compile is
;; permitted, but not required, to establish a handler for conditions
;; of type error."
;; 
;; In our case, certain syntax errors are detected during
;; macroexpansion. By using non-ERROR conditions, as suggested by
;; Nikodemus Siivola, we can communicate that event portably to an
;; outer level. It does require that every compilation of a Python
;; source file is wrapped in the WITH-form.
;;
;; <http://article.gmane.org/gmane.lisp.steel-bank.devel/11291>

(define-condition raise-syntax-error-please (condition)
  ((syntax-error-args :initarg :syntax-error-args :accessor rsep-syntax-error-args)))

(defun compiler-detect-syntax-error (&rest syntax-error-args)
  (signal (make-condition 'raise-syntax-error-please
            :syntax-error-args syntax-error-args))
  ;; If no-one is listening (so not inside with-compiler-generated-syntax-errors)
  (break "Uncaught SyntaxError: ~A" (apply #'format nil syntax-error-args)))

(defmacro with-compiler-generated-syntax-errors (() &body body)
  `(handler-bind ((raise-syntax-error-please
                   (lambda (c)
                     (apply #'py-raise '{SyntaxError} (rsep-syntax-error-args c)))))
     (let ((*raise-syntax-error-hook* 'compiler-detect-syntax-error))
       ,@body)))
