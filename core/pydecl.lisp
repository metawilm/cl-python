;; -*- package: clpython  -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython)

;;; CLPython Compiler Declarations
;;
;; The Python compiler uses its own set of declaration to keep state
;; in generated code. This declaration is named "pydecl".
;; The declarations are in the form of (:key ...val...) pairs.
;; 
;; Currently, the following "pydecl" declaration keys are used:
;;  
;;  :mod-globals-names          : vector of variable names at the module level
;;  :mod-futures                : the features imported from the __future__ module
;;
;;  :context                    : innermost context, one of (:class :module :function)
;;  :context-stack              : list of class and function names, innermost first;
;;                                starts with :lambda if in anonymous function
;;  
;;  :lexically-declared-globals : list of variable names declared global in an outer scope
;;                                (which makes them also global in inner scopes)
;;  :lexically-visible-vars     : list of variable names that can be closed over
;; 
;;  :inside-loop-p              : T iff inside WHILE of FOR  (to check BREAK, CONTINUE)
;;  :inside-function-p          : T iff inside FUNCDEF       (to check RETURN)
;;
;;  :safe-lex-visible-vars      : list of all variables guaranteed to be lexically visible and bound;
;;                                subset of :LEXICALLY-VISIBLE-VARS.
;;
;;  :inside-setf-py-attr        : T if insides a (setf (py-attr ..) ..) form (to work around Allegro CL
;;                                issues w.r.t. compiler macros and setf forms.

(defparameter *use-environment-acccessors*
      #+(and :allegro :new-environments) t
      #-(and :allegro :new-environments) nil
   "Whether to use CLTL-like environment accessor functions.")
;; This is controlled by a dynamic variable and not by a read-time conditional,
;; so that the test suite can force Allegro to not use its new env accessors.

#+(and :allegro :new-environments)
(sys:define-declaration
    pydecl (&rest property-pairs) nil :declare
    (lambda (declaration env)
      (values :declare
              (cons 'pydecl
                    (nconc (copy-list (cdr declaration))
                           (sys:declaration-information 'pydecl env))))))

(defmacro with-pydecl (pairs &body body &environment env)
  (if *use-environment-acccessors*
      `(locally (declare (pydecl ,@pairs))
         ,@body)
    `(macrolet ((%pydecl-state% () ',(nconc (copy-list pairs)
                                            (get-pydecl-state env))))
       ,@body)))

(defun get-pydecl-state (env)
  (if *use-environment-acccessors*
      (progn #+(and :allegro :new-environments)
             (sys:declaration-information 'pydecl env)
             #-(and :allegro :new-environments)
             (error "CLPython compiler item: ~S is true, but don't know how to
use the CLTL-like environment accessors in this Lisp:~%  ~A ~A."
                    '*use-environment-acccessors*
                    (lisp-implementation-type)
                    (lisp-implementation-version)))
    (multiple-value-bind (state state-p)
        (macroexpand-1 '(%pydecl-state%) env)
      (and state-p state))))

(defun get-pydecl (var env)
  (let ((res (second (assoc var (get-pydecl-state env) :test #'eq))))
  
    #+(and :allegro :new-environments)
    (if *use-environment-acccessors*
        ;; Allegro CL 8.0, during file compilation, sometimes calls CONSTANTP without
        ;; environment. Apparently the problem is not triggered by the current compiler
        ;; set-up, but let's keep the check for now.
        (when (and (null res) (eq var :context))
          (warn "Env info for var ~S is missing; assuming global scope." var)
          (setf res :module)))
    res))


#+(and :allegro :new-environments)
(progn
  ;; Because PYDECL declarations are (currently) ignored by Allegro in the
  ;; interpreter environment, use the MACROLET way to keep state in those
  ;; environments.
 
  (defun use-fallback-env-accessors (env)
    (and *use-environment-acccessors*
         (ecase (sys::augmentable-environment-kind env)
           (:interpreter t)
           (:compiler    nil)
           ;; need to handle more cases?
           )))
  
  (excl:def-fwrapper with-pydecl-fwrapper (form env)
    (if (use-fallback-env-accessors env)
        (let ((*use-environment-acccessors* nil))
          (excl:call-next-fwrapper))
      (excl:call-next-fwrapper)))
  
  (excl:fwrap 'with-pydecl 'with-pydecl-fwrapper 'with-pydecl-fwrapper)

  (excl:def-fwrapper get-pydecl-fwrapper (var env)
    (if (use-fallback-env-accessors env)
        (let ((*use-environment-acccessors* nil))
          (excl:call-next-fwrapper))
      (excl:call-next-fwrapper)))
  
  (excl:fwrap 'get-pydecl 'get-pydecl-fwrapper 'get-pydecl-fwrapper))
