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

(defparameter *pydecl-keys*
    '(:context                    ;; Innermost context, one of (:class :module :function).
      :context-stack              ;; List of class and function names, innermost first; car is :lambda if in anonymous function.
      :function-must-save-locals  ;; T to force functions to  support `locals()' call in body (normally deduced from body).
      :inside-function-p          ;; T iff inside FUNCDEF       (to check RETURN and GLOBAL).
      :inside-class-p             ;; T iff inside CLASSDEF      (to check GLOBAL).
      :inside-loop-p              ;; T iff inside WHILE of FOR  (to check BREAK, CONTINUE).
      :inside-setf-py-attr        ;; T if insides a (setf (py-attr ..) ..) form (work around Allegro CL bug).
      :declared-globals-current-scope ;; List of variable that in the current scope must be treated as globals
      :lexically-declared-globals ;; Variables declared global by functions
      :lexically-visible-vars     ;; List of variable names that can be closed over (excludes globals).
      :mod-futures                ;; The features imported from the __future__ module.
      #+(or):mod-globals-names          ;; Vector of variable names at the module level.
      :safe-lex-visible-vars      ;; List of all variables guaranteed to be lexically visible and bound; subset of :LEX-VIS-VARS.
      
      ;; in development (?):
      :class-globals
      )
  "The PYDECL keys currently in use")

(defvar *check-valid-pydecl-keys* nil
  "Whether to check for validity of key upon setting or getting")

(defparameter *use-environment-acccessors*
      #+(and :allegro :new-environments) t
      #-(and :allegro :new-environments) nil
   "Whether to use CLTL-like environment accessor functions.")
;; This is controlled by a dynamic variable and not by a read-time conditional,
;; so that the test suite can force Allegro to not use its new env accessors.

(defun all-use-environment-accessor-values ()
  "Used by unit test"
  (if *use-environment-acccessors* '(nil t) '(nil)))

#+(and :allegro :new-environments)
(sys:define-declaration
    pydecl (&rest property-pairs) nil :declare
    (lambda (declaration env)
      (values :declare
              (cons 'pydecl
                    (nconc (copy-list (cdr declaration))
                           (sys:declaration-information 'pydecl env))))))

(defmacro with-pydecl (pairs &body body &environment env)
  (when *check-valid-pydecl-keys*
    (apply #'ensure-valid-pydecl-keys (mapcar #'car pairs)))
  (if *use-environment-acccessors*
      `(locally (declare (pydecl ,@pairs))
         ,@body)
    `(symbol-macrolet ((%pydecl-state% ,(nconc (copy-list pairs)
                                         (get-pydecl-state env))))
       (declare (ignorable %pydecl-state%))
       ,@body)))

(defun ensure-valid-pydecl-keys (&rest keys)
  (dolist (k keys)
    (unless (member k *pydecl-keys* :test 'eq)
      (error "Unknown pydecl key: `~A'." k))))

(defun get-pydecl-state (env)
  (if *use-environment-acccessors*
      (progn #+(and :allegro :new-environments)
             (sys:declaration-information 'pydecl env)
             #-(and :allegro :new-environments)
             (error "CLPython compiler error: ~S is true, but don't know how to
use the CLTL-like environment accessors in this Lisp: \"~A ~A\"."
                    '*use-environment-acccessors*
                    (lisp-implementation-type)
                    (lisp-implementation-version)))
    (multiple-value-bind (state state-p)
        (macroexpand-1 '%pydecl-state% env)
      (and state-p state))))

(defun get-pydecl (var env)
  (when *check-valid-pydecl-keys*
    (ensure-valid-pydecl-keys var))
  (let ((res (second (assoc var (get-pydecl-state env) :test #'eq))))
  
    #+(and :allegro :new-environments)
    (if *use-environment-acccessors*
        ;; Allegro CL 8.0, during file compilation, sometimes calls CONSTANTP without
        ;; environment. Apparently the problem is not triggered by the current compiler
        ;; set-up, but let's keep the check for now.
        (when (and (null res) (eq var :context))
          (break "Env info for var ~S is missing; assuming global scope." var)
          (setf res :module)))
    res))

#+(and :allegro :new-environments)
(progn
  ;; Because PYDECL declarations are ignored by Allegro (at least 8.0 and 8.1)
  ;; in the interpreter environment, use the MACROLET way to keep state in those
  ;; environments.
 
  (defun use-fallback-env-accessors (env)
    (and *use-environment-acccessors*
         (or (null env) ;; the environment in which EXEC lambda is compiled, is NIL 
             (ecase (sys::augmentable-environment-kind env)
               (:interpreter t)
               (:compiler    nil)
               ;; need to handle more cases?
               ))))
  
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
