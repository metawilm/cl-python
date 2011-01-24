;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.UTIL -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.util)

;;;; Keeping lexical context state in macros

(defmacro define-macro-state-declaration (decl-name allowed-keys)
  "Create macro WITH-{NAME}, functions GET-{NAME}, CHECK-{NAME} to store
and retrieve declarations. ALLOWED-KEYS is a list of allowed keys (symbols).
Usage: if DECL-NAME is D, then macros can expand into:
    `(WITH-D ((:key1 val1) (:key2 val2)) ..body..)
and inside ..body.. macros can evaluate:
    ,(GET-D :key1 environment)
to retrieve value for :key1."
  (check-type decl-name symbol)
  (check-type allowed-keys (and list (not null)))
  (flet ((create-symbol (&rest args)
           (intern (apply #'concatenate 'string (mapcar #'string args))
                   (symbol-package decl-name))))
    (let* ((decl-keys-name (create-symbol '#:* decl-name '#:-declared-keys*))
           (def-add-keys-name (create-symbol '#:define- decl-name '#:-keys))
           (with-name (create-symbol '#:with- decl-name))
           (get-name (create-symbol '#:get- decl-name))
           (check-keys-name (create-symbol '#:check- decl-name '#:-keys))
           (state-name (create-symbol '#:% decl-name '#:-state%)))
      `(progn ;; eval-when ?

         (defparameter ,decl-keys-name ',allowed-keys
           ,(format nil "List of allowed keys for declaration \"~A\"." decl-name))
             
         (defun ,check-keys-name (&rest keys)
           (dolist (key keys)
             (block check-key
               (unless (member key ,decl-keys-name)
                 (restart-case (break "Declaration \"~A\" does not allow key ~S. ~
                                       ~_The allowed keys are: ~{~S~^, ~}." 
                                      ',decl-name key ,decl-keys-name)
                   (declare-new-key ()
                     :report (lambda (s)
                               (format s "Declare \"~A\" as new key for \"~A\"."
                                       key ',decl-name))
                     (pushnew key ,decl-keys-name)
                     (return-from check-key)))))))
         
         (defmacro ,def-add-keys-name (&rest keys)
           `(eval-when (:compile-toplevel :load-toplevel :execute)
              ,@(loop for k in keys collect `(pushnew ,k ,',decl-keys-name))))
                  
         #+(and :allegro :new-environments)
         ,(with-gensyms (env declaration property-pairs)
            `(sys:define-declaration
                 ,decl-name (&rest ,property-pairs) nil :declare
                 (lambda (,declaration ,env)
                   (values :declare
                           (cons ',decl-name
                                 (nconc (copy-list (cdr ,declaration))
                                        (sys:declaration-information ',decl-name ,env)))))))
         
         (defmacro ,with-name (pairs &body body)
           (apply #',check-keys-name (mapcar #'car pairs))
           (list* 'with-custom-decl ',decl-name ',state-name pairs body))
         
         (defgeneric ,get-name (var env)
           (:method (var env)
                    (,check-keys-name var)
                    (second (assoc var (get-decl-state ',decl-name ',state-name env) :test #'eq))))))))

(defparameter *use-environment-acccessors*
      #+(and :allegro :new-environments) t
      #-(and :allegro :new-environments) nil
   "Whether to use CLTL-like environment accessor functions.")

(defun all-use-environment-accessor-values ()
  "For e.g. use by unit tests, to test all compilation variants."
  (if *use-environment-acccessors* '(nil t) '(nil)))

(defmacro with-custom-decl (decl-name state-name pairs &body body &environment env)
  (if *use-environment-acccessors*
      `(locally (declare (,decl-name ,@pairs))
         ,@body)
    `(symbol-macrolet ((,state-name
                           ,(nconc (copy-list pairs)
                             (get-decl-state decl-name state-name env))))
       #-ecl ;; work around compiler bug
       (declare (ignorable ,state-name))
       ,@body)))

(defun get-decl-state (decl-name decl-state-name env)
  (declare (ignorable decl-name))
  (if *use-environment-acccessors*
      (progn #+(and :allegro :new-environments)
             (sys:declaration-information decl-name env)
             #-(and :allegro :new-environments)
             (error "Don't know how to use the CLTL-like environment accessors in this Lisp."))
    (multiple-value-bind (state expanded-p)
        (macroexpand-1 decl-state-name env)
      (and expanded-p state))))

;; Because PYDECL declarations are ignored by Allegro 8.0/8.1 in the interpreter
;; environment, use the MACROLET way to keep state in those environments.
#+(and :allegro :new-environments)
(progn
  (defun use-fallback-env-accessors (env)
    (and *use-environment-acccessors*
         (etypecase env
           (null t)
           (sys::augmentable-environment
            (case (sys::augmentable-environment-kind env)
              (:interpreter
               ;; If excl::*note-interpreted-declarations* is set, things are fine.
               ;; That switch was introduced later than :new-environments in *features*.
               (let ((sym (find-symbol (string '*note-interpreted-declarations*) :excl)))
                 (if sym
                     (not (symbol-value sym))
                   t)))
              ((:compiler :compilation) nil)
              (t (break "New kind of environment found: ~A."
                        (sys::augmentable-environment-kind env))))))))
  
  (excl:def-fwrapper with-custom-decl-fwrapper (form env)
    (if (use-fallback-env-accessors env)
        (let ((*use-environment-acccessors* nil))
          (excl:call-next-fwrapper))
      (excl:call-next-fwrapper)))
  
  (excl:fwrap 'with-custom-decl 'with-custom-decl-fwrapper 'with-custom-decl-fwrapper)
  
  (excl:def-fwrapper get-decl-state-fwrapper (decl-name decl-state-name env)
    (declare (ignore decl-name decl-state-name))
    (if (use-fallback-env-accessors env)
        (let ((*use-environment-acccessors* nil))
          (excl:call-next-fwrapper))
      (excl:call-next-fwrapper)))
  
  (excl:fwrap 'get-decl-state 'get-decl-state-fwrapper 'get-decl-state-fwrapper))
