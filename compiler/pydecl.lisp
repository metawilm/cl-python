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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *pydecl-keys*
      '(;; Contexts: module, class, function
        :context-type-stack   ;; Stack of (:class :module :function)
        :context-name-stack   ;; List of function and class names as symbols
        
        ;; Namespaces map names to values
        :namespace            ;; Current namespace (which has pointer to parent)
        :declared-globals-current-scope ;; Variables that in current scope are globals
        :lexically-declared-globals     ;; Variables declared global by functions
        :lexically-visible-vars         ;; Variables that can be closed over (excludes globals)
        
        :inside-loop-p        ;; T iff inside WHILE of FOR, to check BREAK, CONTINUE)
        :inside-setf-py-attr  ;; T iff inside (setf (py-attr ..) ..), to work around Allegro CL issue
                
        ;; Generator CPS conversion:
        :in-sub-generator
        :in-generator-toplevel
        :inside-cps-list-comprehension ;; T iff inside CPS conversion of LISTCOMPR-EXPR
        :inside-cps-conversion      ;; T iff inside CPS conversion
        
        ;; Optimizaton:
        :safe-lex-visible-vars  ;; Lexically visible variables guaranteed to be bound
        :may-inline-iteration
        
        ;; TODO: 
        ;; :mod-futures         ;; The features imported from the __future__ module.
        )
    "The PYDECL keys currently in use"))

(define-macro-state-declaration pydecl #.*pydecl-keys*)



