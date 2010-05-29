;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython)

;;;; These readtables ease working with Abstract Syntax Trees
;;;; and Python variable names.

;; When parsing Python code, instead of using package prefixes it is must easier to
;; use a syntactical shorthand: "[NAME]" will refer to the symbol clpython.ast::NAME,
;; so we have [+], [def], [newline] etc.
;;
;; In the same way, {NAME} refers to clpython.user::NAME. It is required that
;; [NAME] already exists, while {NAME} is interned if new.

(defun setup-ast-readmacro (&optional (readtable *readtable*))
  (let ((read-[-func (read-package-symbol-func (find-package :clpython.ast) #\[ #\] :intern nil)))
    (set-macro-character #\[ read-[-func t readtable))
  readtable)

(defun setup-user-readmacro (&optional (readtable *readtable*))
  (let ((read-{-func (read-package-symbol-func (find-package :clpython.user) #\{ #\} :intern t)))
    (set-macro-character #\{ read-{-func t readtable))
  readtable)

(defvar *ast-readtable* 
    (setup-ast-readmacro (copy-readtable nil))
  "Readtable where [NAME] refers to clpython.ast::NAME")

(defvar *user-readtable*
    (setup-user-readmacro (copy-readtable nil))
  "Readtable where {NAME} refers to clpython.user::NAME")

(defvar *ast-user-readtable*
    (setup-user-readmacro (setup-ast-readmacro (copy-readtable nil)))
  "Readtable where [NAME] refers to clpython.ast::NAME and {NAME} to clpython.user::NAME")

;; To be able to mention these readtables in mode lines, they have to
;; be named.
#+allegro
(progn (setf (excl:named-readtable :py-ast-readtable) *ast-readtable*)
       (setf (excl:named-readtable :py-user-readtable) *user-readtable*)
       (setf (excl:named-readtable :py-ast-user-readtable) *ast-user-readtable*))

(defmacro with-ast-user-readtable (&body body)
  `(let ((*readtable* *ast-user-readtable*))
     ,@body))
