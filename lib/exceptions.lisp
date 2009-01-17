;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE.EXCEPTIONS; Readtable: PY-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.module.exceptions)
(clpython::in-syntax clpython::*user-readtable*)

(defparameter |ValueError| (find-class '{ValueError}))
(defparameter |SyntaxError| (find-class '{SyntaxError}))