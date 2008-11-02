;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Set and Frozenset

(in-package :clpython)
(in-syntax *user-readtable*)

(defclass py-set (object)
  ()
  (:metaclass py-type))

(defclass frozenset (object)
  ()
  (:metaclass py-type))

;; much TODO...