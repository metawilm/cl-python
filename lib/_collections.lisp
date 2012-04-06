;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE._COLLECTIONS -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package #:clpython.module._collections)

(defclass |deque| (clpython:object)
  ()
  (:metaclass clpython:py-type))

(defclass |defaultdict| (clpython:object)
  ()
  (:metaclass clpython:py-type))

