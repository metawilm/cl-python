;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE._CODECS; Readtable: PY-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.module._codecs)
(in-syntax clpython:*user-readtable*)

(defparameter *search-functions* ())

(defun |lookup_error| (&rest args)
  (py-raise '{LookupError} "_codecs.lookup_error() args: ~S" args))

(defun |register| (search-func)
  ;; TODO check order
  (push search-func *search-functions*))
