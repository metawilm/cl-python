;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON; Readtable: PY-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython)
(in-syntax clpython:*user-readtable*)

(defclass buffer (clpython:object)
  ()
  (:metaclass clpython:py-type))

(defmethod initialize-instance ((x buffer) &rest initargs)
  (declare (ignore initargs))
  (py-raise '{NotImplementedError} 
            "Class BUFFER is not implemented."))