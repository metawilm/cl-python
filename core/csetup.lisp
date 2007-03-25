;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython)

;;; Raising exceptions

(defun py-raise (exc-type string &rest format-args)
  "Raise a Python exception with given format string"
  (error exc-type :args (cons string format-args)))
