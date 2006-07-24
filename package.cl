;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :user)

(defpackage :python
  (:documentation "An implementation of Python in Common Lisp.")
  (:use #:common-lisp)
  (:shadow ))

(defpackage :python-builtin-functions
  (:nicknames :pybf)
  (:use )
  (:export "__import__" "abs" "apply" "callable" "chr" "cmp" "coerce" "compile"
	   "delattr" "dir" "divmod" "eval" "execfile" "filter" "getattr" "globals"
	   "hasattr" "hash" "hex" "id" "input" "intern" "isinstance" "issubclass"
	   "iter" "len" "locals" "map" "max" "min" "oct" "ord" "pow" "range"
	   "raw_input" "reduce" "reload" "repr" "round" "setattr" "sorted" "sum"
	   "unichr" "vars" "zip"))

(defpackage :python-builtin-types
  (:nicknames :pybt)
  (:use )
  (:export "basestring" "bool" "complex" "dict" "enumerate" "float" "int" "list"
	   "long" "slice" "str" "super" "tuple" "xrange" "classmethod"
	   "staticmethod" "property" "object" "type" "unicode"))

(defpackage :python-builtin-values
  (:nicknames :pybv)
  (:use )
  (:export "None" "Ellipsis" "True" "False" "NotImplemented"))
