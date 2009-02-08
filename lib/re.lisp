;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE.RE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;; Regular Expressions
;;; http://docs.python.org/lib/node46.html

(in-package :clpython.module.re)

(clpython::def-proxy-class |reg-exp| ()
  ((pattern :initarg :pattern)
   (flags :initarg :flags)))

(defconstant |I| 2)

(defconstant-once |X| 64)
(defconstant-once |VERBOSE| |X|)

(defun |compile| (pattern &optional flags)
  (make-instance '|reg-exp|
    :pattern pattern
    :flags flags))

(defun |match| (&rest args)
  (declare (ignore args))
  (cl:error "todo"))

(defun |search| (&rest args)
  (declare (ignore args))
  (cl:error "todo"))

(defun |split| (&rest args)
  (declare (ignore args))
  (cl:error "todo"))

(defun |findall| (&rest args)
  (declare (ignore args))
  (cl:error "todo"))

(defun |finditer| (&rest args)
  (declare (ignore args))
  (cl:error "todo"))

(defun |sub| (&rest args)
  (declare (ignore args))
  (cl:error "todo"))

(defun |subn| (&rest args)
  (declare (ignore args))
  (cl:error "todo"))

(defun |escape| (&rest args)
  (declare (ignore args))
  (cl:error "todo"))

(defun |error| (&rest args)
  (declare (ignore args))
  (cl:error "todo"))


(def-py-method |reg-exp.compile| (re pattern &optional flags)
  (cl:error "todo"))

(def-py-method |reg-exp.match| (re string &optional start end)
  (cl:error "todo"))

(def-py-method |reg-exp.search| (re string &optional start end)
  (cl:error "todo"))

(def-py-method |reg-exp.split| (re string &optional (maxsplit 0))
  maxsplit
  (cl:error "todo"))

(def-py-method |reg-exp.findall| (re string &optional start end)
  (cl:error "todo"))

(def-py-method |reg-exp.finditer| (re string &optional start end)
  (cl:error "todo"))

(def-py-method |reg-exp.sub| (re replacement string  &optional (count 0))
  count
  (cl:error "todo"))

(def-py-method |reg-exp.subn| (re replacement string  &optional (count 0))
  count
  (cl:error "todo"))

(def-py-method |reg-exp.flags| :attribute (re)
  (cl:error "todo"))

(def-py-method |reg-exp.groupindex| :attribute (re)
  (cl:error "todo"))

(def-py-method |reg-exp.pattern| :attribute (re)
  (cl:error "todo"))


(clpython::def-proxy-class |reg-exp-match| ()
  ())

(def-py-method |reg-exp-match.expand| (rm template)
  (cl:error "todo"))

(def-py-method |reg-exp-match.group| (rm &rest groups)
  (cl:error "todo"))

(def-py-method |reg-exp-match.groups| (rm &optional default)
  (cl:error "todo"))

(def-py-method |reg-exp-match.groupdict| (rm &optional default)
  (cl:error "todo"))

(def-py-method |reg-exp-match.start| (rm &optional group)
  (cl:error "todo"))

(def-py-method |reg-exp-match.end| (rm &optional group)
  (cl:error "todo"))

(def-py-method |reg-exp-match.span| (rm &optional group)
  (cl:error "todo"))

(def-py-method |reg-exp-match.pos| :attribute (rm)
  (cl:error "todo"))

(def-py-method |reg-exp-match.endpos| :attribute (rm)
  (cl:error "todo"))

(def-py-method |reg-exp-match.lastindex| :attribute (rm)
  (cl:error "todo"))

(def-py-method |reg-exp-match.lastgroup| :attribute (rm)
  (cl:error "todo"))

(def-py-method |reg-exp-match.re| :attribute (rm)
  (cl:error "todo"))

(def-py-method |reg-exp-match.string| :attribute (rm)
  (cl:error "todo"))
