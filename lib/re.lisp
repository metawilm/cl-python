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

(defconstant-once |M| 8)
(defconstant-once |MULTILINE| |M|)

(defun |compile| (pattern &optional flags)
  (make-instance '|reg-exp|
    :pattern pattern
    :flags flags))

(defun |match| (&rest args)
  (declare (ignore args))
  (cl:error "TODO: re.match"))

(defun |search| (&rest args)
  (declare (ignore args))
  (cl:error "TODO: re.search"))

(defun |split| (&rest args)
  (declare (ignore args))
  (cl:error "TODO: re.split"))

(defun |findall| (&rest args)
  (declare (ignore args))
  (cl:error "TODO: re.findall"))

(defun |finditer| (&rest args)
  (declare (ignore args))
  (cl:error "TODO: re.finditer"))

(defun |sub| (&rest args)
  (declare (ignore args))
  (cl:error "TODO: re.sub"))

(defun |subn| (&rest args)
  (declare (ignore args))
  (cl:error "TODO: re.subn"))

(defun |escape| (&rest args)
  (declare (ignore args))
  (cl:error "TODO: re.escape"))

(defun |error| (&rest args)
  (declare (ignore args))
  (cl:error "TODO: re.error"))


(def-py-method |reg-exp.compile| (re pattern &optional flags)
  ;; dummy impl
  (make-instance '|reg-exp| :pattern pattern :flags flags))

(def-py-method |reg-exp.match| (re string &optional start end)
  (cl:error "TODO: reg-exp.match"))

(def-py-method |reg-exp.search| (re string &optional start end)
  (cl:error "TODO: reg-exp.search"))

(def-py-method |reg-exp.split| (re string &optional (maxsplit 0))
  maxsplit
  (cl:error "TODO: reg-exp.split"))

(def-py-method |reg-exp.findall| (re string &optional start end)
  (cl:error "TODO: reg-exp.findall"))

(def-py-method |reg-exp.finditer| (re string &optional start end)
  (cl:error "TODO: reg-exp.finditer"))

(def-py-method |reg-exp.sub| (re replacement string  &optional (count 0))
  count
  (cl:error "TODO: reg-exp.sub"))

(def-py-method |reg-exp.subn| (re replacement string  &optional (count 0))
  count
  (cl:error "TODO: reg-exp.subn"))

(def-py-method |reg-exp.flags| :attribute (re)
  (slot-value re 'flags))

(def-py-method |reg-exp.groupindex| :attribute (re)
  (cl:error "TODO: reg-exp.groupindex"))

(def-py-method |reg-exp.pattern| :attribute (re)
  (slot-value re 'pattern))

(clpython::def-proxy-class |reg-exp-match| ()
  ())

(def-py-method |reg-exp-match.expand| (rm template)
  (cl:error "TODO: reg-exp-match.expand"))

(def-py-method |reg-exp-match.group| (rm &rest groups)
  (cl:error "TODO: reg-exp-match.group"))

(def-py-method |reg-exp-match.groups| (rm &optional default)
  (cl:error "TODO: reg-exp-match.groups"))

(def-py-method |reg-exp-match.groupdict| (rm &optional default)
  (cl:error "TODO: reg-exp-match.groupdict"))

(def-py-method |reg-exp-match.start| (rm &optional group)
  (cl:error "TODO: reg-exp-match.start"))

(def-py-method |reg-exp-match.end| (rm &optional group)
  (cl:error "TODO: reg-exp-match.end"))

(def-py-method |reg-exp-match.span| (rm &optional group)
  (cl:error "TODO: reg-exp-match.span"))

(def-py-method |reg-exp-match.pos| :attribute (rm)
  (cl:error "TODO: reg-exp-match.pos"))

(def-py-method |reg-exp-match.endpos| :attribute (rm)
  (cl:error "TODO: reg-exp-match.endpos"))

(def-py-method |reg-exp-match.lastindex| :attribute (rm)
  (cl:error "TODO: reg-exp-match.lastindex"))

(def-py-method |reg-exp-match.lastgroup| :attribute (rm)
  (cl:error "TODO: reg-exp-match.lastgroup"))

(def-py-method |reg-exp-match.re| :attribute (rm)
  (cl:error "TODO: reg-exp-match.re"))

(def-py-method |reg-exp-match.string| :attribute (rm)
  (cl:error "TODO: reg-exp-match.string"))
