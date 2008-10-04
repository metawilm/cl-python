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

(clpython::def-proxy-class reg-exp ()
  ())

(def-py-method reg-exp.match (re string &optional start end)
  (error "todo"))

(def-py-method reg-exp.search (re string &optional start end)
  (error "todo"))

(def-py-method reg-exp.split (re string &optional (maxsplit 0))
  maxsplit
  (error "todo"))

(def-py-method reg-exp.findall (re string &optional start end)
  (error "todo"))

(def-py-method reg-exp.finditer (re string &optional start end)
  (error "todo"))

(def-py-method reg-exp.sub (re replacement string  &optional (count 0))
  count
  (error "todo"))

(def-py-method reg-exp.subn (re replacement string  &optional (count 0))
  count
  (error "todo"))

(def-py-method reg-exp.flags :attribute (re)
  (error "todo"))

(def-py-method reg-exp.groupindex :attribute (re)
  (error "todo"))

(def-py-method reg-exp.pattern :attribute (re)
  (error "todo"))


(clpython::def-proxy-class reg-exp-match ()
  ())

(def-py-method reg-exp-match.expand (rm template)
  (error "todo"))

(def-py-method reg-exp-match.group (rm &rest groups)
  (error "todo"))

(def-py-method reg-exp-match.groups (rm &optional default)
  (error "todo"))

(def-py-method reg-exp-match.groupdict (rm &optional default)
  (error "todo"))

(def-py-method reg-exp-match.start (rm &optional group)
  (error "todo"))

(def-py-method reg-exp-match.end (rm &optional group)
  (error "todo"))

(def-py-method reg-exp-match.span (rm &optional group)
  (error "todo"))

(def-py-method reg-exp-match.pos :attribute (rm)
  (error "todo"))

(def-py-method reg-exp-match.endpos :attribute (rm)
  (error "todo"))

(def-py-method reg-exp-match.lastindex :attribute (rm)
  (error "todo"))

(def-py-method reg-exp-match.lastgroup :attribute (rm)
  (error "todo"))

(def-py-method reg-exp-match.re :attribute (rm)
  (error "todo"))

(def-py-method reg-exp-match.string :attribute (rm)
  (error "todo"))
