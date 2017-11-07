;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE.DATETIME -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.module.datetime)

(defclass |date| (object)
  ((year  :accessor date-year)
   (month :accessor date-month)
   (day   :accessor date-day))
  (:metaclass py-type))

(def-py-method |date.year| :attribute-read (x)
  (date-year x))

(def-py-method |date.year| :attribute-write (x val)
  (setf (date-year x) val))

(def-py-method |date.month| :attribute-read (x)
  (date-month x))

(def-py-method |date.month| :attribute-write (x val)
  (setf (date-month x) val))

(def-py-method |date.day| :attribute-read (x)
  (date-day x))

(def-py-method |date.day| :attribute-write (x val)
  (setf (date-day x) val))

(def-py-method |date.strftime| (x format)
  (error "TODO"))

(def-py-method |date.toordinal| (x)
  (error "TODO"))
