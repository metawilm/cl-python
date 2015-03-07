;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE._WEAKREF -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.module._weakref)

(defun |getweakrefcount| (x)
  (declare (ignore x))
  (error "TODO"))

(defun |getweakrefs| (x)
  (declare (ignore x))
  (error "TODO"))

(defclass |ref| (clpython:object)
  ()
  (:metaclass clpython:py-type))

(defun |proxy| (x &optional callback)
  (declare (ignore x callback))
  (error "TODO"))

(defclass |ProxyType| (clpython:object)
  ()
  (:metaclass clpython:py-type))

(defclass |CallableProxyType| (clpython:object)
  ()
  (:metaclass clpython:py-type))

(defclass |ReferenceType| (clpython:object)
  ()
  (:metaclass clpython:py-type))

(defparameter |ReferenceError|
    (find-class 'clpython.user.builtin.type.exception:|ReferenceError|))
