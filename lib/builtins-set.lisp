;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Set and Frozenset

(in-package :clpython)
(in-syntax *user-readtable*)

(defclass |py-set| (object)
  ((items :accessor set-items))
  (:metaclass py-type))

(def-py-method |py-set.__init__| (x &optional iterable)
  (setf (set-items x) (when iterable (py-iterate->lisp-list iterable))))

(def-py-method |py-set.__iter__| (x)
  (make-iterator-from-function :name "set-iterator"
                               :func (let ((items (copy-list (set-items x))))
                                       (lambda () (pop items)))))

(def-py-method |py-set.add| (x item)
  (push item (set-items x)))


(defclass |frozenset| (object)
  ((items :accessor set-items))
  (:metaclass py-type))

(def-py-method |frozenset.__init__| (x iterable)
  (setf (set-items x) (py-iterate->lisp-list iterable)))

;; much TODO...