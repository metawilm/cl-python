;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE._FUNCTOOLS; Readtable: PY-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.module._functools)
(in-syntax *user-readtable*)

(defun |partial| (func &rest pos-kw-args-1)
  (let* ((pos-args-1 (loop while pos-kw-args-1
                         until (keywordp (car pos-kw-args-1))
                         collect (pop pos-kw-args-1)))
         (kw-args-1 pos-kw-args-1))
    (clpython.util:named-function :partial
      (lambda (&rest pos-kw-args-2)
        (let* ((pos-args-2 (loop while pos-kw-args-2
                               until (keywordp (car pos-kw-args-2))
                               collect (pop pos-kw-args-2)))
               (kw-args-2 pos-kw-args-2))
          (let ((total-pos-args (append pos-args-1 pos-args-2 kw-args-1 kw-args-2)))
            (apply #'py-call func total-pos-args)))))))

(defun |reduce| (&rest args)
  (apply #'{reduce} args))
