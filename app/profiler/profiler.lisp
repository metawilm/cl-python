;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.APP.PROFILER -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Profiler
;;;;
;;;; For now, only call statistics are collected.

(defpackage :clpython.app.profiler
  (:documentation "Python code profiler")
  (:use :common-lisp :clpython)
  (:import-from :clpython.package #:slurp-file)
  (:export #:profile-source-file #:profile-source-string #:profile-test))

(in-package :clpython.app.profiler)

(defun profile-source-file (fname)
  (profile-source-string (slurp-file fname)))

(defun profile-source-string (str)
  (let ((count-ht (make-hash-table :test #'eql)))
    
    ;; Because not every line has a line-no token, we first initialize all
    ;; available line-nos to zero. In the final print-out, we show 0 for
    ;; lines not visited, and blank when no stats available.
    (break "Line number hack is removed; this does not work anymore")
    #+(or)
    (with-line-numbers (:compile-hook (lambda (n) 
                                        (setf (gethash n count-ht) 0))
                                      :runtime-hook (lambda (n)
                                                      (incf (gethash n count-ht 0))))
      (clpython::run-python-ast (parse str)))
    
    ;; print file source with stats
    (terpri)
    (with-input-from-string (f str)
      (loop for i from 1
	  for line = (read-line f nil nil)
	  while line
	  for count = (gethash i count-ht)
	  do (format t "~4D | " i)
	     (if count
		 (format t "~8D | " count)
	       (format t "~8A | " ""))
	     (write-string line)
	     (write-char #\Newline)))))

(defun profile-test ()
  (profile-source-string "
def fact(n):
  if n <= 1:
    return 1
  else:
    return n * fact(n-1)

fact(6)")
  (profile-source-string "
def magic():
  for c in 'abracadabra':
    if c == 'a':
      pass
    elif c in 'bd':
      pass

magic()
"))