;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE.ITERTOOLS; Readtable: PY-AST-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.module.itertools)
(clpython::in-syntax clpython::*ast-user-readtable*)

(defun |islice| (iterable &optional (start 0) (stop most-positive-fixnum) (step 1))
  (when (or (and start (or (not (integerp start)) (minusp start)))
            (and step  (or (not (integerp step))  (not (plusp step))))
            (and stop  (or (not (integerp stop))  (minusp stop))))
    (clpython::py-raise '{ValueError} "islice() arg error: expect start/stop >= 0, step >= 1. Got: start=~S, stop=~S, step=~S."
                        start stop step))
  (let ((starting t)
        (cur-ix 0)
        (iter-func (clpython::get-py-iterate-fun iterable)))
    (clpython::make-iterator-from-function
     :name :islice-iterator
     :func (lambda ()
             (let ((num-to-skip (cond (starting (progn (setf starting nil)
                                                       start))
                                      (t (1- step)))))
               (loop
                   repeat num-to-skip
                   while (and (funcall iter-func) (< cur-ix stop))
                   do (incf cur-ix))
               (incf cur-ix)
               (when (<= cur-ix stop)
                 (funcall iter-func)))))))
