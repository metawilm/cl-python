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

(defun |count| (&optional (start 0) (step 1))
  (clpython::make-iterator-from-function
   :name :repeat-iterator
   :func (lambda () 
           (prog1 start (incf start step)))))

(defun |ifilter| (predicate iterable)
  (clpython::make-iterator-from-function
   :name :ifilter-iterator
   :func (let ((lisp-pred (if (eq predicate *the-none*)
                              #'clpython::py-val->lisp-bool
                            (lambda (x) (clpython::py-val->lisp-bool (py-call predicate x)))))
               (iter-func (clpython::get-py-iterate-fun iterable)))
           (lambda ()
             (loop for x = (funcall iter-func)
                 when (or (null x) ;; StopIteration
                          (funcall lisp-pred x))
                 return x)))))
    
(defun |imap| (function &rest iterables)
  (clpython::make-iterator-from-function
   :name :imap-iterator
   :func (cond ((null iterables)
                (lambda () nil))
               (t
                (let ((iter-funcs (mapcar 'clpython::get-py-iterate-fun iterables)))
                  (lambda ()
                    (loop for f in iter-funcs
                        for f-val = (funcall f)
                        if f-val collect it into values
                        else return nil
                        finally (return (if (eq function (load-time-value *the-none*))
                                            (make-tuple-from-list values)
                                          (apply function values))))))))))

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

(defun |izip| (&rest iterables)
  (apply '|imap| (load-time-value *the-none*) iterables))

(defun |repeat| (object &optional times)
  (cond ((or (null times)
             (eq times (load-time-value *the-none*)))
         (clpython::make-iterator-from-function
          :name :repeat-iterator
          :func (lambda () object)))
        (t
         (check-type times integer)
         (clpython::make-iterator-from-function
          :name :repeat-iterator
          :func (lambda () 
                  (unless (minusp (decf times))
                    object))))))

(defun |tee| (iterable &optional (n 2))
  "Create N independent generators"
  (check-type n integer)
  (cond ((zerop n)
         (make-tuple-from-list ()))
        (t
         (let ((iter-func (clpython::get-py-iterate-fun iterable))
               (deques (loop repeat n collect (clpython.module._collections:|deque|))))
           (flet ((generator (deque)
                    (clpython::make-iterator-from-function
                     :name :tee-iterator
                     :func (lambda () 
                             (if (clpython::py-val->lisp-bool deque)
                                 (clpython.module._collections::deque.popleft deque)
                               (let ((new-val (funcall iter-func)))
                                 ;; also continue if NEW-VAL is NIL
                                 (dolist (d deques)
                                   (unless (eq d deque)
                                     (clpython.module._collections::deque.append d new-val)))
                                 new-val))))))
             (make-tuple-from-list (mapcar 'generator deques)))))))