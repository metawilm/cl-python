;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE.THREAD -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.module.thread)

(defparameter *lock-counter* 0)

(defclass |lock| (clpython::object)
  ((internal-lock :initarg :internal-lock :accessor internal-lock))
  (:metaclass clpython::py-type))

(defun |allocate_lock| ()
  (let ((lock #+allegro (mp:make-process-lock :name (format nil "lock-~A" (incf *lock-counter*)))
              #-allegro (break "todo")))
    (make-instance 'lock :internal-lock lock)))

(def-py-method |lock.acquire| (x &optional (waitflag 1))
  (check-type waitflag (member 0 1))
  (let ((timeout-sec (ecase waitflag
                       (0 0)
                       (1 most-positive-fixnum))))
    #+allegro (py-bool (mp:process-lock (internal-lock x) system:*current-process* "waiting for lock" timeout-sec))
    #-allegro (break "todo")))

(def-py-method |lock.release| (x)
  #+allegro (mp:process-unlock (internal-lock x))
  #-allegro (break "todo"))

(def-py-method |lock.locked| (x)
  #+allegro (py-bool (eq (mp:process-lock-locker (internal-lock x)) system:*current-process*))
  #-allegro (break "todo"))

(def-py-method |lock.__enter__| (x)
  (|lock.acquire| x 1)
  *the-none*)

(def-py-method |lock.__exit__| (x exc-type exc-value traceback)
  (|lock.release| x)
  clpython::+the-false+)

;(defun |start_new_thread| 