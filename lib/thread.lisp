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

#+allegro
(defparameter *lock-implementation* :process-lock
  "Implementation of locks: either :gate or :process-lock."
  ;; For shootout benchmark chameneos-redux, :process-lock turned out to be faster.
  )
 
(defclass |lock| (object)
  ((id       :initarg :id       :accessor lock-id)
   (wait-msg :initarg :wait-msg :accessor lock-wait-msg))
  (:metaclass py-type))

#+allegro
(defclass allegro-gate-lock (|lock|)
  ((gate :initarg :gate :accessor lock-gate))
  (:metaclass py-type))

#+allegro
(defclass allegro-process-lock (|lock|)
   ((process-lock :initarg :process-lock :accessor lock-process-lock))
   (:metaclass py-type))

(defmethod print-object ((l |lock|) stream)
  (print-unreadable-object (l stream :type t :identity t)
    (format stream "lock ~A" (lock-id l))))

(defun |allocate_lock| ()
  (let* ((id (incf *lock-counter*))
         (wait-msg (format nil "waiting for lock ~A" id)))
    (declare (ignorable wait-msg))
    #+allegro 
    (multiple-value-bind (type args)
        (ecase *lock-implementation*
          (:gate (values 'allegro-gate-lock
                         (list :gate (mp:make-gate t))))
          (:process-lock (values 'allegro-process-lock
                                 (list :process-lock (mp:make-process-lock :name (format nil "lock ~A" id))))))
      (apply #'make-instance type :id id :wait-msg wait-msg args))
    #-allegro (break "todo")))

(def-py-method |lock.acquire| (x &optional (waitflag 1))
  (let ((wait (ecase waitflag
                (0 nil)
                (1 t))))
    (py-bool (lock-acquire x wait))))

(defgeneric lock-acquire (lock wait-p))

#+allegro
(defmethod lock-acquire ((lock allegro-gate-lock) wait-p)
  (let ((g (lock-gate lock)))
    (loop
      (system:without-scheduling
        (when (mp:gate-open-p g)
          (mp:close-gate g)
          (return-from lock-acquire t)))
      (if wait-p
          (mp:process-wait (lock-wait-msg lock) 'mp:gate-open-p g)
        (return-from lock-acquire nil)))))

#+allegro 
(defmethod lock-acquire ((lock allegro-process-lock) wait-p)
  (let* ((pl (lock-process-lock lock))
         (wait-msg (lock-wait-msg lock))
         (timeout-sec (if wait-p most-positive-fixnum 0)))
    ;; MP:PROCESS-LOCK returns success
    (mp:process-lock pl t wait-msg timeout-sec)))

(def-py-method |lock.release| (x)
  "Releases the lock, which had been acquired by this or another thread."
  (lock-release x))

(defgeneric lock-release (lock))

#+allegro
(defmethod lock-release ((lock allegro-gate-lock))
  (mp:open-gate (lock-gate lock)))

#+allegro
(defmethod lock-release ((lock allegro-process-lock))
  (mp:process-unlock (lock-process-lock lock) t))

(def-py-method |lock.locked| (x)
  #+allegro (py-bool (not (mp:gate-open-p (lock-gate x))))
  #-allegro (break "todo"))

(def-py-method |lock.__enter__| (x)
  (|lock.acquire| x 1)
  *the-none*)

(def-py-method |lock.__exit__| (x exc-type exc-value traceback)
  (|lock.release| x)
  clpython::+the-false+)


;;; Threads

(defvar *threads* ()
  "All spawned threads.")

(defclass |thread| (object)
  ((internal-thread :initarg :internal-thread :accessor internal-thread))
  (:metaclass py-type))
  
(defun |start_new_thread| (func args &optional kwargs)
  "Return identifier of new thread. Thread exits silently, or prints stack trace upon exception."
  (flet ((start-thread (func args)
           (declare (ignorable func args))
           #+allegro (mp:process-run-function (clpython::function-name func)
                       (lambda () (apply #'py-call func args)))
           #-allegro (break "todo")))
    (let* ((pa (py-iterate->lisp-list args))
           (ka (when kwargs (loop for (k v) in (clpython::dict.items kwargs)
                                collect (intern k :keyword)
                                collect v)))
           (args (append pa ka))
           (thread (make-instance '|thread| :internal-thread (start-thread func args))))
      (push thread *threads*)
      thread)))

(defun kill_new_threads ()
  (flet ((kill (x)
           (declare (ignorable x))
           #+allegro (mp:process-kill (internal-thread x))
           #-allegro (break "todo")))
    (map nil #'kill *threads*)))
