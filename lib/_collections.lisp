;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE._COLLECTIONS; Readtable: PY-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package #:clpython.module._collections)
(in-syntax *user-readtable*)

(defclass deque (clpython:object)
  ((max-length :initarg :max-length)
   (vector     :initform (make-array 4 :adjustable t :fill-pointer 0)))
  (:metaclass clpython:py-type))

(defun |deque| (&optional iterable max-length)
  (when (eq max-length (load-time-value *the-none*))
    (setf max-length nil))
  (when max-length
    (check-type max-length integer))
  (let ((deque (make-instance 'deque :max-length max-length)))
    (when iterable
      (deque.extend deque iterable))
    deque))

(def-py-method deque.append (deque x)
  (with-slots (max-length vector) deque
    (if (and max-length (= (length vector) max-length))
        (progn (replace vector vector :start1 0 :end1 (1- (length vector)) :start2 1) ;; shift 1 left
               (setf (aref vector (1- max-length)) x))
      (vector-push-extend x vector)))
  (load-time-value *the-none*))

(def-py-method deque.appendleft (deque x)
  (with-slots (max-length vector) deque
    (unless (and max-length (= (length vector) max-length))
      (vector-push-extend nil vector))
    (replace vector vector :start1 1 :end1 (length vector) :start2 0) ;; shift 1 right
    (setf (aref vector 0) x))
  (load-time-value *the-none*))

(def-py-method deque.clear (deque)
  (with-slots (vector) deque
    (dotimes (i (length vector))
      (setf (aref vector i) nil)) ;; GC
    (setf (fill-pointer vector) 0))
  (load-time-value *the-none*))

(def-py-method deque.count (deque x)
  (with-slots (vector) deque
    (loop for val across vector count (py-==->lisp-val x val))))

(def-py-method deque.extend (deque iterable)
  (clpython::map-over-object (lambda (x) (deque.append deque x)) iterable)
  (load-time-value *the-none*))

(def-py-method deque.extendleft (deque iterable)
  (clpython::map-over-object (lambda (x) (deque.appendleft deque x)) iterable)
  (load-time-value *the-none*))

(def-py-method deque.pop (deque)
  (with-slots (vector) deque
    (when (zerop (length vector))
      (py-raise '{IndexError} "Can't pop from empty deque."))
    (prog1 (aref vector (1- (length vector)))
      (decf (fill-pointer vector)))))

(def-py-method deque.popleft (deque)
  (with-slots (vector) deque
    (when (zerop (length vector))
      (py-raise '{IndexError} "Can't popleft from empty deque."))
    (prog1 (aref vector 0)
      (replace vector vector :start1 0 :end1 (1- (length vector)) :start2 1)
      (decf (fill-pointer vector)))))

(def-py-method deque.remove (deque val)
  (with-slots (vector) deque
    (dotimes (i (length vector))
      (when (py-==->lisp-val (aref vector i) val)
        (replace vector vector :start1 i :end1 (1- (length vector)) :start2 (1+ i))
        (decf (fill-pointer vector))
        (return-from deque.remove (load-time-value *the-none*)))))
  (py-raise '{ValueError} "Value ~A not found in deque." val))

(def-py-method deque.reverse (deque)
  (with-slots (vector) deque
    (setf vector (nreverse vector)))
  (load-time-value *the-none*))

(def-py-method deque.rotate (deque n)
  (check-type n integer)
  (cond ((plusp n)
         (dotimes (i n)
           (deque.appendleft deque (deque.pop deque))))
        ((minusp n)
         (dotimes (i (- n))
           (deque.append deque (deque.popleft deque)))))
  (load-time-value *the-none*))

(def-py-method deque.maxlen :attribute (deque)
  (or (slot-value deque 'max-length)
      (load-time-value *the-none*)))

(def-py-method deque.__nonzero__ (deque)
  (with-slots (vector) deque
    (py-bool (plusp (length vector)))))

;; TODO: iteration, pickling, len, reversed, copy.copy, copy.deepcopy, in, subscript

(defclass |defaultdict| (clpython:object)
  ()
  (:metaclass clpython:py-type))

