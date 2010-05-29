;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE._RANDOM -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;; Random

(in-package :clpython.module._random)

#||
import _random
>>> dir(_random)
['Random', '__doc__', '__file__', '__name__']
>>> _random.Random.__doc__
'Random() -> create a random number generator with its own internal state.'
>>> r = _random.Random()
>>> dir(r)
['__class__', '__delattr__', '__doc__', '__getattribute__', '__hash__', '__init__', '__new__', '__reduce__', '__reduce_ex__', '__repr__', '__setattr__', '__str__', 'getrandbits', 'getstate', 'jumpahead', 'random', 'seed', 'setstate']
>>> 
||#

(defclass |Random| (clpython:object)
  ((state))
  (:metaclass clpython:py-type))

(defun make-random-state-with-seed (seed)
  (check-type seed integer)
  ;; http://www.franz.com/support/documentation/8.1/doc/implementation.htm#cl-random-2
  #+allegro (make-random-state t seed)
  #-allegro (break "Don't know how to create random-state with given seed ~
                    in this Lisp implementation."))

(def-py-method |Random.seed| (x &optional n)
  (setf (slot-value x 'state)
    (if n (make-random-state-with-seed n) (make-random-state t)))
  *the-none*)

(def-py-method |Random.random| (x)
  (random 1.0f0 (slot-value x 'state)))

(def-py-method |Random.jumpahead| (x n)
  "Replace state by another state."
  (|Random.seed| x)
  *the-none*)

(def-py-method |Random.getrandbits| (x k)
  "Returns an integer with K random bits, so between 0 and (2**k - 1)."
  (random (expt 2 k) (slot-value x 'state)))