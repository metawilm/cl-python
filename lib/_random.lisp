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

(defclass |Random| (clpython::py-core-object)
  ()
  (:metaclass clpython::py-core-type))

