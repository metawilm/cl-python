;; -*- package: clpython -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython)

;;; Raising exceptions

(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *exceptions-are-python-objects*
    #+allegro nil
    #+lispworks nil
    #+cmu nil    ;; CMUCL does not allow arbitrary meta/superclasses in conditions
    #-(or allegro lispworks cmu) nil)

(if *exceptions-are-python-objects*
    (pushnew :clpython-exceptions-are-python-objects *features*)
  (setf *features* (remove :clpython-exceptions-are-python-objects *features*)))
)

