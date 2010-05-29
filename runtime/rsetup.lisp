;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython)

(defvar *habitat* nil)

(defparameter *exceptions-are-python-objects*
    #+allegro t
    #+lispworks t
    #+cmu nil    ;; CMUCL does not allow arbitrary meta/superclasses in conditions
    #-(or allegro lispworks cmu) nil)

(register-feature :clpython-exceptions-are-python-objects *exceptions-are-python-objects*)


