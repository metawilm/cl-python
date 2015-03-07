;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE.EXCEPTIONS; Readtable: PY-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.module.exceptions)
(clpython:in-syntax clpython::*user-readtable*)

(eval-when (:load-toplevel :execute)
  (do-external-symbols (s 'clpython.user.builtin.type.exception)
    (if (find-class s)
        (let ((sym (intern (symbol-name s) #.*package*)))
          (setf (symbol-value sym) (find-class s))
          (export sym))
      (warn "Skipped non-class symbol: ~S"s))))
