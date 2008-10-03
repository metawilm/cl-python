;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE.GC -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;; Binary - Ascii translations

(in-package :clpython.module.binascii)

(defun |hexlify| (data)
  (check-type data string)
  (flet ((to-hex (x)
           (check-type x (integer 0 15))
           (schar "0123456789abcdef" x))) 
  (loop with res = (make-string (* 2 (length data)))
      for ch across data
      for ch.code = (char-code ch)
      for i from 0 by 2
      do (setf (schar res i)      (to-hex (ash ch.code -4))
               (schar res (1+ i)) (to-hex (logand ch.code 15)))
      finally (return res))))
  