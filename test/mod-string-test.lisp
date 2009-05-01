;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.TEST -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.test)

(defun run-mod-string-test ()
  (run-no-error "
import string
t = string.maketrans('ab', 'xy')
string.translate('abcdxyz', t, 'd') == 'xycxyz'")
  
  (run-no-error "
assert '%% %s' % (1,) == '% 1'"))
