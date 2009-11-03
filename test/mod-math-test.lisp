;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.TEST -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.test)

(defun run-mod-math-test ()
  (with-subtest (:name "module Math")
    (run-no-error "import math; assert 3.14 < math.pi < 3.15")
    (run-no-error "import math; assert 2.71 < math.e < 2.72")))
