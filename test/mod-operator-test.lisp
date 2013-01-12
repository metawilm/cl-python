;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.TEST -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.test)

(defun run-mod-operator-test ()
  (with-subtest (:name "module Operator")
    (run-no-error "import operator; assert operator.floordiv(20, 3) == 6")
    (run-no-error "import operator; assert operator.__floordiv__(20, 3) == 6")
    
    (run-no-error "import operator; assert operator.ifloordiv(20, 3) == 6")
    (run-no-error "import operator; assert operator.__ifloordiv__(20, 3) == 6")
    
    (run-no-error "import operator; assert operator.lt(1, 2) == True")
    (run-no-error "import operator; assert operator.__lt__(1, 2) == True")
    
    (run-no-error "
import operator
class C:
  def __ifloordiv__(self, div): return 123

x = C()
x //= 1
assert x == 123")))