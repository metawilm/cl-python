;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.TEST -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Python builtins test

(in-package :clpython.test)

(defun run-builtin-test ()
  (with-subtest (:name "CLPython-Builtins")
    (dolist (x '(:globals :hash :isinstance :int :range :type :map :list))
      (test-builtin x))))

(defgeneric test-builtin (kind))

(defmethod test-builtin :around (kind)
  (with-subtest (:name (format nil "CLPython-Builtin-~A" kind))
    (call-next-method)))

(defmethod test-builtin ((x (eql :globals)))
  (declare (ignorable x))
  (run-no-error "
g = globals()
g['x'] = 3
assert x == 3")
  (run-no-error "
g = globals()
for s in ('a = 3', 'print a'):
  exec s in g")
  (run-no-error "
g = globals()
a = 3
assert g['a'] == 3"))

(defmethod test-builtin ((x (eql :hash)))
  (declare (ignorable x))
  ;; Inspired by test case in email from Michael Foord to IronPython mailing list
  ;; 28 Feb 2008, <47C72FBC.5070300@voidspace.org.uk>
  "[IronPython] Hashing in IronPython"
  (run-no-error (format nil "
hashes = {}
for i in range(60):
  for j in range(30):
    hashes[ hash( (i,j) ) ] = None
assert len(hashes) > ~A
" #+ccl 100 #-ccl 1000) ;; CCL does not distribute the hashes nicely
                :fail-info "Ensure tuple hashes nicely distributed."))

(defmethod test-builtin ((x (eql :int)))
  (declare (ignorable x))
  (run-no-error "assert int(3) == 3")
  (run-no-error "assert int('3') == 3")
  (run-no-error "assert int('03') == 3")
  (run-no-error "assert int('09', 10) == 9")
  (run-no-error "assert int('09', 16) == 9")
  (run-no-error "assert (2 == 2 == 2)")
  (run-no-error "assert 2 == 2 == 2")
  (run-no-error "assert int('0FF', 16) == 0xFF == 255")
  (run-no-error "
class C:
  def __int__(self): return 42
assert int(C()) == 42"))
  
(defmethod test-builtin ((x (eql :isinstance)))
  (declare (ignorable x))
  (run-no-error "assert isinstance(3, int)")
  (run-no-error "assert isinstance(3, (float, int))")
  (run-no-error "assert isinstance(3, (int, float))"))

(defmethod test-builtin ((x (eql :list)))
  (declare (ignorable x))
  (run-no-error "
x = []
x.insert(1, 10)
assert x == [10]
x.insert(1, 11)
assert x == [10, 11]
x.insert(12, 12)
assert x == [10, 11, 12]"))

(defmethod test-builtin ((x (eql :map)))
  (declare (ignorable x))
  (run-no-error "assert map(lambda x,y: x+y, [1,2,3], [4,5,6]) == [5, 7, 9]"))

(defmethod test-builtin ((x (eql :range)))
  (declare (ignorable x))
  (run-no-error "assert range(3) == list(xrange(3)) == [0,1,2]")
  (run-no-error "assert range(1,3) == list(xrange(1,3)) == [1,2]")
  (run-no-error "assert range(1,10,2) == list(xrange(1,10,2)) == [1,3,5,7,9]")
  (run-no-error "assert range(10,1,-2) == list(xrange(10,1,-2)) == [10,8,6,4,2]"))

(defmethod test-builtin ((x (eql :type)))
  (declare (ignorable x))
  (run-no-error "
def f(): pass
g = lambda: 42
assert type(f) == type(g)"))
