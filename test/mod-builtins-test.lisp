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
    (dolist (x '(:globals :hash :range :int))
      (test-builtin x))))

(defgeneric test-builtin (kind))

(defmethod test-builtin :around (kind)
  (with-subtest (:name (format nil "CLPython-Builtin-~A" kind))
    (let ((*warn-unused-function-vars* nil))
      (call-next-method))))

(defmethod test-builtin ((x (eql :globals)))
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
  ;; Inspired by test case in email from Michael Foord to IronPython mailing list
  ;; 28 Feb 2008, <47C72FBC.5070300@voidspace.org.uk>
  "[IronPython] Hashing in IronPython"
  (run-no-error "
hashes = {}
for i in range(60):
  for j in range(30):
    hashes[ hash( (i,j) ) ] = None
assert len(hashes) > 1000"
                :fail-info "Ensure tuple hashes nicely distributed."))

(defmethod test-builtin ((x (eql :range)))
  (run-no-error "assert range(3) == [0,1,2]")
  (run-no-error "assert range(1,3) == [1,2]")
  (run-no-error "assert range(1,10,2) == [1,3,5,7,9]")
  (run-no-error "assert range(10,1,-2) == [10,8,6,4,2]"))

(defmethod test-builtin ((x (eql :int)))
  (run-no-error "assert int(3) == 3")
  (run-no-error "assert int('3') == 3")
  (run-no-error "assert int('03') == 3")
  (run-no-error "assert int('09', 10) == 9")
  (run-no-error "assert int('09', 16) == 9")
  (run-no-error "assert (2 == 2 == 2)" :known-failure t :fail-info "x == y == z compiled incorectly")
  (run-no-error "assert 2 == 2 == 2" :known-failure t :fail-info "x == y == z compiled incorectly")
  (run-no-error "assert int('0FF', 16) == 0xFF == 255" :known-failure t :fail-info "x == y == z compiled incorectly"))