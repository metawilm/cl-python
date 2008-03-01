;; -*- package: clpython.test -*-
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
    (dolist (x '(:globals :hash))
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
for i in range(100):
  for j in range(50):
    hashes[ hash( (i,j) ) ] = None
assert len(hashes) > 1000"
                :fail-info "Ensure tuple hashes nicely distributed."))