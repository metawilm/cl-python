;; -*- package: clpython.test; readtable: py-ast-user-readtable -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Python language semantics test

(in-package :clpython.test)
(in-syntax *user-readtable*)

(defmacro test-code (string)
  `(test-no-error (run-python-string ,string)))

(defun run-lang-test ()
  (with-subtest (:name "CLPython-Lang")
    (dolist (node '(:assert-stmt :assign-stmt :attributeref-expr :augassign-stmt
		    :backticks-expr :binary-expr :binary-lazy-expr :break-stmt
		    :call-expr :classdef-stmt :comparison-expr :continue-stmt
		    :del-stmt :dict-expr :exec-stmt :for-in-stmt :funcdef-stmt
		    :generator-expr :global-stmt :identifier-expr :if-stmt
		    :import-stmt :import-from-stmt :lambda-expr))
      (test-lang node))))

(defmacro run-error (string condtype &rest options)
  `(test-error (run-python-string ,string) :condition-type ',condtype ,@options))

(defmacro run-no-error (string &rest options)
  `(test-no-error (run-python-string ,string) ,@options))

(defmacro run-test (val string &rest options)
  `(test ,val (run-python-string ,string) ,@options))

(defgeneric test-lang (kind))

(defmethod test-lang :around (kind)
  (with-subtest (:name (format nil "CLPython-Lang-~S" kind))
    (let ((*warn-unused-function-vars* nil))
      (call-next-method))))

(defmethod test-lang ((kind (eql :assert-stmt)))
  (run-error        "assert 0" {AssertionError})
  (run-no-error     "assert 1")
  (run-error "assert \"\"" {AssertionError})
  (run-no-error     "assert \"s\"")
  (run-error "assert []" {AssertionError})
  (run-no-error     "assert [1,2]")
  (run-no-error     "assert True")
  (run-error "assert not True" {AssertionError})
  (run-no-error     "assert not not True")
  (run-no-error     "assert not False")
  
  (multiple-value-bind (x err) 
      (ignore-errors (run-python-string "assert 0, 'abc'"))
    (test-false x)
    (test-true err)
    (test-true (string= (pop (exception-args err)) "abc"))))

(defmethod test-lang ((kind (eql :assign-stmt)))
  (run-test 3 "a = 3; a")
  (run-test 3 "a, = 3,; a")
  (run-test 3 "[a] = [3]; a")
  (run-test 3 "(a,) = (3,); a")
  (run-test 3 "a,b = 3,4; a")
  (run-test 3 "a,b = [3,4]; a")
  (run-error "a,b = 3" {TypeError} :fail-info "Iteration over non-sequence.")
  (run-error "a,b = 3,4,5" {ValueError})
  (run-error "a,b = [3,4,5]" {ValueError}))

(defmethod test-lang ((kind (eql :attributeref-expr)))
  )

(defmethod test-lang ((kind (eql :augassign-stmt)))
  )

(defmethod test-lang ((kind (eql :backticks-expr)))
  )

(defmethod test-lang ((kind (eql :binary-expr)))
  )

(defmethod test-lang ((kind (eql :binary-lazy-expr)))
  )

(defmethod test-lang ((kind (eql :break-stmt)))
  )

(defmethod test-lang ((kind (eql :call-expr)))
  )

(defmethod test-lang ((kind (eql :classdef-stmt)))
  )

(defmethod test-lang ((kind (eql :comparison-expr)))
  )

(defmethod test-lang ((kind (eql :continue-stmt)))
  )

(defmethod test-lang ((kind (eql :del-stmt)))
  )

(defmethod test-lang ((kind (eql :dict-expr)))
  )

(defmethod test-lang ((kind (eql :exec-stmt)))
  )

(defmethod test-lang ((kind (eql :for-in-stmt)))
  )

(defmethod test-lang ((kind (eql :funcdef-stmt)))
  ;; *-arg, **-arg
  (run-no-error "
def f(a, b, c=13, d=14, *e, **f): return [a,b,c,d,e,f]
x = f(1,2,3,4,5,6)
assert x == [1,2,3,4,(5,6),{}], 'x = %s' % x"
)
  (run-no-error "
def f(a, b, c=13, d=14, *e, **f): return [a,b,c,d,e,f]
x = f(a=1,b=2,c=3,d=4,e=5,f=6)
assert x == [1,2,3,4,(),{'e': 5, 'f': 6}], 'x = %s' % x"
))

(defmethod test-lang ((kind (eql :generator-expr)))
  )

(defmethod test-lang ((kind (eql :global-stmt)))
  )

(defmethod test-lang ((kind (eql :identifier-expr)))
  )

(defmethod test-lang ((kind (eql :if-stmt)))
  )

(defmethod test-lang ((kind (eql :import-stmt)))
  (run-no-error "import sys
assert sys" :fail-info "Should work in both ANSI and Modern mode.")
  )

(defmethod test-lang ((kind (eql :import-from-stmt)))
  )

(defmethod test-lang ((kind (eql :lambda-expr)))
  )

;; ...
