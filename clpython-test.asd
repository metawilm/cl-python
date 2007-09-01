;; -*- package: user -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package #:user)

(eval-when (:compile-toplevel)
  (error "This ASDF file should be run interpreted."))

(asdf:defsystem :clpython-test
    :description "CLPython tests"
    :depends-on (:clpython)
    :components ((:module "test"
                          :serial t
                          :components ((:file "tester")
                                       (:file "tsetup")
                                       (:file "parser-test")
                                       (:file "compiler-test")
                                       (:file "lang-test")
                                       (:file "mod-builtins-test")))))
