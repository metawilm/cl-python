;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package #:cl-user)

(eval-when (:compile-toplevel)
  (error "This ASDF file should be run interpreted."))

(asdf:defsystem :clpython-test
    :description "CLPython tests"
    :depends-on (:clpython :ptester)
    ;; PTester: see <http://www.cliki.net/ptester> for download location.
    :components ((:module "test"
                          :serial t
                          :components ((:file "tsetup")
                                       (:file "parser-test")
                                       (:file "compiler-test")
                                       (:file "lang-test")
                                       (:file "mod-builtins-test")
                                       (:file "mod-string-test")
                                       (:file "mod-math-test")))))
