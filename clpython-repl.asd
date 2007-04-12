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

(asdf:defsystem :clpython-repl
    :description "CLPython read-eval-print loop"
    :depends-on (:clpython)
    :components ((:module "app"
			  :components ((:module "repl"
						:components ((:file "repl")))))))