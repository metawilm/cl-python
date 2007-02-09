(in-package #:user)

(asdf:defsystem :clpython-test
    :description "CLPython tests"
    :depends-on (:clpython)
    :components ((:module "test"
			  :serial t
			  :components ((:file "tester")
				       (:file "ast-test")))))