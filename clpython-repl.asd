;;; Applications built on top of CLPython

(asdf:defsystem :clpython-repl
    :description "CLPython read-eval-print loop"
    :depends-on (:clpython)
    :components ((:module "app"
			  :components ((:module "repl"
						:components ((:file "repl")))))))