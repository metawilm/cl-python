(eval-when (compile)
  (error "This defsys file should be loaded interpreted, not compiled."))

(in-package :user)

;; (setf compiler::*compile-with-compilation-unit-override* nil)

(excl:defsystem :python
    (:default-pathname #.*load-pathname*)
  (:serial "package" (:parallel
		      "parser"
		      "lexer"
		      "pyprint"
		      "walk"
		      "gener"
		      "compiler"
		      "repl"
		      (:serial "builtin-classes" "exceptions"))))

(format t "~%;;To compile and load, execute these forms:~%~s~%~s~%"
	'(excl:compile-system :python)
	'(excl:load-system    :python))

(format t ";;To create a single fasl file, execute:~%~s~%~%"
	'(excl:concatenate-system :python "python.fasl"))
