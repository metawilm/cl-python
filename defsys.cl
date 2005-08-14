(eval-when (compile)
  (error "This defsys file should be loaded interpreted, not compiled."))

(in-package :user)

;; (setf compiler::*compile-with-compilation-unit-override* nil)

(excl:defsystem :python
    (:default-pathname #.*load-pathname*)
  (:serial "package" (:parallel
		      (:serial "parser" "lexer") ;; lexer uses with-terminal-code from parser
		      "pyprint"
		      "walk"
		      "repl"
		      (:serial "builtin-classes" "exceptions" "builtins" "compiler")))
  
  (:definitions "walk" "compiler") ;; COMPILER uses code walk macro
  (:definitions "parser" "lexer") ;; lexer uses parser terminal-code macro
  )


(format t "~%;;To compile and load, execute these forms:~%~s~%~s~%"
	'(excl:compile-system :python)
	'(excl:load-system    :python))

(format t ";;To create a single fasl file, execute:~%~s~%~%"
	'(excl:concatenate-system :python "python.fasl"))
