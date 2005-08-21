(eval-when (compile)
  (error "This defsys file should be loaded interpreted, not compiled."))

(in-package :user)

;; (setf compiler::*compile-with-compilation-unit-override* nil)

(excl:defsystem :python
    (:default-pathname #.*load-pathname*)
  
  (:serial "package" 
	   
	   (:definitions "walk" "compiler") ;; COMPILER uses code walk macro
	   (:definitions "parser" "lexer") ;; LEXER uses parser terminal-code macro
	   (:definitions "builtin-classes" "optimize") ;; OPTIMIZE defines methods on b-i classes
	   (:definitions "exceptions" "builtins")
	   (:definitions "builtin-classes" "builtins")
	   
	   (:parallel
	    (:serial "parser" "lexer") ;; lexer uses with-terminal-code from parser
	    "pyprint"
	    "walk"
	    "repl"
	    (:serial "builtin-classes" "exceptions" "builtins" "compiler" "optimize"))))


(defun compy ()
  (excl:compile-system :python))

(defun loadpy ()
  (excl:load-system :python)
  (setf *package* (find-package :python)))

(format t "~%;;To compile and load, execute these forms:~%~s~%~s~%"
	'(excl:compile-system :python)
	'(excl:load-system    :python))

(format t "~%;;Or use the shortcuts:~%~s~%~s~%"
	'(compy)
	'(loadpy))

(format t ";;To create a single fasl file, execute:~%~s~%~%"
	'(excl:concatenate-system :python "python.fasl"))

