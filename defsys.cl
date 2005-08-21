(eval-when (compile)
  (error "This defsys file should be loaded interpreted, not compiled."))

(in-package :user)

;; (setf compiler::*compile-with-compilation-unit-override* nil)

(excl:defsystem :python
    (:default-pathname #.*load-pathname*)

  ("package")
  
  ("pyprint" (:uses-definitions-from "package"))
  ("walk"    (:uses-definitions-from "package"))
  ("repl"    (:uses-definitions-from "package"))
  
  ("builtin-classes" (:uses-definitions-from "package"))
  ("exceptions"      (:uses-definitions-from "builtin-classes"))
  ("builtins"        (:uses-definitions-from "exceptions" "builtin-classes"))
  ("optimize"        (:uses-definitions-from "builtin-classes" "builtins"))
  
  ("parser"  (:uses-definitions-from "package"))
  ("lexer"   (:uses-definitions-from "parser")) ;; parser macro: with-terminal-code
  
  ("compiler" (:uses-definitions-from "builtins" "walk")) ;; fill asts
  
  #+(or)
  (:serial
   "package" (:parallel
	      (:serial "parser" "lexer") 
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

