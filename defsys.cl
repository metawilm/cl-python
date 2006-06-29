(in-package :user)

(eval-when (compile)
  (error "This defsys file should be loaded interpreted, not compiled."))

(excl:defsystem :python
    (:default-pathname #.*load-pathname*)

  ("package")
  
  ("pyprint" (:uses-definitions-from "package"))
  ("walk"    (:uses-definitions-from "package"))
  ("formatstring"    (:uses-definitions-from "package"))
  
  ("classes"     (:uses-definitions-from "package" "formatstring"))
  ("exceptions"  (:uses-definitions-from "classes"))
  ("builtins"    (:uses-definitions-from "exceptions" "classes"))
  ("optimize"    (:uses-definitions-from "classes" "builtins"))
  
  ("parser"  (:uses-definitions-from "package"))
  ("lexer"   (:uses-definitions-from "parser")) ;; parser macro: with-terminal-code
  ("run"     (:uses-definitions-from "lexer" "parser"))
  
  ("compiler" (:uses-definitions-from "builtins" "walk" "run")) ;; fill asts, parse-python-string

  ("repl"    (:uses-definitions-from "package" "compiler" "run" "classes"))
  
  ("classes" (:uses-definitions-from "pyprint" "formatstring")) ;; py-pprint
  )

(defun compy ()
  (excl:compile-system :python))

(defun loadpy ()
  (excl:load-system :python))

;; To create a single fasl file:
;; 
;;   (excl:concatenate-system :python "python.fasl")

