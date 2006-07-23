;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package #:user)

(eval-when (compile)
  (error "This defsys file should be loaded interpreted, not compiled."))

(excl:defsystem #:python
    (:default-pathname #.*load-pathname*)

  ("package")
  
  ("pyprint" (:uses-definitions-from "package"))
  ("walk"    (:uses-definitions-from "package"))
  ("formatstring"    (:uses-definitions-from "package"))
  
  ("classes"     (:uses-definitions-from "package" "pyprint" "formatstring"))
  ("exceptions"  (:uses-definitions-from "classes"))
  ("builtins"    (:uses-definitions-from "exceptions" "classes"))
  ("optimize"    (:uses-definitions-from "classes" "builtins"))
  
  ("parser"  (:uses-definitions-from "package"))
  ("lexer"   (:uses-definitions-from "parser")) ;; parser macro: with-terminal-code
  ("run"     (:uses-definitions-from "lexer" "parser"))
  
  ("compiler" (:uses-definitions-from "builtins" "walk" "run")) ;; fill asts, parse-python-string

  ("repl"    (:uses-definitions-from "package" "compiler" "run" "classes")))

(defun compy ()
  (excl:compile-system #:python))

(defun loadpy ()
  (excl:load-system #:python))

(defun make-python-fasl ()
  (excl:concatenate-system #:python "clpython.fasl"))
