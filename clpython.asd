;; -*- package: user -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; ASDF System Definitions

(in-package #:user)

(eval-when (:compile-toplevel)
  (error "This ASDF file should be run interpreted."))


;;; Check Allegro version
#+(and allegro-version>= (not (version>= 8 1)))
(warn "CLPython is being developed on Allegro Common Lisp 8.1, ~
       but it might work in other environments too.")


;;; Core systems: parser, compiler, runtime

(asdf:defsystem :clpython.package
    :description "CLPython package and readtables"
    :components ((:module "package"
                          :components ((:file "package")
                                       (:file "utils" :depends-on ("package"))
                                       (:file "readtable" :depends-on ("package"))
                                       (:file "aupprint" :depends-on ("package"))))))

(asdf:defsystem :clpython.parser
    :description "Python parser, code walker, and pretty printer"
    :depends-on (:clpython.package)
    :components ((:module "parser"
			  :components ((:file "psetup"  )
				       (:file "grammar"  :depends-on ("psetup"))
                                       (:file "grammar-aclyacc" :depends-on ("grammar"))
                                       (:file "lexer"    :depends-on ("grammar"))
				       (:file "parser"   :depends-on ("grammar" "lexer"))
                                       (:file "ast-match")
                                       (:file "ast-util" :depends-on ("ast-match" "grammar"))
                                       (:file "walk"     :depends-on ("psetup"))
				       (:file "pprint"   :depends-on ("psetup"))

                                       
                                       (:file "grammar-clyacc" :depends-on ("grammar")))))) ;; only loaded if CL-Yacc is available; see below.

(asdf:defsystem :clpython.core
    :description "Python semantics and compiler"
    :depends-on (:clpython.package :clpython.parser)
    :components ((:module "core"
                          :serial t
                          :components ((:file "csetup"       )
                                       (:file "pydecl"       )
                                       (:file "formatstring" )
                                       (:file "early-dict"   )
                                       (:file "classes"      )
                                       (:file "file"         )
                                       (:file "exceptions"   )
                                       (:file "compiler"     )
                                       (:file "generator"    )
                                       (:file "optimize"     )
                                       (:file "habitat"      )
                                       (:file "import"       )))))

(asdf:defsystem :clpython.lib
    :description "Python module library"
    :depends-on (:clpython.package :clpython.parser :clpython.core)
    :components ((:module "lib"
			  :components ((:file "builtins")
				       (:file "sys")
				       (:file "time")
				       (:file "os")
				       (:file "array")
				       (:file "math")
                                       (:file "string")
                                       (:file "re")
                                       (:file "gc")))))

;;; Application systems

(asdf:defsystem :clpython.app.repl
    :description "CLPython read-eval-print loop"
    :depends-on (:clpython.core)
    :components ((:module "app"
			  :components ((:module "repl"
						:components ((:file "repl")))))))

(asdf:defsystem :clpython.app.profiler
    :description "CLPython call count profiler"
    :depends-on (:clpython.core)
    :components ((:module "app"
			  :components ((:module "profiler"
						:components ((:file "profiler")))))))

(asdf:defsystem :clpython.app
    :description "CLPython applications"
    :depends-on (:clpython.app.repl :clpython.app.profiler))


;;; The main system

(asdf:defsystem :clpython
    :description "CLPython - an implementation of Python in Common Lisp"
    :depends-on (:clpython.package :clpython.parser :clpython.core :clpython.lib clpython.app)
    :in-order-to ((asdf:test-op (asdf:load-op :clpython-test))))

(defmethod asdf:perform :after ((op asdf:test-op) (c (eql (asdf:find-system :clpython))))
  (funcall (find-symbol (string '#:run-tests) :clpython.test)))


(defvar *shown-clpython-usage* nil)

(defmethod asdf:perform :after ((op asdf:load-op) (c (eql (asdf:find-system :clpython))))
  (unless *shown-clpython-usage*
    (terpri)
    (format t "CLPython quick start:~%")
    (format t "  Run a string of Python code: (~S \"for i in range(4): print i\")~%" (find-symbol (string '#:run) :clpython))
    (format t "  Run a Python file:           (~S #p\"~~/example/foo.py\")~%" (find-symbol (string '#:run) :clpython))
    (format t "  Start the Python repl:       (~S)~%~%" (find-symbol (string '#:repl) :clpython.app.repl))
    (setf *shown-clpython-usage* t)))


;; Check for presence of CL-Yacc

(let ((cl-yacc-grammar (let* ((sys (asdf:find-system :clpython.parser))
                              (mod (car (asdf:module-components sys))))
                         (asdf:find-component mod "grammar-clyacc"))))
  (defmethod asdf:perform :around ((op asdf:load-op) (c (eql cl-yacc-grammar)))
    (when (asdf:find-system :yacc nil)
      (call-next-method)
      (format t "Note: The asdf system CL-Yacc was found. To use CL-Yacc as parser for CLPython, bind ~S to ~S.~%"
              (find-symbol (string '#:*default-yacc-version*) (find-package '#:clpython.parser)) :cl-yacc)))
  
  (defmethod asdf:perform :around ((op asdf:compile-op) (c (eql cl-yacc-grammar)))
    (when (asdf:find-system :yacc nil)
      (call-next-method))))


;; Testing is never finished.
(defmethod asdf:operation-done-p ((o asdf:test-op)
				  (c (eql (asdf:find-system :clpython))))
  (values nil))