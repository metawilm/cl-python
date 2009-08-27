;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; ASDF System Definitions

(in-package #:cl-user)

(eval-when (:compile-toplevel)
  (error "This ASDF file should be run interpreted."))


;;; Core systems: parser, compiler, runtime

;; Here several ASDF systems are defined. The main system :clpython depends on all of them and has no
;; components of its own. This makes it possible to load a specific part of CLPython, in particular the
;; parser (system :clpython.parser).

(asdf:defsystem :clpython.package
    :description "CLPython package and readtables"
    :components ((:module "package"
                          :components ((:file "package")
                                       (:file "utils" :depends-on ("package"))
                                       (:file "readtable" :depends-on ("package"))
                                       (:file "aupprint" :depends-on ("package"))))))

(asdf:defsystem :clpython.parser
    :description "Python parser, code walker, and pretty printer"
    :depends-on
    #-allegro (:clpython.package :yacc)
    #+allegro #.`(:clpython.package ,@(when (asdf:find-system :yacc nil) `(:yacc)))
    :components ((:module "parser"
			  :components ((:file "psetup"  )
				       (:file "grammar"  :depends-on ("psetup"))
                                       (:file "lexer"    :depends-on ("grammar"))
                                       (:file "parser"   :depends-on ("grammar" "lexer"))
                                       (:file "grammar-aclyacc" :depends-on ("grammar" "lexer" "parser"))
                                       (:file "grammar-clyacc"  :depends-on ("grammar" "lexer" "parser"))
                                       (:file "ast-match" :depends-on ("grammar"))
                                       (:file "ast-util" :depends-on ("ast-match" "grammar"))
                                       (:file "walk"     :depends-on ("psetup"))
				       (:file "pprint"   :depends-on ("psetup"))
                                       (:file "lispy"    :depends-on ("psetup" "parser" "ast-match"))))))

(asdf:defsystem :clpython.core
    :description "Python semantics and compiler"
    :depends-on (:clpython.package :clpython.parser :closer-mop)
    :components ((:module "core"
                          :serial t
                          :components ((:file "csetup"       )
                                       (:file "pydecl"       )
                                       (:file "formatstring" )
                                       (:file "metaclass"    )
                                       (:file "dictattr"     )
                                       (:file "classes"      )
                                       (:file "exceptions"   )
                                       (:file "namespace"    )
                                       (:file "compiler"     )
                                       (:file "generator"    )
                                       (:file "optimize"     )
                                       (:file "habitat"      )
                                       (:file "import"       )
                                       (:file "run"          )
                                       (:file "executable"   )
                                       
                                       #+(or) ;;(and allegro-version>= (version>= 8 2))
                                       (:file "source"       )))))

(asdf:defsystem :clpython.lib
    :description "Python module library"
    :depends-on (:clpython.package :clpython.parser :clpython.core)
    :components ((:module "lib"
                          :components ((:file "builtins-file")
                                       (:file "builtins-set")
                                       (:file "builtins" :depends-on ("builtins-file" "builtins-set"))
                                       (:file "lsetup")
                                       (:file "array" :depends-on ("lsetup"))
                                       (:file "binascii" :depends-on ("lsetup"))
                                       (:file "cStringIO" :depends-on ("lsetup"))
                                       (:file "errno"  :depends-on ("lsetup"))
                                       (:file "exceptions" :depends-on ("lsetup"))
                                       (:file "imp" :depends-on ("lsetup"))
                                       (:file "gc"  :depends-on ("lsetup"))
                                       (:file "math"  :depends-on ("lsetup"))
                                       (:file "operator" :depends-on ("lsetup"))
                                       (:file "posix" :depends-on ("lsetup"))
                                       (:file "_random" :depends-on ("lsetup"))
                                       (:file "re" :depends-on ("lsetup"))
                                       (:file "_socket" :depends-on ("lsetup"))
                                       (:file "sys" :depends-on ("lsetup"))
                                       (:file "string" :depends-on ("lsetup"))
				       (:file "symbol" :depends-on ("lsetup"))
                                       (:file "thread" :depends-on ("lsetup"))
                                       (:file "time" :depends-on ("lsetup"))))))

;;; Application systems

(asdf:defsystem :clpython.app.repl
    :description "CLPython read-eval-print loop"
    :depends-on (:clpython.core)
    :components ((:module "app"
			  :components ((:module "repl"
						:components ((:file "repl")))))))

(asdf:defsystem :clpython.app
    :description "CLPython applications"
    :depends-on (:clpython.app.repl))

;;; The main system

(asdf:defsystem :clpython
    :description "CLPython - an implementation of Python in Common Lisp"
    :depends-on (:clpython.package :clpython.parser :clpython.core :clpython.lib clpython.app)
    :in-order-to ((asdf:test-op (asdf:load-op :clpython-test))))


;; In Allegro, which provides its own Yacc, CL-Yacc can optionally be used.
;; In other implementations, Allegro Yacc can't be used.

(let* ((parser-mod (let ((sys (asdf:find-system :clpython.parser)))
                     (car (asdf:module-components sys)))))
  
  #+allegro
  (let ((cl-yacc-grammar (asdf:find-component parser-mod "grammar-clyacc")))
    
    (defmethod asdf:perform :around ((op asdf:load-op) (c (eql cl-yacc-grammar)))
      (when (asdf:find-system :yacc nil)
        (call-next-method)))
    
    (defmethod asdf:perform :around ((op asdf:compile-op) (c (eql cl-yacc-grammar)))
      (when (asdf:find-system :yacc nil)
        (call-next-method))))
  
  #-allegro
  (let ((allegro-yacc-grammar (asdf:find-component parser-mod "grammar-aclyacc")))
    (defmethod asdf:perform :around ((op asdf:load-op) (c (eql allegro-yacc-grammar)))
      nil)
    (defmethod asdf:perform :around ((op asdf:compile-op) (c (eql allegro-yacc-grammar)))
      nil)))

;;; Show usage

(defun show-clpython-quick-start ()
  (format t "~%CLPython quick start guide:~%")
  (format t "  Run a string of Python code:           (~S \"for i in range(4): print i\")~%" (find-symbol (string '#:run) :clpython))
  (format t "  Run a Python file:                     (~S #p\"~~/example/foo.py\")~%" (find-symbol (string '#:run) :clpython))
  (format t "  Start the Python \"interpreter\" (REPL): (~S)~%" (find-symbol (string '#:repl) :clpython.app.repl))
  (format t "  To start mixed Python/Lisp input mode: (~S)~%" (find-symbol (string '#:enter-mixed-lisp-python-syntax) :clpython.parser))
  (format t "  Run the test suite:                    ~S~%~%" '(asdf:operate 'asdf:test-op :clpython)))

(defmethod asdf:perform :after ((op asdf:load-op) (c (eql (asdf:find-system :clpython))))
  (show-clpython-quick-start))

;;; Link asdf operation "test-op" to asdf system "clpython.test"

(defmethod asdf:perform :after ((op asdf:test-op) (c (eql (asdf:find-system :clpython))))
  (funcall (find-symbol (string '#:run-tests) :clpython.test)))

(defmethod asdf:operation-done-p ((o asdf:test-op)
				  (c (eql (asdf:find-system :clpython))))
  "Testing is never finished."
  nil)
