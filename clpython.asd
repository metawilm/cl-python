;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; ASDF System Definitions

(eval-when (:compile-toplevel)
  (error "This ASDF file should be run interpreted."))


;;; CL-Python is split into several ASDF systems, to make it possible to load
;;; specific components -- in particular, to load the compiler or parser without
;;; the runtime environment.
;;;
;;; The main system :CLPYTHON is the sum of all components, including contributions.

;;; Suppress some warnings about package trickery

(defun call-with-suppressed-clpython-package-warnings (thunk)
  (handler-bind (#+sbcl
                 (sb-int:package-at-variance #'muffle-warning)
                 #+lispworks
                 (simple-warning (lambda (c)
                                   (let ((fmt (slot-value c 'conditions::format-string)))
                                     (when (search "Using DEFPACKAGE" fmt)
                                       (muffle-warning c))))))
    (funcall thunk)))

(defsystem "clpython/basic"
    :description "CLPython package and utils"
    :depends-on ("closer-mop")
    :serial t
    :components ((:file "package" :around-compile call-with-suppressed-clpython-package-warnings)
                 (:module "util"
                          :components ((:file "utils")
                                       (:file "readtable")
                                       (:file "macro-state" :depends-on ("utils"))
                                       (:file "patternmatch")))
                 (:module "shared"
                          :serial t
                          :components ((:file "ssetup")
                                       (:file "aureadtable")
                                       (:file "errors")
                                       (:file "aupprint")))))

;;; In Allegro, which provides its own Yacc, CL-Yacc can optionally be used.
;;; In other implementations, Allegro Yacc is unavailable
(when (asdf:find-system "yacc" nil)
  (pushnew :use-cl-yacc *features*))

(defsystem "clpython/parser"
    :description "Python parser, code walker, and pretty printer"
    :depends-on ("clpython/basic"
                 "closer-mop"
                 (:feature (:or :allegro :use-cl-yacc) "yacc"))
    :components ((:module "parser"
                          :components ((:file "grammar" )
                                       (:file "lexer"    :depends-on ("grammar"))
                                       (:file "parser"   :depends-on ("grammar" "lexer"))
                                       (:file "grammar-aclyacc" :depends-on ("grammar" "lexer" "parser") :if-feature :allegro)
                                       (:file "grammar-clyacc"  :depends-on ("grammar" "lexer" "parser") :if-feature :use-cl-yacc)
                                       (:file "ast-util" :depends-on ("grammar"))
                                       (:file "walk"   )
                                       (:file "pprint" )))))

(defsystem "clpython/compiler"
    :description "Python compiler"
    :depends-on ("clpython/basic" "clpython/parser" "clpython/runtime" "closer-mop")
    :serial t
    :components ((:module "compiler"
                          :serial t
                          :components ((:file "csetup"       )
                                       (:file "pydecl"       )
                                       (:file "namespace"    )
                                       (:file "compiler"     )
                                       (:file "generator"    )
                                       (:file "optimize"     )))))

(defsystem "clpython/runtime"
    :description "Python runtime environment"
    :depends-on ("clpython/basic" "closer-mop" #+ecl "cl-custom-hash-table" "cl-fad")
    :components ((:module "runtime"
                          :serial t
                          :components ((:file "rsetup"       )
                                       (:file "formatstring" )
                                       (:file "metaclass"    )
                                       (:file "dictattr"     )
                                       (:file "classes"      )
                                       (:file "exceptions"   )
                                       (:file "habitat"      )
                                       (:file "run"          )
                                       (:file "import"       )))))

(defsystem "clpython/lib"
    :description "Python module library"
    :depends-on ("clpython/basic" "clpython/runtime" "clpython/compiler" #| TODO: remove compiler dep |#)
    :components ((:module "lib"
                          :serial t
                          :components ((:file "lsetup" :around-compile call-with-suppressed-clpython-package-warnings)
                                       (:file "builtins-file" :depends-on ("lsetup"))
                                       (:file "builtins-set" :depends-on ("lsetup"))
                                       (:file "builtins-buffer" :depends-on ("lsetup"))
                                       (:file "builtins" :depends-on ("builtins-file" "builtins-set" "builtins-buffer"))
                                       (:file "array" :depends-on ("lsetup"))
                                       (:file "_ast" :depends-on ("lsetup"))
                                       (:file "binascii" :depends-on ("lsetup"))
                                       (:file "_bsddb" :depends-on ("lsetup"))
                                       (:file "_collections" :depends-on ("lsetup"))
                                       (:file "_codecs" :depends-on ("lsetup"))
                                       (:file "cStringIO" :depends-on ("lsetup"))
                                       (:file "datetime" :depends-on ("lsetup"))
                                       (:file "errno"  :depends-on ("lsetup"))
                                       (:file "exceptions" :depends-on ("lsetup"))
                                       (:file "_functools" :depends-on ("lsetup"))
                                       (:file "gc"  :depends-on ("lsetup"))
                                       (:file "imp" :depends-on ("lsetup"))
                                       (:file "itertools" :depends-on ("lsetup"))
                                       (:file "marshal"  :depends-on ("lsetup"))
                                       (:file "math"  :depends-on ("lsetup"))
                                       (:file "_md5" :depends-on ("lsetup"))
                                       (:file "operator" :depends-on ("lsetup"))
                                       (:file "posix" :depends-on ("lsetup"))
                                       (:file "_random" :depends-on ("lsetup"))
                                       (:file "re" :depends-on ("lsetup"))
                                       (:file "_sha" :depends-on ("lsetup"))
                                       (:file "_sha256" :depends-on ("lsetup"))
                                       (:file "_sha512" :depends-on ("lsetup"))
                                       (:file "_socket" :depends-on ("lsetup"))
                                       (:file "_sre" :depends-on ("lsetup"))
                                       (:file "_ssl" :depends-on ("lsetup"))
                                       (:file "_struct" :depends-on ("lsetup"))
                                       (:file "sys" :depends-on ("lsetup"))
                                       (:file "string" :depends-on ("lsetup"))
                                       (:file "symbol" :depends-on ("lsetup"))
                                       (:file "thread" :depends-on ("lsetup"))
                                       (:file "time" :depends-on ("lsetup"))
                                       (:file "_weakref" :depends-on ("lsetup"))))))

(defsystem "clpython/contrib"
    :description "CLPython contributions and experiments"
    :depends-on ("clpython/basic" "clpython/runtime" "clpython/compiler")
    :components ((:module "contrib"
                          :components ((:file "repl")
                                       (:file "lispy")
                                       (:file "executable" )
                                       #+(or) ;; disable while in development
                                       ;; #+(and allegro allegro-version>= (version>= 8 2))
                                       (:file "source"       )))))

;;; Show usage after loading the system

(defun show-clpython-quick-start ()
  (format t "~%CLPython quick start guide:~%")
  (format t "  Run a string of Python code:           (~S \"for i in range(4): print i\")~%"
          (find-symbol* '#:run :clpython))
  (format t "  Run a Python file:                     (~S #p\"~~/example/foo.py\")~%"
          (find-symbol* '#:run :clpython))
  (format t "  Start the Python \"interpreter\" (REPL): (~S)~%"
          (find-symbol* '#:repl :clpython.app.repl))
  (format t "  To start mixed Python/Lisp input mode: (~S)~%"
          (find-symbol* '#:enter-mixed-lisp-python-syntax :clpython))
  (format t "  Run the test suite:                    ~S~%~%"
          '(asdf:test-system "clpython")))

;;; The main system

(defsystem "clpython"
    :description "CLPython - an implementation of Python in Common Lisp"
    :author "Willem Broekema <metawilm@gmail.com>"
    :license "LLGPL (Lisp Lesser GNU Public License)"
    :depends-on ("clpython/basic" "clpython/parser" "clpython/runtime" "clpython/compiler" "clpython/lib" "clpython/contrib")
    :in-order-to ((test-op (test-op "clpython/test")))
    :perform (load-op :after (o c) (show-clpython-quick-start)))

;;; Unit test, linked to asdf operation "test-op" on the CL-Python system

(defsystem "clpython/test"
    :description "CLPython tests"
    :depends-on ("clpython" #-allegro "ptester")
    :components ((:module "test"
                          :serial t
                          :components ((:file "tsetup")
                                       (:file "parser-test")
                                       (:file "compiler-test")
                                       (:file "lang-test")
                                       (:file "function-test")
                                       (:file "mod-builtins-test")
                                       (:file "mod-string-test")
                                       (:file "mod-math-test")
                                       (:file "mod-operator-test"))))
    :perform (test-op (o c) (symbol-call :clpython.test :run-tests)))
