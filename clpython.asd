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


;;; CL-Python is split into several ASDF systems, to make it possible to load 
;;; specific components -- in particular, to load the compiler or parser without
;;; the runtime environment.
;;; 
;;; The main system :CLPYTHON is the sum of all components, including contributions.

(asdf:defsystem :clpython.basic
    :description "CLPython package and utils"
    :depends-on (:closer-mop)
    :serial t
    :components ((:file "package")
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

(asdf:defsystem :clpython.parser
    :description "Python parser, code walker, and pretty printer"
    :depends-on 
    #.`(:clpython.basic :closer-mop
        #-allegro :yacc
        #+allegro ,@(when (asdf:find-system :yacc nil) `(:yacc)))
    :components ((:module "parser"
                          :components ((:file "grammar" )
                                       (:file "lexer"    :depends-on ("grammar"))
                                       (:file "parser"   :depends-on ("grammar" "lexer"))
                                       (:file "grammar-aclyacc" :depends-on ("grammar" "lexer" "parser"))
                                       (:file "grammar-clyacc"  :depends-on ("grammar" "lexer" "parser"))
                                       (:file "ast-util" :depends-on ("grammar"))
                                       (:file "walk"   )
                                       (:file "pprint" )))))

(asdf:defsystem :clpython.compiler
    :description "Python compiler"
    :depends-on (:clpython.basic :clpython.parser :clpython.runtime :closer-mop)
    :serial t
    :components ((:module "compiler"
                          :serial t
                          :components ((:file "csetup"       )
                                       (:file "pydecl"       )
                                       (:file "namespace"    )
                                       (:file "compiler"     )
                                       (:file "generator"    )                                       
                                       (:file "optimize"     )))))

(asdf:defsystem :clpython.runtime
    :description "Python runtime environment"
    :depends-on (:clpython.basic :closer-mop #+ecl :cl-custom-hash-table :temporary-file)
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

(asdf:defsystem :clpython.lib
    :description "Python module library"
    :depends-on (:clpython.basic :clpython.runtime :clpython.compiler #| TODO: remove compiler dep |#)
    :components ((:module "lib"
                          :serial t
                          :components ((:file "lsetup")
                                       (:file "builtins-file" :depends-on ("lsetup"))
                                       (:file "builtins-set" :depends-on ("lsetup"))
                                       (:file "builtins-buffer" :depends-on ("lsetup"))
                                       (:file "builtins" :depends-on ("builtins-file" "builtins-set" "builtins-buffer"))
                                       (:file "array" :depends-on ("lsetup"))
                                       (:file "binascii" :depends-on ("lsetup"))
                                       (:file "_collections" :depends-on ("lsetup"))
                                       (:file "_codecs" :depends-on ("lsetup"))
                                       (:file "cStringIO" :depends-on ("lsetup"))
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
                                       (:file "_ssl" :depends-on ("lsetup"))
                                       (:file "_struct" :depends-on ("lsetup"))
                                       (:file "sys" :depends-on ("lsetup"))
                                       (:file "string" :depends-on ("lsetup"))
                                       (:file "symbol" :depends-on ("lsetup"))
                                       (:file "thread" :depends-on ("lsetup"))
                                       (:file "time" :depends-on ("lsetup"))))))

(asdf:defsystem :clpython.contrib
    :description "CLPython contributions and experiments"
    :depends-on (:clpython.basic :clpython.runtime :clpython.compiler)
    :components ((:module "contrib"
                          :components ((:file "repl")
                                       (:file "lispy")
                                       (:file "executable" )
                                       #+(or) ;; disable while in development
                                       ;; #+(and allegro allegro-version>= (version>= 8 2))
                                       (:file "source"       )))))

;;; The main system

(asdf:defsystem :clpython
    :description "CLPython - an implementation of Python in Common Lisp"
    :depends-on (:clpython.basic :clpython.parser :clpython.runtime :clpython.compiler :clpython.lib :clpython.contrib)
    :in-order-to ((asdf:test-op (asdf:load-op :clpython.test))))

;;; Unit test, linked to asdf operation "test-op" on the CL-Python system

(asdf:defsystem :clpython.test
    :description "CLPython tests"
    :depends-on (:clpython #-allegro :ptester)
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
                                       (:file "mod-operator-test")))))


(defmethod asdf:perform :after ((op asdf:test-op) (c (eql (asdf:find-system :clpython))))
  (funcall (find-symbol (string '#:run-tests) :clpython.test)))

(defmethod asdf:operation-done-p ((o asdf:test-op)
                                  (c (eql (asdf:find-system :clpython))))
  "Testing is never finished."
  nil)


;;; In Allegro, which provides its own Yacc, CL-Yacc can optionally be used.
;;; In other implementations, Allegro Yacc is unavailable

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


;;; Nice error messages for missing required libraries

(defmacro with-missing-dep-help (lib-texts &body body)
  `(handler-bind ((asdf:missing-dependency
                   (lambda (c)
		     ,@(loop for (library warning-text) in lib-texts
			     collect `(when (eq (asdf::missing-requires c) ,library)
					(warn ,warning-text))))))
     ,@body))

(let ((clpython (asdf:find-system :clpython)))
  
  (defmethod asdf::traverse :around ((op asdf:compile-op) (system (eql clpython)))
    (with-missing-dep-help ((:closer-mop
			     "CL-Python requires library \"Closer to MOP\". ~
                              Please check it out from the darcs repo: ~
                              \"darcs get http://common-lisp.net/project/closer/repos/closer-mop\" ~
                              or download the latest release from: ~
                              http://common-lisp.net/project/closer/ftp/closer-mop_latest.tar.gz")
			    #-allegro
			    (:yacc
                             "CL-Python requires library \"CL-Yacc\". ~
                              Please check it out from the darcs repo: ~
                              \"darcs get http://www.pps.jussieu.fr/~~jch/software/repos/cl-yacc\" ~
                              or download the latest release from: ~
                              http://www.pps.jussieu.fr/~~jch/software/files/")
                            #+ecl
                            (:cl-custom-hash-table
                             "CL-Python requires library \"CL-CUSTOM-HASH-TABLE\". ~
                              Please check it out from the git repo: ~
                              \"git clone git://github.com/metawilm/cl-custom-hash-table.git\" ~
                              or download the latest release from: ~
                              https://github.com/metawilm/cl-custom-hash-table/zipball/master"))
			   (call-next-method)))
  
  #-allegro
  (defmethod asdf::traverse :around ((op asdf:test-op) (system (eql clpython)))
    (with-missing-dep-help ((:ptester
			     "CL-Python requires library \"ptester\". ~
                              Please download the latest release from: ~
                              http://files.b9.com/ptester/ptester-latest.tar.gz"))
      (call-next-method))))


;;; Suppress some warnings about package trickery 

(defmacro suppress-package-warnings (&body body)
  `(handler-bind (#+sbcl 
		  (sb-int:package-at-variance #'muffle-warning)
		  #+lispworks
		  (simple-warning (lambda (c)
				    (let ((fmt (slot-value c 'conditions::format-string)))
				      (when (search "Using DEFPACKAGE" fmt)
					(muffle-warning c))))))
		 ,@body))
      
(let* ((package-file (let ((sys (asdf:find-system :clpython.basic)))
		       (car (asdf:module-components sys))))
       (lib-mod (let ((sys (asdf:find-system :clpython.lib)))
                  (car (asdf:module-components sys))))
       (lib-pkg-file (asdf:find-component lib-mod "psetup"))
       (pkg-files (list package-file lib-pkg-file)))

  (#+allegro without-redefinition-warnings ;; invalid complaint about method redefinition
   #-allegro progn

   (dolist (pkg-file pkg-files)
     
     (defmethod asdf:perform :around ((op asdf:compile-op) (c (eql pkg-file)))
       (suppress-package-warnings
	(call-next-method)))
     
     (defmethod asdf:perform :around ((op asdf:load-op) (c (eql pkg-file)))
       (suppress-package-warnings
	(call-next-method))))))


;;; Show usage after loading the system

(defun show-clpython-quick-start ()
  (format t "~%CLPython quick start guide:~%")
  (format t "  Run a string of Python code:           (~S \"for i in range(4): print i\")~%" 
          (find-symbol (string '#:run) :clpython))
  (format t "  Run a Python file:                     (~S #p\"~~/example/foo.py\")~%"
          (find-symbol (string '#:run) :clpython))
  (format t "  Start the Python \"interpreter\" (REPL): (~S)~%"
          (find-symbol (string '#:repl) :clpython.app.repl))
  (format t "  To start mixed Python/Lisp input mode: (~S)~%"
          (find-symbol (string '#:enter-mixed-lisp-python-syntax) :clpython))
  (format t "  Run the test suite:                    ~S~%~%"
          '(asdf:operate 'asdf:test-op :clpython)))

(defmethod asdf:perform :after ((op asdf:load-op) (c (eql (asdf:find-system :clpython))))
  (show-clpython-quick-start))
