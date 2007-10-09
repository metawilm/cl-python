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


;;; Check ASDF version

;; The ASDF version initially supplied with ACL 8.0 (in directory acl80/code/asdf.fasl)
;; does not handle (:serial t) correctly (it does not load A before compiling B).
;; That version has revision (1 88).
;;
;; On december 1, 2006, a patch was supplied, to be installed with (sys:update-allegro).
;; That patch updates it to revision (1 102), which is current (2007.01.04).
;; Here we verify that patching been done.

#-(and allegro-version>= (version>= 7 0))
(cerror "Continue anyway"
	"CLPython requires Allegro Common Lisp 8.0 (or perhaps 7.0)")

#+(and allegro-version>= (version>= 7 0) (not (version>= 8 0)))
(warn "CLPython is tested in Allegro Common Lisp 8.0, ~
       but it might work in version 7.0 too.")

#+allegro
(destructuring-bind (maj min) asdf::*asdf-revision*
  (unless (or (> maj 1)
	      (and (= maj 1) (>= min 102)))
    (cerror "Continue anyway, using outdated ASDF"
	    "CLPython requires a newer version of ASDF. ~
             You can upgrade automatically, using (sys:update-allegro)")))


;;; Systems

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
				       (:file "lexer"    :depends-on ("grammar"))
				       (:file "parser"   :depends-on ("grammar" "lexer"))
                                       (:file "ast-match")
                                       (:file "ast-util" :depends-on ("ast-match" "grammar"))
                                       (:file "walk"     )
				       (:file "pprint"   )))))

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

(asdf:defsystem :clpython
    :description "CLPython - an implementation of Python in Common Lisp"
    :depends-on (:clpython.package :clpython.parser :clpython.core :clpython.lib)
    :in-order-to ((asdf:test-op (asdf:load-op :clpython-test)))
    #+(or) 
    :perform
    #+(or) (asdf:test-op :after (op c)
			 (funcall (find-symbol (string '#:run) :clpython.test))))

(defmethod asdf:perform :after ((op asdf:test-op) (c (eql (asdf:find-system :clpython))))
  (funcall (find-symbol (string '#:run) :clpython.test)))

(defmethod asdf:perform :after ((op asdf:load-op) (c (eql (asdf:find-system :clpython))))
  (terpri)
  (format t "CLPython quick start:~%")
  (format t "  Run a Python file: (clpython:run-python-file \"~~/example/foo.py\").~%~%")
  (format t "After loading ASDF system :CLPYTHON-APP you can:~%")
  (format t "  Start the Python read-eval-print loop: (clpython.app.repl:repl)~%")
  (format t "  See the call count profiler: (clpython.app.profiler:profile-test).~%~%"))

(defmethod asdf:operation-done-p ((o asdf:test-op)
				  (c (eql (asdf:find-system :clpython))))
  ;; Testing is never finished.
  (values nil))
