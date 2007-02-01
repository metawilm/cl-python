;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package #:user)

(eval-when (:compile-toplevel)
  (error "This ASDF file should be run interpreted."))

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


;;; System definitions

(asdf:defsystem :clpython.package
    :components ((:file "package")))

(asdf:defsystem :clpython.parser
    :description "Python source code and AST handling"
    :depends-on (:clpython.package)
    :components ((:module "ast"
			  :components ((:file "grammar")
				       (:file "lexer"  :depends-on ("grammar"))
				       (:file "parser" :depends-on ("grammar" "lexer"))
				       (:file "walk")
				       (:file "astpp")
				       (:file "lispy")))))

(asdf:defsystem :clpython.core
    :description "Python objects, semantics, and compiler"
    :depends-on (:clpython.package :clpython.parser)
    :components ((:module "core"
			  :serial t
			  :components ((:file "formatstring" )
				       (:file "classes"      )
				       (:file "exceptions"   )
				       (:file "builtins"     )
				       (:file "compiler"     )
				       (:file "optimize"     )
				       (:file "modules"      )))))

(asdf:defsystem :clpython.lib
    :description "Python libraries"
    :depends-on (:clpython.package :clpython.parser :clpython.core)
    :components ((:module "lib"
			  :components ((:file "sys")
				       (:file "time")
				       (:file "os")
				       (:file "array")))))

(asdf:defsystem :clpython
    :description "CLPython - an implementation of Python in Common Lisp"
    :depends-on (:clpython.package :clpython.parser :clpython.core :clpython.lib))


;;; Applications built on top of CLPython

(asdf:defsystem :clpython.app.repl
    :description "CLPython read-eval-print loop"
    :depends-on (:clpython)
    :components ((:module "app"
			  :components ((:module "repl"
						:components ((:file "repl")))))))