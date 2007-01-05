;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package #:user)

(eval-when (compile)
  (error "This ASDF file should be run interpreted"))

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

(asdf:defsystem "clpython"
    :version "1.0"
    :serial t
    :components ((:file "package")
		 (:file "pyprint"      :depends-on ("package"))
		 (:file "walk"         :depends-on ("package"))
		 (:file "formatstring" :depends-on ("package"))
		 (:file "classes"      :depends-on ("package" "pyprint" "formatstring"))
		 (:file "exceptions"   :depends-on ("classes"))
		 (:file "builtins"     :depends-on ("exceptions" "classes"))
		 (:file "optimize"     :depends-on ("classes" "builtins"))
		 (:file "parser"       :depends-on ("package"))
		 (:file "lexer"        :depends-on ("parser" ))
		 (:file "run"          :depends-on ("lexer" "parser"))
		 (:file "compiler"     :depends-on ("builtins" "walk" "run"))
		 (:file "modules"      )
		 (:file "repl"         :depends-on ("package" "compiler" "run"
							      "classes" "modules"))))

;; ASDF by default only looks for ".lisp" source files, but we use ".cl".
(defmethod asdf:source-file-type :around 
	   ((c asdf:cl-source-file) (s (eql (asdf:find-system "clpython"))))
  "cl")
