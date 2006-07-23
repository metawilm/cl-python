;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package #:user)

;; ASDF (at least the version delivered with Allegro CL 8.0) only
;; looks for ".lisp" source files, but we use ".cl".

(defmethod asdf:source-file-type :around ((c asdf:cl-source-file) (s asdf:module))
  (if (string= (asdf:component-name s) "clpython")
      "cl"
    (call-next-method)))


(asdf:defsystem #:clpython
    :version "1.0.0"
    :serial t
    :components ((:file "package")
		 (:file "pyprint")
		 (:file "walk"   )
		 (:file "formatstring")
		 (:file "classes")
		 (:file "exceptions")
		 (:file "builtins")
		 (:file "optimize")
		 (:file "parser")
		 (:file "lexer")
		 (:file "run")
		 (:file "compiler")
		 (:file "repl")))
