;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package #:user)

;; We'd like to use the ASFD:DEFSYSTEM form below, but the problem is that
;; the option :depends-on does not work as needed:
;;
;;  :file A :depends-on (B)
;; 
;; does ensure A is compiled after B, but not that B is _loaded_ before A
;; is _compiled_, like the option :uses-definitions-from in Allegro's
;; EXCL:DEFSYSTEM.
;; 
;; Solution for now: a dummy ASDF system, that when compiled or loaded
;; executes the corresponding EXCL:DEFSYSTEM commands.

(asdf:defsystem "clpython"
    :version "1.0")

(defmethod asdf::traverse ((o asdf:compile-op) (s (eql (asdf:find-system "clpython"))))
  (format t "Compiling CLPython using defsys...")
  (load "defsys")
  (compy)
  ;; returns value are the steps to take: none
  nil)

(defmethod asdf::traverse ((o asdf:load-op) (s (eql (asdf:find-system "clpython"))))
  (format t "Loading CLPython using defsys...")
  (load "defsys")
  (compy)
  (loadpy)
  ;; returns value are the steps to take: none
  nil)

+(or)
(progn
  ;; This does not work as we we wish it would..."
  (asdf:defsystem "clpython"
      :version "1.0"
      ;;:serial t
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
		   (:file "repl"         :depends-on ("package" "compiler" "run" "classes"))))

  ;; ASDF (at least the version delivered with Allegro CL 8.0) only
  ;; looks for ".lisp" source files, but we use ".cl".

  (defmethod asdf:source-file-type :around
	     ((c asdf:cl-source-file) (s (eql (asdf:find-system "clpython"))))
    "cl"))
