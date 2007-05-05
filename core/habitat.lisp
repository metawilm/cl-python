;; -*- package: clpython -*-
;; 
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Habitat: "Where the snakes live"

(in-package :clpython)
;(in-syntax *ast-user-readtable*)

(defvar *habitat* nil "The current execution context")

(defclass habitat ()
  ((stdin           :initform *standard-input*
		    :initarg :stdin
		    :accessor habitat-stdin)
   (stdout          :initform *standard-output*
		    :initarg :stdout
		    :accessor habitat-stdout)
   (stderr          :initform *error-output* 
		    :initarg :stderr
		    :accessor habitat-stderr)
   (search-paths    :initarg :search-paths
		    :initform ()
		    :accessor habitat-search-paths)
   (known-mods      :initform ()
		    :initarg :known-mods   
		    :accessor habitat-known-mods
		    :documentation "List of modules")
   (loaded-mods     :initform ()
		    :initarg :loaded-mods   
		    :accessor habitat-loaded-mods
		    :documentation "List of modules; subset of KNOWN-MODS")
   (autoload-p      :initform :builtin
		    :initarg :autoload-p
		    :accessor habitat-autoload-p
		    :documentation "Whether modules are imported automatically")
   (comp-mode       :initform nil
		    :initarg :debug
		    :accessor habitat-debug
		    :documentation "Module compilation mode"))
  (:documentation "Python execution context"))

(defun make-habitat (&rest options)
  (apply #'make-instance 'habitat options))

(defun get-known-module (name habitat)
  (check-type name string)
  (check-type habitat habitat)
  (find name (habitat-known-mods habitat)
	:key #'module-name
	:test #'string-equal))

(defun add-known-module (module habitat)
  (check-type module py-module)
  (check-type habitat habitat)
  (remove-known-module (module-name module) habitat)
  (push module (habitat-known-mods habitat)))

(defun remove-known-module (name habitat &key must-exist (also-remove-from-loaded t))
  (check-type name string)
  (check-type habitat habitat)
  (when must-exist
    (assert (member name (habitat-known-mods habitat)
		    :key #'module-name :test #'string-equal)))
  (setf (habitat-known-mods habitat)
    (remove name (habitat-known-mods habitat)
	    :test #'string-equal :key #'module-name))
  (when also-remove-from-loaded
    (remove-loaded-module name habitat)))


(defun get-loaded-module (name habitat)
  (check-type name string)
  (check-type habitat habitat)
  (find name (habitat-loaded-mods habitat)
	:key #'module-name
	:test #'string-equal))

(defun add-loaded-module (module habitat)
  (check-type module py-module)
  (check-type habitat habitat)
  (remove-loaded-module (module-name module) habitat)
  (push module (habitat-loaded-mods habitat)))
  
(defun remove-loaded-module (name habitat &key must-exist)
  (check-type name string)
  (check-type habitat habitat)
  (when must-exist
    (assert (member name (habitat-loaded-mods habitat)
		    :key #'module-name :test #'string-equal)))
  (setf (habitat-loaded-mods habitat)
    (remove name (habitat-loaded-mods habitat)
	    :test #'string-equal :key #'module-name)))


(defun run-python-ast (ast &key habitat)
  "Run Python AST in freshly bound habitat"
  (let* ((*habitat* habitat)
	 (f (compile nil `(lambda () ,ast))))
    (funcall f)))
    
(defun run-python-string (string &rest args)
  (apply #'run-python-ast (parse-python-string string) args))

(defun run-python-file (fname &rest args)
  (apply #'run-python-ast (parse-python-file fname) args))