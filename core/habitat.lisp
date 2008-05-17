;; -*- package: clpython; readtable: py-ast-user-readtable -*-
;; 
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Habitat: "Where the snakes live"

(in-package :clpython)
(in-syntax *user-readtable*)

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

(defvar *compile-python-ast-before-running* t
  "Whether to compile an AST before running it.")

(defmacro without-sbcl-compiler-errors-for-syntax-errors (&body body)
  #+sbcl
  `(call-without-sbcl-compiler-macros-for-syntax-errors
    (lambda ()
      ;; In interpreted code, if a macroexpansion signals an error,
      ;; that error is first SIGNAL-led by SBCL, and then a
      ;; SB-INT:COMPILED-PROGRAM-ERROR is ERROR-ed. Here we
      ;; ensure that a syntax error is aways raised using ERROR.
      (handler-bind (({SyntaxError} #'error))
        (progn ,@body))))
  #-sbcl `(progn ,@body))

#+sbcl
(defun call-without-sbcl-compiler-macros-for-syntax-errors (f)
  ;; SBCL compiler reports errors signalled during macroexpansion.
  ;; But a few Python syntax errors are detected and signalled during macroexpansion.
  ;; We want the SyntaxError to throw the user in the debugger, not to have
  ;; it just reported.
  ;; 
  ;; This is implemented by wrapping sb-c:compiler-error, which has arglist
  ;; (datum &rest arguments). Simply look for SyntaxError as datum or a format arg,
  ;; and in that case call ERROR instead of the normal report functionality.
  (flet ((wrap ()
           (sb-int:encapsulate
              'sb-c:compiler-error
              'do-nothing-for-syntax-errors
              ;; ARG-LIST and BASIC-DEFINITION are special variables, but
              ;; not declared as globals by SBCL, and declaring them special
              ;; here results in package lock violation.
              ;; Therefore using symbol-value.
              `(let ((c (find-if (lambda (x) (typep x '{SyntaxError})) 
                                 (symbol-value 'sb-int:arg-list))))
                 (if c
                     (error c)
                   (apply (symbol-value 'sb-int:basic-definition)
                          (symbol-value 'sb-int:arg-list))))))
         (unwrap ()
           (sb-int:unencapsulate 'sb-c:compiler-error 'do-nothing-for-syntax-errors)))
    (unwind-protect
        (progn (wrap)
               (funcall f))
      (unwrap))))


(defun run-python-ast (ast &key (habitat *habitat*)
                                (compile *compile-python-ast-before-running*)
                                run-args)
  "Run Python AST in freshly bound habitat.
If COMPILE is true, the AST is compiled into a function before running."
  (without-sbcl-compiler-errors-for-syntax-errors
   (let* ((*habitat* habitat)
          (get-module-f `(lambda () ,ast))
          (fc (if compile
                  (compile nil get-module-f)
                (coerce get-module-f 'function))))
     (let* (module-function
            (*module-function* (lambda (f) (setf module-function f))))
       (declare (special *module-function*))
       (funcall fc)
       (unless module-function
         (break "Module ~A did not call *module-function*." fc))
       (apply module-function run-args)))))

(defun run (thing &rest args)
  (apply #'run-python-ast (parse thing) args))
