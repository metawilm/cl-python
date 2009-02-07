;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON; Readtable: PY-USER-READTABLE -*-
;; 
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython)
(in-syntax *user-readtable*)

;;;; Running Python code


(defvar *compile-python-ast-before-running* t
  "Whether to compile an AST before running it.")

(defun run (thing &rest args)
  (apply #'run-python-ast (parse thing) args))

(defun run-python-ast (ast &key (habitat *habitat*)
                                (compile *compile-python-ast-before-running*)
                                module-run-args
                                compile-quiet
                                time
                                args)
  "Run Python AST in freshly bound habitat.
HABITAT is the execution environment; a fresh one will be used otherwie.
If COMPILE is true, the AST is compiled into a function before running.
MODULE-RUN-ARGS is a list with options passed on to the module-function; e.g. %module-globals, module-name, src-module-path.
ARGS are the command-line args, available as `sys.argv'; can be a string or a list of strings."
  (with-compiler-generated-syntax-errors ()
    (let* ((*habitat* habitat)
           (get-module-f `(lambda () ,ast))
           (fc (if compile
                   (let ((*compile-print* (if compile-quiet nil *compile-print*))
                         (*compile-verbose* (if compile-quiet nil *compile-verbose*)))
                     (compile nil get-module-f))
                 (coerce get-module-f 'function))))
      (unless *habitat* (setf *habitat* (make-habitat)))
      (when (or args (null (habitat-cmd-line-args *habitat*)))
        (setf (habitat-cmd-line-args *habitat*) args))
      (let* (module-function
             (*module-function-hook* (lambda (f) (setf module-function f))))
        (funcall fc)
        (unless module-function
          (break "Module ~A did not call *module-function*." fc))
        (if time
            (time (apply module-function module-run-args))
          (apply module-function module-run-args))))))

(defun compile-py-file (fname &key (verbose t))
  (let* ((module (pathname-name fname))
         (fasl-file (compiled-file-name :module module fname))
         (*import-force-recompile* t)
         (*import-compile-verbose* verbose))
    (declare (special *import-force-recompile* *import-compile-verbose*))
    (%compile-py-file fname :mod-name module :output-file fasl-file)))
