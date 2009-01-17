;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON; Readtable: PY-USER-READTABLE -*-
;; 
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Running Python code

(in-package :clpython)
(in-syntax *user-readtable*)

;; If a macro raises an ERROR, it is undefined whether the compiler
;; catches that or passes it on outwards. CLHS on COMPILE: "compile is
;; permitted, but not required, to establish a handler for conditions
;; of type error."
;; 
;; In our case, certain syntax errors are detected during
;; macroexpansion. By using non-ERROR conditions, as suggested by
;; Nikodemus Siivola, we can communicate that event portably to an
;; outer level. It does require that every compilation of a Python
;; source file is wrapped in the WITH-form.
;;
;; <http://article.gmane.org/gmane.lisp.steel-bank.devel/11291>

(define-condition raise-syntax-error-please (condition)
  ((syntax-error-args :initarg :syntax-error-args :accessor rsep-syntax-error-args)))

(defun compiler-detect-syntax-error (&rest syntax-error-args)
  (signal (make-condition 'raise-syntax-error-please
            :syntax-error-args syntax-error-args))
  ;; If no-one is listening (so not inside with-compiler-generated-syntax-errors)
  (break "Uncaught SyntaxError: ~A" (apply #'format nil syntax-error-args)))

(defmacro with-compiler-generated-syntax-errors (() &body body)
  `(handler-bind ((raise-syntax-error-please
                   (lambda (c)
                     (apply #'py-raise '{SyntaxError} (rsep-syntax-error-args c)))))
     (let ((*raise-syntax-error-hook* 'compiler-detect-syntax-error))
       ,@body)))

;;; Running Python code in various forms: string, ast, file

(defvar *compile-python-ast-before-running* t
  "Whether to compile an AST before running it.")

(defun run (thing &rest args)
  (apply #'run-python-ast (parse thing) args))

(defun run-python-ast (ast &key (habitat *habitat*)
                                (compile *compile-python-ast-before-running*)
                                module-run-args
                                args
                                compile-quiet)
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
      ;; set args:
      (setf (habitat-cmd-line-args *habitat*) (make-py-list-from-list '("???.py")))
      (when args
        (typecase args
          (string (setf args (py-string.split args " ")))
          (list (setf args (make-py-list-from-list args))))
        (py-list.extend (habitat-cmd-line-args *habitat*) args))
      (let* (module-function
             (*module-function-hook* (lambda (f) (setf module-function f))))
        (funcall fc)
        (unless module-function
          (break "Module ~A did not call *module-function*." fc))
        (apply module-function module-run-args)))))

(defun compile-py-file (fname &key (verbose t))
  (let* ((module (pathname-name fname))
         (fasl-file (compiled-file-name :module module fname))
         (*import-force-recompile* t)
         (*import-compile-verbose* verbose))
    (declare (special *import-force-recompile* *import-compile-verbose*))
    (%compile-py-file fname :mod-name module :output-file fasl-file)))
