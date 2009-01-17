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


;;; Core systems: parser, compiler, runtime

(asdf:defsystem :clpython.package
    :description "CLPython package and readtables"
    :components ((:module "package"
                          :components ((:file "package")
                                       (:file "utils" :depends-on ("package"))
                                       (:file "readtable" :depends-on ("package"))
                                       (:file "aupprint" :depends-on ("package"))))))
(asdf:defsystem :clpython.depend
    :description "External libraries included with minor modifications, until the changes ~
are accepted in those libraries."
    :components ((:module "depend"
                          :components ((:module "cl-yacc"
                                                :components ((:file "yacc")))))))

(asdf:defsystem :clpython.parser
    :description "Python parser, code walker, and pretty printer"
    :depends-on (:clpython.package :clpython.depend)
    :components ((:module "parser"
			  :components ((:file "psetup"  )
				       (:file "grammar"  :depends-on ("psetup"))
                                       (:file "lexer"    :depends-on ("grammar"))
                                       (:file "parser"   :depends-on ("grammar" "lexer"))
                                       (:file "grammar-aclyacc" :depends-on ("grammar" "lexer" "parser"))
                                       (:file "grammar-clyacc"  :depends-on ("grammar" "lexer" "parser"))
                                       (:file "ast-match")
                                       (:file "ast-util" :depends-on ("ast-match" "grammar"))
                                       (:file "walk"     :depends-on ("psetup"))
				       (:file "pprint"   :depends-on ("psetup"))))))

(asdf:defsystem :clpython.core
    :description "Python semantics and compiler"
    :depends-on (:clpython.package :clpython.parser :closer-mop)
    :components ((:module "core"
                          :serial t
                          :components ((:file "csetup"       )
                                       (:file "pydecl"       )
                                       (:file "formatstring" )
                                       (:file "metaclass"    )
                                       (:file "dictattr"     )
                                       (:file "classes"      )
                                       (:file "exceptions"   )
                                       (:file "compiler"     )
                                       (:file "generator"    )
                                       (:file "optimize"     )
                                       (:file "run"          )
                                       (:file "habitat"      )
                                       (:file "import"       )))))

(asdf:defsystem :clpython.lib
    :description "Python module library"
    :depends-on (:clpython.package :clpython.parser :clpython.core)
    :components ((:module "lib"
                          :components ((:file "builtins-file")
                                       (:file "builtins-set")
                                       (:file "builtins" :depends-on ("builtins-file" "builtins-set"))
                                       (:file "lsetup")
                                       (:file "array" :depends-on ("lsetup"))
                                       (:file "binascii"  :depends-on ("lsetup"))
                                       (:file "exceptions" :depends-on ("lsetup"))
                                       (:file "gc"  :depends-on ("lsetup"))
                                       (:file "math"  :depends-on ("lsetup"))
                                       (:file "operator" :depends-on ("lsetup"))
                                       (:file "os" :depends-on ("lsetup"))
                                       (:file "_random" :depends-on ("lsetup"))
                                       (:file "re" :depends-on ("lsetup"))
                                       (:file "sys" :depends-on ("lsetup"))
                                       (:file "string" :depends-on ("lsetup"))
				       (:file "symbol" :depends-on ("lsetup"))
                                       (:file "thread" :depends-on ("lsetup"))
                                       (:file "time" :depends-on ("lsetup"))))))

;;; Application systems

(asdf:defsystem :clpython.app.repl
    :description "CLPython read-eval-print loop"
    :depends-on (:clpython.core)
    :components ((:module "app"
			  :components ((:module "repl"
						:components ((:file "repl")))))))

(asdf:defsystem :clpython.app.profiler
    :description "CLPython call count profiler"
    :depends-on (:clpython.core)
    :components ((:module "app"
			  :components ((:module "profiler"
						:components ((:file "profiler")))))))

(asdf:defsystem :clpython.app
    :description "CLPython applications"
    :depends-on (:clpython.app.repl :clpython.app.profiler))

;;; The main system

(asdf:defsystem :clpython
    :description "CLPython - an implementation of Python in Common Lisp"
    :depends-on (:clpython.package :clpython.parser :clpython.core :clpython.lib clpython.app)
    :in-order-to ((asdf:test-op (asdf:load-op :clpython-test))))

;; Check for presence of CL-Yacc and Allegro CL Yacc.

(let* ((parser-mod (let ((sys (asdf:find-system :clpython.parser)))
                     (car (asdf:module-components sys)))))
  
  #+(or) ;; Disabled while modified CL-Yacc is included in CLPython/depend
  (let ((cl-yacc-grammar (asdf:find-component parser-mod "grammar-clyacc")))
    (defmethod asdf:perform :around ((op asdf:load-op) (c (eql cl-yacc-grammar)))
      (when (asdf:find-system :yacc nil)
        (call-next-method)
        (format t "Note: The asdf system CL-Yacc was found. ~
                   To use CL-Yacc as parser for CLPython, bind ~S to ~S.~%"
                (find-symbol (string '#:*default-yacc-version*)
                             (find-package '#:clpython.parser)) :cl-yacc)))
    (defmethod asdf:perform :around ((op asdf:compile-op) (c (eql cl-yacc-grammar)))
      (when (asdf:find-system :yacc nil)
        (call-next-method))))
  
  ;; Skip loading Allegro yacc in non-Allegro environment
  (let ((allegro-yacc-grammar (asdf:find-component parser-mod "grammar-aclyacc")))
    (defmethod asdf:perform :around ((op asdf:load-op) (c (eql allegro-yacc-grammar)))
      #+allegro (call-next-method))
    (defmethod asdf:perform :around ((op asdf:compile-op) (c (eql allegro-yacc-grammar)))
      #+allegro (call-next-method))))


;;; Create concatenated fasl file

(defun system-fasl-files (system system-prefix)
  (let ((operation (make-instance 'asdf:compile-op :force t))
        (done (make-hash-table :test 'eq)))
    (labels ((dig (x)
               (unless (gethash x done)
                 (setf (gethash x done) t)
                 (when (include-system-p (component-system x))
                   (cond ((typep x 'asdf:cl-source-file)
                          (list x))
                         (t (loop for (op . thing) in (asdf::traverse operation x)
                                when (typep op 'asdf:load-op)
                                nconc (dig thing)))))))
             (component-system (f)
               (etypecase f
                 (asdf:system f)
                 (asdf:component
                  (loop for parent = (asdf:component-parent f) then (asdf:component-parent parent)
                      until (or (null parent)
                                (typep parent 'asdf:system))
                      finally (return parent)))))
             (include-system-p (s)
               (check-type s asdf:system)
               (let ((name (asdf:component-name s)))
                 (string-equal (subseq name 0 (min (length system-prefix) (length name)))
                               system-prefix))))
      (let* ((files (dig (asdf:find-system system)))
             (fasl-files (mapcan (lambda (f) (asdf:output-files operation f)) files)))
        fasl-files))))

#+allegro ;; Concatenating fasl files might not be supported in other implementations.
(defun write-clpython-fasl (&optional (output-fasl "clpython.fasl"))
  (let ((fasls (system-fasl-files :clpython "clpython")))
    (when (probe-file output-fasl)
      (let ((newest-time (apply #'max (mapcar #'file-write-date fasls)))
            (output-time (file-write-date output-fasl)))
        (when (< newest-time output-time)
          (format t "~%;; Concatenated fasl file ~A still up to date.~%" (probe-file output-fasl))
          (return-from write-clpython-fasl (probe-file output-fasl)))))
    (format t "~%;; Concatenating ~A CLPython fasl files to ~A...~%" (length fasls) output-fasl)
    (with-open-file (out output-fasl :element-type '(unsigned-byte 8)
                     :direction :output
                     :if-exists :supersede)
      ;;:if-does-not-exist :create
      (dolist (fasl fasls)
        (with-open-file (in fasl :element-type '(unsigned-byte 8) :direction :input)
          (let* ((len (file-length in))
                 (vector (make-array len :element-type '(unsigned-byte 8))))
            (assert (= (read-sequence vector in) len))
            (write-sequence vector out))))))
  (format t ";; Writing to ~A done.~%" (truename output-fasl))
  (truename output-fasl))


;;; Show usage

(defmethod asdf:perform :after ((op asdf:load-op) (c (eql (asdf:find-system :clpython))))
  #+allegro
   (let ((concatenated-fasl (write-clpython-fasl)))
     (format t "~%To load CLPython next time, you can use either: ~%")
     (format t "  ~S~%" `(load ,concatenated-fasl))
     (format t "  ~S~%" '(asdf:operate 'asdf:load-op :clpython)))
  
   (progn
     (format t "~%CLPython quick start guide:~%")
     (format t "  Run a string of Python code:           (~S \"for i in range(4): print i\")~%" (find-symbol (string '#:run) :clpython))
     (format t "  Run a Python file:                     (~S #p\"~~/example/foo.py\")~%" (find-symbol (string '#:run) :clpython))
     (format t "  Start the Python \"interpreter\" (REPL): (~S)~%" (find-symbol (string '#:repl) :clpython.app.repl))
     (format t "  Run the test suite:                    ~S~%~%" '(asdf:operate 'asdf:test-op :clpython))))


;;; Link asdf operation "test-op" to asdf system "clpython.test"

(defmethod asdf:perform :after ((op asdf:test-op) (c (eql (asdf:find-system :clpython))))
  (funcall (find-symbol (string '#:run-tests) :clpython.test)))

(defmethod asdf:operation-done-p ((o asdf:test-op)
				  (c (eql (asdf:find-system :clpython))))
  "Testing is never finished."
  (values nil))
