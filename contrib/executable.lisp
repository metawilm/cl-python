;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython)

(defun command-line-args ()
  ;; List of strings, first item = executable path
   #+sbcl cl-user::*POSIX-ARGV*
   #-sbcl (break "COMMAND-LINE-ARGS undefined in this implementation"))

#+cmu
(defun build-executable (&rest args)
  ;; Error in function LISP::ASSERT-ERROR:
  ;; The assertion (EQ (C::FUNCTIONAL-KIND (C::LAMBDA-HOME C::FUN)) :TOP-LEVEL) failed.
  (declare (ignore args))
  (error "Does not work in CMUCL yet (see <http://article.gmane.org/gmane.lisp.cmucl.devel/10782>)"))

#-cmu
(defun build-executable (&key filename function (habitat *habitat*) repl-module-globals
                              #+sbcl (ask-terminate t))
  (declare (special clpython.app.repl:*repl-module-globals*)
           (ignorable habitat))
  (when (and function (not repl-module-globals))
    (setf repl-module-globals clpython.app.repl:*repl-module-globals*))
  (unless filename
    (let ((clpython-date-impl
           (multiple-value-bind (second minute hour date month year day daylight-p zone)
               (get-decoded-time)
             (declare (ignorable second minute hour date month year day daylight-p zone))
             (format nil "clpython-sbcl-~A~2,vD~2,vD" year #\0 month #\0 day))))
      (if function
          (setf filename
            (concatenate 'string clpython-date-impl "-"
                         (format nil "~A" (py-function.__name__ function))))
        (setf filename clpython-date-impl))))
  (flet ((toplevel ()
           ;; closes over FUNCTION, HABITAT
           (let ((exit-status 1)
                 (*muffle-sbcl-compiler-notes* t))
             (handler-bind ((error (lambda (c)
                                     (format t "~%Exception occured: ~A" c)
                                     (format t "~%No backtrace yet, sorry.")
                                     (let ((r (find-restart 'clpython.app.repl:return-python-toplevel)))
                                       (if r
                                           (progn (format t "~%Returning to top level.~%")
                                                  (invoke-restart r))
                                         (progn (format t "~%Exiting.~%")
                                                (return-from toplevel exit-status)))))))
               (destructuring-bind (app . args)
                   (command-line-args)
                 (flet ((restore-and-run-function ()
                          (let ((*habitat* habitat)
                                (clpython.app.repl:*repl-module-globals* repl-module-globals))
                            (declare (special clpython.app.repl:*repl-module-globals*))
                            (setf (habitat-cmd-line-args *habitat*) (make-py-list-from-list args))
                            (py-call function))
                          (setf exit-status 0))
                        (run-repl ()
                          (clpython.app.repl:repl :cmd-args args)
                          (setf exit-status 0))
                        (run-file (path)
                          (run path :cmd-args args)
                          (setf exit-status 0))
                        (print-usage ()
                          (format t "Error: file not found: ~A~%" (car args))
                          (format t "Usage:  start interpreter:   ~A~%" app)
                          (format t "        execute Python file: ~A <FILE>~%" app)))
                   (cond (function (restore-and-run-function))
                         ((null args) (run-repl))
                         (t
                          (let ((path (ignore-errors (truename (car args)))))
                            (if path
                                (run-file path)
                              (print-usage))))))
                 exit-status)))))
    (declare (ignorable #'toplevel))
    (flet (#+sbcl (build-sbcl ()
                    (when (and ask-terminate
                               (not (y-or-n-p
                                     "Building executable will terminate the Lisp session. Continue?")))
                      (return-from build-sbcl))
                    (format t "Writing SBCL repl executable to: ~A~%" filename)
                    (sb-ext:save-lisp-and-die filename
                                              :executable t
                                              :toplevel #'toplevel)))
      
      #-sbcl (error "Building Python executables is not yet supported in this Lisp implementation.")
      #+sbcl (build-sbcl))))

(def-py-method py-function._exe (func)
  "Create executable from function"
  (build-executable :function func))