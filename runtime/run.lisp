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

(defgeneric run (thing &rest args))

;; These :AROUND methods are for the compiler, e.g. used to make
;; "import" of files next to the one currently run works.

(defmethod run :around ((thing file-stream) &rest args)
  (declare (ignore args))
  (let* ((*compile-file-pathname* (namestring thing))
         (*compile-file-truename* (truename *compile-file-pathname*)))
    (call-next-method)))
  
(defmethod run :around ((thing pathname) &rest args)
  (declare (ignore args))
  (let* ((*compile-file-pathname* thing)
         (*compile-file-truename* (truename thing)))
    (call-next-method)))

(defmethod run (thing &rest args)
  (apply #'run-python-ast (parse thing) args))
