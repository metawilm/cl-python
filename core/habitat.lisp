;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON; Readtable: PY-AST-USER-READTABLE -*-
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
  ((stdin          :initform nil :initarg :stdin  :accessor %habitat-stdin)
   (stdout         :initform nil :initarg :stdout :accessor %habitat-stdout)
   (stderr         :initform nil :initarg :stderr :accessor %habitat-stderr)
   (loaded-mods    :initform () :initarg :loaded-mods   :accessor habitat-loaded-mods)
   (cmd-line-args  :initform () :initarg :cmd-line-args :accessor %habitat-cmd-line-args)
   (search-paths   :initform (make-py-list-from-list (list ".")) :accessor habitat-search-paths))
  (:documentation "Python execution context"))

(defgeneric habitat-stdin (habitat))
(defmethod habitat-stdin ((x habitat))
  (or (%habitat-stdin x) *standard-input*))

(defgeneric (setf habitat-stdin) (val habitat))
(defmethod (setf habitat-stdin) (val (x habitat))
  (setf (%habitat-stdin x) val))

(defgeneric habitat-stdout (habitat))
(defmethod habitat-stdout ((x habitat))
  (or (%habitat-stdout x) *standard-output*))

(defgeneric (setf habitat-stdout) (val habitat))
(defmethod (setf habitat-stdout) (val (x habitat))
  (setf (%habitat-stdout x) val))

(defgeneric (setf habitat-stderr) (val habitat))
(defgeneric habitat-stderr (habitat))
(defmethod habitat-stderr ((x habitat))
  (or (%habitat-stderr x) *error-output*))

(defmethod (setf habitat-stderr) (val (x habitat))
  (setf (%habitat-stderr x) val))

(defgeneric (setf habitat-cmd-line-args) (val habitat))
(defmethod (setf habitat-cmd-line-args) (val (x habitat))
  (setf (%habitat-cmd-line-args x)
    (typecase val
      (string (py-string.split val " "))
      (list (make-py-list-from-list val))
      (t val))))
(defgeneric habitat-cmd-line-args (habitat))
(defmethod habitat-cmd-line-args ((x habitat))
  (or (%habitat-cmd-line-args x)
      (make-py-list-from-list '("???.py"))))

(defun make-habitat (&rest options)
  (apply #'make-instance 'habitat options))

(defun get-loaded-module (&key src-pathname bin-pathname
                               src-file-write-date bin-file-write-date
                               habitat)
  (check-type habitat habitat)
  (loop named search
      for m in (habitat-loaded-mods habitat)
      when (and (or (null src-pathname)
                    (pathname-considered-equal (module-src-pathname m) src-pathname))
                (or (null bin-pathname)
                    (pathname-considered-equal (module-bin-pathname m) bin-pathname))
                (or (null src-file-write-date)
                    (= (module-src-file-write-date m) src-file-write-date))
                (or (null bin-file-write-date)
                    (= (module-bin-file-write-date m) bin-file-write-date)))
      return m))

(defun add-loaded-module (module habitat)
  (check-type module module)
  (check-type habitat habitat)
  (remove-loaded-module module habitat)
  (push module (habitat-loaded-mods habitat))
  (setf (gethash (module-name module) (builtin-module-attribute 'sys "modules")) module))

(defun remove-loaded-module (module habitat)
  (setf (habitat-loaded-mods habitat)
    (remove (module-bin-pathname module) (habitat-loaded-mods habitat)
            :key #'module-bin-pathname
            :test #'pathname-considered-equal))
  (remhash (module-name module) (builtin-module-attribute 'sys "modules")))

(defun pathname-considered-equal (x y)
  (check-type x pathname)
  (check-type y pathname)
  (if (and (probe-file x)
           (probe-file y))
      (equal (truename x) (truename y))
    (string-equal (namestring x) (namestring y))))
