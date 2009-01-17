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
   (cmd-line-args  :initform () :initarg :cmd-line-args :accessor habitat-cmd-line-args)
   (search-paths   :initform (make-py-list-from-list (list ".")) :accessor habitat-search-paths))
  (:documentation "Python execution context"))

(defmethod habitat-stdin ((x habitat))
  (or (%habitat-stdin x) *standard-input*))

(defmethod (setf habitat-stdin) (val (x habitat))
  (setf (%habitat-stdin x) val))

(defmethod habitat-stdout ((x habitat))
  (or (%habitat-stdout x) *standard-output*))

(defmethod (setf habitat-stdout) (val (x habitat))
  (setf (%habitat-stdout x) val))

(defmethod habitat-stderr ((x habitat))
  (or (%habitat-stderr x) *error-output*))

(defmethod (setf habitat-stderr) (val (x habitat))
  (setf (%habitat-stderr x) val))

(defun make-habitat (&rest options)
  (apply #'make-instance 'habitat options))

(defun get-loaded-module (&key src-pathname bin-pathname
                               src-file-write-date bin-file-write-date
                               habitat)
  (check-type habitat habitat)
  (loop for m in (habitat-loaded-mods habitat)
      when (and (or (null src-pathname)
                    (progn (check-type src-pathname pathname)
                           (equal (module-src-pathname m) src-pathname)))
                (or (null bin-pathname)
                    (progn (check-type bin-pathname pathname)
                           (equal (module-bin-pathname m) bin-pathname)))
                (or (null src-file-write-date)
                    (= (module-src-file-write-date m) src-file-write-date))
                (or (null bin-file-write-date)
                    (= (module-bin-file-write-date m) bin-file-write-date)))
      return m))

(defun add-loaded-module (module habitat)
  (check-type module module)
  (check-type habitat habitat)
  ;; In general, don't remove the old module with same pathnames:
  ;; when re-import goes wrong, the new failed module is popped,
  ;; so the old one is available again.
  (remove-loaded-module :src-pathname (module-src-pathname module)
                        :bin-pathname (module-bin-pathname module)
                        :habitat habitat)
  (push module (habitat-loaded-mods habitat))
  (setf (gethash (module-name module) (builtin-module-attribute 'sys "modules")) module))

(defun remove-loaded-module (&rest args &key habitat &allow-other-keys)
  (whereas ((m (apply #'get-loaded-module args)))
    (setf (habitat-loaded-mods habitat)
      (remove m (habitat-loaded-mods habitat)))
    (remhash (module-name m) (builtin-module-attribute 'sys "modules"))))
