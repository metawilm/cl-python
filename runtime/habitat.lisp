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
   (search-paths   :initform (make-py-list-from-list (list ".")) :accessor habitat-search-paths)
   (module-globals :initform (make-eq-hash-table) :reader habitat-module-globals))
  (:documentation "Python execution context"))

(defmethod print-object ((habitat habitat) stream)
  (with-slots (stdin stdout stderr loaded-mods cmd-line-args search-paths)
    habitat
    (print-unreadable-object (habitat stream :type t :identity t)
      (format stream "stdin=~A stdout=~A stderr=~A #loaded-modules=~A cmd-line-args=~A search-paths=~S"
              stdin stdout stderr (length loaded-mods) cmd-line-args search-paths))))

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
  (let ((old-val (%habitat-cmd-line-args x))
        (new-val (typecase val
                   (string (py-string.split val " "))
                   (list (make-py-list-from-list val))
                   (t val))))
    (unless (equalp old-val new-val)
      (warn "Changing habitat command-line-args for ~A~%from:~%  ~S~%to:~%  ~S"
            x old-val new-val))
    (setf (%habitat-cmd-line-args x) new-val)))
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

(defun get-sys.modules ()
  (or (builtin-module-attribute 'sys "modules")
      (error "Cannot access builtin module 'sys' or its field 'sys.modules'.")))

(defun add-loaded-module (module habitat)
  (check-type module module)
  (check-type habitat habitat)
  (remove-loaded-module module habitat)
  (push module (habitat-loaded-mods habitat))
  (with-py-dict
      (setf (gethash (module-name module) (get-sys.modules)) module)))

(defun remove-loaded-module (module habitat)
  (setf (habitat-loaded-mods habitat)
    (remove (module-bin-pathname module) (habitat-loaded-mods habitat)
            :key #'module-bin-pathname
            :test #'pathname-considered-equal))
  (with-py-dict
      (remhash (module-name module) (get-sys.modules))))

(defun pathname-considered-equal (x y)
  (check-type x pathname)
  (check-type y pathname)
  (if (and (careful-probe-file x)
           (careful-probe-file y))
      (equal (truename x) (truename y))
    (string-equal (namestring x) (namestring y))))
