;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE.POSIX; Readtable: PY-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.module.posix)
(in-syntax *user-readtable*)

;; #:|O_RDONLY| #:|O_WRONLY| #:|O_RDWR| #:|O_APPEND| #:|O_CREAT| #:|O_EXCL|
;; #:|O_TRUNC| #:|O_DSYNC| #:|O_RSYNC| #:|O_SYNC| #:|O_NDELAY| #:|O_NONBLOCK|
;; #:|O_NOCTTY| #:|O_SLOCK| #:|O_EXLOCK| #:|O_BINARY| #:|O_NOINHERIT|
;; #:|O_TEMPORARY| #:|O_RANDOM| #:|O_SEQUENTIAL| #:|O_TEXT| #:|SEEK_SET|
;; #:|SEEK_CUR| #:|SEEK_END|

;; Constants taken from CPython posix module on Mac OS X, check relevance/use.
(defconstant |O_RDONLY| 0)
(defconstant |O_WRONLY| 1)
(defconstant |O_RDWR| 2)
(defconstant |O_NONBLOCK| 4)
(defconstant |O_NDELAY| 4)
(defconstant |O_APPEND| 8)
(defconstant |O_SHLOCK| 16)
(defconstant |O_EXLOCK| 32)
(defconstant |O_SYNC| 128)
(defconstant |O_NOFOLLOW| 256)
(defconstant |O_CREAT| 512)
(defconstant |O_TRUNC| 1024)
(defconstant |O_EXCL| 2048)
(defconstant |O_NOCTTY| 131072)
(defconstant |O_DIRECTORY| 1048576)

(defun |open| (name flags &optional mode)
  (when flags
    (warn "posix.open(): ignoring flags: ~A" flags))
  (py-call (find-class 'clpython::py-file) name (or mode "r")))

(defun |read| (file n)
  "Read at most N bytes, returned as string. Empty string for EOF"
  (funcall 'clpython::py-file.read file n))

(defun |close| (file)
  (funcall 'clpython::py-file.close file))

(defun |remove| (path)
  (|unlink| path))

(defun |unlink| (path)
  (declare (ignore path))
  (break "TODO: posix.unlink(path)"))

(defconstant-once |error| (find-class '{OSError}))

;;; Stat
;; TODO support stat_float_times()

(defclass stat-result (clpython::object)
  ;; todo make named-tuple?
  ((stat :accessor sr-stat :initarg :stat))
  (:metaclass clpython::py-type))

(defun |stat| (path)
  (declare (ignorable path))
  (let ((stat #+allegro (handler-case (excl.osi:stat path)
                          (excl.osi:syscall-error (c)
                            (py-raise '{OSError} (format nil "~A" c))))
              #-allegro (error "TODO")))
    (make-instance 'stat-result :stat stat)))

(def-py-method stat-result.st_mode :attribute (x)
  #+allegro (excl.osi:stat-mode (sr-stat x))
  #-allegro (break "TODO"))

(def-py-method stat-result.st_ino :attribute (x)
  #+allegro (excl.osi:stat-ino (sr-stat x))
  #-allegro (break "TODO"))

(def-py-method stat-result.st_dev :attribute (x)
  #+allegro (excl.osi:stat-dev (sr-stat x))
  #-allegro (break "TODO"))

(def-py-method stat-result.st_nlink :attribute (x)
  #+allegro (excl.osi:stat-nlink (sr-stat x))
  #-allegro (break "TODO"))

(def-py-method stat-result.st_uid :attribute (x)
  #+allegro (excl.osi:stat-uid (sr-stat x))
  #-allegro (break "TODO"))

(def-py-method stat-result.st_gid :attribute (x)
  #+allegro (excl.osi:stat-gid (sr-stat x))
  #-allegro (break "TODO"))

(def-py-method stat-result.st_size :attribute (x)
  #+allegro (excl.osi:stat-size (sr-stat x))
  #-allegro (break "TODO"))

(def-py-method stat-result.st_atime :attribute (x)
  #+allegro (excl.osi:stat-atime (sr-stat x))
  #-allegro (break "TODO"))

(def-py-method stat-result.st_mtime :attribute (x)
  #+allegro (excl.osi:stat-mtime (sr-stat x))
  #-allegro (break "TODO"))

(def-py-method stat-result.st_ctime :attribute (x)
  #+allegro (excl.osi:stat-ctime (sr-stat x))
  #-allegro (break "TODO"))
