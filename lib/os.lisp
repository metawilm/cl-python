;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE.OS; Readtable: PY-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.module.os)
(in-syntax *user-readtable*)
;;; OS

(defvar |name| "common lisp")
(defvar |error| (find-class '{OSError}))

(defun |urandom| (n)
  "Return string of N random bytes"
  (let ((s (make-string n)))
    (dotimes (i n)
      (setf (aref s i) (code-char (random 256))))
    s))

#+allegro
(defvar |environ| (loop with d = (clpython::make-py-hash-table)
                      for (k . v) in (excl.osi:environment)
                      do (setf (gethash k d) v)
                      finally (return d)))

(set-impl-status '(|name| |error| |urandom| #+allegro |environ|) t)

(defun |listdir| (path)
  ;; Not sure this is correct
  (check-type path string)
  (let ((last (and (plusp (length path))
                   (aref path (1- (length path))))))
    (unless (member last '(#\\ #\/))
      (setf path (concatenate 'string path "/"))))
  (clpython::make-py-list-from-list (mapcar 'namestring (directory path))))

(set-impl-status '(|listdir|) t)

(do-external-symbols (s :clpython.module.os)
  (unless (clpython::impl-status s)
    (set-impl-status s :todo)))

