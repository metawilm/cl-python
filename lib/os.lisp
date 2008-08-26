;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE.OS -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.module.os)

;;; OS

(defvar |name| "common lisp")
(defvar |error| (find-class '{OSError}))
(set-impl-status '(|name| |error|) t)

(set-impl-status '(|path|) :todo)

(in-package :clpython.module.os.process)

(in-package :clpython.module.os.file)

(do-external-symbols (s :clpython.module.os)
  (unless (clpython::impl-status s)
    (set-impl-status s :todo)))

