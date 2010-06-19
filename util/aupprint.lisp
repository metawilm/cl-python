;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.PACKAGE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; A pretty printer, that prints symbol `x' in the `clpython.ast' and
;;;; `clpython.user' packages as [x] and {x}, respectively.
;;;; It is the reverse of the `readtable.lisp'.

(in-package :clpython.package)

(defvar *ast-user-pprint-dispatch* (copy-pprint-dispatch nil))
(defvar *ast-user-print-delims*)

(defun ast-user-print-symbol (stream s)
  (check-type s symbol)
  (when *print-pretty*
    (loop for (pkg pre post) in (load-time-value `((,(find-package :clpython.ast)  #\[ #\])
                                                   (,(find-package :clpython.user) #\{ #\})))
        when (and s ;; prevent nil being printed as [nil]
                  (eq s (find-symbol (symbol-name s) pkg)))
        do (when *ast-user-print-delims* (write-char pre stream))
           (write-string (symbol-name s) stream)
           (when *ast-user-print-delims* (write-char post stream))
           (return-from ast-user-print-symbol)))
  (with-standard-io-syntax (format stream "~S" s)))

(defun dummy (stream x)
  (format stream "dummy ~A" x))

(set-pprint-dispatch 'symbol 'ast-user-print-symbol 0 *ast-user-pprint-dispatch*)

(defmacro with-ast-user-pprinter ((&key (print-delims t)) &body body)
  `(let ((*print-pprint-dispatch* *ast-user-pprint-dispatch*)
         (*ast-user-print-delims* ,print-delims))
     ,@body))
