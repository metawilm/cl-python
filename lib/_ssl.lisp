;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE._SSL; Readtable: PY-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.module._ssl)
(in-syntax *user-readtable*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; FIXME CPL incorrect
  (clpython:define-exception-subclass '|SSLError| '{Exception}))

(defconstant-once |CERT_NONE| 0)
(defconstant-once |CERT_OPTIONAL| 1)
(defconstant-once |CERT_REQUIRED| 2)

(defconstant-once |PROTOCOL_SSLv2| 0)
(defconstant-once |PROTOCOL_SSLv3| 1)
(defconstant-once |PROTOCOL_SSLv23| 2)
(defconstant-once |PROTOCOL_TLSv1| 3)

(defconstant-once |SSL_ERROR_SSL| 1)
(defconstant-once |SSL_ERROR_WANT_READ| 2)
(defconstant-once |SSL_ERROR_WANT_WRITE| 3)
(defconstant-once |SSL_ERROR_WANT_X509_LOOKUP| 4)
(defconstant-once |SSL_ERROR_SYSCALL| 5)
(defconstant-once |SSL_ERROR_ZERO_RETURN| 6)
(defconstant-once |SSL_ERROR_WANT_CONNECT| 7)
(defconstant-once |SSL_ERROR_EOF| 8)
(defconstant-once |SSL_ERROR_INVALID_ERROR_CODE| 9)

(defun |RAND_status| ()
  (error "TODO: _ssl.RAND_status"))

(defun |RAND_egd| (path)
  (declare (ignore path))
  (error "TODO: _ssl.RAND_egd"))

(defun |RAND_add| (string entropy)
  (declare (ignore string entropy))
  (error "TODO: _ssl.RAND_add"))
