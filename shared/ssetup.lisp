;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON; Readtable: PY-USER-READTABLE -*-
;; 
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; Functionality shared across the parser, compiler, runtime components

;;; Parser syntax error handling
;;
;; The parser may encounter syntax errors. If the core of CLPython is
;; loaded, a SyntaxError is thrown, otherwise a regular ERROR.
;; Same for EOF error.

(in-package :clpython)

;;; Allegro's Source level debugging

(defparameter *source-level-debugging*
  #+ (and allegro allegro-version>= (version>= 8 2)) nil ;; disable while in development
  #- (and allegro allegro-version>= (version>= 8 2)) nil)

(register-feature :clpython-source-level-debugging *source-level-debugging*)

