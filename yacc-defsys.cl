;; -*- mode: common-lisp; package: user -*-
;;
;; regexp.cl -[Fri Aug 27 08:25:18 2004 by smh]-
;;
;; copyright (c) 2003-2004 Franz Inc, Oakland, CA - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.
;;

(eval-when (compile)
  (error "This defsys file should be loded interpreted, not compiled."))

(in-package :user)

(defpackage :yacc (:use :cl :excl))

(excl:defsystem :yacc
    (:default-pathname #.*load-pathname*)
;;;;**** NOTE: ****
;;;;  THIS SYSTEM IS DUPLICATED IN THE ACL SOURCE CODE.  ANY CHANGES TO IT
;;;;  MUST BE COORDINATED WITH THAT MODULE.  THANKS, and sorry for yelling.
  (:serial
   "yacc-defs"
   "yacc-compile"
   "yacc-runtime"))

(format t "~%;;To compile and load, execute these forms:~%~s~%~s~%~s~%"
	'(excl:compile-system :yacc)
	'(excl:load-system    :yacc)
	'(excl:concatenate-system :yacc "yacc.fasl"))
