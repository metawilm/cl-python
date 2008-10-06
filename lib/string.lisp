;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE.STRING -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.module.string)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun char-range (from to)
    "Bounds are inclusive."
    (check-type from character)
    (check-type to character)
    (let* ((min (char-code from))
           (max (char-code to))
           (arr (make-array (1+ (- max min)) :element-type 'character)))
      (loop for i from min to max
          for arr-i from 0
          do (setf (schar arr arr-i) (code-char i)))
      arr))
  
  (defun chars-satisfying (pred)
    ;; Allegro does stack allocation if size is a literal number, but not if
    ;; it is a constant number; therefore #. of char-code-limit. Also,
    ;; stack-allocated bit vectors can't have an initializer.
    (let ((bit-arr (make-array +max-char-code+
                               :element-type 'bit
                               :initial-element 0)))
      (declare (dynamic-extent bit-arr))
      (let* ((num-chars (loop with num-chars = 0
                            for i from 0 below +max-char-code+
                            for ch = (code-char i)
                            when (and ch ;; CHAR-CODE-LIMIT could be > actual num chars
                                      (funcall pred ch))
                            do (setf (sbit bit-arr i) 1)
                               (incf num-chars)
                            finally (return num-chars)))
             (char-arr (make-array num-chars :element-type 'character)))
        (loop for i fixnum from 0
            with res-i fixnum = 0
            when (= (sbit bit-arr i) 1)
            do (setf (schar char-arr res-i) (code-char i))
               (incf res-i)
               (when (= res-i num-chars)
                 (return)))
        char-arr))))

(defconstant-once |ascii_lowercase| #.(char-range #\a #\z))
(defconstant-once |ascii_uppercase| #.(char-range #\A #\Z))
(defconstant-once |ascii_letters| (concatenate 'string
                                    |ascii_lowercase|
                                    |ascii_uppercase|))
(set-impl-status '(|ascii_letters| |ascii_lowercase| |ascii_uppercase|) t)

(defconstant-once |digits| #.(char-range #\0 #\9))
(defconstant-once |hexdigits| #.(concatenate 'string
                                  |digits|
                                  (char-range #\a #\f)
                                  (char-range #\A #\F)))
(defconstant-once |octdigits| #.(char-range #\0 #\7))
(set-impl-status '(|digits| |hexdigits| |octdigits|) t)

(defconstant-once |lowercase| #.(chars-satisfying #'lower-case-p))
(defconstant-once |uppercase| #.(chars-satisfying #'upper-case-p))
(defconstant-once |letters| (concatenate 'string |lowercase| |uppercase|))
(set-impl-status '(|lowercase| |uppercase| |letters|) t
                 "Note that values differ from CPython values")

(defconstant-once |punctuation| "!\"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~")
(defconstant-once |whitespace|
    #.(coerce '(#\Space #\Tab #\Newline #\Return #\Page) 'string))
(defconstant-once |printable|
    (concatenate 'string |digits| |letters| |punctuation| |whitespace|))

(set-impl-status '|punctuation| t "Value copied from CPython.")
(set-impl-status '(|printable| |whitespace|) t)
