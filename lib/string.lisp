(defpackage :clpython.module.string
  (:use :clpython :common-lisp)
  (:export "ascii_letters" "ascii_lowercase" "ascii_uppercase" "digits"
           "hexdigits" "octdigits" "lowercase" "uppercase" "letters"
           "punctuation" "whitespace" "printable")
  (:import-from :clpython.package #:defconstant-once))
  
(in-package :clpython.module.string)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun char-range (from to)
    (declare (optimize (speed 3)))
    (let ((f (lambda (ch) (<= (the fixnum (char-code from))
                              (the fixnum (char-code ch))
                              (the fixnum (char-code to))))))
      (declare (dynamic-extent f))
      (chars-satisfying f)))
  
  (defun chars-satisfying (pred)
    (declare (optimize (speed 3)))
    ;; Allegro does stack allocation if size is a literal number, but not if
    ;; it is a constant number; therefore #. of char-code-limit. Also,
    ;; stack-allocated bit vectors can't have an initializer.
    (let ((bit-arr (make-array #.char-code-limit :element-type 'bit)))
      (declare (dynamic-extent bit-arr))
      (do ((i (1- char-code-limit) (1- i)))
          ((< i 0))
        (declare (fixnum i))
        (setf (sbit bit-arr i)
          (if (funcall pred (code-char i)) 1 0)))
      (let* ((num-chars (loop for i across bit-arr sum i))
             (char-arr (make-array num-chars :element-type 'character)))
        (declare (fixnum num-chars))
        (loop for i fixnum from 0 below char-code-limit
            with ci fixnum = 0
            when (= (sbit bit-arr i) 1)
            do (setf (schar char-arr ci) (code-char i))
               (incf ci)
               (when (= ci num-chars)
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
                                  (char-range #\0 #\9)
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
(defconstant-once |whitespace| #.(coerce '(#\Space #\Tab #\Newline #\Return #\Page) 'string))
(defconstant-once |printable| (concatenate 'string |digits| |letters| |punctuation| |whitespace|))

(set-impl-status '|punctuation| t "Value copied from CPython.")
(set-impl-status '(|printable| |whitespace|) t)
