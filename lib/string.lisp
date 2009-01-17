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

(defun |maketrans| (from to)
  (check-type from string)
  (check-type to string)
  (unless (= (length from) (length to))
    (py-raise '{ValueError} "Lengths not the same: ~A vs ~A." (length from) (length to)))
  
  (loop with conv = (copy-seq #.(coerce (loop for i from 0 to 255 collect (code-char i)) 'string))
      for from-char across from
      for from-code = (char-code from-char)
      for to-char across to
      for to-code = (char-code to-char)
      do (if (or (> from-code 255)
                 (> to-code 255))
             (py-raise '{ValueError}
                       "Translation character with code > 255: ~S ~S." from-char to-char)
           (setf (aref conv from-code) to-char))
      finally (return conv)))

(defun |translate| (string table &optional delete-chars)
  "Delete chars in DELETE-CHARS; TABLE is trans of 256 -> 256. If TABLE is None, then only delete chars."
  (check-type string string)
  (check-type table (or string clpython::py-none))
  (flet ((calc-filter ()
           (when delete-chars
             (let ((filter (make-array 256 :initial-element t)))
               (prog1 filter
                 (flet ((mapper (ch)
                          (let ((code (char-code ch)))
                            (if (<= code 255)
                                (setf (svref filter code) nil)
                              (py-raise '{ValueError} "Character ~S in delete-chars has code > 255." ch)))))
                   (declare (dynamic-extent #'mapper))
                   (unless (typep delete-chars '(or vector list))
                     (setf delete-chars (py-iterate->lisp-list delete-chars)))
                   (map nil #'mapper delete-chars)))))))
    (loop with filter = (calc-filter)
        with res = (make-array (length string)
                               :element-type 'character
                               :fill-pointer (when delete-chars 0))
        with dest-i = 0
        for ch across string
        for ch.code = (char-code ch)
        do (cond ((> ch.code 255) (py-raise '{ValueError} "Char ~S has code > 255." ch))
                 ((and filter (null (svref filter ch.code)))) ;; skip
                 (t (let ((repl (if (clpython::none-p table)
                                    ch
                                  (aref table ch.code))))
                      (setf (aref res dest-i) repl)
                      (incf dest-i))))
          finally (when delete-chars (setf (fill-pointer res) dest-i))
                  (return res))))
