(defpackage :clpython.module.string
  (:use :clpython :common-lisp)
  (:export "ascii_letters" "ascii_lowercase" "ascii_uppercase" "digits"
           "hexdigits" "octdigits" "lowercase" "uppercase" "letters"
           "punctuation" "whitespace" "printable"))
  
(in-package :clpython.module.string)

#.(progn
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
          char-arr)))

    (compile 'char-range)
    (compile 'chars-satisfying)
    
    nil)

(defconstant |ascii_lowercase| #.(char-range #\a #\z))
(defconstant |ascii_uppercase| #.(char-range #\A #\Z))
(defconstant |ascii_letters| (concatenate 'string
                               |ascii_lowercase|
                               |ascii_uppercase|))
(set-impl-status '(|ascii_letters| |ascii_lowercase| |ascii_uppercase|) t)

(defconstant |digits| #.(char-range #\0 #\9))
(defconstant |hexdigits| #.(concatenate 'string
                             (char-range #\0 #\9)
                             (char-range #\a #\f)
                             (char-range #\A #\F)))
(defconstant |octdigits| #.(char-range #\0 #\7))
(set-impl-status '(|digits| |hexdigits| |octdigits|) t)

(defconstant |lowercase| #.(chars-satisfying #'lower-case-p))
(defconstant |uppercase| #.(chars-satisfying #'upper-case-p))
(defconstant |letters| (concatenate 'string |lowercase| |uppercase|))
(set-impl-status '(|lowercase| |uppercase| |letters|) t)

(defconstant |punctuation| "'!\"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~'")
(defconstant |whitespace| #.(coerce '(#\Space #\Tab #\Newline #\Return #\Page) 'string))
(defconstant |printable| (concatenate 'string |digits| |letters| |punctuation| |whitespace|))

(set-impl-status '(|punctuation| |printable|)
                 :incomplete "Value of punctuation copied from CPython; need to verify")
(set-impl-status '|whitespace| t)
