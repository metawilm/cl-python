;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.UTIL -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;; Readtable utilities

(in-package :clpython.util)

;; The readtable setup is as suggested by Kent Pitman in
;;  http://groups.google.nl/group/comp.lang.lisp/msg/d97a08bd49db2b82?dmode=source

(defmacro in-syntax (readtable-expression)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *readtable* ,readtable-expression)))

(defparameter *reader-error-has-format*
    (class-initargs-p 'reader-error :format-arguments :format-control))

(defun read-package-symbol-func (package start-char end-char &key intern)
  "Create a reader function that reads from start-char until endchar,
and returns a symbol in the given package. When no such symbol exists,
it will be interned if INTERN, otherwise an error is raised."
  (check-type start-char character)
  (check-type end-char character)
  
  (lambda (stream ch)
    (assert (char= ch start-char))
    (loop with name = (make-array 5 :element-type 'character
                                  :adjustable t :fill-pointer 0)
	for prev-char = ch
	for ch = (read-char stream)
	do (cond ((and prev-char (char= prev-char #\\))
		  (setf (aref name (1- (length name))) ch))
		 ((char/= ch end-char)
                  (vector-push-extend ch name))
                 (t 
                  (return (or (find-symbol name package)
                              (when intern
                                (intern name package))
                              (apply #'error 'reader-error :stream stream
                                     (when *reader-error-has-format*
                                       (list :format-control
                                             "No symbol named ~S in package ~S"
                                             :format-arguments
                                             (list name (find-package package))))))))))))

;;; Readtable that takes in everything available

(defparameter *omnivore-debug* nil)

(defun setup-omnivore-readmacro (&key function initial-forms (readtable *readtable*))
  "Create a readtable that, for any initial character, will let FUNCTION do the parsing.
The reader will return `(PROGN ,@INITAL-FORMS ,(FUNCALL FUNCTION STREAM)).

The STREAM supplied to FUNCTION will not be the original stream passed to the readtable
function, but rather a CONCATENATED-STREAM wrapper.

INITIAL-FORMS typically contains an IN-PACKAGE form to ensure that compilation and loading of
the fasl file satisfies CLHS 3.2.4.4 \"Additional Constraints on Externalizable Objects\"."
  (check-type function function)
  (check-type readtable readtable)
  (flet ((omnivore-read-func (stream char)
           ;; In Allegro CL, is stream is the REPL stream, unreading the first char fails.
           ;; Therefore below a concatenated-stream is created.
           ;; On SBCL a concatenated stream reports EOF too soon
           ;; but unreading the first char seems to work (https://bugs.launchpad.net/sbcl/+bug/690408)
           (let ((res `(progn ,@initial-forms
                              ,(let ((prefixed-stream
                                      #+sbcl (progn (unread-char char stream)
                                                    stream)
                                      #-sbcl (make-concatenated-stream
                                              (make-string-input-stream (string char))
                                              stream)))
                                 (funcall function prefixed-stream)))))
             (when *omnivore-debug*
               (format t "~&OMNIVORE-READ-FUNC result:~%  ~S~%" res))
             res)))
    (dotimes (i 256) ;; XXX use file encoding or char-code-limit?
      (set-macro-character (code-char i) #'omnivore-read-func t readtable)))
  readtable)

(defun concatenated-stream-active-stream (stream)
  "If STREAM is a CONCATENATED-STREAM then returns its active stream (recursively),
otherwise return STREAM."
  (loop while (typep stream 'concatenated-stream)
      do (setf stream (car (concatenated-stream-streams stream)))
      finally (return stream)))

(defun interactive-stream-p-recursive (stream)
  "In case of CONCATENATED-STREAM it looks at the active stream within."
  (interactive-stream-p (concatenated-stream-active-stream stream)))
    
