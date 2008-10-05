;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.PACKAGE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.package)

(defun py-raise (exc-type string &rest format-args)
  "Raise a Python exception with given format string"
  (error exc-type :args (cons string format-args)))

(defmacro with-auto-mode-recompile ((&key verbose) &body body)
  "Automatically recompile on ANSI/Modern conflicts."
  (assert (atom verbose))
  `(handler-bind
       (#+allegro
        ((or excl::file-incompatible-fasl-error
	     excl::fasl-casemode-mismatch)
	 (lambda (c)
	   (declare (ignore c))
	   (let ((r (find-restart 'excl::recompile-due-to-incompatible-fasl)))
	     (when r
	       ,@(when verbose
		   `((format t "Recompiling ~A due to incompatible fasl..."
			     (or *load-pathname* *compile-file-pathname* "file"))))
	       (invoke-restart r))))))
     ,@body))

;;; To have Emacs properly indent the DEF-PY-METHOD form, add to .emacs:
;;;  (put 'when-let 'fi:lisp-indent-hook (get 'when 'fi:lisp-indent-hook))

(defmacro whereas (bindings &body body)
  "A simple version of Erik Naggum's idea in
http://groups.google.nl/group/comp.lang.lisp/msg/bc7772aa5ab1f3e4"
  (let ((res `(locally ,@body))) ;; allow declarations
    (dolist (b (reverse bindings) res)
      (unless (and (= (length b) 2)
		   (symbolp (car b)))
	(error "invalid binding: ~A" b))
      (destructuring-bind (k v) b
	(setf res `(let ((,k ,v))
		     (when ,k
                         ,res)))))))

(defun sans (plist &rest keys)
  "By Erik Naggum,
http://groups.google.nl/group/comp.lang.lisp/msg/2520fe9bc7749328?dmode=source"
  (let ((sans ()))
    (loop
      (let ((tail (nth-value 2 (get-properties plist keys))))
        ;; this is how it ends
        (unless tail
          (return (nreconc sans plist)))
        ;; copy all the unmatched keys
        (loop until (eq plist tail) do
              (push (pop plist) sans)
              (push (pop plist) sans))
        ;; skip the matched key
        (setq plist (cddr plist))))))

(defgeneric slurp-file (file)
  (:documentation "Returns file/stream contents as string")
  (:method ((fname string))
           (slurp-file (pathname fname)))
  (:method ((fname pathname))
           #+allegro
           (excl:file-contents fname)
           #-allegro
           (with-open-file (f fname :direction :input :element-type '(unsigned-byte 8))
             (slurp-file f)))
  (:method ((s stream))
	   (let ((vec (make-array (file-length s) :element-type '(unsigned-byte 8))))
	     (read-sequence vec s)
	     (map 'string #'code-char vec))))

(defmacro named-function (name lambda-form)
  (declare (ignorable name))
  (assert (eq (car lambda-form) 'lambda))
  #+allegro `(excl:named-function ,name ,lambda-form)
  #+sbcl `(sb-int:named-lambda ,name ,@(cdr lambda-form)) ;; skip 'lambda symbol
  #-(or allegro sbcl) lambda-form)

(defmacro with-stack-list ((name &rest items) &body body)
  (check-type name symbol)
  #+allegro
  `(excl:with-stack-list (,name ,@items)
     ,@body)
  #-allegro
  `(let ((,name (list ,@items)))
     (declare (dynamic-extent ,name))
     ,@body))

(defmacro without-redefinition-warnings (&body body)
  #+allegro `(excl:without-redefinition-warnings ,@body)
  #-allegro `(progn ,@body))

(defmacro fast (&body body)
  `(locally (declare (optimize (speed 3)))
     ,@body))

(defmacro defconstant-once (name &rest args)
  (check-type name symbol)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (boundp ',name)
       (defconstant ,name ,@args))))


(defconstant-once +max-char-code+
    ;; On Allegro CHAR-CODE-LIMIT is the largest value across all
    ;; implementations, while EXCL:REAL-CHAR-CODE-LIMIT. is "a better estimate".
    ;; http://www.franz.com/support/documentation/8.1/doc/variables/excl/real-char-code-limit.htm
    #+allegro (progn (assert (<= excl:real-char-code-limit char-code-limit))
                     excl:real-char-code-limit)
    #-allegro char-code-limit
    "Like CHAR-CODE-LIMIT, but possible lower.")

(deftype char-code-type ()
  "CHAR-CODE return value type"
  '(integer 0 (#.+max-char-code+)))