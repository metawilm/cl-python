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
       (((or excl::file-incompatible-fasl-error
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
           #+allegro
           (excl:file-contents fname)
           #-allegro
           (with-open-file (f fname :direction :input :element-type '(unsigned-byte 8))
             (slurp-file f)))
  (:method ((s stream))
	   (let ((vec (make-array (file-length s) :element-type '(unsigned-byte 8))))
	     (read-sequence vec s)
	     (map 'string #'code-char vec))))
