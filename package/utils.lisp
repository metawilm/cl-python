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

(defmacro with-auto-mode-recompile ((&key verbose
                                          (restart-name #+allegro 'excl::recompile-due-to-incompatible-fasl #-allegro nil))
                                    &body body)
  "Automatically recompile on ANSI/Modern conflicts."
  (declare (ignorable restart-name))
  (assert (atom verbose))
  `(handler-bind
       (#+allegro
        ((or excl::file-incompatible-fasl-error
	     excl::fasl-casemode-mismatch)
	 (lambda (c)
	   (declare (ignore c))
	   (let ((r (and ',restart-name (find-restart ',restart-name))))
	     (when r
	       ,@(when verbose
		   `((format t "; Recompiling ~A due to incompatible fasl"
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
  (:documentation "Returns file/stream contents as string.
The element type can be CHARACTER or (UNSIGNED-BYTE <=8).")
  (:method ((fname string))
           (slurp-file (pathname fname)))
  (:method ((fname pathname))
           #+allegro
           (excl:file-contents fname)
           #-allegro
           (with-open-file (f fname :direction :input :element-type '(unsigned-byte 8))
             (slurp-file f)))
  (:method ((s stream))
	   (let ((vec (make-array (file-length s) :element-type (stream-element-type s))))
	     (read-sequence vec s)
             (cond ((eq (stream-element-type s) 'character)
                    vec)
                   ((subtypep (stream-element-type s) '(unsigned-byte 8))
                    (map 'string #'code-char vec))
                   (t (break "Unexpected file element type: expected CHARACTER or (UNSIGNED-BYTE <=8), got: ~A."
                             (stream-element-type s)))))))

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
  #+sbcl `(handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
            ,@body)
  #-(or allegro sbcl) `(progn ,@body))

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

(defmacro ltv-find-class (clsname)
  ;; As the order of executing load-time-value forms relative to the order
  ;; of loading toplevel forms is explicitly undefined, need to be careful.
  `(or (load-time-value (find-class ,clsname nil))
       (let ((.cache (load-time-value (cons nil nil))))
         (or (car .cache)
             (setf (car .cache) (find-class ,clsname))))))

(defun alist-remove-prop (alist attr)
  "Removes first occurrence, if any. Returns NEW-LIST, FOUNDP."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (if (eq (caar alist) attr)
      (values (cdr alist) t)
    (loop for cons on alist
        when (eq (car (second cons)) attr)
        do (setf (cdr cons) (cddr cons))
           (return-from alist-remove-prop (values alist t))
        finally (return (values alist nil)))))


;;; Alist versus hashtable performance for symbol keys

(defconstant +dict-alist-to-hashtable-threshold+ 100
  "When #items <= threashold, represent attributes as alist, otherwise switch to eq hash table.
Value is somewhat arbitrary, as relative performance depends on how many lookups fail.
See function ALIST-VS-HT.")

#+(or)
(defun alist-vs-ht ()
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (macrolet ((with-measure-passed-time (msg &body body)
               `(let* ((.msg ,msg)
                       (.start (get-internal-run-time)))
                  ,@body
                  (let ((.stop (get-internal-run-time)))
                    (format t "  ~3A=~4D " .msg (- .stop .start))))))
    (loop with iter-repeat = 100000
        with fail-factor = 10 ;; only 1 in FAIL-FACTOR lookups succeeds
        for num-items from 0 to 30
        do (let ((alist (loop for i from 0 below num-items collect (cons i (1+ i))))
                 (hash-table (let ((ht (make-hash-table :test 'eq)))
                               (loop for i from 0 below num-items do
                                     (setf (gethash i ht) (1+ i)))
                               ht))
                 (keys (loop repeat (* 10 num-items) collect (random (* fail-factor num-items)))))
             (dolist (key keys)
               (when (< key num-items)
                 (assert (eq (cdr (assoc key alist :test 'eq)) (1+ key)))
                 (assert (eq (gethash key hash-table) (1+ key)))))
             (format t "~%Num-items = ~3D: " num-items)
             (with-measure-passed-time "alist"
               (dotimes (i iter-repeat)
                 (dolist (key keys)
                   (cdr (assoc key alist :test 'eq)))))
             (with-measure-passed-time "ht"
               (dotimes (i iter-repeat)
                 (dolist (key keys)
                   (gethash key hash-table))))))))

;;; Weak-keys hash table

(defun make-weak-key-hash-table (&rest options)
  #+allegro (apply #'make-hash-table :weak-keys t options)
  #-allegro (apply #'make-hash-table options))

(defun schedule-finalization (object function)
  "Schedule FUNCTION to be run when OBJECT is garbage collected."
  (declare (ignorable object function))
  #+allegro (excl:schedule-finalization object function)
  #-allegro (warn "Don't know how to SCHEDULE-FINALIZATION in this lisp."))

(defun unschedule-finalization (sf)
  "Removes a finalization function. SF is a value returned by SCHEDULE-FINALIZATION."
  (declare (ignorable sf))
  #+allegro (excl:unschedule-finalization sf)
  #-allegro (warn "Don't know how to UNSCHEDULE-FINALIZATION in this lisp."))

(defun quit (&optional code)
  ;; Adapted from Rob Warnock's post "How to programmatically exit?"
  ;;  http://groups.google.nl/group/comp.lang.lisp/msg/94c9a579608dcd9a
  (declare (ignorable code))
  #+allegro (excl:exit code :quiet t) ;; added (:quiet t) -WB
  #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
  #+cmu (ext:quit code)
  #+cormanlisp (win32:exitprocess code)
  #+gcl (lisp:bye code)                 ; XXX Or is it LISP::QUIT?
  #+lispworks (lw:quit :status code)
  #+lucid (lcl:quit code)
  #+sbcl (sb-ext:quit
          :unix-status (typecase code (number code)
                                 (null 0)
                                 (t 1)))
  #+kcl (lisp::bye)                     ; XXX Does this take an arg?
  #+scl (ext:quit code)                 ; XXX Pretty sure this *does*.
  #+(or openmcl mcl) (ccl::quit)
  #+abcl (cl-user::quit)
  #+ecl (si:quit)
  #+poplog (poplog::bye)                ; XXX Does this take an arg?
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl
        kcl scl openmcl mcl abcl ecl poplog)
  (error "QUIT not-implemented in this implementation"))

(defun abbreviate-to-one-line (string)
  (whereas ((ix (position #\Newline string)))
    (setf string (concatenate 'string (subseq string 0 ix) "...")))
  string)

(defun register-feature (feature present-p)
  (check-type feature keyword)
  (if present-p
      (pushnew feature *features*)
    (setf *features* (remove feature *features*))))

(defmacro with-gensyms (list &body body)
  `(let ,(loop for x in list
	     collect `(,x (gensym ,(symbol-name x))))
     ,@body))
