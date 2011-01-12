;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.UTIL -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.util)

(defmacro with-auto-mode-recompile
    ((&key (restart-name #+allegro 'excl::recompile-due-to-incompatible-fasl #-allegro nil)
           filename)
     &body body)
  "Automatically recompile when FASL is apparently intended for another implementation"
  (declare (ignorable restart-name))
  `(flet ((.invoke-recompile-restart (&optional c)
            (declare (ignore c))
            ,(when restart-name
               `(progn 
                  (whereas ((r (find-restart ',restart-name)))
                    (format t "~&; Recompiling ~A due to wrong fasl format.~%"
                            (or ,filename *load-pathname* *compile-file-truename* "fasl file"))
                    (invoke-restart r))
                  (break "No expected restart named ~A" ',restart-name)))))
     
     (handler-bind (#+allegro
                    ((or excl::file-incompatible-fasl-error excl::fasl-casemode-mismatch)
                     #'.invoke-recompile-restart)
                    #+ccl
                    (ccl::simple-reader-error
                     (lambda (c)
                       (declare (ignore c))
                       (when (and *load-truename*
                                  (string-equal (pathname-type *load-truename*) "fasl"))
                         (.invoke-recompile-restart))))
                    #+sbcl
                    (sb-fasl::fasl-header-missing
                     #'.invoke-recompile-restart)
                    #+lispworks
                    (conditions:fasl-error
                     #'.invoke-recompile-restart))
       ,@body)))

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
The element type can be CHARACTER or (UNSIGNED-BYTE <=8).
If the stream length can not be determined (e.g. for standard input), all available chars are read.")
  (:method ((fname string))
           (slurp-file (pathname fname)))
  (:method ((fname pathname))
           #+allegro
           (excl:file-contents fname)
           #-allegro
           (with-open-file (f fname :direction :input :element-type '(unsigned-byte 8))
             (slurp-file f)))
  (:method ((stream stream))
           (let* ((stream-type (stream-element-type stream))
                  (array-element-type (cond ((subtypep stream-type 'character)
                                             'character)
                                            ((subtypep (stream-element-type stream) '(unsigned-byte 8))
                                             '(unsigned-byte 8))
                                            (t (error "Unexpected stream element type: expected CHARACTER ~
                                                       or (UNSIGNED-BYTE <=8), got: ~A." stream-type))))
                  (stream-length (when (typep stream 'file-stream)
                                   (file-length stream)))
                  (vec (if stream-length
                           (let* ((vec (make-array stream-length :element-type array-element-type))
                                  (num-read (read-sequence vec stream)))
                             (adjust-array vec num-read))
                         (prog1 (coerce (loop for ch = (read-char-no-hang stream nil nil) while ch collect ch)
                                        'string)
                           (setf array-element-type 'character)))))
             (cond ((equal array-element-type 'character)
                    vec)
                   ((equal array-element-type '(unsigned-byte 8))
                    (map 'string #'code-char vec))))))

(defmacro checking-reader-conditionals (&whole whole &body body)
  "Break unless the body contains exactly one form. Based on idea from Steve Haflich."
  (let ((num (length body)))
    (unless (= num 1)
      (break "A CHECKING-READER-CONDITIONALS expression returned ~r forms: ~s" num whole))
    (car body)))

(defmacro named-function (name lambda-form)
  (declare (ignorable name))
  (assert (eq (car lambda-form) 'lambda))
  (checking-reader-conditionals
   #+allegro `(excl:named-function ,name ,lambda-form)
   #+sbcl `(sb-int:named-lambda ,name ,@(cdr lambda-form)) ;; skip 'lambda symbol
   #-(or allegro sbcl) lambda-form))

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
  (checking-reader-conditionals
   #+allegro `(excl:without-redefinition-warnings ,@body)
   #+lispworks `(system::without-warning-on-redefinition ,@body)
   #+sbcl `(handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
             ,@body)
   #-(or allegro lispworks sbcl) `(progn ,@body)))

(defmacro fast (&body body)
  `(locally (declare (optimize (speed 3)))
     ,@body))

(defmacro defconstant-once (name &rest args)
  (check-type name symbol)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (boundp ',name)
       (defconstant ,name ,@args))))


(defconstant-once +max-char-code+
    #+allegro (progn
                ;; On Allegro CHAR-CODE-LIMIT is the largest value across all
                ;; implementations, while EXCL:REAL-CHAR-CODE-LIMIT. is "a better estimate".
                ;; http://www.franz.com/support/documentation/8.1/doc/variables/excl/real-char-code-limit.htm
                (assert (<= excl:real-char-code-limit char-code-limit))
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

(defconstant +dict-alist-to-hashtable-threshold+ 8
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
        with fail-factor = 3 ;; only 1 in FAIL-FACTOR lookups succeeds
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
  (checking-reader-conditionals
   #+allegro (excl:exit code :quiet t)
   #+cmu (ext:quit code)
   #+lispworks (lw:quit :status code)
   #+sbcl (sb-ext:quit :unix-status (typecase code (number code)
                                              (null 0)
                                              (t 1)))
   #+(or openmcl mcl) (ccl::quit)
   #+abcl (cl-user::quit)
   #+ecl (si:quit)
   ))

(defun abbreviate-to-one-line (string)
  (loop for i from 0
      for ch across string
      while (char= ch #\Newline)
      finally (when (plusp i)
                (setf string (subseq string i))))
  (whereas ((ix (position #\Newline string)))
    (setf string (concatenate 'string (subseq string 0 ix) " ...")))
  string)

(defun abbreviate-string (string max-length)
  (check-type string string)
  (check-type max-length (integer 1))
  (if (> (length string) max-length)
      (concatenate 'string (subseq string 0 max-length) "...")
    string))

(defun register-feature (feature present-p)
  (check-type feature keyword)
  (if present-p
      (pushnew feature *features*)
    (setf *features* (remove feature *features*))))

(defmacro with-gensyms (list &body body)
  `(let ,(loop for x in list
	     collect `(,x (gensym ,(symbol-name x))))
     ,@body))

#+(or)
(defun directory-p (pathname)
  (check-type pathname pathname)
  (checking-reader-conditionals
   #+allegro (excl:file-directory-p pathname)
   #+lispworks (lispworks:file-directory-p pathname)
   #+(or cmu sbcl) (null (pathname-type pathname))))

(defmacro with-line-prefixed-output ((prefix) &body body)
  (check-type prefix string)
  `(pprint-logical-block (nil nil :per-line-prefix ,prefix)
     ,@body))

(defmacro with-sane-debugging ((error-format-string) &body body)
  "Reset the readtable to standard IO syntax in case of uncaught serious conditions (errors).
 This makes debugging readtable issues. The single argument to error-format-string is the condition object."
  `(call-with-sane-debugging ',error-format-string (lambda () ,@body)))

(defun call-with-sane-debugging (error-format-string func)
  (check-type error-format-string string)
  (handler-bind
      ((serious-condition (lambda (c)
                            (signal c)
                            (with-standard-io-syntax
                              (format t "~%")
                              (with-line-prefixed-output (";; ")
                                (format t error-format-string c)
                                (format t "~&Standard IO syntax (*readtable* etc) has been activated to enable debugging."))
                              (format t "~%~%")
                              (error c)))))
    (funcall func)))

(defun class-initargs-p (class &rest initargs)
  (check-type class (or symbol class))
  (when (symbolp class)
    (setf class (find-class class)))
  #+allegro
  (unless (closer-mop:class-finalized-p class)
    (closer-mop:finalize-inheritance class))
  (loop for initarg in initargs
      do (check-type initarg keyword)
      always (loop for slot in (closer-mop:class-slots class)
                 thereis (member initarg (closer-mop:slot-definition-initargs slot)))))

(defun derive-pathname (pathname &key (type      (pathname-type pathname      :case :common))
                                      (name      (pathname-name pathname      :case :common))
                                      (host      (pathname-host pathname      :case :common))
                                      (device    (pathname-device pathname    :case :common))
                                      (directory (pathname-directory pathname :case :common))
                                      (version   (pathname-version pathname)))
  (make-pathname :type type :name name :host host :device device
                 :directory directory :version version :case :common))

(defun ensure-path-is-directory (path)
  (let* ((truename (truename path))
         (true-string (namestring truename)))
    (if (member (aref true-string (1- (length true-string))) '(#\\ #\/))
        true-string
      (concatenate 'string true-string "/"))))

(defun careful-probe-file (pathspec)
  "Like PROBE-FILE, but this function never signals FILE-ERROR: that is mapped to NIL."
  (handler-case (probe-file pathspec)
    (file-error () nil)))
