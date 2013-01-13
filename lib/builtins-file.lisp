;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON; Readtable: PY-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;;;; File class

(in-package :clpython)
(in-syntax *user-readtable*)

;; File (User object)

(defclass py-file (dicted-object)
  ((file-stream :initform nil   :accessor py-file-stream)
   (mode        :initform nil   :accessor py-file-mode)
   (binary-mode-p :initform nil :accessor py-file-binary-mode-p)
   (closed-p    :initform t     :accessor py-file-closed-p))
  (:metaclass py-type))

(defun ensure-open-file (f)
  (when (py-file-closed-p f)
    (py-raise '{ValueError} "File is closed")))
    
(def-py-method py-file.__new__ :static (cls &rest args)
  (make-instance cls))

(def-py-method py-file.__init__ (f^ name^ &optional mode^ buffering^)
  ;; The mode can be 'r', 'w' or 'a' for reading (default), writing
  ;; or appending.  The file will be created if it doesn't exist when
  ;; opened for writing or appending; it will be truncated when opened
  ;; for writing.  Add a 'b' to the mode for binary files. Add a '+'
  ;; to the mode to allow simultaneous reading and writing.
  ;; 
  ;; Add a 'U' to mode to open the file for input with universal
  ;; newline support.  Any line ending in the input file will be seen
  ;; as a '\n' in Python.  Also, a file so opened gains the attribute
  ;; 'newlines'; the value for this attribute is one of None (no
  ;; newline read yet), '\\r', '\\n', '\\r\\n' or a tuple containing
  ;; all the newline types seen.  'U' cannot be combined with 'w' or
  ;; '+' mode.
  ;; 
  ;; If the buffering argument is given, 0 means unbuffered, 1 means
  ;; line buffered, and larger numbers specify the buffer size.
  ;; 
  ;; Note: open() is an alias for file().   [pydoc]
  (unless (stringp name)
    (py-raise '{TypeError} "Invalid file name: ~S." name))
  (setf mode (if mode (string-downcase (py-val->string mode)) "r"))
  (let* ((binary-mode-p (find #\b mode))
	 (rest-mode (coerce (loop for ch across mode unless (char= ch #\b) collect ch) 'string))
	 (modes '(("r"  . :read)  ("w"  . :write) ("a"  . :append)
		  ("r+" . :read+) ("w+" . :write) ("a+" . :append)
		  ("u"  . :read)  ("ur" . :read)))
	 (fmode (or (cdr (assoc rest-mode modes :test #'string-equal))
		    (py-raise '{ValueError} "Invalid file mode: ~S." mode)))
	 (stream (handler-case (open name
                                     :direction (ecase fmode                  
                                                  (:read            :input)
                                                  (:write           :output)
                                                  ((:read+ :append) :io))
                                     :element-type '(unsigned-byte 8)
                                     :if-exists (ecase fmode
                                                  ((:read :read+) nil)
                                                  (:write         :supersede)
                                                  (:append        :append))
                                     :if-does-not-exist (ecase fmode
                                                          (:read            :error)
                                                          (:write           :create)
                                                          ((:append :read+) :create)))
                   (error (e)
                     (py-raise '{IOError} "Opening file `~A' failed: ~_~A" name e)))))
    (when (eq mode :read+)
      (unless (file-position stream :end)
	(py-raise '{IOError} "Moving to end of file failed.")))
    (setf (py-file-stream f)        stream
	  (py-file-binary-mode-p f) binary-mode-p
	  (py-file-closed-p f)      nil
	  (py-file-mode f)          mode)
    
    *the-none*))

(def-py-method py-file.__repr__ (f)
  (with-output-to-string (s)
    (print-unreadable-object (f s :identity t)
      (with-slots (mode closed-p) f
	(format s "file ~A :mode ~S" (py-file.name f) mode)
	(when closed-p
	  (write-string " :closed t" s))))))


(def-py-method py-file.close (f)
  ;; Calling close() more than once is allowed.
  (unless (py-file-closed-p f)
    (close (py-file-stream f))
    (setf (py-file-closed-p f) t))
  *the-none*)

(def-py-method py-file.flush (f)
  (ensure-open-file f)
  (finish-output (py-file-stream f))
  *the-none*)

(def-py-method py-file.fileno (f)
  "File descriptor"
  ;; ?? (ensure-open-file f)
  (py-raise '{IOError} "file.fileno(): todo"))

(def-py-method py-file.isatty (f)
  ;; Return True if the file is connected to a tty(-like) device, else
  ;; False.  If a file-like object is not associated with a real file,
  ;; this method should not be implemented. [pydoc]
  (py-bool #+allegro (excl.osi:isatty (py-file-stream f))
           #-allegro (error "Error: py-file.isatty")))

(def-py-method py-file.__iter__ (f)
  (ensure-open-file f)
  f)
  
(def-py-method py-file.next (f)
  ;; In order to make a for loop the most efficient way of looping
  ;; over the lines of a file (a very common operation), the next()
  ;; method uses a hidden read-ahead buffer. As a consequence of using
  ;; a read-ahead buffer, combining next() with other file methods
  ;; (like readline()) does not work right. However, using seek() to
  ;; reposition the file to an absolute position will flush the
  ;; read-ahead buffer. [pydoc]
  (ensure-open-file f)
  (multiple-value-bind (line eof-p)
      (py-file.readline f)
    (if eof-p
	(py-raise '{StopIteration} "File reading finished.")
      line)))

(def-py-method py-file.read (f^ &optional size^)
  ;; Read at most size bytes from the file (less if the read hits EOF
  ;; before obtaining size bytes). If the size argument is negative or
  ;; omitted, read all data until EOF is reached. The bytes are
  ;; returned as a string object. An empty string is returned when EOF
  ;; is encountered immediately. (For certain files, like ttys, it
  ;; makes sense to continue reading after an EOF is hit.) Note that
  ;; this method may call the underlying C function fread() more than
  ;; once in an effort to acquire as close to size bytes as
  ;; possible. Also note that when in non-blocking mode, less data
  ;; than what was requested may be returned, even if no size
  ;; parameter was given.
  (ensure-open-file f)
  (let* ((size (if size (py-val->integer size) 100))
	 (check-size-p (>= size 0))
	 (chars (loop
		    for i from 0
		    for not-at-limit = (or (null check-size-p) (< i size))
                                       ;; TODO check file encoding
		    for ch = (and not-at-limit
                                  (code-char (read-byte (py-file-stream f) nil nil)))
		    while ch 
		    collect ch)))
    (coerce chars 'string)))

(def-py-method py-file.readline (f^ &optional size^)
  ;; Read one entire line from the file. A trailing newline character
  ;; is kept in the string (but may be absent when a file ends with an
  ;; incomplete line). If the size argument is present and
  ;; non-negative, it is a maximum byte count (including the trailing
  ;; newline) and an incomplete line may be returned. An empty string
  ;; is returned only when EOF is encountered immediately. Note:
  ;; Unlike stdio's fgets(), the returned string contains null
  ;; characters ('\0') if they occurred in the input.
  
  ;; XXX Currently every kind of newline char (sequence) is replaced
  ;; by whatever #\Newline is for the platform.
  ;;
  ;; Second value returned indicates EOF-P.
  (ensure-open-file f)
  (when size
    (warn "The `size' arg to file.readline() is currently ignored."))
  (multiple-value-bind (line eof-p)
      (read-line (py-file-stream f) nil nil)
    (cond ((null line) (values "" t))
	  (eof-p       line)
	  (t           (concatenate 'string line (string #\Newline))))))

(def-py-method py-file.readlines (f &optional sizehint^)
  ;; Read until EOF using readline() and return a list containing the
  ;; lines thus read. If the optional sizehint argument is present,
  ;; instead of reading up to EOF, whole lines totalling approximately
  ;; sizehint bytes (possibly after rounding up to an internal buffer
  ;; size) are read. Objects implementing a file-like interface may
  ;; choose to ignore sizehint if it cannot be implemented, or cannot
  ;; be implemented efficiently.
  (ensure-open-file f)
  (make-py-list-from-list (loop for line = (multiple-value-bind (line eof-p) 
					       (py-file.readline f)
					     (and (null eof-p) line))
			      while line collect line)))

(def-py-method py-file.xreadlines (f)
  ;; This method returns the same thing as iter(f). New in version
  ;; 2.1. Deprecated since release 2.3. Use "for line in file"
  ;; instead.
  (ensure-open-file f)
  (make-iterator-from-function :func (lambda ()
				       (multiple-value-bind (line eof-p) 
					   (py-file.readline f)
					 (and (null eof-p) line)))
			       :name :file.xreadlines))

(defconstant +file-seek-absolute+            0)
(defconstant +file-seek-relative-to-current+ 1)
(defconstant +file-seek-relative-to-end+     2)

(def-py-method py-file.seek (f offset^ &optional (whence +file-seek-absolute+))
  ;; Set the file's current position, like stdio's fseek(). The whence
  ;; argument is optional and defaults to 0 (absolute file
  ;; positioning) ; other values are 1 (seek relative to the current
  ;; position) and 2 (seek relative to the file's end). There is no
  ;; return value. Note that if the file is opened for appending (mode
  ;; 'a' or 'a+'), any seek() operations will be undone at the next
  ;; write. If the file is only opened for writing in append mode
  ;; (mode 'a'), this method is essentially a no-op, but it remains
  ;; useful for files opened in append mode with reading enabled (mode
  ;; 'a+'). If the file is opened in text mode (mode 't'), only
  ;; offsets returned by tell() are legal. Use of other offsets causes
  ;; undefined behavior.
  (ensure-open-file f)
  (let* ((s (py-file-stream f))
         (new-pos (cond ((= whence +file-seek-absolute+)
                         offset)
                        ((= whence +file-seek-relative-to-current+)
                         (+ (file-position s) offset))
                        ((= whence +file-seek-relative-to-end+)
                         (+ (file-length s) offset))
                        (t (py-raise '{ValueError} 
                                     "Invalid position option for file.seek. ~@
                                      Expected one of ~A (abs), ~A (rel to curr), ~A (rel to end); got: ~A."
                                     +file-seek-absolute+ +file-seek-relative-to-current+
                                     +file-seek-relative-to-end+ whence)))))
    (if (file-position s new-pos)
        *the-none*
      (py-raise '{TypeError} "Setting file position of ~A to ~A failed."
                f new-pos))))

(def-py-method py-file.tell (f)
  ;; Return the file's current position, like stdio's ftell().
  (ensure-open-file f)
  (file-position (py-file-stream f)))

(def-py-method py-file.truncate (f &optional size)
  ;; Truncate the file's size. If the optional size argument is
  ;; present, the file is truncated to (at most) that size. The size
  ;; defaults to the current position. The current file position is
  ;; not changed. Note that if a specified size exceeds the file's
  ;; current size, the result is platform-dependent: possibilities
  ;; include that file may remain unchanged, increase to the specified
  ;; size as if zero-filled, or increase to the specified size with
  ;; undefined new content. Availability: Windows, many Unix variants.
  (ensure-open-file f)
  (error "todo"))

(def-py-method py-file.write (f str)
  ;; Write a string to the file. There is no return value. Due to
  ;; buffering, the string may not actually show up in the file until
  ;; the flush() or close() method is called.
  (ensure-open-file f)
  (write-string str (py-file-stream f))
  *the-none*)

(def-py-method py-file.writelines (f sequence)
  ;; Write a sequence of strings to the file. The sequence can be any
  ;; iterable object producing strings, typically a list of
  ;; strings. There is no return value. (The name is intended to match
  ;; readlines(); writelines() does not add line separators.)
  (ensure-open-file f)
  (error "todo"))

;; File attributes

(def-py-method py-file.closed :attribute (f)
  ;; bool indicating the current state of the file object. This is a
  ;; read-only attribute; the close() method changes the value. It may
  ;; not be available on all file-like objects.
  (py-bool (py-file-closed-p f)))

(def-py-method py-file.encoding :attribute (f)
  ;; The encoding that this file uses. When Unicode strings are
  ;; written to a file, they will be converted to byte strings using
  ;; this encoding. In addition, when the file is connected to a
  ;; terminal, the attribute gives the encoding that the terminal is
  ;; likely to use (that information might be incorrect if the user
  ;; has misconfigured the terminal). The attribute is read-only and
  ;; may not be present on all file-like objects. It may also be None,
  ;; in which case the file uses the system default encoding for
  ;; converting Unicode strings.
  (error "todo"))

(def-py-method py-file.mode :attribute (f)
  ;; The I/O mode for the file. If the file was created using the
  ;; open() built-in function, this will be the value of the mode
  ;; parameter. This is a read-only attribute and may not be present
  ;; on all file-like objects. 
  (py-file-mode f))

(def-py-method py-file.name :attribute (f)
  ;; If the file object was created using open(), the name of the
  ;; file. Otherwise, some string that indicates the source of the
  ;; file object, of the form "<...>". This is a read-only attribute
  ;; and may not be present on all file-like objects.
  (namestring (truename (py-file-stream f))))

(def-py-method py-file.newlines :attribute (f)
  ;; If Python was built with the --with-universal-newlines option to
  ;; configure (the default) this read-only attribute exists, and for
  ;; files opened in universal newline read mode it keeps track of the
  ;; types of newlines encountered while reading the file. The values
  ;; it can take are '\r', '\n', '\r\n', None (unknown, no newlines
  ;; read yet) or a tuple containing all the newline types seen, to
  ;; indicate that multiple newline conventions were encountered. For
  ;; files not opened in universal newline read mode the value of this
  ;; attribute will be None.
  (error "todo"))

(def-py-method py-file.softspace :attribute (f)
  ;; Boolean that indicates whether a space character needs to be
  ;; printed before another value when using the print
  ;; statement. Classes that are trying to simulate a file object
  ;; should also have a writable softspace attribute, which should be
  ;; initialized to zero. This will be automatic for most classes
  ;; implemented in Python (care may be needed for objects that
  ;; override attribute access); types implemented in C will have to
  ;; provide a writable softspace attribute. Note: This attribute is
  ;; not used to control the print statement, but to allow the
  ;; implementation of print to keep track of its internal state.
  (error "todo"))
    