(in-package :clpython)

(defparameter *builtin-modules* (make-hash-table :test #'eq) "List of module objects")

(defun initial-py-modules ()
  (let ((ht (make-hash-table :test #'eq)))
    (maphash (lambda (k v) (setf (gethash k ht) v)) *builtin-modules*)
    ht))

(defmacro with-builtin-module ((name) &body body)
  (let ((m '#:m)
	(dg '#:dg)
	(name.sym (if (symbolp name) name (intern name #.*package*))))
    `(let* ((,m (or (gethash ',name.sym *builtin-modules*)
		    (make-module :name ',name.sym
			     :path (format nil "/builtin-module/~A" ',name.sym)
			     :builtin t)))
	    (,dg (slot-value ,m 'dyn-globals)))
       
       (setf (gethash ',name.sym *builtin-modules*) ,m)
       
       (flet ((.register (name val)
		(setf (gethash name ,dg) val)))
	 (macrolet ((reg-var (vname value)
		      `(.register ',vname ,value))
		    (reg-func (fname args &body body)
		      (let ((dotted-fname (intern (concatenate 'string ',name "." (string fname))
						  #.*package*)))
			`(progn (defun ,dotted-fname ,args
				  (block ,fname
				    (locally
				      ,@body)))
				(.register ',fname (function ,dotted-fname))))))
	   ,@body)))))

(with-builtin-module ("sys")

  ;; Comand line args passed to script; argv[0] is script name (rel or abs)
  (reg-var |argv| (make-py-list))
  
  ;; "big" or "little"
  (reg-var |byteorder| "???")
  
  ;; Not implemented, and no intention to: 
  ;;  subversion, _current_frames, dllhandle, exc_type, exc_value, exc_traceback,
  ;;  getrefcount, _getframe, settscdump, winver
  ;;
  ;; Not implemented, but perhaps later:
  ;;  exc_info, exc_clear, exec_prefix, executable, getwindowsversion,
  ;;  last_{type,value,traceback}, {g,s}etdlopenflags, setprofile, settrace,
  ;;  tracebacklimit, warnoptions

  (reg-func |exc_info| () 
	    (declare (special *try-except-current-handled-exception*))
	    (make-tuple-from-list 
	     (if *try-except-current-handled-exception*
		 (list (py-class-of *try-except-current-handled-exception*)
		       *try-except-current-handled-exception*
		       *the-none*) ;; we don't have traceback objects (yet)
	       (list *the-none* *the-none* *the-none*))))

  (reg-var |copyright| "Copyright (c) Franz Inc. and Willem Broekema.")

  (reg-var |builtin_module_names| (make-tuple-from-list
				   (loop for hk being the hash-value in *builtin-modules*
				       collect (string (py-module-name hk)))))
  
  ;; Func of one arg, called by REPL to print val
  (reg-var |displayhook| *the-none*) ;; xxx not called by repl yet
  
  ;; Function to be called on uncatched exception, to print stack trace (at least in CPython..)
  (reg-var |excepthook| *the-none*)
  
  ;; Original values of displayhook and excepthook
  (reg-var |__displayhook__| *the-none*)
  (reg-var |__excepthook__| *the-none*)
  
  (reg-var |exit| (lambda (&optional arg)
		    (error "sys.exit(~@[~A~]) called" arg)))

  ;; Function to be called upon exit
  (reg-var |exitfunc| *the-none*)
  
  ;; No-ops
  (reg-func |setcheckinterval| (arg) 
	    (declare (ignore arg))
	    (warn "Function sys.setcheckinterval() not implemented."))
  (reg-func |getcheckinterval| () (warn "Function sys.getcheckinterval() not implemented."))
  
  ;; Default string encoding of Unicode strings
  (reg-func |getdefaultencoding| ()    "todo")
  (reg-func |setdefaultencoding| (val) (declare (ignore val)) "todo")
  (reg-func |getfilesystemencoding| () "todo")
  (reg-func |getrecursionlimit| ()     "todo")
  (reg-func |setrecursionlimit| (val)  (declare (ignore val)) "todo")
  
  (reg-var  |hexversion| "todo")
  
  ;; At least 2**31 - 1; makes not really sense for us but oh well.
  (reg-var  |maxint| #.(expt 2 100))
  
  ;; Largest supported unicode code point
  (reg-var  |maxunicode| "todo")
  
  ;; Mapping from module names to modules
  (reg-var  |modules| *py-modules*) ;; xxx except string->module, not symbol->module
  
  ;; List of search paths
  (reg-var  |path| (make-py-list-from-list (list ".")))
  
  (reg-var  |platform| "Common Lisp")
  
  ;; Site-specific directory prefix for installing platform independent Python files
  (reg-var  |prefix| *the-none*) ;; xxx
  
  ;; REPL input prefixes
  (reg-var |ps1| ">>> ")
  (reg-var |ps2| "... ")
  
  (reg-var |stdin| "todo")
  (reg-var |stdout| "todo")
  (reg-var |stderr| "todo")
  (reg-var |__stdin__| "todo")
  (reg-var |__stdout__| "todo")
  (reg-var |__stderr__| "todo")
  
  ;; The Lisp API version
  (reg-var |api_version| "todo")
  (let ((py-version (make-tuple-from-list '(2 5 0 "alpha" 0))))  ;; XXX figure out which we resemble
    (reg-var |version_info| py-version)
    (reg-var |version|      (format nil "CLPython 2.5.0 alpha (~A ~A)"
				    (lisp-implementation-type) (lisp-implementation-version))))
  )

(with-builtin-module ("time")
  
  ;; Current processor time, in seconds, floating point
  (reg-func |clock| () (coerce (/ (get-internal-run-time)
				  internal-time-units-per-second)
			       'float))
  (reg-func |sleep| (n) (sleep (py-val->number n)))
  (reg-func |time|  ()  (excl.osi:universal-to-unix-time (get-universal-time)))
  )

(with-builtin-module ("math")
  (reg-var |pi| pi))


;;; Array
(defclass py-array (py-core-object)
  ((kind    :type character :initarg :kind    :accessor py-array-kind)
   (array   :type array     :initarg :array   :accessor py-array-array)
   (elmtype :type t         :initarg :elmtype :accessor py-array-elmtype)
   (elmsize :type fixnum    :initarg :elmsize :accessor py-array-elmsize))
  (:metaclass py-core-type))

(defparameter *py-array-types*
    ;; code  type              item size
    '(( #\c  (unsigned-byte 8) nil)  ;; char  XXX 7 or 8 chars?
      ( #\b  (signed-byte   8) nil)  ;; signed char
      ( #\B  (unsigned-byte 8) nil)  ;; unsigned char
      ( #\u  character         nil)  ;; Unicode char
      ( #\h  (signed-byte   16) nil) ;; signed short
      ( #\H  (unsigned-byte 16) nil) ;; signed short
      ( #\i  (signed-byte   16) nil) ;; signed int
      ( #\I  (unsigned-byte 16) nil) ;; unsigned int
      ( #\l  (signed-byte   32) nil) ;; signed long
      ( #\L  (unsigned-byte 32) nil) ;; unsigned long
      ( #\f  single-float       nil) ;; float
      ( #\f  double-float       nil) ;; double
      ))

(def-py-method py-array.__new__ :static (cls typecode &optional initializer)
  ;; Both creation and initialization, as array size is dependent on initializer.
  (assert (eq cls (find-class 'py-array)) () "Subclassing ARRAY not yet supported...") 
  (let* ((type-code (let ((s (py-val->string typecode)))
		      (if (= (length s) 1)
			  (aref typecode 0)
			(py-raise 'TypeError "Type indicator must be char (got: ~S)" typecode))))
	 (kind (or (find type-code *py-array-types* :test #'char= :key #'car)
		   (py-raise 'ValueError "Unknown array type indicator: ~S" (aref typecode 0))))
	 (lisp-type (second kind))
	 (item-size (or (third kind)
			;; Determine item size by writing to file and reading file size.
			;; XXX Is there a better way?
			(setf (third kind)
			  (let ((fname (format nil "__tmp_~A" (gensym))))
			    (with-open-file (f fname
					     :direction :output
					     :element-type lisp-type
					     :if-does-not-exist :create
					     :if-exists :supersede)
			      (write-byte 0 f))
			    (with-open-file (f fname
					     :direction :input
					     :element-type '(unsigned-byte 8)
					     :if-does-not-exist :error)
			      (file-length f)))))))
    (warn "code ~A = type ~A = ~A bytes per item" type-code lisp-type item-size)
    (flet ((create-array (&optional (size 0))
	     (make-instance 'py-array 
	       :kind type-code
	       :array (make-array size :element-type lisp-type :adjustable t :fill-pointer 0)
	       :elmtype lisp-type
	       :elmsize item-size)))
      (if initializer
	  (typecase initializer
	    ;; XXX how about user-defined subclasses?
	    ((string vector)
	     (let ((arr (create-array (length initializer))))
	       (funcall (if (stringp initializer) #'py-array.fromstring #'py-array.fromlist)
			arr initializer)
	       arr))
	    (t
	     (py-array.extend (create-array) initializer))) ;; XXX could take __len__
	(create-array)))))

(def-py-method py-array.__repr__ (x)
  (with-output-to-string (s)
    (print-unreadable-object (x s :type t :identity t)
      (format s "typecode '~A', elm-type ~A, elm-size ~A bytes, ~A items~@[~A~]"
	      (py-array-kind x)
	      (py-array-elmtype x)
	      (py-array-elmsize x)
	      (length (py-array-array x))
	      (when (< (length (py-array-array x)) 10)
		(format nil " [~{~A~^ ~}]" (loop for i across (py-array-array x) collect i)))))))


(def-py-method py-array.fromstring (py-arr string)
  (loop with vec = (py-array-array py-arr)
      for ch across (py-val->string string)
      do (vector-push-extend (string ch) vec))
  *the-none*)

(def-py-method py-array.fromlist (py-arr list)
  (py-array.fromstring py-arr list))

(def-py-method py-array.extend (py-arr iterable)
  (loop with vec = (py-array-array py-arr)
      for item in (py-iterate->lisp-list iterable)
      do (vector-push-extend item vec))
  *the-none*)

(with-builtin-module ("array")
  (reg-var |array| (find-class 'py-array)))
