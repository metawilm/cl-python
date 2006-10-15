(in-package :python)

(defparameter *builtin-modules* (make-hash-table :test #'eq) "List of module objects")

(defun initial-py-modules ()
  (let ((ht (make-hash-table :test #'eq)))
    (maphash (lambda (k v) (setf (gethash k ht) v)) *builtin-modules*)
    ht))

(defmacro with-builtin-module ((name) &body body)
  (let ((m '#:m)
	(dg '#:dg)
	(name.sym (if (symbolp name) name (intern name #.*package*))))
    `(let* ((,m (make-module :name ',name.sym
			     :path (format nil "/builtin-module/~A" ',name.sym)
			     :builtin t))
	    (,dg (slot-value ,m 'dyn-globals)))
       
       (setf (gethash ',name.sym *builtin-modules*) ,m)
       
       (flet ((register (name val)
		(setf (gethash name ,dg) val)))
	 
	 ,@body))))

(with-builtin-module ("sys")

  ;; Comand line args passed to script; argv[0] is script name (rel or abs)
  (register 'argv (make-py-list))
  
  ;; "big" or "little"
  (register 'byteorder "???")
  
  ;; Not implemented, and no intention to: 
  ;;  subversion, _current_frames, dllhandle, exc_type, exc_value, exc_traceback,
  ;;  getrefcount, _getframe, settscdump, winver
  ;;
  ;; Not implemented, but perhaps later:
  ;;  exc_info, exc_clear, exec_prefix, executable, getwindowsversion,
  ;;  last_{type,value,traceback}, {g,s}etdlopenflags, setprofile, settrace,
  ;;  tracebacklimit, warnoptions
  
  (register 'copyright "Copyright (c) Franz Inc. and Willem Broekema.")

  (register 'builtin_module_names (make-tuple-from-list
				   (loop for hk being the hash-value in *builtin-modules*
				       collect (string (py-module-name hk)))))
  
  ;; Func of one arg, called by REPL to print val
  (register 'displayhook *the-none*) ;; xxx not called by repl yet
  
  ;; Function to be called on uncatched exception, to print stack trace (at least in CPython..)
  (register 'excepthook *the-none*)
  
  ;; Original values of displayhook and excepthook
  (register '__displayhook__ *the-none*)
  (register '__excepthook__ *the-none*)
  
  (register 'exit (lambda (&optional arg)
		    (error "sys.exit(~@[~A~]) called" arg)))

  ;; Function to be called upon exit
  (register 'exitfunc *the-none*)
  
  ;; No-ops
  (register 'setcheckinterval (lambda (arg) 
				(declare (ignore arg))
				(warn "Function sys.setcheckinterval() not implemented.")))
  (register 'getcheckinterval (lambda ()
				(warn "Function sys.getcheckinterval() not implemented.")))
  
  ;; Default string encoding of Unicode strings
  (register 'getdefaultencoding (lambda () "todo"))
  (register 'setdefaultencoding (lambda (val) (declare (ignore val)) "todo"))
  (register 'getfilesystemencoding (lambda () "todo"))
  (register 'getrecursionlimit (lambda () "todo"))
  (register 'setrecursionlimit (lambda (val) (declare (ignore val)) "todo"))
  
  (register 'hexversion "todo")
  
  ;; At least 2**31 - 1; makes not really sense for us but oh well.
  (register 'maxint (expt 2 32))
  
  ;; Largest supported unicode code point
  (register 'maxunicode "todo")
  
  ;; Mapping from module names to modules
  (register 'modules *py-modules*) ;; xxx except string->module, not symbol->module
  
  ;; List of search paths
  (register 'path (make-py-list-from-list (list ".")))
  
  (register 'platform (format nil "~A ~A" (lisp-implementation-type) (lisp-implementation-version)))
  
  ;; Site-specific directory prefix for installing platform independent Python files
  (register 'prefix "/usr/local") ;; xxx
  
  ;; REPL input prefixes
  (register 'ps1 ">>> ")
  (register 'ps2 "... ")
  
  (register 'stdin "todo")
  (register 'stdout "todo")
  (register 'stderr "todo")
  (register '__stdin__ "todo")
  (register '__stdout__ "todo")
  (register '__stderr__ "todo")
  
  ;; The Lisp API version
  (register 'api_version "todo")
  
  (register 'version_info "???") ;; (make-tuple-from-list (list 2 5 0 "final" 0)))
  )

(with-builtin-module ("time")
  
  ;; Current processor time, in seconds, floating point
  (register 'clock (lambda () (coerce (/ (get-internal-run-time)
					 internal-time-units-per-second)
				      'float))))


