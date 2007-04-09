(defpackage :clpython.module.sys
  (:use :clpython :common-lisp)
  (:export #:|argv| #:|byteorder| #:|ecv_info| #:|copyright|
	   #:|builtin_module_names| #:|displayhook| #:|excepthook|
	   #:|__displayhook__| #:|__excepthook__| #:|exit| #:|exitfunc|
	   #:|setcheckinterval| #:|getcheckinterval|
	   #:|getdefaultencoding| #:|setdefaultencoding|
	   #:|getfilesystemencoding|
	   #:|getrecursionlimit| #:|setrecursionlimit|
	   #:|hexversion| #:|maxint|
	   #:|maxunicode| #:|modules| #:|path| #:|platform|
	   #:|prefix| #:|ps1| #:|ps2| #:|stdin| #:|stdout| #:|stderr|
	   #:|__stdin__| #:|__stdout__| #:|__stderr__|
	   #:|api_version| #:|version_info| #:|version| ))

(in-package :clpython.module.sys)

(defvar |argv| (clpython::make-py-list) ;; TODO
  "Comand line args passed to script; argv[0] is script name (rel or abs)")

(defvar |byteorder| "(Byteorder: N/A)"
  "Byte order of implementation: 'big' or 'little'")

;; Not implemented, and no intention to: 
;;  subversion, _current_frames, dllhandle, exc_type, exc_value, exc_traceback,
;;  getrefcount, _getframe, settscdump, winver
;;
;; Not implemented, but perhaps later:
;;  exc_info, exc_clear, exec_prefix, executable, getwindowsversion,
;;  last_{type,value,traceback}, {g,s}etdlopenflags, setprofile, settrace,
;;  tracebacklimit, warnoptions

(defun |exc_info| ()
  (declare (special *try-except-current-handled-exception*))
  (clpython::make-tuple-from-list 
   (if *try-except-current-handled-exception*
       (list (clpython::py-class-of *try-except-current-handled-exception*)
	     *try-except-current-handled-exception*
	     *the-none*) ;; we don't have traceback objects (yet)
     (list *the-none* *the-none* *the-none*))))

(defvar |copyright| "Copyright (c) Franz Inc. and Willem Broekema.")

(defvar |builtin_module_names| "todo"
  #+(or)(clpython::make-tuple-from-list
	 (loop for hk being the hash-value in clpython::*builtin-modules*
	     collect (string (clpython::py-module-name hk)))))

(defvar |displayhook| *the-none*
  "Func of one arg, called by REPL to print val") ;; XXX not called by repl yet

(defvar |excepthook| *the-none*
  "Function to be called on uncatched exception, to print stack trace (at least in CPython..)")
  
(defvar |__displayhook__| *the-none* "Original value of displayhook")
(defvar |__excepthook__| *the-none* "Original value of excepthook")

(defun |exit| (&optional arg)
  (error "sys.exit(~@[~A~]) called" arg))

(defvar |exitfunc| *the-none* "Function to be called upon exit")

;; No-ops
(defun |setcheckinterval| (arg) 
  (declare (ignore arg))
  (warn "Function sys.setcheckinterval() not implemented."))

(defun |getcheckinterval| ()
  (warn "Function sys.getcheckinterval() not implemented."))

;; Default string encoding of Unicode strings
(defun |getdefaultencoding| ()
  "todo")

(defun |setdefaultencoding| (val)
  (declare (ignore val)) 
  "todo")

(defun |getfilesystemencoding| () 
  "todo")

(defun |getrecursionlimit| ()
  "todo")

(defun |setrecursionlimit| (val)
  (declare (ignore val))
  "todo")

(defvar |hexversion| "todo")
(defvar |maxint| #.(expt 2 100)
	"At least 2**31 - 1; makes not really sense for us but oh well.")

(defvar |maxunicode| "todo" "Largest supported unicode code point")

(defvar |modules| "todo" "Mapping from module names to modules")
;; XXX string->module, not symbol->module
  
;; List of search paths
(defvar |path| (clpython::make-py-list-from-list (list ".")))

(defvar |platform| "Common Lisp")

(defvar |prefix| *the-none*  ;; xxx
  "Site-specific directory prefix for installing platform independent Python files")

;; REPL input prefixes
(defvar |ps1| ">>> ")
(defvar |ps2| "... ")

(defvar |stdin| "todo")
(defvar |stdout| "todo")
(defvar |stderr| "todo")
(defvar |__stdin__| "todo")
(defvar |__stdout__| "todo")
(defvar |__stderr__| "todo")

(defvar |api_version| "todo" "The Lisp API version")

(defvar |version_info|)
(defvar |version|)

(let ((py-version (clpython::make-tuple-from-list '(2 5 0 "alpha" 0)))) 
  ;; XXX figure out which we resemble
  (setf |version_info| py-version)
  (setf |version|      (format nil "CLPython 2.5.0 alpha (~A ~A)"
				 (lisp-implementation-type) (lisp-implementation-version))))
