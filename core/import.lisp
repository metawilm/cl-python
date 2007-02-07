;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython)

;;;; Importing Python and Lisp modules

;; For "import foo", the order of search is:
;; 
;;   1. Python file "foo.py" in *py-module-search-path* (or foo.fasl, if up-to-date);
;;      this corresponds to normal Python behaviour;
;; 
;;   2. Lisp file "foo.{cl,lisp}" in *py-module-search-path-lisp* (or .fasl);
;;      this allows easy use of Lisp files as Python modules;
;;
;;   3. The Lisp _package_ named "foo" (case-insensitive);
;;      effectively making the package available as a Python module.
;;
;; Todo: the import hooks in CPython are described in PEP 302; maybe our rules fit
;;       in that setup.  http://www.python.org/dev/peps/pep-0302

;; Modules are kept in a dictionary `sys.modules', mapping from string
;; to module object.

(defparameter *py-modules* (make-hash-table :test #'eq))

(defvar *py-import-force-reload* nil
  "Force recompile of all imported modules (for debugging)")

(defun path-kind (path)
  (assert (stringp path))
  (cond ((not (probe-file path))       nil)
	((excl:file-directory-p path)  :directory)
	(t                             :file)))

(defmacro with-py-readtable (&body body)
  ;; In this context every character is dispatched to the Python parser.
  `(let ((*readtable* (load-time-value
		       (let ((rt (copy-readtable nil))
			     (f (lambda (stream char)
				  (unread-char char stream)
				  (parse-python-file stream))))
			 (loop for i from 0 below 256
			     do (set-macro-character (code-char i) f t rt))
			 rt))))
     ,@body))


(defun recompile-py-if-needed (mod-name py-fname fasl-fname force-reload verbose)
  (let* ((*current-module-name* (string mod-name)) ;; used by compiler
	 (*current-module-path* py-fname))
    (declare (special *current-module-name* *current-module-path*))
    (with-py-readtable
	(compile-file py-fname
		      :output-file fasl-fname
		      :if-newer (not force-reload)
		      :verbose verbose))))

(defun register-loaded-python-module (&key name package builtin &allow-other-keys)
  ;; XXX for now only does something if builtin
  (if (and name package (packagep package) builtin)
      (let ((mod (make-module :name name :builtin t)))
	(do-symbols (sym package mod)
	  (when (eq (symbol-package sym) (find-package package))
	    (warn "Symbol ~A: bound: ~A  fbound: ~A" sym (boundp sym) (fboundp sym))
	    (let ((val (cond ((boundp sym)   (symbol-value sym))
			     ((fboundp sym)  (symbol-function sym)))))
	      (when val
		(py-module-set-kv mod (intern (symbol-name sym) :clpython.ast.user) val)))))
	(setf (gethash (intern (string name) :clpython) *builtin-modules*) mod))
    (warn "Bogus IN-PYTHON-MODULE form: name or package missing")))

(defun py-import (mod-name-as-list &rest options &key (force-reload *py-import-force-reload*)
						      (verbose t)
						      src-mod)
  ;; Registers module in *py-modules* and returns it.
  ;; If SRC-MOD is supplied, then the directory of that module's .py file is the first
  ;; directory in the search path. 
  (declare (special *builtin-modules*)
	   (optimize (debug 3)))
  (assert (listp mod-name-as-list))
  
  (labels ((builtin-module (name)
	     (when (listp name)
	       (setf name (dotted-name name)))
	     (assert (stringp name))
	     (gethash (intern name #.*package*) *builtin-modules*))
	   (imported-module (name)
	     (when (listp name)
	       (setf name (dotted-name name)))
	     (assert (stringp name))
	     (gethash (intern name #.*package*) *py-modules*))
	   (register-module (name mod-obj)
	     (when (listp name)
	       (setf name (dotted-name name)))
	     (assert (stringp name))
	     (setf (gethash (intern name #.*package*) *py-modules*) mod-obj))
	   (unregister-module (name)
	     (when (listp name)
	       (setf name (dotted-name name)))
	     (assert (stringp name))
	     (remhash (intern name #.*package*) *py-modules*))
	   (path-kind (path)
	     (assert (stringp path))
	     (cond ((not (probe-file path))       nil)
		   ((excl:file-directory-p path) :directory)
		   (t                            :file)))
	   (dotted-name (list-name)
	     (format nil "~{~A~^.~}" list-name))
	   
	   (search-paths ()
	     (if (> (length mod-name-as-list) 1)
		 
		 (let ((parent-mod (apply #'py-import (butlast mod-name-as-list) options)))
		   (unless (py-module-package-p parent-mod)
		     (py-raise '|ImportError|
			       "Cannot import '~A', as '~A' is not a package (directory)"
			       (dotted-name mod-name-as-list)
			       (dotted-name (butlast mod-name-as-list))))
		   (let ((parent-path (slot-value parent-mod 'filepath)))
		     (assert (eq (path-kind parent-path) :file))
		     (list (excl.osi:dirname parent-path))))
	       
	       (append
		(when (and src-mod (slot-value src-mod 'filepath))
		  (list (excl:path-pathname (slot-value src-mod 'filepath)))) ;; XXX take directory
		(let ((sys-mod (gethash '|sys| *py-modules*)))
		  (assert sys-mod () "The built-in `sys' module has disappeared")
		  (py-iterate->lisp-list (py-attr sys-mod '|path|))))))
	   
	   (py-file-to-load (paths pkg-name init-file file-name fasl-name)
	     (dolist (path paths)
	       ;; Try as package
	       (let ((pkg-path (format nil "~A/~A" path pkg-name)))
		 (when (eq (path-kind pkg-path) :directory)
		   (let ((fn (format nil "~A/~A/~A" path pkg-name init-file)))
		     (when (eq (path-kind fn) :file)
		       (return-from py-file-to-load
			 (values fn (format nil "~A/~A/__init__.fasl" path pkg-name))))
		     (warn "During import of `~A': Directory `~A' skipped, as it does not ~
                            contain `__init__.py'" (dotted-name mod-name-as-list) path))))
	       ;; Try as file
	       (let ((fn (format nil "~A/~A" path file-name)))
		 (when (eq (path-kind fn) :file)
		   (return-from py-file-to-load
		     (values fn (format nil "~A/~A" path fasl-name))))))))
    
    ;; Module already imported earlier?
    (unless force-reload
      (when (imported-module mod-name-as-list)
	(return-from py-import (imported-module mod-name-as-list))))
    
    ;; Reloading built-in module?
    (when (and force-reload
	       (imported-module mod-name-as-list)
	       (eq (imported-module mod-name-as-list) (builtin-module mod-name-as-list)))
      ;; reloading built-in module
      ;; for now, does not remove user-set attributes (import sys; sys.a = 3; reload(sys); sys.a == 3)
      (return-from py-import (builtin-module mod-name-as-list)))

    ;; In case of a dotted import ("import a.b"), search in parent module directory.
    ;; Otherwise, look in `sys.path'.
    (let* ((pkg-name     (string (car (last mod-name-as-list))))
	   (file-name    (format nil "~A.py" pkg-name))
	   (fasl-name    (format nil "~A.fasl" pkg-name))
	   (init-file    "__init__.py"))
      
      (multiple-value-bind (py-file fasl-file)
	  (py-file-to-load (search-paths) pkg-name init-file file-name fasl-name)
	
	;; The user can shadow built-in module, therefore check only now.
	(unless py-file
	  (when (builtin-module mod-name-as-list)
	    (return-from py-import (builtin-module mod-name-as-list))))
	
	(unless py-file
	  (py-raise '|ImportError| "Could not find module '~A'; ~_search paths tried: ~{~S~^, ~}"
		    (dotted-name mod-name-as-list) (search-paths)))
	
	;; Compile .py -> .fasl
	(recompile-py-if-needed (dotted-name mod-name-as-list) py-file fasl-file
				force-reload verbose)
	
	;; Load .fasl
	(let* ((old-module (gethash mod-name-as-list *py-modules*))
	       (new-module nil)
	       (*module-hook* (lambda (mod)
				(setf new-module mod)
				;; Need to register module before it is fully loaded,
				;; otherwise infinite recursion if modules import each other.
				(register-module mod-name-as-list new-module))))
	  (declare (special *module-hook*))
	  
	  (let ((success nil))
	    (unwind-protect
		(progn (load fasl-file :verbose verbose)
		       (setf success t))
	      (unless success
		(unregister-module mod-name-as-list)
		(warn "Loading of module \"~A\" aborted ~@[(within ~A)~]"
		      (dotted-name mod-name-as-list) src-mod))))
	  	  
	  (unless new-module
	    (py-raise '|ImportError|
		      "CLPython bug: Module ~A did not call *module-hook* upon loading"
		      (dotted-name mod-name-as-list)))
	  
	  (if old-module

	      (progn 
		;; Update old module object with info from new one
		;; (slot `name' is assumed to be set correctly)
		(dolist (f '(globals-names globals-values dyn-globals))
		  (setf (slot-value old-module f) (slot-value new-module f)))
		(return-from py-import old-module))
	    
	    (return-from py-import new-module)))))))