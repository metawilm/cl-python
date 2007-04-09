;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython)
(in-syntax *user-readtable*)

(defvar *py-compiled-file-type* "FASL"
  "File types of compiled Python files, in :common pathname case")

(defvar *py-source-file-types* '("PY" "LISP" "CL")
  "File types of source Python files, in :common pathname case")

(defvar *package-indicator-filename* "__INIT__"
  "File that indicates a directory is a package, in :common pathname case")

;; Pathname handling is as suggested by Kent Pitman on comp.lang.lisp
;; <sfwzo21um0k.fsf@shell01.TheWorld.com>

(defun py-source-file-names (modname filepath)
  "Get a list of possible source file names, given module name."
  (check-type modname string)
  (mapcar (lambda (filetype)
	    (make-pathname :type      filetype 
			   :version   nil
			   :name      (pathname-name      modname :case :common) 
			   :host      (pathname-host      filepath :case :common)
			   :device    (pathname-device    filepath :case :common)
			   :directory (pathname-directory filepath :case :common)
			   :case      :common))
	  *py-source-file-types*))

(defun py-pkg-source-file-names (modname filepath)
  "Get a list of possible package directory names, given module name.
The returned pathnames lead tot <modname>/__init__.{py/lisp/..}"
  (check-type modname string)
  (mapcar (lambda (filetype)
	    (merge-pathnames
	     (make-pathname :directory `(:relative ,(pathname-name modname :case :common))
			    :case      :common)
	     (make-pathname :type      filetype 
			    :version   nil
			    :name      *package-indicator-filename* 
			    :host      (pathname-host      filepath :case :common)
			    :device    (pathname-device    filepath :case :common)
			    :directory (pathname-directory filepath :case :common)
			    :case      :common)))
	  *py-source-file-types*))

(defun py-compiled-file-name (modname filepath)
  "Get file name for compiled source file"
  (check-type modname string)
  (make-pathname :type      *py-compiled-file-type*
		 :version   nil
		 :name      (pathname-name modname :case :common)
		 :host      (pathname-host      filepath :case :common)
		 :device    (pathname-device    filepath :case :common)
		 :directory (pathname-directory filepath :case :common)
		 :case      :common))

(defun py-pkg-compiled-file-name (pkgname filepath)
  "Get file name for compiled package __init__ file"
  (check-type pkgname string)
  (merge-pathnames
   (make-pathname :directory `(:relative ,(pathname-name pkgname :case :common))
		  :case      :common)
   (make-pathname :type      *py-compiled-file-type* 
		  :version   nil
		  :name      *package-indicator-filename* 
		  :host      (pathname-host      filepath :case :common)
		  :device    (pathname-device    filepath :case :common)
		  :directory (pathname-directory filepath :case :common)
		  :case      :common)))

(defun lisp-package-as-py-module (modname)
  "Return Lisp package with given name, that is a package-child of :clpython.module
This function build on Allegro's 'relative package names'."
  (check-type modname symbol)
  (let ((*package* :clpython.module))
    (excl::relative-package-name-to-package (concatenate 'string "." (symbol-name modname)))))
  
(defun find-py-file (name search-paths)
  "Returns pathnames of SRC and/or BINARY file found earliest.
Returns (values KIND SRC-PATH BIN-PATH), where KIND one of :module, :package.
Returns NIL if nothing found."
  (loop for path in search-paths
		    
      for src-path = (find-if #'probe-file (py-source-file-names name path))
      for bin-path = (find-if #'probe-file (list (py-compiled-file-name name path)))
      for pkg-src-path = (find-if #'probe-file (py-pkg-source-file-names name path))
      for pkg-bin-path = (find-if #'probe-file (list (py-pkg-compiled-file-name name path)))
			 
      if (or src-path bin-path)
      return (values :module src-path bin-path)
      else if (or pkg-src-path pkg-bin-path)
      return (values :package pkg-src-path pkg-bin-path)))

(defun compile-py-file (filename &key (force-recompile nil)
				      (verbose t)
				      (output-file (error "required")))
  "Compile Python source file into FASL. Source file must exist."
  (check-type filename pathname)
  (assert (probe-file filename) (filename)
    "Python source file ~A does not exist" filename)
  
  (let* ((*current-module-path* filename)) ;; used by compiler
    (declare (special *current-module-path*))
    
    (with-auto-mode-recompile (:verbose verbose)
      (with-python-code-reader ()
	(compile-file filename
		      :output-file output-file
		      :if-newer (not force-recompile)
		      :verbose verbose)))))

(defun load-compiled-python-file (filename
				  &key (mod-name (pathname-name filename))
				       (context-mod-name mod-name)
				       (verbose t)
				       within-mod
				       habitat
				       (update-existing-mod t)
				  &allow-other-keys)
  
  "Loads and registers given compiled Python file.
Returns the loaded module, or NIL on error."
    
  (check-type filename pathname)
  (check-type mod-name (or symbol string))
  (check-type context-mod-name (or symbol string))
  (assert (probe-file filename) (filename)
    "Compiled Python file ~A does not exist" filename)
  
  (let* ((old-module (when habitat (get-known-module mod-name habitat))))
    
    (flet ((do-loading ()
	     (let* ((new-module nil)
		    (*module-hook* (lambda (mod)
				     (setf new-module mod)
				     ;; Need to register module before it is fully loaded,
				     ;; otherwise infinite recursion if two modules import
				     ;; each other.
				     (when habitat
				       (add-known-module mod habitat)))))
	       (declare (special *module-hook*))
	       
	       (with-auto-mode-recompile (:verbose verbose)
		 (let ((*current-module-name* mod-name))
		   (declare (special *current-module-name*))
		   (load filename :verbose verbose)))
	       
	       (assert new-module ()
		 "CLPython bug: module ~A did not call *module-hook* upon loading"
		 mod-name)
	       
	       (when (and old-module update-existing-mod)
		 (copy-module-contents :from new-module :to old-module)
		 (setf new-module old-module))
	       
	       (values new-module t))))
      
      (let (new-module success)
	(unwind-protect
	    
	    (progn
	      (multiple-value-setq (new-module success) (do-loading))
	      (assert success () "Loading went OK, but no success status?")
	      (return-from load-compiled-python-file new-module))
	  
	  (unless success
	    (warn "Loading of module \"~A\" aborted ~@[(within ~A)~]"
		  mod-name within-mod)
	    (when habitat
	      (remove-known-module mod-name habitat))
	    (return-from load-compiled-python-file nil)))))))

(defun parent-package-local-search-path (mod-name-as-list &rest import-options)
  "Path of MOD-NAME-AS-LIST's parent package."
  (assert (> (length mod-name-as-list) 1))
  (let* ((parent-name-list (butlast mod-name-as-list))
	 (parent-mod (apply #'py-import parent-name-list import-options)))
    
    (unless (module-package-p parent-mod)
      (py-raise '{ImportError}
		"Cannot import '~A', as parent '~A' is not a package (it is: ~S)."
		(module-dotted-name mod-name-as-list)
		(module-dotted-name parent-name-list)
		parent-mod))
    
    (let ((parent-path (slot-value parent-mod 'filepath)))
      (assert (typep parent-path 'pathname) (parent-path)
	"Python package ~S should have a pathname as :filepath, but it is: ~S"
	parent-mod parent-path)
      (assert (string= (pathname-name parent-path :case :common)
		       *package-indicator-filename*) ()
	"Package ~A should have filepath pointing to the ~A file, but ~
         it is pointing to somewhere else: ~A."
	parent-mod *package-indicator-filename* parent-path)
      
      (make-pathname :name nil :defaults parent-path :case :common))))

(defun module-dotted-name (list-name)
  (format nil "~{~A~^.~}" list-name))

(defun py-import (mod-name-as-list 
		  &rest options
		  &key (habitat (or *habitat*
				    (make-habitat :search-paths '("."))))
		       force-reload
		       (verbose t)
		       within-mod
		       (search-paths (when habitat (habitat-search-paths habitat)))
		       (add-to-habitat-loaded t))
  "Returns the module, which may have been imported before."
  ;; If WITHIN-MOD is supplied, then the directory of that
  ;; module's .py file is the first directory in the search path. 
  (declare (special *habitat* *py-import-force-reload*)
	   (optimize (debug 3)))
  (check-type mod-name-as-list list)
  (check-type habitat habitat)
  
  ;; Todo: set up precendence rules. 
  (when (= (length mod-name-as-list) 1)
    (whereas ((pkg (lisp-package-as-py-module (car mod-name-as-list))))
      (return-from py-import pkg)))
  
  (unless search-paths
    #+(or)(warn "No search paths specified for import of ~A; current directory will be used."
		mod-name-as-list)
    (setf search-paths '("."))
    
    ;; XXX sys.path is now shared between all habitats; should perhaps be habitat-specific
    (dolist (p (reverse (py-iterate->lisp-list clpython.module.sys:path)))
      (pushnew p search-paths)))
  
  (let* ((just-mod-name (string (car (last mod-name-as-list))))
	 (dotted-name (module-dotted-name mod-name-as-list)))

    ;; Try builtin loaded module first XXX
    (loop for p in (excl:package-children :clpython.modules)
	when (string-equal (package-name p) dotted-name)
	do (return-from py-import p)) 
    
    ;; Module already imported earlier?
    (unless force-reload
      (when habitat
	(whereas ((m (get-loaded-module dotted-name habitat)))
	  (return-from py-import m))))
    
    ;; Reloading built-in module?
    #+(or) ;; todo 
    (when force-reload
      (whereas ((im (get-loaded-module dotted-name habitat))
		(bm (builtin-module mod-name-as-list)))
	;; For now, does not remove user-set attributes
	;;  (import sys; sys.a = 3; reload(sys); sys.a == 3)
	(return-from py-import bm)))

    ;; In case of a dotted import ("import a.b"), search only in parent ("a") directory.
    (when (> (length mod-name-as-list) 1)
      (unless (apply #'py-import (butlast mod-name-as-list)
		     :add-to-habitat-loaded nil options)
	(py-raise '{ImportError}
		  "Could not import module `~A', as importing `~A' failed."
		  dotted-name (module-dotted-name (butlast mod-name-as-list))))
      (setf search-paths (list (parent-package-local-search-path mod-name-as-list))))
    
    (multiple-value-bind (kind src-file bin-file)
	(find-py-file just-mod-name search-paths)
      
      (unless kind
	(py-raise '{ImportError}
		  "Could not find module `~A'. ~_Search paths tried: ~{~S~^, ~}"
		  just-mod-name search-paths))
      
      (assert (member kind '(:module :package)))
      (assert (or src-file bin-file))
      
      (when src-file
	(let ((output-file (py-compiled-file-name just-mod-name src-file)))
	  (compile-py-file src-file :verbose verbose :output-file output-file)
	  (setf bin-file output-file)))
      
      (assert bin-file)
      
      (let* ((new-module (load-compiled-python-file bin-file
						    :mod-name just-mod-name
						    :verbose verbose
						    :within-mod within-mod
						    :habitat habitat)))
	(assert new-module)
	(when (and habitat add-to-habitat-loaded)
	  (add-loaded-module new-module habitat))
	(return-from py-import new-module)))))

(defun path-kind (path)
  "The file kind, which is either :FILE, :DIRECTORY or NIL"
  (assert (stringp path))
  (cond ((not (probe-file path))       nil)
	((excl:file-directory-p path)  :directory)
	(t                             :file)))
