;; -*- package: clpython; readtable: py-user-readtable -*-
;; 
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

(defun derive-pathname (pathname &key (type      (pathname-type pathname      :case :common))
                                      (name      (pathname-name pathname      :case :common))
                                      (host      (pathname-host pathname      :case :common))
                                      (device    (pathname-device pathname    :case :common))
                                      (directory (pathname-directory pathname :case :common))
                                      (version   (pathname-version pathname)))
  (make-pathname :type type :name name :host host :device device
                 :directory directory :version version :case :common))

(defun source-file-names (kind modname filepath)
  (check-type modname string)
  (loop for file-type in *py-source-file-types*
      collect (ecase kind
                (:module (derive-pathname
                          filepath
                          :name (pathname-name modname :case :common) :type file-type))
                (:package (merge-pathnames
                           (make-pathname :directory `(:relative ,(pathname-name modname :case :common))
                                          :case :common)
                           (derive-pathname filepath
                                            :type file-type
                                            :name *package-indicator-filename*))))))

(defparameter *use-asdf-binary-locations* t
  "Whether to store fasl files in directory determined by asdf-binary-locations.")

(defgeneric compiled-file-name (kind modname filepath &key include-dir)
  (:method :around (kind modname filepath &key include-dir)
           (declare (ignore include-dir filepath kind))
           (check-type modname string)
           (let ((bin-path (call-next-method))
                 bin-path-2)
             (when *use-asdf-binary-locations*
               (setf bin-path-2 
                 (ignore-errors
                  (let ((new-path (car (asdf:output-files
                                        (make-instance 'asdf:compile-op)
                                        (make-instance 'asdf:cl-source-file
                                          :parent (asdf:find-system :clpython)
                                          :pathname bin-path)))))
                    (when new-path
                      (setf new-path (ensure-directories-exist new-path)))))))
             (or bin-path-2 bin-path)))
  
  (:method ((kind (eql :module)) modname filepath &key include-dir)
           (declare (ignore include-dir))
           (derive-pathname filepath
                            :name (pathname-name modname :case :common)
                            :type *py-compiled-file-type* ))
  
  (:method ((kind (eql :package)) modname filepath &key (include-dir t))
           (merge-pathnames
            (if include-dir
                (make-pathname :directory `(:relative ,(pathname-name modname :case :common))
                               :case :common)
              modname)
            (derive-pathname filepath
                             :type *py-compiled-file-type* 
                             :name *package-indicator-filename*))))
  
(defun lisp-package-as-py-module (modname)
  "Return Lisp package with given name, that is a package-child of :clpython.module
This function builds upon Allegro's 'relative package names'."
  (check-type modname symbol)
  (let ((*package* :clpython.module))
    (or (excl::relative-package-name-to-package (concatenate 'string "." (symbol-name modname)))
        ;; for ANSI:
        (excl::relative-package-name-to-package
         (concatenate 'string "." (string-upcase (symbol-name modname)))))))

(defun find-py-file (name search-paths)
  "Returns pathnames of SRC and/or BINARY file found earliest.
Returns (values KIND SRC-PATH BIN-PATH), where KIND one of :module, :package.
Returns NIL if nothing found."
  (loop for path in search-paths
		    
      for src-path = (find-if #'probe-file (source-file-names :module name path))
      for bin-path = (find-if #'probe-file (list (compiled-file-name :module name path)))
      for pkg-src-path = (find-if #'probe-file (source-file-names :package name path))
      for pkg-bin-path = (find-if #'probe-file (list (compiled-file-name :package name path)))
			 
      if (or src-path bin-path)
      return (values :module src-path bin-path)
      else if (or pkg-src-path pkg-bin-path)
      return (values :package pkg-src-path pkg-bin-path)))

(defparameter *import-force-recompile* nil)
(defparameter *import-force-reload*    nil)
(defparameter *import-compile-verbose* t)
(defparameter *import-load-verbose*    t)

(defmacro with-python-code-reader (() &body body)
  ;; The Python parser handles all reading.
  `(let ((*readtable* (load-time-value (setup-omnivore-readmacro #'clpython.parser:parse (copy-readtable nil)))))
     ,@body))

(defun compile-py-file (filename &key (mod-name (error ":mod-name required"))
                                      (output-file (error ":output-file required")))
  "Compile Python source file into FASL. Source file must exist."
  (check-type filename pathname)
  (assert (probe-file filename) (filename)
    "Python source file ~A does not exist" filename)
  
  (let* ((*current-module-path* filename) ;; used by compiler
         (*current-module-name* mod-name))
    (declare (special *current-module-path*))
    
    (with-auto-mode-recompile (:verbose *import-compile-verbose*)
      (with-python-code-reader ()
	(compile-file filename
		      :output-file output-file
		      :if-newer (not *import-force-recompile*)
		      :verbose *import-compile-verbose*)))))

(defun load-compiled-python-file (filename
				  &key (mod-name (error ":mod-name required"))
                                       #+(or)(dotted-name (error ":dotted-name required")) 
				       (context-mod-name mod-name)
                                       within-mod
				       (habitat (error "habitat required"))
				       (update-existing-mod t)
				  &allow-other-keys)
  
  "Loads and registers given compiled Python file.
Returns the loaded module, or NIL on error."
  (check-type filename pathname)
  (check-type mod-name (or symbol string))
  (check-type context-mod-name (or symbol string))
  (assert (probe-file filename) (filename)
    "Compiled Python file ~A does not exist" filename)
    
  (when (get-loaded-module mod-name habitat)
    (break "Module ~A already known, but imported again (file ~A)" mod-name filename))
         
  (let* ((old-module (when habitat (get-known-module mod-name habitat))))
    (flet ((do-loading ()
	     (let* (new-module
                    loaded-okay
		    (*module-hook* (lambda (mod)
				     (setf new-module mod)
                                     ;; Need to register module before it is fully loaded,
				     ;; otherwise infinite recursion if two modules import
				     ;; each other.
                                     #+(or)(warn "Register module ~A as ~A in habitat ~A"
                                                 mod (module-name mod) habitat)
                                     (add-loaded-module mod habitat))))
	       (declare (special *module-hook*))
               (with-auto-mode-recompile (:verbose *import-load-verbose*)
		 (let ((*current-module-name* mod-name))
		   (declare (special *current-module-name*))
		   (setf loaded-okay (load filename :verbose *import-load-verbose*))))
               (unless loaded-okay
                 (return-from do-loading (values nil nil)))
               (unless new-module
		 (break "CLPython bug: module ~A did not call *module-hook* upon loading"
                        mod-name))
               (when (and old-module update-existing-mod)
		 (copy-module-contents :from new-module :to old-module)
		 (setf new-module old-module))
               (values new-module t))))
      
      (let (new-module success)
        (multiple-value-setq (new-module success) (do-loading))
        (if success
            (return-from load-compiled-python-file new-module)
          (progn
            (warn "Loading of module \"~A\" failed~@[ (within ~A)~]."
                  mod-name within-mod)
            (remove-loaded-module mod-name habitat)
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
         it is pointing somewhere else: ~A."
	parent-mod *package-indicator-filename* parent-path)
      (make-pathname :name nil :defaults parent-path :case :common))))

(defun module-dotted-name (list-name)
  (format nil "~{~A~^.~}" list-name))

(defun py-import (mod-name-as-list 
		  &rest options
		  &key (habitat (or *habitat* (error "PY-IMPORT called without habitat")))
		       (force-reload *import-force-reload*)
                       within-mod
		       (search-paths (when habitat (habitat-search-paths habitat))))
  "Returns the module, which may have been imported before."
  
  ;; If WITHIN-MOD is supplied, then the directory of that
  ;; module's .py file is the first directory in the search path.
  (declare (special *habitat*)
           (optimize (debug 3)))
  (check-type mod-name-as-list list)
  (check-type habitat habitat)

  ;; 1. Lisp module package, as child of `clpython.module'
  (when (= (length mod-name-as-list) 1)
    (whereas ((pkg (lisp-package-as-py-module (car mod-name-as-list))))
      (return-from py-import pkg)))
  
  (let* ((just-mod-name (string (car (last mod-name-as-list))))
         (dotted-name (module-dotted-name mod-name-as-list)))
    
    ;; 2. (Dotted) Module already imported into habitat
    (unless (and nil force-reload)
      (whereas ((m (get-loaded-module dotted-name habitat)))
        (return-from py-import m)))
    
    (unless search-paths
      ;; Use current directory and `sys.path' as search paths
      (setf search-paths '("."))
      ;; XXX sys.path is now shared between all habitats; should perhaps be habitat-specific
      (when (find-package :clpython.module.sys)
        (setf search-paths (nconc (reverse (py-iterate->lisp-list 
                                            (symbol-value
                                             (find-symbol "path" :clpython.module.sys))))
                                  search-paths))))
    
    ;; In case of a dotted import ("import a.b"), search only in parent ("a") directory.
    (when (> (length mod-name-as-list) 1)
      (unless (apply #'py-import (butlast mod-name-as-list) options)
        (py-raise '{ImportError}
                  "Could not import module `~A', as importing `~A' failed."
                  dotted-name (module-dotted-name (butlast mod-name-as-list))))
      (setf search-paths (list (parent-package-local-search-path mod-name-as-list))))
    
    ;; 3. Find a source or binary file somewhere in the collection of search paths
    (multiple-value-bind (kind src-file bin-file)
        (let ((find-paths search-paths))
          (when within-mod
            (if (module-filepath within-mod)
                (push (module-filepath within-mod) find-paths)
              ;; Module __main__ is allowed to not have a module path. (Fix?)
              (assert (string= (module-name within-mod) "__main__"))))
          (find-py-file just-mod-name find-paths))
      (unless kind
        (py-raise '{ImportError}
                  "Could not find module `~A'. Search paths tried: ~{~S~^, ~}"
                  just-mod-name search-paths))
      (assert (member kind '(:module :package)))
      (assert (or src-file bin-file))

      (let ((bin-file (compiled-file-name kind just-mod-name src-file :include-dir nil)))
        (compile-py-file src-file :mod-name dotted-name :output-file bin-file)
        (let ((new-module (load-compiled-python-file bin-file
                                                     :mod-name dotted-name
                                                     :within-mod within-mod
                                                     :habitat habitat)))
          (when new-module ;; Maybe loading failed (which already gave a warning)
            (add-loaded-module new-module habitat)
            (return-from py-import new-module)))))))

(defun path-kind (path)
  "The file kind, which is either :FILE, :DIRECTORY or NIL"
  (assert (stringp path))
  (cond ((not (probe-file path))       nil)
	((excl:file-directory-p path)  :directory)
	(t                             :file)))
