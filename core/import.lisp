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

(defgeneric compiled-file-name (kind modname filepath &key include-dir create-dir)
  (:method :around (kind modname filepath &key include-dir create-dir)
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
                    (when (and new-path create-dir)
                      (setf new-path (ensure-directories-exist new-path)))))))
             (or bin-path-2 bin-path)))
  
  (:method ((kind (eql :module)) modname filepath &key include-dir create-dir)
           (declare (ignore include-dir create-dir))
           (derive-pathname filepath
                            :name (pathname-name modname :case :common)
                            :type *py-compiled-file-type* ))
  
  (:method ((kind (eql :package)) modname filepath &key (include-dir t) create-dir)
           (declare (ignore create-dir))
           (merge-pathnames
            (if include-dir
                (make-pathname :directory `(:relative ,(pathname-name modname :case :common))
                               :case :common)
              modname)
            (derive-pathname filepath
                             :type *py-compiled-file-type* 
                             :name *package-indicator-filename*))))
  
(defun lisp-package-as-py-module (modname)
  "Return Lisp package with given name.
First a package with that name as subpackage of CLPYTHON.MODULE is searched.
Otherwise a package with MODNAME as regular name.
As for case: both MODNAME's own name its upper-case variant are tried."
  (check-type modname symbol)
  (let ((variants (list (symbol-name modname) (string-upcase modname))))
    (dolist (v variants)
      (whereas ((pkg (or (find-package (concatenate 'string
                                         (string (package-name :clpython.module)) "." v))
                         (find-package v))))
        (return-from lisp-package-as-py-module pkg)))))

(defun find-py-file (name search-paths &key must-be-package)
  "Returns pathnames of SRC and/or BINARY file found earliest.
Returns (values KIND SRC-PATH BIN-PATH), where KIND one of :module, :package.
Returns NIL if nothing found."
  (loop for path in search-paths
      for src-path = (find-if #'probe-file (source-file-names :module name path))
      for bin-path = (find-if #'probe-file (list (compiled-file-name :module name path)))
      for pkg-src-path = (find-if #'probe-file (source-file-names :package name path))
      for pkg-bin-path = (find-if #'probe-file (list (compiled-file-name :package name path)))
			 
      if (and (not must-be-package)
              (or src-path bin-path))
      return (values :module src-path bin-path)
      else if (or pkg-src-path pkg-bin-path)
      return (values :package pkg-src-path pkg-bin-path)))

(defparameter *import-force-recompile* nil)
(defparameter *import-force-reload*    nil)
(defparameter *import-compile-verbose* t)
(defparameter *import-load-verbose*    t)

(defmacro with-python-code-reader (() &body body)
  ;; The Python parser handles all reading.
  `(handler-bind
       ((error (lambda (c)
                 (signal c)
                 ;; ERROR with normal *readtable*, otherwise interactive
                 ;; debugging becomes impossible.
                 (with-standard-io-syntax (error c)))))
     (let ((*readtable* (load-time-value 
                         (setup-omnivore-readmacro #'clpython.parser:parse (copy-readtable nil)))))
       ,@body)))

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
                      #+allegro #+allegro :if-newer (not *import-force-recompile*)
                      :verbose *import-compile-verbose*)))))

(defun load-compiled-python-file (filename
				  &key (mod-name (error ":mod-name required"))
                                       #+(or)(dotted-name (error ":dotted-name required")) 
				       (context-mod-name mod-name)
                                       within-mod-path
				       (habitat (error "habitat required"))
				       (update-existing-mod t)
                                       src-file
				  &allow-other-keys)
  
  "Loads and registers given compiled Python file.
Returns the loaded module, or NIL on error."
  (check-type filename pathname)
  (check-type mod-name (or symbol string))
  (check-type context-mod-name (or symbol string))
  (assert (probe-file filename) (filename)
    "Compiled Python file ~A does not exist" filename)
  
  #+(or)(when (get-loaded-module mod-name habitat)
          (break "Module ~A already known, but imported again (file ~A)" mod-name filename))
         
  (let* ((old-module (when habitat (get-known-module mod-name habitat))))
    (flet ((do-loading ()
	     (let* (new-module
                    module-function
                    (*module-function* (lambda (f) (setf module-function f)))
		    (*module-preload-hook* (lambda (mod)
                                             (setf new-module mod)
                                             ;; Need to register module before it is fully loaded,
                                             ;; otherwise infinite recursion if two modules import
                                             ;; each other.
                                             #+(or)(warn "Register module ~A as ~A in habitat ~A"
                                                         mod (module-name mod) habitat)
                                             (add-loaded-module mod habitat))))
	       (declare (special *module-preload-hook* *module-function*))
               (with-auto-mode-recompile (:verbose *import-load-verbose*)
		 (let ((*current-module-name* mod-name))
		   (declare (special *current-module-name*))
		   (unless (load filename :verbose *import-load-verbose*)
                     ;; Might happen if loading fails and there is a restart that lets LOAD return NIL.
                     (return-from do-loading))))
               (if module-function
                   (progn (funcall module-function) ;; Execute the module toplevel forms
                          ;; XXX call module-function with :%module-globals if updating existing mod
                          (unless new-module
                            (break "CLPython bug: module ~A did not call *module-preload-hook* upon loading"
                                   mod-name))
                          (when (and old-module update-existing-mod)
                            (copy-module-contents :from new-module :to old-module)
                            (setf new-module old-module))
                          (values new-module t))
                 (progn (warn "The FASL of module ~A was not produced by CLPython ~
                               (or CLPython bug: module did not call *module-function*)." mod-name)
                        (values (make-instance 'module :name "non-Python fasl" :path filename)
                                t))))))
      (let (new-module success)
        (unwind-protect
            (progn (multiple-value-setq (new-module success)
                     (do-loading))
                   (assert success) ;; Second value is sanity check
                   new-module)
          (unless success
            (warn "Loading of module `~A' was aborted.~@[~:@_Source: ~A~]~@[~:@_Binary: ~A~]~@[~:@_Imported by: ~A~]"
                  mod-name src-file filename within-mod-path)
            (remove-loaded-module mod-name habitat)))))))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp 'cl-user::*clpython-module-search-paths*)
    (defvar cl-user::*clpython-module-search-paths* ()
      "Default search paths for imported modules. Should at least contain the location
of the Python stanard libraries. (This variable is in the CL-USER package to allow
it being set before CLPython is loaded, e.g. in a Lisp configuration file.)")))

(defun maybe-warn-set-search-paths (at-error)
  (cond ((or (not (boundp 'cl-user::*clpython-module-search-paths*))
             (not cl-user::*clpython-module-search-paths*))
         (warn "Please customize variable ~S to be a list of paths that are tried ~
\(in addition to the current directory and `sys.path') when locating a module in order to import it. ~
Typically it should at least contain the path to the Python (2.5) standard libraries, ~
as those are not distributed with CLPython."
               'cl-user::*clpython-module-search-paths*))
        ((not at-error)
         (format t "Using ~A default module search paths set in ~S~%"
                 (length cl-user::*clpython-module-search-paths*)
                 'cl-user::*clpython-module-search-paths*))))

(defun py-import (mod-name-as-list 
		  &rest options
		  &key must-be-package
                       (outer-mod-name-as-list mod-name-as-list)
                       (habitat (or *habitat* (error "PY-IMPORT called without habitat")))
		       (force-reload *import-force-reload*)
                       within-mod-path
		       (search-paths (when habitat (habitat-search-paths habitat))))
  "Returns the module (which may have been freshly imported) or raises an error."
  ;; If WITHIN-MOD-PATH is supplied, then the directory of that
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
    (unless force-reload
      (whereas ((m (get-loaded-module dotted-name habitat)))
        (return-from py-import m)))
    
    (unless search-paths
      ;; Use current directory and `sys.path' as search paths (in that order)
      ;; XXX sys.path is now shared between all habitats; should perhaps be habitat-specific
      (setf search-paths (py-iterate->lisp-list 
                          (symbol-value (find-symbol "path" :clpython.module.sys))))
      (push "." search-paths)
      (setf search-paths (nconc search-paths cl-user::*clpython-module-search-paths*)))
    
    ;; In case of a dotted import ("import a.b"), search only in parent ("a") directory.
    (when (> (length mod-name-as-list) 1)
      (let (success)
        (unwind-protect
            (setf success (apply #'py-import (butlast mod-name-as-list)
                                 :must-be-package t
                                 :outer-mod-name-as-list outer-mod-name-as-list
                                 options))
          (unless success
            ;; PY-IMPORT did not import successfully, and we are unwinding.
            ;; No need for an additional ImportError.
            (warn "Could not import module `~A' as importing package `~A' failed.~@[~:@_Import attempted by: ~A~]"
                  dotted-name (module-dotted-name (butlast mod-name-as-list))
                  within-mod-path))))
      (setf search-paths (list (parent-package-local-search-path mod-name-as-list))))
    
    ;; 3. Find a source or binary file somewhere in the collection of search paths
    (multiple-value-bind (kind src-file bin-file)
        (let ((find-paths search-paths))
          (when within-mod-path
            (push within-mod-path find-paths))
          (find-py-file just-mod-name find-paths :must-be-package must-be-package))
      (unless kind
        (maybe-warn-set-search-paths t)
        (py-raise '{ImportError}
                  "Could not find ~A `~A'.~:@_Search paths tried: ~{~S~^, ~}~@[~:@_Import ~
                   of `~A' attempted by: ~A~]"
                  (if must-be-package "package" "module/package")
                  just-mod-name search-paths
                  (when within-mod-path (module-dotted-name outer-mod-name-as-list))
                  within-mod-path))
      (assert (member kind '(:module :package)))
      (assert (or src-file bin-file))

      (when src-file
        (setf bin-file (compiled-file-name kind just-mod-name src-file :include-dir nil :create-dir t))
        ;; This would be a good place for a "try recompiling" restart,
        ;; but implementations have that already. :)
        (compile-py-file src-file :mod-name dotted-name :output-file bin-file))
      
      (flet ((delete-fasl-try-again ()
               (declare (ignore c))
               (delete-file bin-file)
               (return-from py-import (apply #'py-import mod-name-as-list options))))
        
      (restart-bind ((delete-fasl-try-again #'delete-fasl-try-again
                         :test-function (lambda (c)
                                          (declare (ignore c))
                                          (and (probe-file src-file) (probe-file bin-file)))
                         :report-function (lambda (stream)
                                            (format stream "Recompile module `~A' file ~A" dotted-name src-file))))
        
        (let ((new-module (load-compiled-python-file bin-file
                                                     :mod-name dotted-name
                                                     :within-mod-path within-mod-path
                                                     :habitat habitat
                                                     :src-file src-file)))
          (when new-module ;; Maybe loading failed (which already gave a warning)
            (add-loaded-module new-module habitat)
            new-module)))))))
  
(defun directory-p (pathname)
  (check-type pathname pathname)
  #+allegro (excl:file-directory-p pathname)
  #+lispworks (lispworks:file-directory-p pathname)
  #+(or cmu sbcl) (null (pathname-type pathname))
  #-(or allegro cmu lispworks sbcl) (error "TODO: No DIRECTORY-P for this implementation."))

(defun path-kind (path)
  "The file kind, which is either :FILE, :DIRECTORY or NIL"
  (check-type path string)
  (whereas ((pathname (probe-file path)))
    (if (directory-p pathname) :directory :file)))
