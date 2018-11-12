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

(defvar *compile-for-import* t
  "Whether source files are compiled before being run")
    
(defvar *py-compiled-file-type* "FASL"
  "File types of compiled Python files, in :common pathname case")

(defvar *py-source-file-types* '("PY" "LISP" "CL")
  "File types of source Python files, in :common pathname case")

(defvar *package-indicator-filename* "__INIT__"
  "File that indicates a directory is a package, in :common pathname case")

;; Pathname handling is as suggested by Kent Pitman on comp.lang.lisp
;; <sfwzo21um0k.fsf@shell01.TheWorld.com>

(defun %get-py-file-name (kind modname filepath type &key (case :common))
  #+sbcl
  (when (member type *py-source-file-types*)
    ;; For SBCL don't use :COMMON as that gives case problems; use :LOCAL instead.
    ;; https://github.com/metawilm/cl-python/issues/1
    ;; https://github.com/metawilm/cl-python/pull/20
    ;; https://bugs.launchpad.net/sbcl/+bug/695486
    (setf case :local
          type (string-downcase type)))
  
  (ecase kind
    (:module (derive-pathname filepath
                              :name (pathname-name modname :case case)
                              :type type
                              :case case))
    (:package (merge-pathnames
               (make-pathname :directory `(:relative ,(pathname-name modname :case case))
                              :case case)
               (derive-pathname filepath
                                :type type
                                :name
                                #+sbcl (string-downcase *package-indicator-filename*)
                                #-sbcl *package-indicator-filename*
                                :case case)))))
  
(defun source-file-names (kind modname filepath)
  (check-type modname string)
  (mapcar (lambda (type)
            (%get-py-file-name kind modname filepath type))
          *py-source-file-types*))

(defparameter *use-asdf-fasl-locations* t
  "Whether to store fasl files in the place determined by ASDF.
In particular, asdf-binary-locations is used if available.")

(defgeneric compiled-file-name (kind modname filepath)
  (:method :around (kind modname filepath)
           (declare (ignorable filepath))
           (check-type modname string)
           (let* ((bin-path (call-next-method))
                  (asdf-path (when *use-asdf-fasl-locations*
                               (ignore-errors
                                (let ((path (car (asdf:output-files
                                                  (asdf:make-operation 'asdf:compile-op)
                                                  (make-instance 'asdf:cl-source-file
                                                    :parent (asdf:find-system :clpython)
                                                    :pathname bin-path)))))
                                  (when path
                                    (ensure-directories-exist path))))))
                  (result-path (or asdf-path bin-path)))
             (unless (file-writable-p result-path)
               ;; Fall back to using a temporary file.
               (setf result-path
                 (get-temporary-file :key (list kind modname filepath)
                                     :filename-items (list "clpython" modname "fasl"))))
             result-path))
  
  (:method (kind modname filepath)
           (%get-py-file-name kind modname filepath *py-compiled-file-type*)))

(defparameter *temp-file-map* (make-hash-table :test 'equal)
  "Mapping from filename-items to corresponding temporary fasl file.")

(defun get-temporary-file (&key key filename-items)
  "Returns the pathname of a new temporary file.
When FILENAME-ITEMS is (:A :B :C) result could look like #p'/tmp/clpython-A.B.C-XYZ123'
Might signal TEMPORARY-FILE:CANNOT-CREATE-TEMPORARY-FILE"
  (whereas ((file-name (gethash key *temp-file-map*)))
           (return-from get-temporary-file file-name))
  (let ((file-stream (cl-fad:open-temporary :template (format nil "TEMPORARY-FILES:~{~A~^-~}-%" filename-items)
					    :direction :output)))
    (prog1 (setf (gethash key *temp-file-map*) (pathname file-stream))
      (close file-stream))))

(defun file-writable-p (f)
  (handler-case
      (and (with-open-file (file f :direction :output :if-exists :supersede)
             (declare (ignorable file))
             t)
           ;; Deleting the file is important, as e.g. Allegro happily loads
           ;; zero-length fasl files, leading to bogus Python import behavior.
           (delete-file f)
           (prog1 t ;; DELETE-FILE returns T, or FILE-ERROR
             (cached-probe-file f t)))
    (file-error () nil)
    ;; I don't expect errors other than FILE-ERROR, but who knows.
    (error () nil)))

(defun lisp-package-as-py-module (modname)
  "Return Lisp package with given name.
First a package with that name as subpackage of CLPYTHON.MODULE is searched.
Otherwise a package with MODNAME as regular name.
As for case: both MODNAME's own name its upper-case variant are tried."
  (check-type modname symbol)
  (let ((variants (list (symbol-name modname) (string-upcase modname))))
    (dolist (v variants)
      (whereas ((pkg (find-package (concatenate 'string
                                     (string (package-name :clpython.module)) "." v))))
        (return-from lisp-package-as-py-module pkg)))))

(defun find-py-file (name search-paths &key (allow-bin t))
  "Finds for source or fasl file in SEARCH-PATHS, returning earliest match.
Returns values KIND SRC-PATH BIN-PATH FIND-PATH, or NIL if not found,
with: KIND one of :module, :package
      SRC-PATH, BIN-PATH the truenames of the existing files
      FIND-PATH the directory in which the files were found."
  (flet ((probe-src (fname) 
           (cached-probe-file fname))
         (probe-bin (fname)
           (whereas ((path (cached-probe-file fname)))
             (and #+allegro (excl::check-fasl-magic path nil)
                  path))))
    ;; Ignore non-existent directories
    (setf search-paths (remove-if-not #'cached-probe-file search-paths))

    ;; Add missing directory slashes at the end
    (map-into search-paths #'ensure-path-is-directory search-paths)
    
    (loop for path in search-paths
        for src-path = (find-if #'probe-src (source-file-names :module name path))
        for bin-path = (and allow-bin (find-if #'probe-bin (list (compiled-file-name :module name path))))
        for pkg-src-path = (find-if #'probe-src (source-file-names :package name path))
        for pkg-bin-path = (and allow-bin (find-if #'probe-bin (list (compiled-file-name :package name path))))
			 
        if (or src-path bin-path)
        return (values :module
                       (when src-path (probe-src src-path))
                       (when bin-path (probe-bin bin-path))
                       path)
        else if (or pkg-src-path pkg-bin-path)
        return (values :package
                       (when pkg-src-path (probe-src pkg-src-path))
                       (when pkg-bin-path (probe-bin pkg-bin-path))
                       path))))

(defparameter *import-force-recompile* nil)
(defparameter *import-force-reload*    nil)

(defun %compile-py-file (filename &key (mod-name (error ":mod-name required"))
                                       (output-file (error ":output-file required")))
  "Compile Python source file into FASL. Source file must exist.
Caller is responsible for deciding if recompiling is really necessary."
  (check-type mod-name string)
  (check-type filename pathname)
  (check-type output-file pathname)
  (assert (cached-probe-file filename) (filename)
    "Python source file ~A does not exist" filename)
  (compile-py-source-file :filename filename :mod-name mod-name :output-file output-file)
  (cached-probe-file output-file t))

(defun load-python-file (filename
                         &key (mod-name (error ":mod-name required"))
                              (habitat (error "habitat required"))
                         &aux (*habitat* habitat))
  "Loads and registers given compiled Python file, which can be either a Lisp source or fasl file.
Returns the (updated) loaded module, or NIL on load error."
  (check-type mod-name (or symbol string))
  (assert (cached-probe-file filename t))

  #+sbcl ;; SBCL only forces loading as fasl, if extension is _lower-case_ 'fasl' (2009.11.26)
  (when (string= (pathname-type filename) *py-compiled-file-type*)
    (setf filename
      (merge-pathnames (make-pathname :type (string-downcase *py-compiled-file-type*)) filename)))
  
  (load-python-file-with-hooks 
   :filename filename
   :on-pre-import (lambda (%module %new-module-p)
                    (declare (ignore %new-module-p))
                    (add-loaded-module %module habitat))
   :on-import-success (lambda (%module %new-module-p %source-func %source)
                        (module-import-post %module %new-module-p %source-func %source))
   :on-import-fail (lambda (%module %new-module-p)
                     (when %new-module-p
                       ;; It's a hack that there are two places:
                       (remove-loaded-module %module habitat)
                       (remhash %module *all-modules*)))))

(defun module-import-post #1=(module new-module-p source-func source)
  (declare (ignorable . #1#))
  #+clpython-source-level-debugging
  (progn
    ;; Allegro looks for the .fasl file right next to the source file. When using e.g.
    ;; asdf-binary-locations this assumption does not hold, and Allegro fails to load
    ;; the source information. By explicitly loading the info from the fasl now,
    ;; there's no need for Allegro to look for the fasl file anymore.
    (excl::load-source-debug-info bin-filename)
    #+(or) ;; debug
    (let ((*print-level* 3))
      (excl::dump-lisp-source source-func :terse t))
    ;; This REGISTER-... cannot happen during LOAD of the fasl file,
    ;; as source information only becomes available after the file is loaded.
    (register-python-module-source :module-function-name source-func
                                   :source-path (module-src-pathname module)
                                   :source source)))

(defun module-dotted-name (list-name)
  (format nil "~{~A~^.~}" list-name))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (without-redefinition-warnings
   (defvar cl-user::*clpython-module-search-paths* nil
     "Default search paths for imported modules. Should at least contain the location
of the Python stanard libraries. (This variable is in the CL-USER package to allow
it being set before CLPython is loaded, e.g. in a Lisp configuration file.)")))
  
(defun maybe-warn-set-search-paths (at-error)
  (cond ((or (not (boundp 'cl-user::*clpython-module-search-paths*))
             (not cl-user::*clpython-module-search-paths*))
         (format t "~%;; Consider customizing variable ~S
;; to be a list of paths that are tried when locating a module in order to import it
;; (in addition to the current directory and `sys.path'). Typically it should at least
;; contain the path to the Python 2.x standard libraries, as those are not
;; distributed with CLPython.
"
               'cl-user::*clpython-module-search-paths*))
        ((not at-error)
         (format t "Using ~A default module search paths set in ~S~%"
                 (length cl-user::*clpython-module-search-paths*)
                 'cl-user::*clpython-module-search-paths*))))

(defun calc-import-search-paths (habitat)
  ;; Use current directory and `sys.path' as search paths (in that order)
  ;; XXX sys.path is now shared between all habitats; should perhaps be habitat-specific
  (check-type cl-user::*clpython-module-search-paths* list)
  (append (py-iterate->lisp-list (habitat-search-paths habitat))
          cl-user::*clpython-module-search-paths*))

(defvar *import-recompiled-files* nil
  "The fasl files created by the current (parent) import action.
Used to avoid infinite recompilation/reloading loops.")

(defvar *import-probe-file-cache* nil
  "Cache for probe-file results, to avoid repeating the fairly expensive PROBE-FILE
operation on the same path again and again during one import action.")

(defun make-probe-file-cache ()
  (make-hash-table :test 'equal))

(defun delete-probe-file-entry (name)
  (remhash name *import-probe-file-cache*))

(defun cached-probe-file (name &optional update-cache)
  (unless *import-probe-file-cache*
    (return-from cached-probe-file (careful-probe-file name)))
  (unless update-cache
    (multiple-value-bind (value found-p)
        (gethash name *import-probe-file-cache*)
      (when found-p
        (return-from cached-probe-file value))))
  (setf (gethash name *import-probe-file-cache*)
    (careful-probe-file name)))

(defparameter *__main__-module-name* "__main__")

(defun py-import (mod-name-as-list 
		  &rest options
		  &key (habitat (or *habitat* (error "PY-IMPORT called without habitat")))
                       (force-reload *import-force-reload*)
                       (force-recompile *import-force-recompile*)
                       within-mod-path
                       within-mod-name
		       (search-paths (calc-import-search-paths habitat))
                       (%outer-mod-name-as-list mod-name-as-list)
                       (*import-recompiled-files* (or *import-recompiled-files*
                                                      (make-hash-table :test 'equal)))
                       (if-not-found-value :not-found if-not-found-p)
                  &aux (*import-probe-file-cache* (or *import-probe-file-cache*
                                                      (make-probe-file-cache))))
  "Returns the (sub)module, which may have been freshly imported.
If module could not be found and IF-NOT-FOUND is specified, that value is returned.
Otherwise raises ImportError."
  (declare (special *habitat*)
           (optimize (debug 3)))
  (check-type mod-name-as-list list)
  (check-type habitat habitat)
  (unless (string= within-mod-name *__main__-module-name*)
    (assert (eq (null within-mod-path) (null within-mod-name))
        () "Supply both or neither: within-mod-path=~S within-mod-name=~S eqMain=~S"
        within-mod-path within-mod-name))
  (loop
    (catch 'py-import-retry

      ;; Builtin modules are represented by Lisp module child packages of clpython.module
      ;; (e.g. clpython.module.sys).
      (when (= (length mod-name-as-list) 1)
        (whereas ((pkg (lisp-package-as-py-module (car mod-name-as-list))))
          (return-from py-import (values pkg :lisp-package))))
  
      (let* ((just-mod-name (string (car (last mod-name-as-list))))
             (dotted-name (module-dotted-name mod-name-as-list)) )
    
        ;; If name already registered in sys.modules, return it.
        ;; (Example: os.py sets the "os.path" entry)
        ;; XXX this should not happen for e.g. module "dist", as that is in either "distutils.dist" or "setuptools.dist".
        ;; Need to figure out the exact rule; use this dot check for now.
        (unless force-reload
          (whereas ((mod (with-py-dict (gethash dotted-name (find-symbol-value '#:|modules| :clpython.module.sys)))))
            (return-from py-import (values mod :sys.modules))))
           
        ;; In case of a dotted import ("import a.b.c"), recursively import the parent "a.b"
        ;; and set the search path to only the location of that package.
        (when (> (length mod-name-as-list) 1)
          (let (parent)
            (unwind-protect
                (setf parent (apply #'py-import (butlast mod-name-as-list)
                                    :%outer-mod-name-as-list %outer-mod-name-as-list
                                    options))
              (unless parent
                ;; PY-IMPORT did not import successfully, and we are unwinding.
                ;; No need for an additional ImportError.
                (warn "Could not import module `~A' as importing package `~A' failed.~@[ ~:@_Import attempted by: ~A~]"
                      dotted-name (module-dotted-name (butlast mod-name-as-list))
                      within-mod-path)))
            (unless (module-package-p parent)
              (py-raise '{ImportError}
                        "Cannot import '~A', as parent '~A' is not a package (it is: ~S)."
                        (module-dotted-name mod-name-as-list)
                        (module-dotted-name (butlast mod-name-as-list))
                        parent))
            (let ((parent-path (module-src-pathname parent)))
              (check-type parent-path pathname)
              (assert (string= (pathname-name parent-path :case :common)
                               *package-indicator-filename*) ()
                "Package ~A should have filepath pointing to the ~A file, but ~
             it is pointing somewhere else: ~A." parent *package-indicator-filename* parent-path)
              (setf search-paths
                (list (derive-pathname parent-path :type nil :name nil :version nil))))))
    
        (let ((find-paths search-paths))
          (when within-mod-path
            (let ((parent-directory (derive-pathname within-mod-path :type nil :name nil :version nil)))
              (push (truename parent-directory) find-paths)))
          (setf find-paths (remove-duplicates find-paths :test 'equal))
      
          ;; Find the source or fasl file somewhere in the collection of search paths
          (multiple-value-bind (kind src-file bin-file find-path)
              (find-py-file just-mod-name find-paths :allow-bin *compile-for-import*)
            (let* ((sub-module-p (and within-mod-name 
                                      within-mod-path
                                      (string/= within-mod-name *__main__-module-name*)
                                      (whereas ((within-mod (with-py-dict
                                                                (gethash within-mod-name
                                                                         (find-symbol-value '#:|modules| :clpython.module.sys)))))
                                               (let ((within-src-pathname (module-src-pathname within-mod)))
                                                 (and (eq t (module-package-p within-mod)) ;; not "maybe"
                                                      within-src-pathname
                                                      src-file
                                                      (equal (pathname-device within-src-pathname)
                                                             (pathname-device src-file))
                                                      (equal (pathname-host within-src-pathname)
                                                             (pathname-host src-file))
                                                      (equal (pathname-directory within-src-pathname)
                                                             (pathname-directory src-file)))))))
                   (new-mod-dotted-name (if sub-module-p
                                            (concatenate 'string within-mod-name "." dotted-name)
                                          dotted-name)))
              (unless kind
                (when if-not-found-p
                  (return-from py-import (values if-not-found-value :not-found-value)))
                
                ;; For "import XYZ", if XYZ names a package, import it. Note that a file named XYZ.py
                ;; on the search path takes precedence.
                (when (= (length mod-name-as-list) 1)
                  (whereas ((pkg (find-package (symbol-name (car mod-name-as-list)))))
                           (return-from py-import (values pkg :lisp-package))))
                
                (maybe-warn-set-search-paths t)
                (py-raise '{ImportError}
                          "Could not find module `~A'. ~:@_Search paths tried: ~{~S~^, ~_~}~@[ ~:@_Import ~
                     of `~A' attempted by: ~A~]"
                          just-mod-name search-paths
                          (when within-mod-path (module-dotted-name %outer-mod-name-as-list))
                          within-mod-path))
              
              (assert (member kind '(:module :package)))
              (if *compile-for-import*
                  (assert (or src-file bin-file))
                (assert src-file))
              (assert find-path)
              
              ;; If we have a source file, then recompile fasl if outdated.
              (when (and force-recompile (not src-file))
                (warn "Requested recompilation of ~A can not be performed: no source file available."
                      bin-file))
              (when src-file
                (setf bin-file (compiled-file-name kind just-mod-name find-path))
                (let ((need-recompile (and (not (gethash bin-file *import-recompiled-files*))
                                           (or force-recompile
                                               (not (cached-probe-file bin-file))
                                               (< (file-write-date bin-file) (file-write-date src-file))
                                               (with-open-file (f bin-file :direction :input) ;; just created temp output file
                                                 (zerop (file-length f)))))))
                  (when need-recompile
                    ;; This would be a good place for a "try recompiling" restart,
                    ;; but implementations tend to provide that already.
                    (cond (*compile-for-import*
                           (%compile-py-file src-file :mod-name new-mod-dotted-name :output-file bin-file)
                           (setf (gethash bin-file *import-recompiled-files*) t))
                          (t
                           (setf bin-file (pathname (concatenate 'string (namestring src-file) ".lisp")))
                           (unless (file-writable-p bin-file)
                             ;; Fall back to using a temporary file.
                             (setf bin-file (get-temporary-file :key (list kind just-mod-name src-file)
                                                                :filename-items (list "clpython" just-mod-name "lisp"))))
                           (format t ";; Parsing ~S into ~S~%" src-file bin-file)
                           (compile-py-source-file-to-lisp-source :filename src-file 
                                                                  :output-file bin-file))))))
              
              (when *compile-for-import*
                ;; Now we have an up-to-date fasl file.
                (assert (and bin-file (cached-probe-file bin-file t)))
                (when src-file
                  (assert (>= (file-write-date bin-file) (file-write-date src-file)))))
              
              ;; If current fasl file was already imported, then return its module.
              (unless force-reload
                (whereas ((m (get-loaded-module :bin-pathname bin-file
                                                :bin-file-write-date (file-write-date bin-file)
                                                :habitat habitat)))
                         (return-from py-import (values m :already-imported-not-force-reload))))
              
              (let ((new-module 'uninitialized))
                (unwind-protect
                    (restart-bind ((delete-fasl-try-again
                                       (lambda (&optional c)
                                         (declare (ignore c))
                                         (setf new-module 'recompiling-fasl)
                                         (setf force-recompile t)
                                         
                                         ;; Try to delete the fasl file, to ensure it won't be reused
                                         (handler-case (delete-file bin-file)
                                           (file-error ()
                                             ;; happens on SBCL, fasl might still be in use
                                             ))
                                         ;; In some implementations PROBE-FILE on the bin-file succeeds
                                         ;; See e.g. http://trac.clozure.com/ccl/ticket/633
                                         
                                         ;; Delete fasl from caches
                                         (remhash bin-file *import-recompiled-files*)
                                         (cached-probe-file bin-file t)
                                         
                                         (throw 'py-import-retry nil)) ;; cause unwinding
                                     
                                       :test-function (lambda
                                                          #+allegro (&optional c) ;; ACL bug: C not supplied
                                                          #-allegro (c)
                                                          (declare (ignore c))
                                                          (and src-file bin-file
                                                               (cached-probe-file src-file t)
                                                               (cached-probe-file bin-file t)))
                                       :report-function
                                         (lambda (s) (format s "Recompile and re-import module `~A' ~
                                                              from file ~A" dotted-name src-file))))
                      (setf new-module
                        (if *compile-for-import*
                            (load-python-file bin-file :mod-name new-mod-dotted-name :habitat habitat)
                          (let ((*current-module-name* new-mod-dotted-name)  ;; used by compiler
                                (*compile-file-truename* src-file)) ;; needed for import stmts
                            (declare (special *current-module-name*))
                            (load-python-file bin-file :mod-name new-mod-dotted-name :habitat habitat)))))
                  ;; Cleanup form:
                  (flet ((log-abort (error-p)
                           (let ((args (list "Loading of module `~A' was aborted. ~
                                            ~@[~:@_Source: ~A~]~@[~:@_Binary: ~A~]~@[~:@_Imported by: ~A~]"
                                             new-mod-dotted-name src-file bin-file within-mod-path)))
                             (if error-p
                                 (apply #'py-raise '{ImportError} args)
                               (apply #'warn args)))))
                    (cond ((eq new-module 'recompiling-fasl)
                           ;; We'll retry the py-import call; no need for a message now.
                           )
                          ((eq new-module 'uninitialized)
                           ;; An unwinding restart was invoked from within the LOAD.
                           (log-abort nil))
                          ((null new-module)
                           ;; LOAD returned nil, probably due to the user invoking "skip loading" restart.
                           ;; The contract of this function requires raising an error.
                           (log-abort t))
                          (t
                           ;; Imported successfully
                           (return-from py-import (values new-module :imported))))))))))))))

(defun builtin-module-attribute (module attr)
  (check-type module symbol)
  (check-type attr string)
  (let* ((pkg-name (concatenate 'string (package-name :clpython.module) "." (symbol-name module)))
         (package (or (find-package pkg-name)
                      (error "Builtin Python module (Lisp package) ~A not found." pkg-name)))) 
    (symbol-value (find-symbol attr package))))

(defun %reset-import-state ()
  ;; ugly hack
  (when clpython::*all-modules*
    (check-type clpython::*all-modules* hash-table)
    (clrhash clpython::*all-modules*))
  (whereas ((ht (symbol-value (find-symbol (symbol-name '#:|modules|) :clpython.module.sys))))
    (check-type ht (or hash-table #+ecl cl-custom-hash-table::custom-hash-table))
    (with-py-dict
        (clrhash ht))))