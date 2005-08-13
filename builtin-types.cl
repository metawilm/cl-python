(in-package :python)

(eval-when (:compile-toplevel :load-toplevel :execute)

;; The corresponding Python classes are defined in BUILTIN-CLASSES.CL
  
(defmacro def-bi-types ()
  (let ((pbt-pkg (find-package :python-builtin-types)))
    `(progn ,@(loop for (name py-cls) in
		    `((basestring   py-string       )
		      (bool         py-bool         )
		      (classmethod  py-class-method )
		      (complex      py-complex      )
		      (dict         py-dict         )
		      (enumerate    py-enumerate    )
		      (float        py-float        )
		      (int          py-int          )
		      (list         py-list         )
		      (long         py-int          )
		      (object       py-object       )
		      (property     py-property     )
		      (slice        py-slice        )
		      (staticmethod py-static-method)
		      (str          py-string       )
		      (super        py-super        )
		      (tuple        py-tuple        )
		      (type         py-type         )
		      (unicode      py-string       )
		      (xrange       py-xrange       ))
		  for sym = (intern (string name) pbt-pkg)
		  collect `(defconstant ,sym (find-class ',py-cls))
		  collect `(export ',sym ,pbt-pkg)))))

(def-bi-types)
		       

;; The Python exceptions are defined in EXCEPTIONS.CL

(defmacro def-bi-excs ()
  (let ((pbt-pkg (find-package :python-builtin-types)))
    `(progn ,@(loop for c in *exception-classes*
		  for sym = (intern (string (class-name c)) pbt-pkg)
		  collect `(defconstant ,sym ,c)
		  collect `(export ',sym ,pbt-pkg)))))
(def-bi-excs)
) ;; eval-when
