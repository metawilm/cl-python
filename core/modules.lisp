(in-package :clpython)

(defun initial-py-modules ()
  (update-loaded-lisp-modules)
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
		(setf (gethash (intern (string name) :clpython.user) ,dg) val)))
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

(defmacro in-python-module (name &key builtin)
  (let ((sname (string name)))
    `(eval-when (:load-toplevel :execute)
       (push (list ,sname (load-time-value *package*) ',builtin) *modules-to-load*))))

