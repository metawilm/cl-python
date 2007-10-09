(defpackage :clpython.module.gc
  (:use :clpython :common-lisp)
  (:export #:|disable| )
  (:import-from :clpython #:*try-except-currently-handled-exception*))

;;; Garbage collector interface

(in-package :clpython.module.gc)

(defun |disable| ()
  (warn "Disabling GC not implemented.")
  *the-none*)