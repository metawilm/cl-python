(defpackage :clpython.module.math
  (:use :clpython :common-lisp))

(in-package :clpython.module.math)

(clpython::in-python-module "math" :builtin t)

(defconstant pi pi)