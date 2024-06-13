(in-package :cl-user)

#+sbcl
(require :asdf)

(load "/tmp/ql-dir/quicklisp.lisp")
(quicklisp-quickstart:install)

(ql:quickload :closer-mop)
(ql:quickload :yacc)
(ql:quickload :cl-custom-hash-table)
(ql:quickload :cl-fad)

#-allegro
(ql:quickload :ptester)

(load "/home/runner/work/cl-python/cl-python/clpython.asd")

(ql:quickload :clpython)

(asdf:test-system :clpython)
