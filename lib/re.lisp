(defpackage :clpython.module.re
  (:use :clpython :common-lisp)
  (:export #:|compile| #:|search| #:|match| #:|split| #:|findall|
           #:|finditer| #:|sub| #:|subn| #:|escape| #:|exception|
           #:|I| #:|IGNORECASE|
           #:|L| #:|LOCALE|
           #:|M| #:|MULTILINE|
           #:|S| #:|DOTALL|
           #:|U| #:|UNICODE|
           #:|X| #:|VERBOSE|
           )
  (:import-from :clpython #:def-proxy-class #:def-py-method))

;;; Regular Expressions
;;; http://docs.python.org/lib/node46.html

(in-package :clpython.module.re)

(clpython::def-proxy-class reg-exp ()
  ())

(def-py-method reg-exp.match (re string &optional start end)
  (declare (ignorable re string start end))
  (error "todo"))

(def-py-method reg-exp.search (re string &optional start end)
  (declare (ignorable re string start end))
  (error "todo"))

(def-py-method reg-exp.split (re string &optional (maxsplit 0))
  (declare (ignorable re string maxsplit))
  (error "todo"))

(def-py-method reg-exp.findall (re string &optional start end)
  (declare (ignorable re string start end))
  (error "todo"))

(def-py-method reg-exp.finditer (re string &optional start end)
  (declare (ignorable re string start end))
  (error "todo"))

(def-py-method reg-exp.sub (re repl string  &optional (count 0))
  (declare (ignorable re repl string count))
  (error "todo"))

(def-py-method reg-exp.subn (re repl string  &optional (count 0))
  (declare (ignorable re repl string count))
  (error "todo"))

(def-py-method reg-exp.flags :attribute (re)
  (declare (ignorable re))
  (error "todo"))

(def-py-method reg-exp.groupindex :attribute (re)
  (declare (ignorable re))
  (error "todo"))

(def-py-method reg-exp.pattern :attribute (re)
  (declare (ignorable re))
  (error "todo"))


(clpython::def-proxy-class reg-exp-match ()
  ())

(def-py-method reg-exp-match.expand (rm template)
  (declare (ignorable rm template))
  (error "todo"))

(def-py-method reg-exp-match.group (rm &rest groups)
  (declare (ignorable rm groups))
  (error "todo"))

(def-py-method reg-exp-match.groups (rm &optional default)
  (declare (ignorable rm default))
  (error "todo"))

(def-py-method reg-exp-match.groupdict (rm &optional default)
  (declare (ignorable rm default))
  (error "todo"))

(def-py-method reg-exp-match.start (rm &optional group)
  (declare (ignorable rm group))
  (error "todo"))

(def-py-method reg-exp-match.end (rm &optional group)
  (declare (ignorable rm group))
  (error "todo"))

(def-py-method reg-exp-match.span (rm &optional group)
  (declare (ignorable rm group))
  (error "todo"))

(def-py-method reg-exp-match.pos :attribute (rm)
  (declare (ignorable rm))
  (error "todo"))

(def-py-method reg-exp-match.endpos :attribute (rm)
  (declare (ignorable rm))
  (error "todo"))

(def-py-method reg-exp-match.lastindex :attribute (rm)
  (declare (ignorable rm))
  (error "todo"))

(def-py-method reg-exp-match.lastgroup :attribute (rm)
  (declare (ignorable rm))
  (error "todo"))

(def-py-method reg-exp-match.re :attribute (rm)
  (declare (ignorable rm))
  (error "todo"))

(def-py-method reg-exp-match.string :attribute (rm)
  (declare (ignorable rm))
  (error "todo"))
