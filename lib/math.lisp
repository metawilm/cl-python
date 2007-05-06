(defpackage :clpython.module.math
  (:use :clpython :common-lisp)
  (:export #:|pi| #:|e|
           #:|pow| #:|exp| #:|log| #:|log10| #:|sqrt|
           #:|sin| #:|cos| #:|tan| #:|asin| #:|acos| #:|atan| #:|sinh| #:|cosh| #:|tanh| #:|atan2|
           #:|ceil| #:|floor| #:|degrees| #:|radians|
           #:|fmod| #:|fabs| #:|modf| #:|frexp| #:|hypot| #:|ldexp| ))

(in-package :clpython.module.math)

(defconstant |e| (exp 1))
(set-impl-status '(|pi| |e|) t)

(set-impl-status '(|pi| |sin| |cos| |tan| |asin| |acos| |atan| |sinh| |cosh| |tanh|) t)
(set-impl-status '|atan2| :todo "CL does not have it?")

(defun |ceil| (x) (ceiling x))
(set-impl-status '(|ceil| |floor|) t)

(defun |pow| (x y) (expt x y))
(defun |log10| (x) (log x 10))
(set-impl-status '(|pow| |exp| |log| |log10| |sqrt|) t)
                 
(set-impl-status '(|degrees| |radians|) :todo)

(defun |fmod| (x y) (mod x y))
(defun |fabs| (x) (abs x))
(defun |modf| (x) (make-tuple-from-list (multiple-value-list (truncate x))))
(set-impl-status '(|fmod| |fabs| |modf|) t)

(set-impl-status '|frexp| :todo)

(defun |hypot| (x y) (abs (complex x y)))
(defun |ldexp| (x y) (* x (expt 2 y)))
(set-impl-status '(|hypot| |ldexp|) t)
