(in-package :python)


(defconstant pyt:bool    (find-class 'py-bool))
(defconstant pyt:complex (find-class 'py-complex))
(defconstant pyt:dict    (find-class 'py-dict))
(defconstant pyt:enumerate (find-class 'py-enumerate))
(defconstant pyt:float   (find-class 'py-float))
(defconstant pyt:int     (find-class 'py-int))
(defconstant pyt:list    (find-class 'py-list))
(defconstant pyt:long    (find-class 'py-int))
(defconstant pyt:slice   (find-class 'py-slice))
(defconstant pyt:str     (find-class 'py-string))
(defconstant pyt:tuple   (find-class 'py-tuple))
(defconstant pyt:xrange  (find-class 'py-xrange))

#+(or) ;; todo
(progn 
  (defconstant pyt:classmethod  (find-class 'py-classmethod))
  (defconstant pyt:staticmethod (find-class 'py-staticmethod))
  (defconstant pyt:property     (find-class 'py-property))
  (defconstant pyt:object  (find-class 'py-object)) ;; or `user-defined-class'
  (defconstant pyt:type    (find-class 'py-type))
  (defconstant pyy:unicode (find-class ???)))


;; XXX TODO: "type(name, bases, dict)" creates a new type