(in-package :python)


(defconstant pyt:bool    (find-class 'py-bool))
(defconstant pyt:complex (find-class 'py-complex))
(defconstant pyt:dict    (find-class 'py-dict))
(defconstant pyt:enumerate (find-class 'py-enumerate))
(defconstant pyt:float   (find-class 'py-float))
(defconstant pyt:int     (find-class 'py-int))
(defconstant pyt:list    (find-class 'py-list))
(defconstant pyt:long    (find-class 'py-int))
(defconstant pyt:object  (find-class 'python-object))
(defconstant pyt:property (find-class 'py-property))
(defconstant pyt:slice   (find-class 'py-slice))
(defconstant pyt:str     (find-class 'py-string))
(defconstant pyt:super   (find-class 'py-super))
(defconstant pyt:tuple   (find-class 'py-tuple))
(defconstant pyt:type    (find-class 'python-type))
(defconstant pyt:unicode (find-class 'py-string))
(defconstant pyt:xrange  (find-class 'py-xrange))
(defconstant pyt:classmethod  (find-class 'class-method))
(defconstant pyt:staticmethod (find-class 'static-method))


#+(or) ;; todo
(progn 
  (defconstant pyt:basestring   xxx) ;; abstract type basestring has subtypes str and unicode

  )


;; XXX TODO: "type(name, bases, dict)" creates a new type