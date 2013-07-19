(in-package :cl-user)
(defpackage cl-infix
  (:use :cl
	:cl-infix-parser))
(in-package :cl-infix)

(export 'infix)

(setf (symbol-function 'i/constant) #'p/number)

(defmacro infix (&body tokens)
  (multiple-value-bind (ok result left) (i/constant tokens)
    result))
