(in-package :cl-user)
(defpackage cl-infix
  (:use :cl
	:cl-infix-parser))
(in-package :cl-infix)

(export 'infix)

(defvar *r-value*
  #'p/number)

(defvar *unary-plus*
  (p/seq '+ *r-value* :=> #'(lambda ($1 $2) $2)))

(defvar *unary-minus*
  (p/seq '- *r-value*))

(defvar *precedence-level-3*
  (p/or
    *unary-plus*
    *unary-minus*
    *r-value*))

(defmacro infix (&body tokens)
  (multiple-value-bind (ok result left) (funcall *precedence-level-3* tokens)
    result))
