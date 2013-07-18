(in-package :cl-user)
(defpackage cl-infix-parser
  (:use :cl))
(in-package :cl-infix-parser)

(export '(p/number p/eq))

(defun p/number (tokens)
  (if (numberp (car tokens))
    (values t (car tokens) (cdr tokens))
    (values)))

(defun p/eq (value)
  #'(lambda (tokens)
      (if (eq (car tokens) value)
	(values t (car tokens) (cdr tokens))
	(values))))
