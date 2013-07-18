(in-package :cl-user)
(defpackage cl-infix-parser
  (:use :cl))
(in-package :cl-infix-parser)

(export 'p/number)

(defun p/number (tokens)
  (if (numberp (car tokens))
    (values t (car tokens) (cdr tokens))
    (values)))
