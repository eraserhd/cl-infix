(in-package :cl-user)
(defpackage cl-infix-parser
  (:use :cl))
(in-package :cl-infix-parser)

(export '(p/number p/eq p/seq))

(defun as-parser (p)
  (if (functionp p)
    p
    (p/eq p)))

(defun p/number (tokens)
  (if (numberp (car tokens))
    (values t (car tokens) (cdr tokens))
    (values)))

(defun p/eq (value)
  #'(lambda (tokens)
      (if (eq (car tokens) value)
	(values t (car tokens) (cdr tokens))
	(values))))

(defun p/seq (&rest parsers)
  #'(lambda (tokens)
      (let ((result ())
	    (tokens-left tokens))
	(loop for p in parsers
	      do (multiple-value-bind (p-ok p-result p-left)
		      (funcall (as-parser p) tokens-left)
		   (if (not p-ok)
		     (return (values)))
		   (setf tokens-left p-left)
		   (setf result (append result (list p-result))))
	      finally (return (values t result tokens-left))))))
