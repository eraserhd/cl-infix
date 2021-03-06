(in-package :cl-user)
(defpackage cl-infix-parser
  (:use :cl))
(in-package :cl-infix-parser)

(export '(number-parser
	   symbol-parser
	   parsers-in-series
	   either-parser))

(defun as-parser (p)
  (if (symbolp p)
    (symbol-parser p)
    p))

(defun number-parser (tokens)
  (if (numberp (car tokens))
    (values t (car tokens) (cdr tokens))
    (values)))

(defun symbol-parser (name)
  #'(lambda (tokens)
      (if (and (symbolp (first tokens))
	       (equal (symbol-name (first tokens))
		      (symbol-name name)))
	(values t (car tokens) (cdr tokens))
	(values))))

(defun parsers-in-series (&rest parsers)
  (let* ((real-parsers (loop for p in parsers
			     while (not (eq :=> p))
			     collect (as-parser p)))
	 (=>-value (second (member :=> parsers)))
	 (transform (if =>-value
		      =>-value
		      #'(lambda (&rest rest)
			  rest))))
    #'(lambda (tokens)
	(let ((result ())
	      (tokens-left tokens))
	  (loop for p in real-parsers
		do (multiple-value-bind (p-ok p-result p-left)
			(funcall p tokens-left)
		     (if (not p-ok)
		       (return (values)))
		     (setf tokens-left p-left)
		     (setf result (append result (list p-result))))
		finally (return (values t (apply transform result) tokens-left)))))))

(defun either-parser (&rest parsers)
  #'(lambda (tokens)
      (loop for p in parsers
	    do (multiple-value-bind (p-ok p-result p-left)
		   (funcall p tokens)
		 (if p-ok
		   (return (values t p-result p-left))))
	    finally (return (values)))))
