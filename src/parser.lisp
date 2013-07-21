(in-package :cl-user)
(defpackage cl-infix-parser
  (:use :cl))
(in-package :cl-infix-parser)

(export '(number-parser p/eq p/seq p/or))

(defun as-parser (p)
  (cond
    ((symbolp p)
     (p/symbol-named (symbol-name p)))
    ((functionp p)
     p)
    (t
     (p/eq p))))

(defun number-parser (tokens)
  (if (numberp (car tokens))
    (values t (car tokens) (cdr tokens))
    (values)))

(defun p/eq (value)
  #'(lambda (tokens)
      (if (eq (car tokens) value)
	(values t (car tokens) (cdr tokens))
	(values))))

(defun p/symbol-named (name)
  #'(lambda (tokens)
      (if (and (symbolp (first tokens))
	       (equal (symbol-name (first tokens)) name))
	(values t (car tokens) (cdr tokens))
	(values))))

(defun p/seq (&rest parsers)
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

(defun p/or (&rest parsers)
  #'(lambda (tokens)
      (loop for p in parsers
	    do (multiple-value-bind (p-ok p-result p-left)
		   (funcall p tokens)
		 (if p-ok
		   (return (values t p-result p-left))))
	    finally (return (values)))))
