(in-package :cl-user)
(defpackage cl-infix
  (:use :cl
	:cl-infix-parser))
(in-package :cl-infix)

(export '(infix))

(defvar *reserved-symbols* '(+ - ++ --))

(defvar *l-value*
  #'(lambda (tokens)
      (let ((sym (car tokens)))
	(if (and (symbolp sym)
		 (not (member sym *reserved-symbols*)))
	  (values t sym (cdr tokens))
	  (values)))))

(defvar *r-value*
  (p/or
    #'p/number
    *l-value*))

;; LEVEL 2 OPERATORS

(defvar *pre-increment*
  (p/seq '++ *l-value* :=> #'(lambda ($1 $2)
			       `(incf ,$2))))

(defvar *pre-decrement*
  (p/seq '-- *l-value* :=> #'(lambda ($1 $2)
			       `(decf ,$2))))

(defvar *precedence-level-2*
  (p/or
    *pre-increment*
    *pre-decrement*
    *r-value*))

;; LEVEL 3 OPERATORS

(defvar *unary-plus*
  (p/seq '+ *precedence-level-2* :=> #'(lambda ($1 $2) $2)))

(defvar *unary-minus*
  (p/seq '- *precedence-level-2*))

(defvar *precedence-level-3*
  (p/or
    *unary-plus*
    *unary-minus*
    *precedence-level-2*))

;; INFIX

(defmacro infix (&body tokens)
  (multiple-value-bind (ok result left) (funcall *precedence-level-3* tokens)
    result))
