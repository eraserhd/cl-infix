(in-package :cl-user)
(defpackage cl-infix
  (:use :cl
	:cl-infix-parser))
(in-package :cl-infix)

;; Operator precedence levels refer to the chart on
;; http://en.cppreference.com/w/cpp/language/operator_precedence

(export '(infix))

(defmacro binary (op next-precedence-level)
  `(parsers-in-series ,next-precedence-level ,op ,next-precedence-level
		      :=> #'(lambda ($1 $2 $3)
			      `(,$2 ,$1 ,$3))))

(defvar *reserved-symbols* '(+ - ++ -- %))

(defvar *l-value*
  #'(lambda (tokens)
      (let ((sym (car tokens)))
	(if (and (symbolp sym)
		 (not (member sym *reserved-symbols*)))
	  (values t sym (cdr tokens))
	  (values)))))

(defvar *r-value*
  (either-parser
    #'number-parser
    *l-value*))

;; LEVEL 2 OPERATORS

(defvar *pre-increment*
  (parsers-in-series '++ *l-value* :=> #'(lambda ($1 $2)
			                   `(incf ,$2))))

(defvar *pre-decrement*
  (parsers-in-series '-- *l-value* :=> #'(lambda ($1 $2)
			                   `(decf ,$2))))

(defvar *precedence-level-2*
  (either-parser
    *pre-increment*
    *pre-decrement*
    *r-value*))

;; LEVEL 3 OPERATORS

(defvar *unary-plus*
  (parsers-in-series '+ *precedence-level-2* :=> #'(lambda ($1 $2) $2)))

(defvar *unary-minus*
  (parsers-in-series '- *precedence-level-2*))

(defvar *precedence-level-3*
  (either-parser
    *unary-plus*
    *unary-minus*
    *precedence-level-2*))

;; LEVEL 5 OPERATORS

(defvar *precedence-level-5*
  (either-parser
    (binary '* *precedence-level-3*)
    (binary '/ *precedence-level-3*)
    (binary '% *precedence-level-3*)
    *precedence-level-3*))

;; INFIX

(defmacro infix (&body tokens)
  (multiple-value-bind (ok result left) (funcall *precedence-level-5* tokens)
    result))
