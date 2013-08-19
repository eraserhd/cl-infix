(in-package :cl-user)
(defpackage cl-infix
  (:use :cl
	:cl-infix-parser))
(in-package :cl-infix)

;; Operator precedence levels refer to the chart on
;; http://en.cppreference.com/w/cpp/language/operator_precedence

(export '(infix % << >> == !=))

(defun flip (a b c)
  (list b a c))

(defvar *binary-operators*
  (list
    5 (list
	'* #'flip
	'/ #'flip
	'% #'(lambda (left op right) (list 'mod left right)))

    6 (list
	'+ #'flip
	'- #'flip)

    7 (list
	'<< #'(lambda (left op right) (list 'ash left right))
	'>> #'(lambda (left op right) (list 'ash left (list '- right))))

    8 (list
	'< #'flip
	'> #'flip
	'<= #'flip
	'>= #'flip)

    9 (list
	'== #'(lambda (left op right) (list '= left right))
	'!= #'(lambda (left op right) (list 'not (list '= left right))))
    ))

(defun binaries-of-equal-precedence-parser (precedence term-parser)
  (let ((operator-table (getf *binary-operators* precedence)))
    #'(lambda (tokens)
	(block parser
	  (multiple-value-bind (first-ok result tokens)
	      (funcall term-parser tokens)
	    (if (not first-ok)
	      (return-from parser (values)))
	    (loop
	      (let* ((op (car tokens))
		     (op-handler (getf operator-table op)))
		(if (not op-handler)
		  (return-from parser (values t result tokens)))
		(multiple-value-bind (term-ok term-result term-tokens)
		    (funcall term-parser (cdr tokens))
		  (if (not term-ok)
		    (return-from parser (values t result tokens)))
		  (setf tokens term-tokens)
		  (setf result (funcall op-handler result op term-result))))))))))

(defvar *reserved-symbols* '(+ - ++ -- % << >> == !=))

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
  (binaries-of-equal-precedence-parser 5 *precedence-level-3*))

(defvar *precedence-level-6*
  (binaries-of-equal-precedence-parser 6 *precedence-level-5*))

(defvar *precedence-level-7*
  (binaries-of-equal-precedence-parser 7 *precedence-level-6*))

(defvar *precedence-level-8*
  (binaries-of-equal-precedence-parser 8 *precedence-level-7*))

(defvar *precedence-level-9*
  (binaries-of-equal-precedence-parser 9 *precedence-level-8*))

;; INFIX

(defmacro infix (&body tokens)
  (multiple-value-bind (ok result left) (funcall *precedence-level-9* tokens)
    result))
