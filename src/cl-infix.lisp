(in-package :cl-user)
(defpackage cl-infix
  (:use :cl
	:cl-infix-parser))
(in-package :cl-infix)

;; Operator precedence levels refer to the chart on
;; http://en.cppreference.com/w/cpp/language/operator_precedence

(defun flip (a b c)
  (list b a c))

(defun mutating-assignment (mutating-operator)
  #'(lambda (left _ right)
      (list 'setf left (list mutating-operator left right))))

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
    
    10 (list
	 '& #'(lambda (left op right) (list 'logand left right)))

    11 (list
	 '^ #'(lambda (left op right) (list 'logxor left right)))

    13 (list
	 '&& #'(lambda (left op right) (list 'and left right)))

    14 (list
	 '|| #'(lambda (left op right) (list 'or left right)))

    15 (list
	 '= #'(lambda (left op right) (list 'setf left right))
	 '+= (mutating-assignment '+)
	 '-= (mutating-assignment '-)
	 '*= (mutating-assignment '*))

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

(defun binary-symbols ()
  (mapcan #'(lambda (e)
	      (if (listp e)
		(loop for op in e by #'cddr
		      collect op)
		())) *binary-operators*))

(defvar *reserved-symbols* 
  (nconc (list '+ '- '++ '--)
	 (binary-symbols)))

(defun cl-symbol-p (sym)
  (eq (symbol-package sym)
      (symbol-package 'list)))

(defun operator-symbols-to-export ()
  (remove-if #'cl-symbol-p *reserved-symbols*))

(export (cons 'infix (operator-symbols-to-export)))

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

;; THE BINARY PARSER STACK

(defvar *top-binary-parser*
  (loop with parser = *precedence-level-3*
	for i on *binary-operators* by #'cddr
	do (let ((precedence (first i))
		 (operator-table (second i)))
	     (setf parser (binaries-of-equal-precedence-parser precedence parser)))
	finally (return parser)))

;; INFIX

(defmacro infix (&body tokens)
  (multiple-value-bind (ok result left) (funcall *top-binary-parser* tokens)
    result))
