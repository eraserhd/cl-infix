(in-package :cl-user)
(defpackage cl-infix-parser-test
  (:use :cl
	:cl-infix-parser
	:cl-test-more))
(in-package :cl-infix-parser-test)

(plan 2)

(is (multiple-value-list (p/number '()))
    (list)
    "P/NUMBER fails on an empty list")

(is (multiple-value-list (p/number '(42 x)))
    (list t 42 '(x))
    "P/NUMBER parses '(42)")

(finalize)
