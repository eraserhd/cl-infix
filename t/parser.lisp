(in-package :cl-user)
(defpackage cl-infix-parser-test
  (:use :cl
	:cl-infix-parser
	:cl-test-more))
(in-package :cl-infix-parser-test)

(defmacro parse (parser tokens &rest rest)
  (ecase (first rest)
    (:fails
     `(multiple-value-bind (parse-ok) (,parser ',tokens)
        (ok (not parse-ok))))
    (:returns
     `(multiple-value-bind (parse-ok result) (,parser ',tokens)
	(if parse-ok
	  (is result ,(second rest))
	  (ok nil))))
    (:leaves
     `(let ((parse-result (multiple-value-list (,parser ',tokens))))
	(if (first parse-result)
	  (is (third parse-result) ',(second rest))
	  (ok nil))))))

(plan 3)

(parse p/number () :fails)
(parse p/number (42 x) :returns 42)
(parse p/number (42 x) :leaves (x))

(finalize)
