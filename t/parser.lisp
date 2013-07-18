(in-package :cl-user)
(defpackage cl-infix-parser-test
  (:use :cl
	:cl-infix-parser
	:cl-test-more))
(in-package :cl-infix-parser-test)

(defmacro parse (parser tokens &rest rest)
  (ecase (first rest)
    (:fails
     `(multiple-value-bind (parse-ok) (funcall ,parser ',tokens)
        (ok (not parse-ok))))
    (:returns
     `(multiple-value-bind (parse-ok result) (funcall ,parser ',tokens)
	(if parse-ok
	  (is result ',(second rest))
	  (ok nil))))
    (:leaves
     `(let ((parse-result (multiple-value-list (funcall ,parser ',tokens))))
	(if (first parse-result)
	  (is (third parse-result) ',(second rest))
	  (ok nil))))))

(plan 6)

(parse #'p/number () :fails)
(parse #'p/number (42 x) :returns 42)
(parse #'p/number (42 x) :leaves (x))

(parse (p/eq 'y) () :fails)
(parse (p/eq '=) (=) :returns =)
(parse (p/eq '=) (= 7) :leaves (7))

(finalize)
