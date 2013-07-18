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
	  (is result ',(second rest) "" :test #'equal)
	  (ok nil))))
    (:leaves
     `(let ((parse-result (multiple-value-list (funcall ,parser ',tokens))))
	(if (first parse-result)
	  (is (third parse-result) ',(second rest) "" :test #'equal)
	  (ok nil))))))

(plan 10)

(parse #'p/number () :fails)
(parse #'p/number (42 x) :returns 42)
(parse #'p/number (42 x) :leaves (x))

(parse (p/eq 'y) () :fails)
(parse (p/eq '=) (=) :returns =)
(parse (p/eq '=) (= 7) :leaves (7))

(parse (p/seq (p/eq '+) #'p/number) (+) :fails)
(parse (p/seq (p/eq '+) #'p/number) (+ 7) :returns (+ 7))
(parse (p/seq (p/eq '+) #'p/number) (+ 7 7) :returns (+ 7))
(parse (p/seq (p/eq '+) #'p/number) (+ 7 7) :leaves (7))

(finalize)
