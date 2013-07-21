(in-package :cl-user)
(defpackage cl-infix-parser-test
  (:use :cl
	:cl-infix-parser
	:cl-test-more))
(in-package :cl-infix-parser-test)

(defmacro taking-something-from (tokens with parser expectation &optional (value nil has-value))
  "DSL for testing parser combinators.

  Three forms are used:

    (taking-something-from i with p fails)
    (taking-something-from i with p returns r)
    (taking-something-from i with p leaves l)

  where

    i is the input token list.
    p is the parser being tested.
    r is the expected parser result (implying success).
    l is a list of tokens which should remain after success.
  "
  (let* ((condition-msg (if (not has-value)
			  (format nil "~(~S~)" expectation)
			  (format nil "~(~S~) ~:A" expectation value)))
	 (message (format nil
			  "taking-something-from ~:A with ~W ~A."
			  tokens
			  parser
			  condition-msg)))
    (ecase expectation
      (fails
       `(multiple-value-bind (parse-ok) (funcall ,parser ',tokens)
	  (ok (not parse-ok) ,message)))
      (returns
       `(multiple-value-bind (parse-ok result) (funcall ,parser ',tokens)
	  (if parse-ok
	    (is result ',value ,message :test #'equal)
	    (ok nil ,message))))
      (leaves
       `(let ((parse-result (multiple-value-list (funcall ,parser ',tokens))))
	  (if (first parse-result)
	    (is (third parse-result) ',value ,message :test #'equal)
	    (ok nil ,message)))))))

(plan 18)

(taking-something-from () with #'number-parser fails)
(taking-something-from (42 x) with #'number-parser returns 42)
(taking-something-from (42 x) with #'number-parser leaves (x))
(taking-something-from (23 2 3 1) with #'number-parser returns 23)
(taking-something-from (23 2 3 1) with #'number-parser leaves (2 3 1))

(taking-something-from () with (eq-parser 'y) fails)
(taking-something-from (x) with (eq-parser 'y) fails)
(taking-something-from (=) with (eq-parser '=) returns =)
(taking-something-from (= 7) with (eq-parser '=) leaves (7))

(taking-something-from (+) with (p/seq (eq-parser '+) #'number-parser) fails)
(taking-something-from (+ 7) with (p/seq (eq-parser '+) #'number-parser) returns (+ 7))
(taking-something-from (+ 7 7) with (p/seq (eq-parser '+) #'number-parser) returns (+ 7))
(taking-something-from (+ 7 7) with (p/seq (eq-parser '+) #'number-parser) leaves (7))

(taking-something-from (+ 7 7) with (p/seq '+ #'number-parser) returns (+ 7))

(taking-something-from (7 % 2)
	 with (p/seq #'number-parser '% #'number-parser :=> #'(lambda ($1 $2 $3)
						      `(mod ,$1 ,$3)))
	 returns (mod 7 2))

(taking-something-from (7 8) with (p/or #'number-parser (eq-parser 'x)) returns 7)
(taking-something-from (7 8) with (p/or #'number-parser (eq-parser 'x)) leaves (8))
(taking-something-from (x 8) with (p/or #'number-parser (eq-parser 'x)) returns x)
(taking-something-from (x 8) with (p/or #'number-parser (eq-parser 'x)) leaves (8))

(taking-something-from (y) with (p/or #'number-parser (eq-parser 'x)) fails)

(finalize)
