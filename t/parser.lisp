(in-package :cl-user)
(defpackage cl-infix-parser-test
  (:use :cl
	:cl-infix-parser
	:cl-test-more))
(in-package :cl-infix-parser-test)

(defmacro parsing (tokens with parser &rest rest)
  "DSL for testing parser combinators.

  Three forms are used:

    (parsing i with p fails)
    (parsing i with p returns r)
    (parsing i with p leaves l)

  where

    i is the input token list.
    p is the parser being tested.
    r is the expected parser result (implying success).
    l is a list of tokens which should remain after success.
  "
  (let* ((condition-msg (if (= 1 (length rest))
			  (format nil "~(~S~)" (first rest))
			  (format nil "~(~S~) ~:A" (first rest) (second rest))))
	 (message (format nil
			  "parsing ~:A with ~W ~A."
			  tokens
			  parser
			  condition-msg)))
    (ecase (first rest)
      (fails
       `(multiple-value-bind (parse-ok) (funcall ,parser ',tokens)
	  (ok (not parse-ok) ,message)))
      (returns
       `(multiple-value-bind (parse-ok result) (funcall ,parser ',tokens)
	  (if parse-ok
	    (is result ',(second rest) ,message :test #'equal)
	    (ok nil ,message))))
      (leaves
       `(let ((parse-result (multiple-value-list (funcall ,parser ',tokens))))
	  (if (first parse-result)
	    (is (third parse-result) ',(second rest) ,message :test #'equal)
	    (ok nil ,message)))))))

(plan 12)

(parsing () with #'p/number fails)
(parsing (42 x) with #'p/number returns 42)
(parsing (42 x) with #'p/number leaves (x))

(parsing () with (p/eq 'y) fails)
(parsing (=) with (p/eq '=) returns =)
(parsing (= 7) with (p/eq '=) leaves (7))

(parsing (+) with (p/seq (p/eq '+) #'p/number) fails)
(parsing (+ 7) with (p/seq (p/eq '+) #'p/number) returns (+ 7))
(parsing (+ 7 7) with (p/seq (p/eq '+) #'p/number) returns (+ 7))
(parsing (+ 7 7) with (p/seq (p/eq '+) #'p/number) leaves (7))

(parsing (+ 7 7) with (p/seq '+ #'p/number) returns (+ 7))

(parsing (7 % 2)
	 with (p/seq #'p/number '% #'p/number :=> #'(lambda ($1 $2 $3)
						      `(mod ,$1 ,$3)))
	 returns (mod 7 2))

(finalize)
