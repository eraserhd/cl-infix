(in-package :cl-user)
(defpackage cl-infix-parser-test
  (:use :cl
	:cl-infix-parser
	:cl-test-more))
(in-package :cl-infix-parser-test)

(defmacro running (parser on tokens expectation &optional (value nil has-value))
  "DSL for testing parser combinators.

  Three forms are used:

    (running i with p fails)
    (running i with p returns r)
    (running i with p leaves l)

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
			  "running ~:A with ~W ~A."
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

(plan 16)

(running #'number-parser on () fails)
(running #'number-parser on (42 x) returns 42)
(running #'number-parser on (42 x) leaves (x))
(running #'number-parser on (23 2 3 1) returns 23)
(running #'number-parser on (23 2 3 1) leaves (2 3 1))

(running (parsers-in-series (symbol-parser '+) #'number-parser)
	 on (+)
	 fails)

(running (parsers-in-series (symbol-parser '+) #'number-parser)
	 on (+ 7)
	 returns (+ 7))

(running (parsers-in-series (symbol-parser '+) #'number-parser)
	 on (+ 7 7)
	 returns (+ 7))

(running (parsers-in-series (symbol-parser '+) #'number-parser)
	 on (+ 7 7)
	 leaves (7))

(running (parsers-in-series (symbol-parser '+) #'number-parser)
	 on (+ 7 7)
	 returns (+ 7))

(running (parsers-in-series
	   #'number-parser
	   (symbol-parser '%)
	   #'number-parser
	   :=> #'(lambda ($1 $2 $3) `(mod ,$1 ,$3)))
	 on (7 % 2)
	 returns (mod 7 2))

(running (p/or #'number-parser (symbol-parser 'x))
	 on (7 8)
	 returns 7)

(running (p/or #'number-parser (symbol-parser 'x))
	 on (7 8)
	 leaves (8))

(running (p/or #'number-parser (symbol-parser 'x))
	 on (x 8)
	 returns x)

(running (p/or #'number-parser (symbol-parser 'x))
	 on (x 8)
	 leaves (8))

(running (p/or #'number-parser (symbol-parser 'x))
	 on (y)
	 fails)

(finalize)
