(in-package :cl-user)
(defpackage cl-infix-test
  (:use :cl
        :cl-infix
	:cl-infix-parser
        :cl-test-more))
(in-package :cl-infix-test)

(defmacro the-form (form verb object)
  (let ((message (let ((*print-pretty* nil))
		   (format nil "~A ~(~A~) ~A." form verb object))))
    (ecase verb
      (expands-to
	`(is (macroexpand-1 ',form) ',object ,message :test #'equalp)))))

(plan 32)

(the-form (infix 42) expands-to 42)
(the-form (infix + 2) expands-to 2)
(the-form (infix - 4) expands-to (- 4))

(the-form (infix x) expands-to x)
(the-form (infix ++ x) expands-to (incf x))
(the-form (infix -- y) expands-to (decf y))
(the-form (infix 10 * 5) expands-to (* 10 5))
(the-form (infix 10 / 4) expands-to (/ 10 4))
(the-form (infix 10 % 7) expands-to (mod 10 7))
(the-form (infix 10 * -- y) expands-to (* 10 (decf y)))
(the-form (infix 10 * 1 / 7) expands-to (/ (* 10 1) 7))
(the-form (infix 10 + 1 - 2) expands-to (- (+ 10 1) 2))
(the-form (infix 10 + 7 * 9 - 6 / 1) expands-to (- (+ 10 (* 7 9))
						   (/ 6 1)))
(the-form (infix 1 << 10) expands-to (ash 1 10))
(the-form (infix 1024 >> 2) expands-to (ash 1024 (- 2)))

(the-form (infix 1 < 3) expands-to (< 1 3))
(the-form (infix 1 <= 3) expands-to (<= 1 3))
(the-form (infix 1 > 3) expands-to (> 1 3))
(the-form (infix 1 >= 3) expands-to (>= 1 3))

(the-form (infix 7 == 9) expands-to (= 7 9))
(the-form (infix 7 != 9) expands-to (not (= 7 9)))

(the-form (infix 7 & 1) expands-to (logand 7 1))
(the-form (infix 7 ^ 1) expands-to (logxor 7 1))

(the-form (infix a && b) expands-to (and a b))
(the-form (infix b || c) expands-to (or b c))

(the-form (infix x = 42) expands-to (setf x 42))
(the-form (infix x += 5) expands-to (setf x (+ x 5)))
(the-form (infix x -= 5) expands-to (setf x (- x 5)))
(the-form (infix x *= 2) expands-to (setf x (* x 2)))
(the-form (infix x /= 7) expands-to (setf x (/ x 7)))
(the-form (infix x %= 3) expands-to (setf x (mod x 3)))
(the-form (infix x &= 11) expands-to (setf x (logand x 11)))

(finalize)
