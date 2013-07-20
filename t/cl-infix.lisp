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

(plan 6)

(the-form (infix 42) expands-to 42)
(the-form (infix + 2) expands-to 2)
(the-form (infix - 4) expands-to (- 4))

(the-form (infix x) expands-to x)
(the-form (infix ++ x) expands-to (incf x))
(the-form (infix -- y) expands-to (decf y))

(finalize)
