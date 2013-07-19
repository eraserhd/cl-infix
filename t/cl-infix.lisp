(in-package :cl-user)
(defpackage cl-infix-test
  (:use :cl
        :cl-infix
        :cl-test-more))
(in-package :cl-infix-test)

(defmacro the-form (form verb object)
  (let ((message (format nil "~A ~(~A~) ~A." form verb object)))
    (ecase verb
      (expands-to
	`(is ,(macroexpand-1 form) ,object ,message)))))

(plan 1)

(the-form (infix 42) expands-to 42)

(finalize)
