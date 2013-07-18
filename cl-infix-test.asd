#|
  This file is a part of cl-infix project.
  Copyright (c) 2013 Jason Felice (jason.m.felice@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-infix-test-asd
  (:use :cl :asdf))
(in-package :cl-infix-test-asd)

(defsystem cl-infix-test
  :author "Jason Felice"
  :license ""
  :depends-on (:cl-infix
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "cl-infix"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
