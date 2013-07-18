#|
  This file is a part of cl-infix project.
  Copyright (c) 2013 Jason Felice (jason.m.felice@gmail.com)
|#

#|
  Author: Jason Felice (jason.m.felice@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-infix-asd
  (:use :cl :asdf))
(in-package :cl-infix-asd)

(defsystem cl-infix
  :version "0.1"
  :author "Jason Felice"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "cl-infix"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op cl-infix-test))))
