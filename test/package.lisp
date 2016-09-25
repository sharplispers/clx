(defpackage #:clx-test
  (:use :cl :xlib)
  (:export :asd))
(in-package :clx-test)

(defun asd (x)
  (* x x))

(fiasco:define-test-package #:fiasco-clx-test
  (:use :clx-test :fiasco)
  (:export :asd-test))
(in-package :fiasco-clx-test)

(deftest asd-test ()
  (is (= 25 (asd 5))))
