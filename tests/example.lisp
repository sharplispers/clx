;;; -*- Mode:Lisp; Package:XLIB-TEST; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;; A placeholder example of how to define test suites.

(fiasco:define-test-package (:xlib-test-example :in xlib-test:xlib-all-tests))

(in-package :xlib-test-example)

(deftest this-should-pass ()
  (is t))

(deftest this-should-fail ()
  (is nil))

(defun the-answer ()
  (cons 42 (the-answer)))

(deftest this-should-cause-an-error ()
  (the-answer))

