;;; -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;; A placeholder example of how to define test suites.

(xlib-test:define-test-suite :xlib-test-example)

(deftest this-should-pass ()
  (is t))

(deftest this-should-fail ()
  (is nil))

(defun the-answer ()
  (cons 42 (the-answer)))

(deftest this-should-cause-an-error ()
  (the-answer))

