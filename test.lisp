;;; -*- Mode:Lisp; Package:XLIB; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;; A small utility for defining test suites and keeping track of them.
;;; We're using Fiasco for the heavy lifting.


(in-package :xlib-test)

(defvar *test-suites* nil)

(defun run-all-tests (&rest fiasco-params)
  (apply #'fiasco:run-package-tests :packages *test-suites* fiasco-params))

(defmacro define-test-suite (name &body package-options)
  (setf *test-suites* (adjoin name *test-suites*))
  `(progn (fiasco:define-test-package ,name
	      ,@package-options)
	  (in-package ,name)))
