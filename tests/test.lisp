;;; -*- Mode:Lisp; Package:XLIB-TEST; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;; A small utility for defining test suites and keeping track of them.
;;; We're using Fiasco for the heavy lifting.


(in-package :xlib-test)

(fiasco:defsuite (xlib-all-tests :bind-to-package :xlib-test))

(defvar *test-suites* nil)

(defun run-all-tests (&rest fiasco-params)
  (apply #'fiasco:run-package-tests :packages *test-suites* fiasco-params))

(defmacro define-test-suite (name &body package-options)
  `(progn (fiasco:define-test-package (,name :in xlib-all-tests)
            ,@package-options
            (:use :xlib-test))
          (in-package ,name)
          (setf *test-suites* (adjoin ,name *test-suites*))))
