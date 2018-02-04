(defpackage #:xlib-test
  (:use :cl)
  (:export #:run-all-tests #:xlib-test #:xlib-all-tests))
(in-package #:xlib-test)

(fiasco:defsuite (xlib-all-tests :bind-to-package #:xlib-test))
