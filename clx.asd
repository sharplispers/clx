;;; -*- Lisp -*- mode

;;; Original copyright message from defsystem.lisp:

;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 2909
;;;			       AUSTIN, TEXAS 78769
;;;
;;; Portions Copyright (C) 1987 Texas Instruments Incorporated.
;;; Portions Copyright (C) 1988, 1989 Franz Inc, Berkeley, Ca.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;
;;; Franz Incorporated provides this software "as is" without express or
;;; implied warranty.

(defpackage :clx-system (:use :cl :asdf))
(in-package :clx-system)  

(pushnew :clx-ansi-common-lisp *features*)

(defclass clx-source-file (cl-source-file) ())

(defsystem CLX
    :depends-on (sb-bsd-sockets)
    :version "0.5.1"
    :serial t
    :default-component-class clx-source-file
    :components
    ((:file "package")
     (:file "depdefs")
     (:file "clx")
     (:file "dependent")
     (:file "macros")
     (:file "bufmac")
     (:file "buffer")
     (:file "display")
     (:file "gcontext")
     (:file "input")
     (:file "requests")
     (:file "fonts")
     (:file "graphics")
     (:file "text")
     (:file "attributes")
     (:file "translate")
     (:file "keysyms")
     (:file "manager")
     (:file "image")
     (:file "resource")
     (:file "shape")
     (:file "xvidmode")))
;;; (:module doc ("doc") (:type :lisp-example))

#+sbcl
(defmethod perform :around ((o compile-op) (f clx-source-file))
  ;; a variety of accessors, such as AREF-CARD32, are not declared
  ;; INLINE.  Without this (non-ANSI) static-type-inference behaviour,
  ;; SBCL emits about 100 optimization notes (roughly one fifth of all
  ;; of the notes emitted).  Since the internals are unlikely to
  ;; change much, and certainly the internals should stay in sync,
  ;; enabling this extension is a win.  (Note that the use of this
  ;; does not imply that applications using CLX calls that expand into
  ;; calls to these accessors will be optimized in the same way).
  (let ((sb-ext:*derive-function-types* t))
    (call-next-method)))
