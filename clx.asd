;;; -*- Lisp mode -*-

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

(defsystem CLX
    :depends-on (sb-bsd-sockets)
    :version "0.4"
    :serial t
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
     (:file "shape")))
;;; (:module doc ("doc") (:type :lisp-example))
