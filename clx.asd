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
(defclass xrender-source-file (clx-source-file) ())

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
     ;; FIXME: I'm fairly sure that these don't need to be serially
     ;; compiled.  We should reflect that here, so that a change in
     ;; "shape" doesn't trigger recompilation of "xvidmode" and
     ;; "xrender".
     (:file "shape")
     (:file "xvidmode")
     (:xrender-source-file "xrender")))
;;; (:module doc ("doc") (:type :lisp-example))

#+sbcl
(defmethod perform :around ((o compile-op) (f xrender-source-file))
  ;; RENDER would appear to be an inherently slow protocol; further,
  ;; it's not set in stone, and consequently we care less about speed
  ;; than we do about correctness.
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (call-next-method)))

#+sbcl
(defmethod perform :around ((o compile-op) (f clx-source-file))
  ;; our CLX library should compile without WARNINGs, and ideally
  ;; without STYLE-WARNINGs.  Since it currently does, let's enforce
  ;; it here so that we can catch regressions easily.
  (setf (operation-on-warnings o) :error
	(operation-on-failure o) :error)
  ;; a variety of accessors, such as AREF-CARD32, are not declared
  ;; INLINE.  Without this (non-ANSI) static-type-inference behaviour,
  ;; SBCL emits an extra 100 optimization notes (roughly one fifth of
  ;; all of the notes emitted).  Since the internals are unlikely to
  ;; change much, and certainly the internals should stay in sync,
  ;; enabling this extension is a win.  (Note that the use of this
  ;; does not imply that applications using CLX calls that expand into
  ;; calls to these accessors will be optimized in the same way).
  (let ((sb-ext:*derive-function-types* t))
    (call-next-method)))

#+sbcl
(defmethod perform :around (o (f clx-source-file))
  ;; SBCL signals an error if DEFCONSTANT is asked to redefine a
  ;; constant unEQLly.  For CLX's purposes, however, we are defining
  ;; structured constants (lists and arrays) not for EQLity, but for
  ;; the purposes of constant-folding operations such as (MEMBER FOO
  ;; +BAR+), so it is safe to abort the redefinition provided the
  ;; structured data is sufficiently equal.
  (handler-bind
      ((sb-ext:defconstant-uneql
	   (lambda (c)
	     ;; KLUDGE: this really means "don't warn me about
	     ;; efficiency of generic array access, please"
	     (declare (optimize (sb-ext:inhibit-warnings 3)))
	     (let ((old (sb-ext:defconstant-uneql-old-value c))
		   (new (sb-ext:defconstant-uneql-new-value c)))
	       (typecase old
		 (list (when (equal old new) (abort c)))
		 (string (when (and (typep new 'string)
				    (string= old new))
			   (abort c)))
		 (simple-vector
		  (when (and (typep new 'simple-vector)
			     (= (length old) (length new))
			     (every #'eql old new))
		    (abort c)))
		 (array
		  (when (and (typep new 'array)
			     (equal (array-dimensions old)
				    (array-dimensions new))
			     (equal (array-element-type old)
				    (array-element-type new))
			     (dotimes (i (array-total-size old) t)
			       (unless (eql (row-major-aref old i)
					    (row-major-aref new i))
				 (return nil))))
		    (abort c))))))))
    (call-next-method)))
