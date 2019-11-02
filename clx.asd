;;; -*- Lisp -*- mode

;;; Original copyright message from defsystem.lisp:

;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 2909
;;;			       AUSTIN, TEXAS 78769
;;;
;;; Portions Copyright (C) 1987 Texas Instruments Incorporated.
;;; Portions Copyright (C) 1988, 1989 Franz Inc, Berkeley, Ca.
;;;
;;; Permission is granted to any individual or institution to use,
;;; copy, modify, and distribute this software, provided that this
;;; complete copyright and permission notice is maintained, intact, in
;;; all copies and supporting documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is"
;;; without express or implied warranty.
;;;
;;; Franz Incorporated provides this software "as is" without express
;;; or implied warranty.

(defpackage :clx-system (:use :cl :asdf))
(in-package :clx-system)  

(defclass clx-source-file (cl-source-file) ())
(defclass xrender-source-file (clx-source-file) ())

;;; CL-SOURCE-FILE, not CLX-SOURCE-FILE, so that we're not accused of
;;; cheating by rebinding *DERIVE-FUNCTION-TYPES* :-)
(defclass legacy-file (static-file) ())

(defsystem #:clx
  :description "An implementation of the X Window System protocol in Lisp."
  :author "Texas Instruments Incorporated.
Franz Inc, Berkeley, Ca.
Independent FOSS developers"
  :maintainer "sharplispers"
  :license "MIT"
  :depends-on (#+(or ecl sbcl) sb-bsd-sockets)
  :version "0.7.5"
  :serial t
  :default-component-class clx-source-file
  :in-order-to ((test-op (test-op "clx/test")))
  :components
  ((:file "package")
   (:file "depdefs")
   (:file "clx")
   #-(or openmcl allegro lispworks) (:file "dependent")
   #+openmcl (:file "dep-openmcl")
   #+allegro (:file "dep-allegro")
   #+lispworks (:file "dep-lispworks")
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
   #+allegro (:file "excldep")
   (:module "extensions"
	    :components
	    ((:file "shape")
	     (:file "big-requests")
	     (:file "xvidmode")
	     (:xrender-source-file "xrender")
             (:file "glx")
             (:file "gl" :depends-on ("glx"))
	     (:file "dpms")
             (:file "xtest")
             (:file "screensaver")
             (:file "randr")
             (:file "xinerama")
             (:file "dbe")
             (:file "xc-misc")
             (:file "dri2")
             (:file "composite")))
   (:static-file "NEWS")
   (:static-file "CHANGES")
   (:static-file "README.md")
   (:static-file "README-R5")
   (:legacy-file "exclMakefile")
   (:legacy-file "exclREADME")
   (:legacy-file "exclcmac" :pathname "exclcmac.lisp")
   (:legacy-file "excldepc" :pathname "excldep.c")
   (:legacy-file "sockcl" :pathname "sockcl.lisp")
   (:legacy-file "socket" :pathname "socket.c")
   (:legacy-file "defsystem" :pathname "defsystem.lisp")
   (:legacy-file "provide" :pathname "provide.lisp")
   (:legacy-file "cmudep" :pathname "cmudep.lisp")
   (:module "manual"
	    ;; TODO: teach asdf how to process texinfo files
	    :components ((:static-file "clx.texinfo")))
   (:module "debug"
	    :default-component-class legacy-file
	    :components
	    ((:file "debug" :pathname "debug.lisp")
	     (:file "describe" :pathname "describe.lisp")
	     (:file "event-test" :pathname "event-test.lisp")
	     (:file "keytrans" :pathname "keytrans.lisp")
	     (:file "trace" :pathname "trace.lisp")
	     (:file "util" :pathname "util.lisp")))))

(defsystem #:clx/demo
  :depends-on ("clx")
  :components
  ((:module "demo"
	    :components
	    ((:file "menu")
             (:file "bezier")
	     (:file "beziertest" :depends-on ("bezier"))
	     (:file "clclock")
	     (:file "clipboard")
	     (:file "clx-demos")
	     (:file "gl-test")
	     ;; FIXME: compiling this generates 30-odd spurious code
	     ;; deletion notes.  Find out why, and either fix or
	     ;; workaround the problem.
	     (:file "mandel")
	     (:file "zoid")
	     (:file "image")
	     (:file "trapezoid" :depends-on ("zoid"))))))

(defsystem #:clx/test
  :depends-on ("clx" "fiasco")
  :perform (test-op (o s)
		    (uiop:symbol-call :fiasco :run-tests :xlib-test))
  :components
  ((:module "tests"
	    :components
	    ((:file "package")
             (:file "util")
             (:file "core-protocol" :depends-on ("package" "util"))))))

#+sbcl
(defmethod perform :around ((o compile-op) (f xrender-source-file))
  ;; RENDER would appear to be an inherently slow protocol; further,
  ;; it's not set in stone, and consequently we care less about speed
  ;; than we do about correctness.
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (call-next-method)))

#+sbcl
(defmethod perform :around ((o compile-op) (f clx-source-file))
  ;; a variety of accessors, such as AREF-CARD32, are not
  ;; declared INLINE.  Without this (non-ANSI)
  ;; static-type-inference behaviour, SBCL emits an extra 100
  ;; optimization notes (roughly one fifth of all of the
  ;; notes emitted).  Since the internals are unlikely to
  ;; change much, and certainly the internals should stay in
  ;; sync, enabling this extension is a win.  (Note that the
  ;; use of this does not imply that applications using CLX
  ;; calls that expand into calls to these accessors will be
  ;; optimized in the same way).
  (let ((sb-ext:*derive-function-types* t)
        (sadx (find-symbol "STACK-ALLOCATE-DYNAMIC-EXTENT" :sb-c))
        (sadx-var (find-symbol "*STACK-ALLOCATE-DYNAMIC-EXTENT*" :sb-ext)))
    ;; deeply unportable stuff, this.  I will be shot.  We
    ;; want to enable the dynamic-extent declarations in CLX.
    (when (and sadx (sb-c::policy-quality-name-p sadx))
      ;; no way of setting it back short of yet more yukky stuff
      (proclaim `(optimize (,sadx 3))))
    (if sadx-var
        (progv (list sadx-var) (list t)
          (call-next-method))
        (call-next-method))))
