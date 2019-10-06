;;; -*- Mode: common-lisp; Package: xlib; Base: 10; Lowercase: Yes -*-
;;;
;;; CLX -- excldep.cl
;;;
;;; Copyright (c) 1987, 1988, 1989 Franz Inc, Berkeley, Ca.
;;;
;;; Permission is granted to any individual or institution to use, copy,
;;; modify, and distribute this software, provided that this complete
;;; copyright and permission notice is maintained, intact, in all copies and
;;; supporting documentation.
;;;
;;; Franz Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

(in-package :xlib)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :foreign)
  (require :process)			; Needed even if scheduler is not
					; running.  (Must be able to make
					; a process-lock.)
  )

(eval-when (:load-toplevel)
  (provide :clx))

#-(or little-endian big-endian)
(eval-when (eval compile load)
  (let ((x '#(1)))
    (if (not (eq 0 (sys::memref x
				#.(sys::mdparam 'comp::md-lvector-data0-norm)
				0 :unsigned-byte)))
	(pushnew :little-endian *features*)
	(pushnew :big-endian *features*))))

(defmacro correct-case (string)
  ;; This macro converts the given string to the
  ;; current preferred case, or leaves it alone in a case-sensitive mode.
  (let ((str (gensym)))
    `(let ((,str ,string))
       (case excl::*current-case-mode*
	 (:case-insensitive-lower
	  (string-downcase ,str))
	 (:case-insensitive-upper
	  (string-upcase ,str))
	 ((:case-sensitive-lower :case-sensitive-upper)
	  ,str)))))

(defconstant type-pred-alist
  '((card8  . card8p)
    (card16 . card16p)
    (card29 . card29p)
    (card32 . card32p)
    (int8   . int8p)
    (int16  . int16p)
    (int32  . int32p)
    (mask16 . card16p)
    (mask32 . card32p)
    (pixel  . card32p)
    (resource-id . card29p)
    (keysym . card32p)
    (angle  . anglep)
    (color  . color-p)
    (bitmap-format . bitmap-format-p)
    (pixmap-format . pixmap-format-p)
    (display  . display-p)
    (drawable . drawable-p)
    (window   . window-p)
    (pixmap   . pixmap-p)
    (visual-info . visual-info-p)
    (colormap . colormap-p)
    (cursor . cursor-p)
    (gcontext .  gcontext-p)
    (screen . screen-p)
    (font . font-p)
    (image-x . image-x-p)
    (image-xy . image-xy-p)
    (image-z . image-z-p)
    (wm-hints . wm-hints-p)
    (wm-size-hints . wm-size-hints-p)
    ))

;; This (if (and ...) t nil) stuff has a purpose -- it lets the old
;; sun4 compiler opencode the `and'.

(defun card8p (x)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum x))
  (if (and (excl:fixnump x) (> #.(expt 2 8) x) (>= x 0))
      t
      nil))

(defun card16p (x)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum x))
  (if (and (excl:fixnump x) (> #.(expt 2 16) x) (>= x 0))
      t
      nil))

(defun card29p (x)
  (declare (optimize (speed 3) (safety 0)))
  (if (or (and (excl:fixnump x) (>= (the fixnum x) 0))
	  (and (excl:bignump x) (> #.(expt 2 29) (the bignum x))
	       (>= (the bignum x) 0)))
      t
      nil))

(defun card32p (x)
  (declare (optimize (speed 3) (safety 0)))
  (if (or (and (excl:fixnump x) (>= (the fixnum x) 0))
	  (and (excl:bignump x) (> #.(expt 2 32) (the bignum x))
	       (>= (the bignum x) 0)))
      t
      nil))

(defun int8p (x)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum x))
  (if (and (excl:fixnump x) (> #.(expt 2 7) x) (>= x #.(expt -2 7)))
      t
      nil))

(defun int16p (x)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum x))
  (if (and (excl:fixnump x) (> #.(expt 2 15) x) (>= x #.(expt -2 15)))
      t
      nil))

(defun int32p (x)
  (declare (optimize (speed 3) (safety 0)))
  (if (or (excl:fixnump x)
	  (and (excl:bignump x) (> #.(expt 2 31) (the bignum x))
	       (>= (the bignum x) #.(expt -2 31))))
      t
      nil))

;; This one can be handled better by knowing a little about what we're
;; testing for.  Plus this version can handle (single-float pi), which
;; is otherwise larger than pi!
(defun anglep (x)
  (declare (optimize (speed 3) (safety 0)))
  (if (or (and (excl:fixnump x) (>= (the fixnum x) #.(truncate (* -2 pi)))
	       (<= (the fixnum x) #.(truncate (* 2 pi))))
	  (and (excl:single-float-p x)
	       (>= (the single-float x) #.(float (* -2 pi) 0.0s0))
	       (<= (the single-float x) #.(float (* 2 pi) 0.0s0)))
	  (and (excl:double-float-p x)
	       (>= (the double-float x) #.(float (* -2 pi) 0.0d0))
	       (<= (the double-float x) #.(float (* 2 pi) 0.0d0))))
      t
      nil))

(eval-when (:load-toplevel :execute :compile-toplevel)
  (mapcar #'(lambda (elt) (excl:add-typep-transformer (car elt) (cdr elt)))
	  type-pred-alist))

(defun fd-char-avail-p (socket-stream)
  (excl:read-no-hang-p socket-stream))

(defun stream-char-avail-p (socket-stream)
  (excl:read-no-hang-p socket-stream))

(defmacro with-interrupt-checking-on (&body body)
  `(locally (declare (optimize (safety 1)))
     ,@body))

(defun fd-read-bytes (fd vector start-index length)
  ;; Read from the given stream fd into 'vector', which has element type card8.
  ;; Start storing at index 'start-index' and read exactly 'length' bytes.
  ;; Return t if an error or eof occurred, nil otherwise.
  (declare (fixnum start-index length))
  (with-interrupt-checking-on
    (let ((end-index (+ start-index length)))
      (loop
	 (let ((next-index (excl:read-vector vector fd
					     :start start-index
					     :end end-index)))
	   (excl:if* (eq next-index start-index)
		     then			; end of file before was all filled up
		     (return t)
		     elseif (eq next-index end-index)
		     then			; we're all done
		     (return nil)
		     else (setq start-index next-index)))))))


(defun stream-read-bytes (stream vector start-index length)
  ;; Read from the given stream fd into 'vector', which has element type card8.
  ;; Start storing at index 'start-index' and read exactly 'length' bytes.
  ;; Return t if an error or eof occurred, nil otherwise.
  (declare (fixnum start-index length))
  (declare (type (or null stream) stream))
  (with-interrupt-checking-on
    (let ((end-index (+ start-index length)))
      (loop
	 (let ((next-index (excl:read-vector vector stream
					     :start start-index
					     :end end-index)))
	   (excl:if* (eq next-index start-index)
		     then			; end of file before was all filled up
		     (return t)
		     elseif (eq next-index end-index)
		     then			; we're all done
		     (return nil)
		     else (setq start-index next-index)))))))
