;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XLIB; -*-
;;;
;;; (c) copyright 2006 Richard Kreuter
;;; (c) copyright 2007 by Christophe Rhodes
;;; (c) copyright 2019 by Johannes Martinez Calzada 
;;;
;;; Permission is granted to any individual or institution to use,
;;; copy, modify, and distribute this software, provided that this
;;; complete copyright and permission notice is maintained, intact, in
;;; all copies and supporting documentation.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(in-package "XLIB")

;;; No new events or errors are defined by this extension.  (Big
;;; Requests Extension, section 3)
;;;
;;; The name of this extension is "BIG-REQUESTS" (Big Requests
;;; Extension, section 4)
(define-extension "BIG-REQUESTS")

(defparameter *sensible-buffer-size* 262140 "Size in bytes. Not everyone is going to want a 4Mb buffer for each DISPLAY. Set to 256kb the max the core protocol can transmit in a card16 index.")


(defun enable-big-requests (display)
  (declare (type display display))
  (let ((opcode (extension-opcode display "BIG-REQUESTS")))
    (with-buffer-request-and-reply (display opcode nil)	
	((data 0))
      (let ((maximum-request-length (card32-get 8)))  ; this is in 4byte units
	(setf (display-extended-max-request-length display) maximum-request-length)))))

 ;;ensure buffer-bbuf is point to the right place
 ;;we need to flush the current buffer to not lose data 
 ;;we need to create new buffer
;;and set it up in all the weird places clx references it

(defun set-new-buffer-size (display size)
  "Replaces the current BUFFER in DISPLAY with one of SIZE * CARD8."
  (declare (type display display)
	   (type fixnum *sensible-buffer-size*)
	   (type card32 size)) ; should be an INDEX type when we get 32bit indexes.
  (let* ((ext-max-length 262140;(display-extended-max-request-length display) ;when br working
	  )
	 (new-buffer-size  (min (* ext-max-length 4) *sensible-buffer-size* size))
	 (old-buffer-size (display-size display))
	 (new-buffer nil))
    (unless (= new-buffer-size old-buffer-size)
      (setf new-buffer (make-array new-buffer-size :element-type 'card8 :initial-element 0))
      ;;we need to grab the current buffer so no one uses it. recursive safe.
      (holding-lock ((display-event-lock display) display "CLX Event Lock")
	(holding-lock ((buffer-lock display) display "CLX Display Lock")
	  (display-force-output display)
	  (setf (display-obuf8 display) new-buffer
		(display-size display) new-buffer-size
		*output-buffer-size* new-buffer-size))))))

;; use buffer replace? or just make sure to empty?
;; *output-buffer-size* in dependent.lisp is in bytes. 

;; Todo: Big requests require card32 indices, some functions depend on indices being card16s.
;; An index32 type might have to be created.
;; Big Requests put 0 in the length spot (2) followed by a card32 of actual length.
;; Retrieving and setting the actual length shouldn't be a problem (advance-buffer-offset?).
;; Keeping index arithmetic performant while allowing both card16 and card32 indices and
;; not have the buffer code blow up is the issue. JMC



