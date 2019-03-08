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

(defun enable-big-requests (display)
  (declare (type display display))
  (let ((opcode (extension-opcode display "BIG-REQUESTS")))
    (with-buffer-request-and-reply (display opcode nil)
	((data 0))
      (let ((maximum-request-length (card32-get 8)))
	(setf (display-extended-max-request-length display) maximum-request-length)
;	(set-new-buffer-size display maximum-request-length)
	))))

(defun set-new-buffer-size (display size)
  "Replaces the current BUFFER in DISPLAY with one of SIZE * CARD8."
  (declare (type display display)
	   (type card32 size))
  (assert (<= size (display-extended-max-request-length display))
	  (size) "~a is larger than extended-max-request-length" size)
  (let ((new-buffer (make-array size :element-type 'card8 :initial-element 0)))
    ;;we need to grab the current buffer so no one uses it.
    (holding-lock ((display-event-lock display) display "CLX Event Lock")
      (holding-lock ((buffer-lock display) display "CLX Display Lock")
	(display-force-output display)
	(setf (display-obuf8 display) new-buffer
	      (display-size display) size
	      (display-max-request-length display) (1- size)
	      *output-buffer-size* (1-  size)))))
  ;;we need to flusx the current buffer to not lose data 
  
  ;;we need to create new buffer
  
  ;;and set it
  ;;update display
  )
