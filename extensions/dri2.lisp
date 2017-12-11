;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XLIB; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: DRI Extension
;;;   Created: 2014-11-17
;;;    Author: Johannes Martinez <johannes.martinez@gmail.com>
;;; ---------------------------------------------------------------------------
;;;
;;; (c) copyright 2014 by Johannes Martinez
;;;
;;; Permission is granted to any individual or institution to use,
;;; copy, modify, and distribute this software, provided that this
;;; complete copyright and permission notice is maintained, intact, in
;;; all copies and supporting documentation.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;;
(in-package :xlib)

(export '(dri2-query-version
	  dri2-authenticate
	  dri2-connect
	  dri2-get-buffersxo))

(define-extension "DRI2")

(defun dri2-opcode (display)
  (extension-opcode display "DRI2"))

;; version
(defconstant +dri-major+			2)
(defconstant +dri-minor+			0)

;; drivers
(defconstant DRI   0)
(defconstant VDPAU 1)

;; 0x0	DRI2BufferFrontLeft
;; 	0x1	DRI2BufferBackLeft
;; 	0x2	DRI2BufferFrontRight
;; 	0x3	DRI2BufferBackRight
;; 	0x4	DRI2BufferDepth
;; 	0x5	DRI2BufferStencil
;; 	0x6	DRI2BufferAccum
;; 	0x7	DRI2BufferFakeFrontLeft
;; 	0x8	DRI2BufferFakeFrontRight
;; 	0x9	DRI2BufferDepthStencil
;; 	0xa	DRI2BufferHiz

;; x requests
(defconstant +dri2-query-version+		0)
(defconstant +dri2-connect+			1)
(defconstant +dri2-authenticate+		2)
(defconstant +dri2-create-drawable+		3)
(defconstant +dri2-destroy-drawable+		4)
(defconstant +dri2-get-buffers+		        5)
(defconstant +dri2-copy-region+		        6)
(defconstant +dri2-get-buffers-with-format+	7)
(defconstant +dri2-swap-buffers+		8)
(defconstant +dri2-get-msc+			9)
(defconstant +dri2-wait-msc+			10)
(defconstant +dri2-wait-sbc+			11)
(defconstant +dri2-swap-interval+		12)
(defconstant +dri2-get-param+			13)


;; structs

(def-clx-class (dri2-buffer (:copier nil))
  (attachment 0 :type card32)
  (name 0 :type card32)
  (pitch 0 :type card32)
  (cpp 0 :type card32)
  (flags 0 :type card32))



;; x requests

(defun dri2-query-version (display)
""
  (with-buffer-request-and-reply (display (dri2-opcode display) nil)
				 ((data +dri2-query-version+)
				  (card32 +dri-major+)
				  (card32 +dri-minor+))
    (values
     (card32-get 8)
     (card32-get 12))
))

(defun dri2-connect (window driver-type)
  ""
  (let ((display (window-display window)))
    (with-buffer-request-and-reply (display (dri2-opcode display) nil)
				   ((data +dri2-connect+)
				    (window window)
				    (card32 driver-type))
      (let* ((driver-name-length (card32-get 8))
	    (device-name-length (card32-get 12))
	    (device-start (+ 32 (- 4 (mod driver-name-length 4)))))
	(values
	 (string-get driver-name-length 32)
	 (string-get device-name-length device-start))))))

(defun dri2-authenticate (window token)
""
  (let ((display (window-display window)))
    (with-buffer-request-and-reply (display (dri2-opcode display) nil)
				   ((data +dri2-authenticate+)
				    (window window)
				    (card32 token))
      (values
       (card32-get 8)))))

(defun dri2-create-drawable (drawable)
""
  (let ((display (drawable-display drawable)))
    (with-buffer-request-and-reply (display (dri2-opcode display) nil)
				   ((data +dri2-create-drawable+))
      (values))))
(defun dri2-destroy-drawable (drawable)
""
  (let ((display (drawable-display drawable)))
    (with-buffer-request-and-reply (display (dri2-opcode display) nil)
				   ()
      (values))))
(defun dri2-get-buffers (drawable attachment-list)
  ""
  (let* ((display (drawable-display drawable))
	 (len (length attachment-list) )
	 (seq (make-array len :initial-contents attachment-list))
	)
    (with-buffer-request-and-reply (display (dri2-opcode display) nil)
				   ((data +dri2-get-buffers+)
				    (drawable drawable)
				    (card32 len)
				    (( sequence :format card32) seq))
      ;; (let ((num (card32-get 16)))
      ;; 	(values 
      ;; 	 (card32-get 8)
      ;; 	 (card32-get 12)
      ;; 	 (loop :for buf :from 1 :to num
      ;; 	       :for offset := 32 :then (+ offset 20)
      ;; 	       :collect (make-dri2-buffer :attachment (card32-get offset)
      ;; 					  :name       (card32-get (+ offset 4))
      ;; 					  :pitch (card32-get (+ offset 8))
      ;; 					  :cpp (card32-get (+ offset 12))
      ;; 					  :flags (card32-get (+ offset 16))))))
      (values
       (card32-get 16)
       (card32-get 12)
       (card32-get 8))
      )))
(defun dri2-copy-region (drawable)
""
  (let ((display (drawable-display drawable)))
    (with-buffer-request-and-reply (display (dri2-opcode display) nil)
				   ((data +dri2-copy-region+))
      (values))))

(defun dri2-get-buffers-with-format (drawable)
""
  (let ((display (drawable-display drawable)))
    (with-buffer-request-and-reply (display (dri2-opcode display) nil)
				   ((data +dri2-get-buffers-with-format+))
      (values))))

(defun dri2-swap-buffers (drawable)
""
  (let ((display (drawable-display drawable)))
    (with-buffer-request-and-reply (display (dri2-opcode display) nil)
				   ((data +dri2-swap-buffers+))
      (values))))
(defun dri2-get-msc (drawable)
""
  (let ((display (drawable-display drawable)))
    (with-buffer-request-and-reply (display (dri2-opcode display) nil)
				   ((data +dri2-get-msc+))
      (values))))

(defun dri2-wait-msc (drawable)
""
  (let ((display (drawable-display drawable)))
    (with-buffer-request-and-reply (display (dri2-opcode display) nil)
				   ((data +dri2-wait-msc+))
      (values))))

(defun dri2-swap-interval (drawable)
  ""
  (let ((display (drawable-display drawable)))
    (with-buffer-request (display (dri2-opcode display))
      (data +dri2-swap-interval+))))

(defun dri2-get-param (drawable)
""
  (let ((display (drawable-display drawable)))
    (with-buffer-request-and-reply (display (dri2-opcode display) nil)
				   ((data +dri2-get-param+))
      (values))))
