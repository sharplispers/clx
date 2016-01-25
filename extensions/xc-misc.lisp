;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XLIB; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: XC Misc Extension
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

(export '(xc-get-version
	  xc-get-xid-range
	  xc-get-xid-list
	  ))

(define-extension "XC-MISC")


;; version
(defconstant +xc-major+               1)
(defconstant +xc-minor+               1)


;; xc major opcode
(defun xc-opcode (display)
  (extension-opcode display "XC-MISC"))

;; xc minor opcodes

(defconstant +xc-get-version+         0)
(defconstant +xc-get-xid-range+       1)
(defconstant +xc-get-xid-list+        2)

;; x requests

(defun xc-get-version (display)
  (declare (type display display))
  (with-buffer-request-and-reply (display (xc-opcode display) nil :sizes (16))
				 ((data +xc-get-version+)
				  (card16 +xc-major+)
				  (card16 +xc-minor+))
    (values
     (card16-get 8)
     (card16-get 10))))

(defun xc-get-xid-range (display)
  "returns a range of available resource IDs for the client issuing the request."
  (declare (type display display))
  (with-buffer-request-and-reply (display (xc-opcode display) nil :sizes (32))
				 ((data +xc-get-xid-range+))
    (values
     (card32-get 8)
     (card32-get 12))))

(defun xc-get-xid-list (display count &optional (result-type 'list))
  "This request returns a sequence of individual resource IDs in ids. Count is the number 
of resource IDs requested. The number returned may be smaller than the number requested."
  (declare (type display display)
	   (type card32 count))
  (with-buffer-request-and-reply (display (xc-opcode display) nil :sizes (32))
				 ((data +xc-get-xid-list+)
				  (card32 count))
    (let ((num (card32-get 8)))
      (values
       (sequence-get :format card32 :result-type result-type 
		     :length num :index 32)))))










