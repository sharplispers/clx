;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XLIB; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Double Buffer Extension
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

(export '(;;  x requests
	  dbe-query-version
	  dbe-get-visual-info
	  dbe-allocate-back-buffer-name
	  dbe-deallocate-back-buffer-name
	  dbe-swap-buffers
	  dbe-begin-idiom
	  dbe-end-idiom
	  dbe-get-back-buffer-attributes
	  ;; convenience function
	  create-back-buffer
	  ;; swap-hint constants
	  +undefined+
	  +background+
	  +untouched+
	  +copied+))

(define-extension "DOUBLE-BUFFER"
  :errors (dbe-bad-buffer))

;; version

(defconstant +dbe-major+                    1)
(defconstant +dbe-minor+                    0)

;; request codes

(defconstant +query-version+                0)
(defconstant +allocate-back-buffer-name+    1)
(defconstant +deallocate-back-buffer-name+  2)
(defconstant +swap-buffers+                 3)
(defconstant +begin-idiom+                  4)
(defconstant +end-idiom+                    5)
(defconstant +get-visual-info+              6)
(defconstant +get-back-buffer-attributes+   7)

;; swap actions

(defconstant +undefined+  #x00)
(defconstant +background+ #x01)
(defconstant +untouched+  #x02)
(defconstant +copied+     #x03)

;; Errors ??

(define-condition dbe-bad-buffer (request-error) ())
(define-error dbe-bad-buffer decode-core-error)

;; class and structure definitions

;; use def-clx-class instead of deftype to be consistent with clx and be able to submit
;; back-buffers to other x-requests that accept drawables, for convenience should probably just
;; have get-visual-info return visual-ids or visual-info structs

(def-clx-class (back-buffer (:include drawable) (:copier nil)
			    (:print-function print-drawable)))

(defstruct visinfo 
  (visual-id 0 :type (unsigned-byte 32))
  (depth 0 :type (unsigned-byte 8))
  (perflevel 0 :type (unsigned-byte 8))
  (unused 0 :type (unsigned-byte 16)))

;; convenience function

(defun create-back-buffer (window)
  "Returns a back-buffer structure associated with the given window"
  
  (let* ((display (window-display window))
	 (bb (make-back-buffer :display display))
	 (id (allocate-resource-id display bb 'back-buffer)))
    (declare (type display display)
	     (type window window)
	     (type drawable bb))
    (setf (back-buffer-id bb) id)
    (if (window-p window)
	(dbe-allocate-back-buffer-name window bb))
    bb))

;; X requests

(defun dbe-query-version (display)
  "Returns Major and Minor versions as values"
  (declare (display display))
  (with-buffer-request-and-reply (display (extension-opcode display "DOUBLE-BUFFER") nil)
				   ((data +query-version+)
				    (card8 +dbe-major+)
				    (card8 +dbe-minor+))
      (values
       (card8-get 8)
       (card8-get 9))))

(defun dbe-allocate-back-buffer-name (window back-buffer &optional (swap-hint +copied+))
  "Associates the given back-buffer with given window, optional swap-hint "
  (let ((display (window-display window)))
    (declare (type display display)
	     (type window window)
	     (type back-buffer back-buffer)
	     (type card8 swap-hint))
    (with-buffer-request (display (extension-opcode display "DOUBLE-BUFFER")) 
      (data +allocate-back-buffer-name+)
      (window window)
      (drawable back-buffer)
      (card8 swap-hint)
      (pad8)                                  ;unused
      (pad16))))				 ;unused

(defun dbe-deallocate-back-buffer-name (back-buffer)
  "disconnects back-buffer from window, does not free xid from the server allowing buffer to be associated with another window"
  (let ((display (back-buffer-display back-buffer)))
    (declare (type display display)
	     (type back-buffer back-buffer))
    (with-buffer-request (display (extension-opcode display "DOUBLE-BUFFER"))
      (data +deallocate-back-buffer-name+)
      (drawable back-buffer))))

(defun dbe-swap-buffers  (window-list)
  "takes a list of (window swap-action) pairs and swaps their back-buffers"
  (let ((display (window-display (caar window-list)))
	 (num (length window-list))
	 (seq (lst->array window-list)))
    (declare (type display display)
	     (type fixnum num)
	     (type simple-array seq))
       (with-buffer-request (display (extension-opcode display "DOUBLE-BUFFER"))
	 (data +swap-buffers+)
	 (card32 num)
	 ((sequence :format card32) seq))))

(defun dbe-begin-idiom (display)
  (with-buffer-request (display (extension-opcode display "DOUBLE-BUFFER"))
    (data +begin-idiom+)))

(defun dbe-end-idiom (display)
  (with-buffer-request (display (extension-opcode display "DOUBLE-BUFFER"))
    (data +end-idiom+)))

(defun dbe-get-visual-info (drawable-list)
  "Not very useful in this modern graphics age, nothing added or taken away from the bare x." 
  (let ((display (drawable-display (car drawable-list))) 
	(num (length drawable-list))
	(wl (map 'vector #'drawable-id drawable-list)))
    (declare (type display display)
	     (type fixnum num))
    (with-buffer-request-and-reply (display (extension-opcode display "DOUBLE-BUFFER") nil :sizes (8 16 32))
				   ((data +get-visual-info+)
				    (card32 num)
				    ((sequence :format card32) wl))
      (values (let ((num (card32-get 8))
      		    (next  32)
      		    (result-list ()))
      		(declare (type fixnum num next))
      		(dotimes (i num (nreverse result-list))
      		  (push 
      		   (loop :for i :from 1 :to (card32-get next)  
      			 :for off := (+ next 4) :then (+ off 8)
      			 :collect (make-visinfo :visual-id (card32-get off)
      						:depth (card8-get (+ off 4))
      						:perflevel (card8-get (+ off 5)))
      			 :finally (setf next (+ off 8 )))
      		   result-list)))))))

(defun dbe-get-back-buffer-attributes (back-buffer)
  "Returns the window that a back-buffer outputs to or nil if not associated with any window"
  (declare (type back-buffer back-buffer))
  (let ((display (back-buffer-display back-buffer)))
    (declare (type display display))
    (with-buffer-request-and-reply (display (extension-opcode display "DOUBLE-BUFFER") nil :sizes 32)
				   ((data +get-back-buffer-attributes+)
				    (drawable back-buffer))
      (values
       (or-get 8 null window)))))

;;  utility functions

(defun lst->array (lst)
  (make-array (* 2 (length lst)) :initial-contents 
	      (loop :for x :in lst
		    :collect (drawable-id (car x))
		    :collect (cadr x))))
