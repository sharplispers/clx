;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XLIB; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Composite Extension
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

(export '(composite-query-version
	  composite-redirect-window
	  composite-redirect-subwindows
	  composite-unredirect-window
	  composite-unredirect-subwindows
	  composite-get-overlay-window
	  
	  ))
(define-extension "Composite")

(defconstant +composite-major+    0)
(defconstant +composite-minor+    4)


(defconstant +redirect-automatic+		0)
(defconstant +redirect-manual+			1)

;; xrequests

(defconstant  +composite-QueryVersion+			0)
(defconstant  +composite-RedirectWindow+		1)
(defconstant  +composite-RedirectSubwindows+		2)
(defconstant  +composite-UnredirectWindow+		3)
(defconstant  +composite-UnredirectSubwindows+		4)
(defconstant  +composite-CreateRegionFromBorderClip+	5)
(defconstant  +composite-NameWindowPixmap+		6)
(defconstant  +composite-GetOverlayWindow+             7)
(defconstant  +composite-ReleaseOverlayWindow+         8)


(defmacro composite-opcode (display)
  `(extension-opcode ,display "Composite"))

;; types

(deftype update-type () '(card8))



;; x requests

(defun composite-query-version (display)
  ""
  (declare (type display display))
  (with-buffer-request-and-reply (display (composite-opcode display) nil :sizes (32))
				 ((data +composite-QueryVersion+)
				  (card32 +composite-major+)
				  (card32 +composite-minor+))
    (values
     (card32-get 8)
     (card32-get 12))))



(defun composite-redirect-window (window update-type)
  ""
  (let ((display (window-display window)))
    (declare (type display display)
	     (type window window)
	     (type update-type update-type))  
    (with-buffer-request (display (composite-opcode display))
      (data +composite-redirectwindow+)
      (window window)
      (card8 update-type)
      (card8 0)
      (card16 0))))

(defun composite-redirect-subwindows (window update-type)
  ""
  (let ((display (window-display window)))
    (declare (type display display)
	     (type window window)
	     (type update-type update-type))  
    (with-buffer-request (display (composite-opcode display))
      (data +composite-redirectsubwindows+)
      (window window)
      (card8 update-type)
      (card8 0)
      (card16 0))))

(defun composite-unredirect-window (window)
  ""
  (let ((display (window-display window)))
    (declare (type display display)
	     (type window window))  
    (with-buffer-request (display (composite-opcode display))
      (data +composite-unredirectwindow+)
      (window window))))

(defun composite-unredirect-subwindows (window)
  ""
  (let ((display (window-display window)))
    (declare (type display display)
	     (type window window))  
    (with-buffer-request (display (composite-opcode display))
      (data +composite-unredirectsubwindows+)
      (window window))))

(defun composite-create-region-from-border-clip (window region)
  ""
  (let ((display (window-display window)))
    (declare (type display display)
	     (type window window))
    (with-buffer-request (display (composite-opcode display)) 
      (data +composite-createregionfromborderclip+)
      (card32 region)
      (window window))))
	    
(defun composite-name-window-pixmap (window drawable)
  ""
  (let ((display (window-display window)))
    (declare (type display display)
	     (type window window))
    (with-buffer-request (display (composite-opcode display))
      ((data +composite-namewindowpixmap+)
       (window window)
       (drawable drawable)))))

(defun composite-get-overlay-window (window)
  ""
  (let ((display (window-display window)))
    (declare (type display display)
	     (type window window))
    (with-buffer-request-and-reply (display (composite-opcode display) nil :sizes (32))
				   ((data +composite-getoverlaywindow+)
				    (window window)
				    )
      (values
       (card32-get 8)))))

(defun composite-release-overlay-window (window)
  ""
  (let ((display (window-display window))))
  (declare (type display display)
	   (type window window))
  (with-buffer-request (display (composite-opcode display))
				 ((data +composite-releaseoverlaywindow+)
				  (window window))))
