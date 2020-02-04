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
          composite-release-overlay-window))

(define-extension "Composite")

(defconstant +composite-major+ 0
  "Major version.")
(defconstant +composite-minor+ 4
  "Minor version.")


(defconstant +redirect-automatic+ 0
  "The automatic update type automatically updates the parent window.")
(defconstant +redirect-manual+ 1
  "Prevents some activities that would otherwise be automatic.")

;; xrequests

(defconstant  +composite-QueryVersion+ 0
  "Query for the version of composite.")
(defconstant  +composite-RedirectWindow+ 1
  "Store this hierarchy off-screen.")
(defconstant  +composite-RedirectSubwindows+ 2
  "Store only the sub-hierarchy.")
(defconstant  +composite-UnredirectWindow+ 3
  "Stop storing the window and subwindows.")
(defconstant  +composite-UnredirectSubwindows+ 4
  "Stop storing the sub-hierarchy.")
(defconstant  +composite-CreateRegionFromBorderClip+ 5
  "The region clinpped against the surrounding windows.")
(defconstant  +composite-NameWindowPixmap+ 6
  "The off-screen pixmap for the window.")
(defconstant  +composite-GetOverlayWindow+ 7
  "Get a surface to draw on.")
(defconstant  +composite-ReleaseOverlayWindow+ 8
  "Release the overlay surface.")


(defmacro composite-opcode (display)
  `(extension-opcode ,display "Composite"))

;; types

(deftype update-type () '(card8))

;; x requests

(defun composite-query-version (display)
  "Query for the version. All clients are expected to query!"
  (declare (type display display))
  (with-buffer-request-and-reply (display (composite-opcode display) nil :sizes (32))
                                 ((data +composite-QueryVersion+)
                                  (card32 +composite-major+)
                                  (card32 +composite-minor+))
    (values
     (card32-get 8)
     (card32-get 12))))

(defun composite-redirect-window (window update-type)
  "Store window and its children off-screen, using update-type for whether to
sync those or not."
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
  "Store the subwindows of the window (but not the window itself).
update-type determines if syncing is allowed."
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
  "Terminates the redirection."
  (let ((display (window-display window)))
    (declare (type display display)
             (type window window))
    (with-buffer-request (display (composite-opcode display))
      (data +composite-unredirectwindow+)
      (window window))))

(defun composite-unredirect-subwindows (window)
  "Terminates the redirection of the child hierarchies of window."
  (let ((display (window-display window)))
    (declare (type display display)
             (type window window))
    (with-buffer-request (display (composite-opcode display))
      (data +composite-unredirectsubwindows+)
      (window window))))

(defun composite-create-region-from-border-clip (window region)
  "Region clipped on surrounding windows."
  (let ((display (window-display window)))
    (declare (type display display)
             (type window window))
    (with-buffer-request (display (composite-opcode display))
      (data +composite-createregionfromborderclip+)
      (card32 region)
      (window window))))

(defun composite-name-window-pixmap (window drawable)
  "Refer to an off-screen pixmap for the window."
  (let ((display (window-display window)))
    (declare (type display display)
             (type window window))
    (with-buffer-request (display (composite-opcode display))
      (data +composite-namewindowpixmap+)
      (window window)
      (drawable drawable))))

(defun composite-get-overlay-window (window)
  "Take control of the window for composite use. A place to draw things without
interference. Requires a compositing window manager to be running in order to
use the overlay. Release it with COMPOSITE-RELEASE-OVERLAY-WINDOW."
  (let ((display (window-display window)))
    (declare (type display display)
             (type window window))
    (with-buffer-request-and-reply (display (composite-opcode display) nil :sizes (32))
                                   ((data +composite-getoverlaywindow+)
                                    (window window))
      (values (card32-get 8)))))

(defun composite-release-overlay-window (window)
  "Release a window which was controlled by COMPOSITE-GET-OVERLAY-WINDOW."
  (let ((display (window-display window)))
    (declare (type display display)
             (type window window))
    (with-buffer-request (display (composite-opcode display))
      (data +composite-releaseoverlaywindow+)
      (window window))))
