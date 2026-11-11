;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XLIB/XINPUT2; -*-
;;;
;;; A small, deliberately focused XInput 2 extension interface.  This file
;;; only contains the requests and event decoding needed for raw keyboard
;;; events; other XI2 requests can be added without changing this interface.

(defpackage #:xlib/xinput2
  (:use #:common-lisp #:xlib)
  (:import-from #:xlib
                #:card16-get
                #:card32-get
                #:card16
                #:card32
                #:resource-id
                #:data
                #:pad16
                #:with-buffer-request
                #:with-buffer-request-and-reply)
  (:export
   #:query-version
   #:select-raw-key-events
   #:clear-raw-key-events
   #:xinput-opcode
   #:xinput-generic-event-p
   #:decode-raw-key-event
   #:raw-key-event
   #:raw-key-event-evtype
   #:raw-key-event-deviceid
   #:raw-key-event-time
   #:raw-key-event-detail
   #:raw-key-event-sourceid
   #:raw-key-event-valuators-len
   #:raw-key-event-flags
   #:raw-key-event-data
   #:+all-master-devices+
   #:+raw-key-press+
   #:+raw-key-release+))

(in-package #:xlib/xinput2)

;; XI2 has no traditional event-number range.  Its events are carried by the
;; core GenericEvent (event code 35), whose extension and evtype fields are
;; decoded by CLX itself.
(define-extension "XInputExtension")

(defconstant +query-version+ 47)
(defconstant +select-events+ 46)
(defconstant +all-master-devices+ 1)
(defconstant +raw-key-press+ 13)
(defconstant +raw-key-release+ 14)

;; The extension's advertised version is intentionally 2.1.  Version 2.1 is
;; needed for raw events to be delivered while another client owns a grab.
(defconstant +major-version+ 2)
(defconstant +minor-version+ 1)

(defun xinput-opcode (display)
  "Return the major opcode assigned to XInputExtension on DISPLAY."
  (extension-opcode display "XInputExtension"))

(defun query-version (display &optional (major +major-version+)
                                      (minor +minor-version+))
  "Negotiate XI2 VERSION with DISPLAY.

The returned values are the version selected by the server.  Callers that
require raw events should reject a result below 2.1 before selecting events."
  (declare (type display display)
           (type card16 major minor))
  (with-buffer-request-and-reply (display (xinput-opcode display) nil)
    ((data +query-version+)
     (card16 major)
     (card16 minor))
    (values (card16-get 8)
            (card16-get 10))))

(defun %raw-key-mask ()
  ;; XISelectEvents masks are 32-bit words.  RawKeyPress and RawKeyRelease
  ;; are bits 13 and 14 respectively.
  (logior (ash 1 +raw-key-press+)
          (ash 1 +raw-key-release+)))

(defun select-raw-key-events (window)
  "Select XI2 raw key press and release events on WINDOW.

The X11 protocol's MASK_LEN is measured in four-byte units, hence the one
word mask below (libXi's C API exposes a byte count instead)."
  (declare (type window window))
  (let ((display (window-display window)))
    (with-buffer-request (display (xinput-opcode display))
      (data +select-events+)
      (resource-id (window-id window))
      (card16 1)                       ;number of XIEventMask records
      (pad16 0)
      (card16 +all-master-devices+)
      (card16 1)                       ;mask_len, in CARD32s
      (card32 (%raw-key-mask)))))

(defun clear-raw-key-events (window)
  "Clear all XI2 event selection for XIAllMasterDevices on WINDOW."
  (declare (type window window))
  (let ((display (window-display window)))
    (with-buffer-request (display (xinput-opcode display))
      (data +select-events+)
      (resource-id (window-id window))
      (card16 1)
      (pad16 0)
      (card16 +all-master-devices+)
      (card16 0))))

(defclass raw-key-event ()
  ((evtype :initarg :evtype :reader raw-key-event-evtype)
   (deviceid :initarg :deviceid :reader raw-key-event-deviceid)
   (time :initarg :time :reader raw-key-event-time)
   (detail :initarg :detail :reader raw-key-event-detail)
   (sourceid :initarg :sourceid :reader raw-key-event-sourceid)
   (valuators-len :initarg :valuators-len :reader raw-key-event-valuators-len)
   (flags :initarg :flags :reader raw-key-event-flags)
   ;; Keep the complete GenericEvent payload available to future XI2 users.
   (data :initarg :data :reader raw-key-event-data)))

(defun xinput-generic-event-p (display extension evtype)
  "Whether EXTENSION and EVTYPE identify an XI2 raw-key GenericEvent."
  (and (= extension (xinput-opcode display))
       (or (= evtype +raw-key-press+)
           (= evtype +raw-key-release+))))

(defun %card16 (data index little-endian-p)
  (logior (if little-endian-p
              (aref data index)
              (ash (aref data index) 8))
          (if little-endian-p
              (ash (aref data (1+ index)) 8)
              (aref data (1+ index)))))

(defun %card32 (data index little-endian-p)
  (if little-endian-p
      (logior (aref data index)
              (ash (aref data (+ index 1)) 8)
              (ash (aref data (+ index 2)) 16)
              (ash (aref data (+ index 3)) 24))
      (logior (ash (aref data index) 24)
              (ash (aref data (+ index 1)) 16)
              (ash (aref data (+ index 2)) 8)
              (aref data (+ index 3)))))

(defun decode-raw-key-event (display extension evtype data)
  "Decode the fixed XI2 raw-key header in DATA.

DATA is the fresh byte vector supplied by CLX's GenericEvent decoder.  It
starts at wire byte 10, immediately before the XI2 deviceid field.  The
remaining valuator data is not interpreted, but is retained in the returned
RAW-KEY-EVENT object."
  (declare (type display display)
           (type card8 extension)
           (type card16 evtype)
           (type (vector (unsigned-byte 8)) data))
  (unless (xinput-generic-event-p display extension evtype)
    (return-from decode-raw-key-event nil))
  ;; deviceid, time, detail, sourceid, valuators_len, and flags occupy 18
  ;; bytes starting at GenericEvent byte 10.
  (when (< (length data) 18)
    (return-from decode-raw-key-event nil))
  (let ((little-endian-p (eq (display-byte-order display) :lsbfirst)))
    (make-instance 'raw-key-event
                   :evtype evtype
                   :deviceid (%card16 data 0 little-endian-p)
                   :time (%card32 data 2 little-endian-p)
                   :detail (%card32 data 6 little-endian-p)
                   :sourceid (%card16 data 10 little-endian-p)
                   :valuators-len (%card16 data 12 little-endian-p)
                   :flags (%card32 data 14 little-endian-p)
                   :data data)))

;;; EOF
