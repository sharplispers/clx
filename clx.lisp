;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 2909
;;;			       AUSTIN, TEXAS 78769
;;;
;;; Copyright (C) 1987 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

;; Primary Interface Author:
;;	Robert W. Scheifler
;;	MIT Laboratory for Computer Science
;;	545 Technology Square, Room 418
;;	Cambridge, MA 02139
;;	rws@zermatt.lcs.mit.edu

;; Design Contributors:
;;	Dan Cerys, Texas Instruments
;;	Scott Fahlman, CMU
;;      Charles Hornig, Symbolics
;;      John Irwin, Franz
;;	Kerry Kimbrough, Texas Instruments
;;	Chris Lindblad, MIT
;;	Rob MacLachlan, CMU
;;	Mike McMahon, Symbolics
;;	David Moon, Symbolics
;;	LaMott Oren, Texas Instruments
;;	Daniel Weinreb, Symbolics
;;	John Wroclawski, MIT
;;	Richard Zippel, Symbolics

;; Primary Implementation Author:
;;	LaMott Oren, Texas Instruments

;; Implementation Contributors:
;;      Charles Hornig, Symbolics
;;      John Irwin, Franz
;;	Chris Lindblad, MIT
;;	Robert Scheifler, MIT

;;;
;;; Change history:
;;;
;;;  Date	Author		Description
;;; -------------------------------------------------------------------------------------
;;; 04/07/87	R.Scheifler	Created code stubs
;;; 04/08/87	L.Oren		Started Implementation
;;; 05/11/87	L.Oren		Included draft 3 revisions
;;; 07/07/87	L.Oren		Untested alpha release to MIT
;;; 07/17/87	L.Oren		Alpha release
;;; 08/**/87	C.Lindblad	Rewrite of buffer code
;;; 08/**/87	et al		Various random bug fixes
;;; 08/**/87	R.Scheifler	General syntactic and portability cleanups
;;; 08/**/87	R.Scheifler	Rewrite of gcontext caching and shadowing
;;; 09/02/87	L.Oren		Change events from resource-ids to objects
;;; 12/24/87	R.Budzianowski	KCL support
;;; 12/**/87	J.Irwin		ExCL 2.0 support
;;; 01/20/88	L.Oren		Add server extension mechanisms
;;; 01/20/88	L.Oren		Only force output when blocking on input
;;; 01/20/88	L.Oren		Uniform support for :event-window on events
;;; 01/28/88	L.Oren		Add window manager property functions
;;; 01/28/88	L.Oren		Add character translation facility
;;; 02/**/87	J.Irwin		Allegro 2.2 support

;;; This is considered a somewhat changeable interface.  Discussion of better
;;; integration with CLOS, support for user-specified subclassess of basic
;;; objects, and the additional functionality to match the C Xlib is still in
;;; progress.  Bug reports should be addressed to bug-clx@expo.lcs.mit.edu.

;; Note: all of the following is in the package XLIB.

(in-package :xlib)

(pushnew :clx *features*)
(pushnew :xlib *features*)

(defparameter *version* "MIT R5.02")
(pushnew :clx-mit-r4 *features*)
(pushnew :clx-mit-r5 *features*)

(defparameter *protocol-major-version* 11.)
(defparameter *protocol-minor-version* 0)

(defparameter *x-tcp-port* 6000) ;; add display number

;; Note: if you have read the Version 11 protocol document or C Xlib manual, most of
;; the relationships should be fairly obvious.  We have no intention of writing yet
;; another moby document for this interface.

;; Types employed: display, window, pixmap, cursor, font, gcontext, colormap, color.
;; These types are defined solely by a functional interface; we do not specify
;; whether they are implemented as structures or flavors or ...  Although functions
;; below are written using DEFUN, this is not an implementation requirement (although
;; it is a requirement that they be functions as opposed to macros or special forms).
;; It is unclear whether with-slots in the Common Lisp Object System must work on
;; them.

;; Windows, pixmaps, cursors, fonts, gcontexts, and colormaps are all represented as
;; compound objects, rather than as integer resource-ids.  This allows applications
;; to deal with multiple displays without having an explicit display argument in the
;; most common functions.  Every function uses the display object indicated by the
;; first argument that is or contains a display; it is an error if arguments contain
;; different displays, and predictable results are not guaranteed.

;; Each of window, pixmap, cursor, font, gcontext, and colormap have the following
;; five functions:

;(defun make-<mumble> (display resource-id)
;  ;; This function should almost never be called by applications, except in handling
;  ;; events.  To minimize consing in some implementations, this may use a cache in
;  ;; the display.  Make-gcontext creates with :cache-p nil.  Make-font creates with
;  ;; cache-p true.
;  (declare (type display display)
;	   (type integer resource-id)
;	   (clx-values <mumble>)))

;(defun <mumble>-display (<mumble>)
;  (declare (type <mumble> <mumble>)
;	   (clx-values display)))

;(defun <mumble>-id (<mumble>)
;  (declare (type <mumble> <mumble>)
;	   (clx-values integer)))

;(defun <mumble>-equal (<mumble>-1 <mumble>-2)
;  (declare (type <mumble> <mumble>-1 <mumble>-2)))

;(defun <mumble>-p (<mumble>-1 <mumble>-2)
;  (declare (type <mumble> <mumble>-1 <mumble>-2)
;	   (clx-values boolean)))


(deftype generalized-boolean () 't)	; (or null (not null))

(deftype card32 () '(unsigned-byte 32))

(deftype card29 () '(unsigned-byte 29))

(deftype card24 () '(unsigned-byte 24))

(deftype int32 () '(signed-byte 32))

(deftype card16 () '(unsigned-byte 16))

(deftype int16 () '(signed-byte 16))

(deftype card8 () '(unsigned-byte 8))

(deftype int8 () '(signed-byte 8))

(deftype card4 () '(unsigned-byte 4))

; Note that we are explicitly using a different rgb representation than what
; is actually transmitted in the protocol.

(deftype rgb-val () '(real 0 1))

; Note that we are explicitly using a different angle representation than what
; is actually transmitted in the protocol.

(deftype angle () '(real #.(* -2 pi) #.(* 2 pi)))

(deftype mask32 () 'card32)

(deftype mask16 () 'card16)

(deftype pixel () '(unsigned-byte 32))
(deftype image-depth () '(integer 0 32))

(deftype resource-id () 'card29)

(deftype keysym () 'card32)

; The following functions are provided by color objects:

; The intention is that IHS and YIQ and CYM interfaces will also exist.
; Note that we are explicitly using a different spectrum representation
; than what is actually transmitted in the protocol.

(defclass color ()
  ((red :initarg :red :initform 0.0 :type rgb-val :accessor color-red)
   (green :initarg :green :initform 0.0 :type rgb-val :accessor color-green)
   (blue :initarg :blue :initform 0.0 :type rgb-val :accessor color-blue)))

(defmethod print-object ((color color) stream)
  (with-slots (red green blue) color
    (print-unreadable-object (color stream :type t)
      (format stream "~@{~d~^ ~}" red green blue))))

(defun make-color (&key (red 1.0) (green 1.0) (blue 1.0) &allow-other-keys)
  (declare (type rgb-val red green blue))
  (declare (clx-values color))
  (make-instance 'color :red red :green green :blue blue))

(defun color-rgb (color)
  (declare (type color color))
  (declare (clx-values red green blue))
  (values (color-red color) (color-green color) (color-blue color)))

(defclass bitmap-format ()
  ((unit :initform 8 :type (member 8 16 32) :accessor bitmap-format-unit)
   (pad :initform 8 :type (member 8 16 32) :accessor bitmap-format-pad)
   (lsb-first-p :initform nil :type generalized-boolean
		:accessor bitmap-format-lsb-first-p)))

(defmethod print-object ((bitmap-format bitmap-format) stream)
  (with-slots (unit pad lsb-first-p) bitmap-format
    (print-unreadable-object (bitmap-format stream :type t)
      (format stream "unit ~d pad ~d ~:[M~;L~]SB first" unit pad lsb-first-p))))

(defclass pixmap-format ()
  ((depth :initarg :depth :initform 0 :type image-depth
	  :reader pixmap-format-depth)
   (bits-per-pixel :initarg :bits-per-pixel :initform 8
		   :type (member 1 4 8 12 16 24 32)
		   :reader pixmap-format-bits-per-pixel)
   (scanline-pad :initarg :scanline-pad :initform 8
		 :type (member 8 16 32)
		 :reader pixmap-format-scanline-pad)))

(defmethod print-object ((pixmap-format pixmap-format) stream)
  (with-slots (depth bits-per-pixel scanline-pad) pixmap-format
    (print-unreadable-object (pixmap-format stream :type t)
      (format stream "depth ~d bits-per-pixel ~d scanline-pad ~d"
	      depth bits-per-pixel scanline-pad))))

(defun pixmap-format-p (object)
  (typep object 'pixmap-format))

(defparameter *atom-cache-size* 200)
(defparameter *resource-id-map-size* 500)

(defclass display (buffer)
  ((host :initarg :host :reader display-host :documentation "Server host")
   (display :initarg :display :initform 0 :type integer
	    :reader display-display
	    :documentation "Display number on host")
   (after-function :initform nil
		   :accessor display-after-function
		   :documentation "Function to call after every request")
   (event-lock :initform (make-process-lock "CLX Event Lock")
	       :reader display-event-lock
	       :documentation "with-event-queue lock")
   (event-queue-lock :initform (make-process-lock "CLX Event Queue Lock")
		     :reader display-event-queue-lock
		     :documentation "new-events/event-queue lock")
   (event-queue-tail :initform nil :type (or null reply-buffer)
		     :accessor display-event-queue-tail
		     :documentation "last event in the event queue")
   (event-queue-head :initform nil :type (or null reply-buffer)
		     :accessor display-event-queue-head
		     :documentation "Threaded queue of events")
   (atom-cache :initform (make-hash-table :test (atom-cache-map-test) :size *atom-cache-size*)
	       :type hash-table
	       :reader display-atom-cache
	       :documentation "Hash table relating atoms keywords to atom id's")
   (font-cache :initform nil :accessor display-font-cache :documentation "List of font")
   (protocol-major-version :initform 0 :type card16
			   :accessor display-protocol-major-version
			   :documentation "Major version of server's X protocol")
   (protocol-minor-version :initform 0 :type card16
			   :accessor display-protocol-minor-version
			   :documentation "minor version of servers X protocol")
   (vendor-name :initform "" :type string
		:accessor display-vendor-name
		:documentation "Vendor of the server hardware")
   (resource-id-base :initform 0 :type resource-id
		     :accessor display-resource-id-base
		     :documentation "resouce ID base")
   (resource-id-mask :initform 0 :type resource-id
		     :accessor display-resource-id-mask
		     :documentation "resource ID mask bits")
   (resource-id-byte :initform nil
		     :accessor display-resource-id-byte
		     :documentation "resource ID mask field (used with DPB & LDB)")
   (resource-id-count :initform 0 :type resource-id
		      :accessor display-resource-id-count
		      :documentation "resource ID mask count (used for allocating ID's)")
   (resource-id-map :initform (make-hash-table :test (resource-id-map-test) :size *resource-id-map-size*)
		    :reader display-resource-id-map
		    :type hash-table
		    :documentation "hash table maps resource-id's to objects (used in lookup functions)")
   (xid :initform #'resourcealloc :reader display-xid :documentation "allocator function")
   (byte-order :initform #+clx-little-endian :lsbfirst #-clx-little-endian :msbfirst
	       :reader display-byte-order
	       :documentation "Connection byte order")
   (release-number :initform 0 :type card32
		   :accessor display-release-number
		   :documentation "Release of the server")
   (max-request-length :initform 0 :type card16
		       :accessor display-max-request-length
		       :documentation "Maximum number 32 bit words in request")
   (default-screen :accessor display-default-screen :documentation "Default screen for operations")
   (roots :initform nil :type list :accessor display-roots :documentation "List of screens")
   (motion-buffer-size :initform 0 :type card32
		       :accessor display-motion-buffer-size
		       :documentation "Size of motion buffer")
   (xdefaults :documentation "Contents of defaults from server")
   (image-lsb-first-p :initform nil :type generalized-boolean
		      :accessor display-image-lsb-first-p)
   (bitmap-format :initform (make-instance 'bitmap-format) :type bitmap-format
		  :reader display-bitmap-format
		  :documentation "Screen image info")
   (pixmap-formats :initform nil :type sequence
		   :accessor display-pixmap-formats
		   :documentation "List of pixmap formats")
   (min-keycode :initform 0 :type card8
		:accessor display-min-keycode
		:documentation "Minimum key-code")
   (max-keycode :initform 0 :type card8
		:accessor display-max-keycode
		:documentation "Maximun key-code")
   (error-handler :initform #'default-error-handler
		  :accessor display-error-handler
		  :documentation "Error handler function")
   (close-down-mode :initform :destroy :documentation "Close down mode saved by Set-Close-Down-Mode")
   (authorization-name :initform "" :type string :accessor display-authorization-name)
   (authorization-data :initform "" :type (or (array (unsigned-byte 8)) string)
		       :accessor display-authorization-data)
   (last-width :initform nil :type (or null card29) :documentation "Accumulated width of last string")
   (keysym-mapping :initform nil :type (or null (array * (* *)))
		   :accessor display-keysym-mapping
		   :documentation "Keysym mapping cached from server")
   (modifier-mapping :initform nil :type list
		     :accessor display-modifier-mapping
		     :documentation "ALIST of (keysym . state-mask) for all modifier keysyms")
   (keysym-translation :initform nil :type list
		       :reader display-keysym-translation
		       :documentation "An alist of (keysym object function) for display-local keysyms")
   (extension-alist :initform nil :type list
		    :accessor display-extension-alist
		    :documentation "Extension alist, which has elements: (name major-opcode first-event first-error)")
   (event-extensions :initform (vector) :type vector
		     :accessor display-event-extensions
		     :documentation "Vector mapping X event-codes to event keys")
   (performance-info :documentation "Hook for gathering performance info")
   (trace-history :documentation "Hook for debug trace")
   (plist :initform nil :type list
	  :accessor display-plist
	  :documentation "Hook for extension to hang data")
   ;; These slots are used to manage multi-process input.
   (input-in-progress :initform nil
		      :accessor display-input-in-progress
		      :documentation "Some process
		      reading from the stream. Updated with
		      CONDITIONAL-STORE.")
   (pending-commands :initform nil
		     :accessor display-pending-commands
		     :documentation "Threaded list of
		     PENDING-COMMAND objects for all commands awaiting
		     replies. Protected by
		     WITH-EVENT-QUEUE-INTERNAL.")
   (asynchronous-errors :initform nil
			:reader display-asynchronous-errors
			:documentation "Threaded list of REPLY-BUFFER
			objects containing error messages for commands
			which did not expect replies. Protected by
			WITH-EVENT-QUEUE-INTERNAL.")
   (report-asynchronous-errors :initform (list :immediately)
			       :type list
			       :reader display-report-asynchronous-errors
			       :documentation "When to report
			       asynchronous errors. The keywords that
			       can be on this list are :IMMEDIATELY,
			       :BEFORE-EVENT-HANDLING and
			       :AFTER-FINISH-OUTPUT")
   (event-process :initform nil :accessor display-event-process
		  :documentation "Process ID of process awaiting
		  events. Protected by WITH-EVENT-QUEUE.")
   (new-events :initform nil :type (or null reply-buffer)
	       :accessor display-new-events
	       :documentation "Pointer to the first new event in the
	       event queue. Protected by WITH-EVENT-QUEUE.")
   (current-event-symbol :initform (list (gensym) (gensym)) :type cons
			 :reader display-current-event-symbol
			 :documentation "Bound with PROGV by event
			 handling macros")
   (atom-id-map :initform (make-hash-table :test (resource-id-map-test) :size *atom-cache-size*)
		:type hash-table
		:reader display-atom-id-map)
   (extended-max-request-length :initform 0 :type card32
				:accessor display-extended-max-request-length)))

(defun print-display-name (display stream)
  (with-slots (host display) display
    (format stream "~a:~a" host display)))

(defmethod print-object ((dpy display) stream)
  (with-slots (vendor-name release-number) dpy
    (print-unreadable-object (dpy stream :type t)
      (print-display-name dpy stream)
      (format stream " (~a R~d)" vendor-name release-number))))

(defun display-p (thing)
  (typep thing 'display))

(defun display-input-stream (display)
  (buffer-input-stream display))

(defclass drawable ()
  ((id :initarg :id :initform 0 :type resource-id :accessor drawable-id)
   (display :initarg :display :initform nil :type (or null display)
            :reader drawable-display)
   (plist :initform nil :type list
	  :accessor drawable-plist
	  :documentation "Extension hook")))

(defmethod print-object ((drawable drawable) stream)
  (with-slots (id display) drawable
    (print-unreadable-object (drawable stream :type t)
      (print-display-name display stream)
      (format stream " ~x" id))))

(defclass window (drawable)
  ((id :accessor window-id)
   (display :accessor window-display)
   (plist :accessor window-plist)))

(defclass pixmap (drawable)
  ((id :accessor pixmap-id)
   (display :accessor pixmap-display)
   (plist :accessor pixmap-plist)))

(defun drawable-p (thing) (typep thing 'drawable))
(defun window-p (thing) (typep thing 'window))
(defun pixmap-p (thing) (typep thing 'pixmap))

(defclass visual-info ()
  ((id :initarg :id :initform 0 :type resource-id :reader visual-info-id)
   (display :initarg :display :initform nil :type (or null display) :reader visual-info-display)
   (class :initarg :class :initform :static-gray
	  :type (member :static-gray :static-color :true-color :gray-scale :pseudo-color :direct-color)
	  :reader visual-info-class)
   (red-mask :initarg :red-mask :initform 0 :type pixel :reader visual-info-red-mask)
   (green-mask :initarg :green-mask :initform 0 :type pixel :reader visual-info-green-mask)
   (blue-mask :initarg :blue-mask :initform 0 :type pixel :reader visual-info-blue-mask)
   (bits-per-rgb :initarg :bits-per-rgb :initform 1 :type card8 :reader visual-info-bits-per-rgb)
   (colormap-entries :initarg :colormap-entries :initform 0 :type card16
		     :reader visual-info-colormap-entries)
   (plist :initform nil :type list :documentation "Extension hook")))

(defmethod print-object ((visual-info visual-info) stream)
  (with-slots (id display class bits-per-rgb) visual-info
    (print-unreadable-object (visual-info stream :type t)
      (format stream "~s-bit ~a " bits-per-rgb class)
      (print-display-name display stream)
      (format stream " ~x" id))))

(defclass colormap ()
  ((id :initarg :id :initform 0 :type resource-id
       :reader colormap-id)
   (display :initarg :display :initform nil :type (or null display)
	    :reader colormap-display)
   (visual-info :initform nil :type (or null visual-info)
		:accessor colormap-visual-info)))

(defmethod print-object ((colormap colormap) stream)
  (with-slots (id display visual-info) colormap
    (print-unreadable-object (colormap stream :type t)
      (when visual-info
	(format stream "~a " (visual-info-class visual-info)))
      (print-display-name display stream)
      (format stream " ~d" id))))

(defclass cursor ()
  ((id :initform 0 :type resource-id :accessor cursor-id)
   (display :initarg :display :initform nil :reader cursor-display
            :type (or null display))))

(defmethod print-object ((cursor cursor) stream)
  (with-slots (id display) cursor
    (print-unreadable-object (cursor stream :type t)
      (print-display-name display stream)
      (format stream " ~D" id))))

; Atoms are accepted as strings or symbols, and are always returned as keywords.
; Protocol-level integer atom ids are hidden, using a cache in the display object.

(deftype xatom () '(or string symbol))

(defconstant +predefined-atoms+
 '#(nil :PRIMARY :SECONDARY :ARC :ATOM :BITMAP
    :CARDINAL :COLORMAP :CURSOR
    :CUT_BUFFER0 :CUT_BUFFER1 :CUT_BUFFER2 :CUT_BUFFER3
    :CUT_BUFFER4 :CUT_BUFFER5 :CUT_BUFFER6 :CUT_BUFFER7
    :DRAWABLE :FONT :INTEGER :PIXMAP :POINT :RECTANGLE
    :RESOURCE_MANAGER :RGB_COLOR_MAP :RGB_BEST_MAP
    :RGB_BLUE_MAP :RGB_DEFAULT_MAP
    :RGB_GRAY_MAP :RGB_GREEN_MAP :RGB_RED_MAP :STRING
    :VISUALID :WINDOW :WM_COMMAND :WM_HINTS
    :WM_CLIENT_MACHINE :WM_ICON_NAME :WM_ICON_SIZE
    :WM_NAME :WM_NORMAL_HINTS :WM_SIZE_HINTS
    :WM_ZOOM_HINTS :MIN_SPACE :NORM_SPACE :MAX_SPACE
    :END_SPACE :SUPERSCRIPT_X :SUPERSCRIPT_Y
    :SUBSCRIPT_X :SUBSCRIPT_Y
    :UNDERLINE_POSITION :UNDERLINE_THICKNESS
    :STRIKEOUT_ASCENT :STRIKEOUT_DESCENT
    :ITALIC_ANGLE :X_HEIGHT :QUAD_WIDTH :WEIGHT
    :POINT_SIZE :RESOLUTION :COPYRIGHT :NOTICE
    :FONT_NAME :FAMILY_NAME :FULL_NAME :CAP_HEIGHT
    :WM_CLASS :WM_TRANSIENT_FOR))

(deftype stringable () '(or string symbol))

(deftype fontable () '(or stringable font))

; Nil stands for CurrentTime.

(deftype timestamp () '(or null card32))

(defconstant +bit-gravity-vector+
 '#(:forget :north-west :north :north-east :west
    :center :east :south-west :south
    :south-east :static))

(deftype bit-gravity ()
  '(member :forget :north-west :north :north-east :west
	   :center :east :south-west :south :south-east :static))

(defconstant +win-gravity-vector+
 '#(:unmap :north-west :north :north-east :west
    :center :east :south-west :south :south-east
    :static))

(defparameter *protocol-families*
  '(;; X11/X.h, Family*
    (:internet . 0)
    (:decnet . 1)
    (:chaos . 2)
    ;; X11/Xauth.h "not part of X standard"
    (:Local . 256)
    (:Wild . 65535)
    (:Netname . 254)
    (:Krb5Principal . 253)
    (:LocalHost . 252)))

(deftype win-gravity ()
  '(member :unmap :north-west :north :north-east :west
	   :center :east :south-west :south :south-east :static))

(deftype grab-status ()
  '(member :success :already-grabbed :invalid-time :not-viewable))

; An association list.

(deftype alist (key-type-and-name datum-type-and-name)
  (declare (ignore key-type-and-name datum-type-and-name))
  'list)

(deftype clx-list (&optional element-type) (declare (ignore element-type)) 'list)
(deftype clx-sequence (&optional element-type) (declare (ignore element-type)) 'sequence)

; A sequence, containing zero or more repetitions of the given elements,
; with the elements expressed as (type name).

(deftype repeat-seq (&rest elts) elts 'sequence)

(deftype point-seq () '(repeat-seq (int16 x) (int16 y)))

(deftype seg-seq () '(repeat-seq (int16 x1) (int16 y1) (int16 x2) (int16 y2)))

(deftype rect-seq () '(repeat-seq (int16 x) (int16 y) (card16 width) (card16 height)))

(deftype arc-seq ()
  '(repeat-seq (int16 x) (int16 y) (card16 width) (card16 height)
	       (angle angle1) (angle angle2)))

(deftype gcontext-state () 'simple-vector)

(defclass gcontext ()
  ((id :initarg :id :initform 0 :type resource-id
       :accessor gcontext-id)
   (display :initarg :display :initform nil :type (or null display)
	    :reader gcontext-display)
   (drawable :initarg :drawable :initform nil :type (or null drawable))
   (cache-p :initarg :cache-p :initform t :type generalized-boolean
	    :reader gcontext-cache-p)
   (server-state :initarg :server-state :initform (allocate-gcontext-state)
		 :type gcontext-state
		 :reader gcontext-server-state)
   (local-state :initarg :local-state :initform (allocate-gcontext-state)
		:type gcontext-state
		:reader gcontext-local-state)
   (plist :initform nil :type list
	  :accessor gcontext-plist
	  :documentation "Extension hook")
   (next :initform nil :type (or null gcontext))))

(defmethod print-object ((gcontext gcontext) stream)
  (with-slots (id display) gcontext
    (print-unreadable-object (gcontext stream :type t)
      (print-display-name display stream)
      (format stream " ~d" id))))

(defconstant +event-mask-vector+
 '#(:key-press :key-release :button-press :button-release
    :enter-window :leave-window :pointer-motion :pointer-motion-hint
    :button-1-motion :button-2-motion :button-3-motion :button-4-motion
    :button-5-motion :button-motion :keymap-state :exposure :visibility-change
    :structure-notify :resize-redirect :substructure-notify :substructure-redirect
    :focus-change :property-change :colormap-change :owner-grab-button))

(deftype event-mask-class ()
  '(member :key-press :key-release :owner-grab-button :button-press :button-release
	   :enter-window :leave-window :pointer-motion :pointer-motion-hint
	   :button-1-motion :button-2-motion :button-3-motion :button-4-motion
	   :button-5-motion :button-motion :exposure :visibility-change
	   :structure-notify :resize-redirect :substructure-notify :substructure-redirect
	   :focus-change :property-change :colormap-change :keymap-state))

(deftype event-mask ()
  '(or mask32 (clx-list event-mask-class)))

(defconstant +pointer-event-mask-vector+
  ;; the first two elements used to be '%error '%error (i.e. symbols, 
  ;; and not keywords) but the vector is supposed to contain 
  ;; keywords, so I renamed them -dan 2004.11.13
  '#(:%error :%error :button-press :button-release
     :enter-window :leave-window :pointer-motion :pointer-motion-hint
     :button-1-motion :button-2-motion :button-3-motion :button-4-motion
     :button-5-motion :button-motion :keymap-state))

(deftype pointer-event-mask-class ()
  '(member :button-press :button-release
	   :enter-window :leave-window :pointer-motion :pointer-motion-hint
	   :button-1-motion :button-2-motion :button-3-motion :button-4-motion
	   :button-5-motion :button-motion :keymap-state))

(deftype pointer-event-mask ()
  '(or mask32 (clx-list pointer-event-mask-class)))

(defconstant +device-event-mask-vector+
 '#(:key-press :key-release :button-press :button-release :pointer-motion
    :button-1-motion :button-2-motion :button-3-motion :button-4-motion
    :button-5-motion :button-motion))

(deftype device-event-mask-class ()
  '(member :key-press :key-release :button-press :button-release :pointer-motion
	   :button-1-motion :button-2-motion :button-3-motion :button-4-motion
	   :button-5-motion :button-motion))

(deftype device-event-mask ()
  '(or mask32 (clx-list device-event-mask-class)))

(defconstant +state-mask-vector+
 '#(:shift :lock :control :mod-1 :mod-2 :mod-3 :mod-4 :mod-5
    :button-1 :button-2 :button-3 :button-4 :button-5))

(deftype modifier-key ()
  '(member :shift :lock :control :mod-1 :mod-2 :mod-3 :mod-4 :mod-5))

(deftype modifier-mask ()
  '(or (member :any) mask16 (clx-list modifier-key)))

(deftype state-mask-key ()
  '(or modifier-key (member :button-1 :button-2 :button-3 :button-4 :button-5)))

(defconstant +gcontext-components+
 '(:function :plane-mask :foreground :background
   :line-width :line-style :cap-style :join-style :fill-style
   :fill-rule :tile :stipple :ts-x :ts-y :font :subwindow-mode
   :exposures :clip-x :clip-y :clip-mask :dash-offset :dashes
   :arc-mode))

(deftype gcontext-key ()
  '(member :function :plane-mask :foreground :background
	   :line-width :line-style :cap-style :join-style :fill-style
	   :fill-rule :tile :stipple :ts-x :ts-y :font :subwindow-mode
	   :exposures :clip-x :clip-y :clip-mask :dash-offset :dashes
	   :arc-mode))

(deftype event-key ()
  '(or (member :key-press :key-release :button-press :button-release 
        :motion-notify :enter-notify :leave-notify :focus-in :focus-out 
        :keymap-notify :exposure :graphics-exposure :no-exposure 
        :visibility-notify :create-notify :destroy-notify :unmap-notify 
        :map-notify :map-request :reparent-notify :configure-notify 
        :gravity-notify :resize-request :configure-request :circulate-notify 
        :circulate-request :property-notify :selection-clear 
        :selection-request :selection-notify :colormap-notify :client-message 
        :mapping-notify)
       (satisfies extension-event-key-p)))

(deftype error-key ()
  '(member :access :alloc :atom :colormap :cursor :drawable :font :gcontext :id-choice
	   :illegal-request :implementation :length :match :name :pixmap :value :window))

(deftype draw-direction ()
  '(member :left-to-right :right-to-left))

(defconstant +boole-vector+
 '#(#.boole-clr #.boole-and #.boole-andc2 #.boole-1
    #.boole-andc1 #.boole-2 #.boole-xor #.boole-ior
    #.boole-nor #.boole-eqv #.boole-c2 #.boole-orc2
    #.boole-c1 #.boole-orc1 #.boole-nand #.boole-set))

(deftype boole-constant ()
  `(member ,boole-clr ,boole-and ,boole-andc2 ,boole-1
	   ,boole-andc1 ,boole-2 ,boole-xor ,boole-ior
	   ,boole-nor ,boole-eqv ,boole-c2 ,boole-orc2
	   ,boole-c1 ,boole-orc1 ,boole-nand ,boole-set))

(defclass screen ()
  ((root :initarg :root :initform nil :type (or null window) :reader screen-root)
   (width :initarg :width :initform 0 :type card16 :accessor screen-width)
   (height :initarg :height :initform 0 :type card16 :accessor screen-height)
   (width-in-millimeters :initarg :width-in-millimeters :initform 0 :type card16
			 :reader screen-width-in-millimeters)
   (height-in-millimeters :initarg :height-in-millimeters :initform 0 :type card16
			 :reader screen-height-in-millimeters)
   (depths :initform nil :type (alist (image-depth depth) ((clx-list visual-info) visuals))
	   :accessor screen-depths)
   (root-depth :initarg :root-depth :initform 1 :type image-depth :reader screen-root-depth)
   (root-visual-info :initform nil :type (or null visual-info) :accessor screen-root-visual-info)
   (default-colormap :initarg :default-colormap :initform nil :type (or null colormap)
		     :reader screen-default-colormap)
   (white-pixel :initarg :white-pixel :initform 0 :type pixel :reader screen-white-pixel)
   (black-pixel :initarg :black-pixel :initform 1 :type pixel :reader screen-black-pixel)
   (min-installed-maps :initarg :min-installed-maps :initform 1 :type card16
		       :reader screen-min-installed-maps)
   (max-installed-maps :initarg :max-installed-maps :initform 1 :type card16
		       :reader screen-max-installed-maps)
   (backing-stores :initarg :backing-stores :initform :never :type (member :never :when-mapped :always)
		   :reader screen-backing-stores)
   (save-unders-p :initarg :save-unders-p :initform nil :type generalized-boolean
		  :reader screen-save-unders-p)
   (event-mask-at-open :initarg :event-mask-at-open :initform 0 :type mask32
		       :reader screen-event-mask-at-open)
   (plist :initform nil :type list :documentation "Extension hook"
	  :accessor screen-plist)))

(defmethod print-object ((screen screen) stream)
  (with-slots (root width height root-depth root-visual-info) screen
    (print-unreadable-object (screen stream :type t)
      (let ((display (drawable-display root)))
	(print-display-name display stream)
	(write-string "." stream)
	(princ (position screen (display-roots display)) stream))
      (format stream " ~dx~dx~d" width height root-depth)
      (when root-visual-info
	(write-string " " stream)
	(princ (visual-info-class root-visual-info) stream)))))

(defun screen-root-visual (screen)
  (declare (type screen screen)
	   (clx-values resource-id))
  (visual-info-id (screen-root-visual-info screen)))

(defun screen-p (object)
  (typep object 'screen))

;; The list contains alternating keywords and integers.
(deftype font-props () 'list)

(defclass font-info ()
  ((direction :initarg :direction :initform :left-to-right :type draw-direction
	      :reader font-info-direction)
   (min-char :initarg :min-char :initform 0 :type card16
	     :reader font-info-min-char
	     :documentation "First character in font.")
   (max-char :initarg :max-char :initform 0 :type card16
	     :reader font-info-max-char
	     :documentation "Last character in font.")
   ;; The following are for 16 bit fonts
   ;; and specify min&max values for
   ;; the two character bytes
   (min-byte1 :initarg :min-byte1 :initform 0 :type card8
	      :reader font-info-min-byte1)
   (max-byte1 :initarg :max-byte1 :initform 0 :type card8
	      :reader font-info-max-byte1)
   (min-byte2 :initarg :min-byte2 :initform 0 :type card8
	      :reader font-info-min-byte2)
   (max-byte2 :initarg :max-byte2 :initform 0 :type card8
	      :reader font-info-max-byte2)
   (all-chars-exist-p :initarg :all-chars-exist-p :initform nil
		      :type generalized-boolean
		      :reader font-info-all-chars-exist-p)
   (default-char :initarg :default-char :initform 0 :type card16
		 :reader font-info-default-char)
   (min-bounds :initarg :min-bounds :initform nil :type (or null vector)
	       :reader font-info-min-bounds)
   (max-bounds :initarg :max-bounds :initform nil :type (or null vector)
	       :reader font-info-max-bounds)
   (ascent :initarg :ascent :initform 0 :type int16
	   :reader font-info-ascent)
   (descent :initarg :descent :initform 0 :type int16
	    :reader font-info-descent)
   (properties :initarg :properties :initform nil :type font-props
	       :accessor font-info-properties)))

(defclass font ()
  ((id-internal :initarg :id-internal :initform nil :type (or null resource-id)
		:accessor font-id-internal)
   (display :initarg :display :initform nil :type (or null display)
	    :reader font-display)
   (reference-count :initarg :reference-count :initform 0 :type fixnum
		    :accessor font-reference-count)
   (name :initarg :name :initform "" :type (or null string)
	 :reader font-name
	 :documentation "NIL when ID is for a GContext")
   (font-info-internal :initarg :font-info-internal :initform nil
		       :type (or null font-info)
		       :accessor font-font-info-internal)
   (char-infos-internal :initform nil :type (or null (simple-array int16 (*)))
			:accessor font-char-infos-internal)
   (local-only-p :initarg :local-only-p :initform t :type generalized-boolean
		 :documentation "When T, always calculate text extents locally")
   (plist :initform nil :type list :documentation "Extension hook" :reader font-plist)))

(defmethod print-object ((font font) stream)
  (with-accessors ((name font-name)
		   (display font-display)
		   (id font-id)) font
    (print-unreadable-object (font stream :type t)
      (if name
	  (princ name stream)
	  (write-string "(gcontext)" stream))
      (write-string " " stream)
      (print-display-name display stream)
      (when id
	(write-string " " stream)
	(prin1 id stream)))))

(defun font-id (font)
  ;; Get font-id, opening font if needed
  (or (font-id-internal font)
      (open-font-internal font)))

(defun font-font-info (font)
  (or (font-font-info-internal font)
      (query-font font)))

(defun font-char-infos (font)
  (or (font-char-infos-internal font)
      (progn (query-font font)
	     (font-char-infos-internal font))))

(defun make-font (&key id
		  display
		  (reference-count 0)
		  (name "")
		  (local-only-p t)
		  font-info-internal)
  (make-instance 'font :id-internal id
		       :display display
		       :reference-count reference-count
		       :name name
		       :local-only-p local-only-p
		       :font-info-internal font-info-internal))

; For each component (<name> <unspec> :type <type>) of font-info,
; there is a corresponding function:

;(defun font-<name> (font)
;  (declare (type font font)
;	   (clx-values <type>)))

(macrolet ((make-font-info-accessors (&body fields)
	     `(progn
		,@(mapcar
		    #'(lambda (field)
			(let* ((type (second field))
			       (n (string (first field)))
			       (name (xintern 'font- n))
			       (accessor (xintern 'font-info- n)))
			  `(defun ,name (font)
			     (declare (type font font))
			     (declare (clx-values ,type))
			     (,accessor (font-font-info font)))))
		    fields))))
  (make-font-info-accessors
    (direction draw-direction)
    (min-char card16)
    (max-char card16)
    (min-byte1 card8)
    (max-byte1 card8)
    (min-byte2 card8)
    (max-byte2 card8)
    (all-chars-exist-p generalized-boolean)
    (default-char card16)
    (min-bounds vector)
    (max-bounds vector)
    (ascent int16)
    (descent int16)
    (properties font-props)))

(defun font-property (font name)
  (declare (type font font)
	   (type keyword name))
  (declare (clx-values (or null int32)))
  (getf (font-properties font) name))

(macrolet ((make-mumble-equal (type)
	     ;; Since caching is only done for objects created by the
	     ;; client, we must always compare ID and display for
	     ;; non-identical mumbles.
	     (let ((predicate (xintern type '-equal))
		   (id (xintern type '-id))
		   (dpy (xintern type '-display)))
	       `(defun ,predicate (a b)
		  (declare (type ,type a b))
		    (or (eql a b)
			(and (= (,id a) (,id b))
			     (eq (,dpy a) (,dpy b))))))))
  (make-mumble-equal window)
  (make-mumble-equal pixmap)
  (make-mumble-equal cursor)
  (make-mumble-equal font)
  (make-mumble-equal gcontext)
  (make-mumble-equal colormap)
  (make-mumble-equal drawable))

;;;
;;; Event-mask encode/decode functions
;;;    Converts from keyword-lists to integer and back
;;;
(defun encode-mask (key-vector key-list key-type)
  ;; KEY-VECTOR is a vector containg bit-position keywords.  The
  ;; position of the keyword in the vector indicates its bit position
  ;; in the resulting mask.  KEY-LIST is either a mask or a list of
  ;; KEY-TYPE Returns NIL when KEY-LIST is not a list or mask.
  (declare (type (simple-array keyword (*)) key-vector)
	   (type (or mask32 list) key-list))
  (declare (clx-values (or mask32 null)))
  (typecase key-list
    (mask32 key-list)
    (list (let ((mask 0))
	    (dolist (key key-list mask)
	      (let ((bit (position key (the vector key-vector) :test #'eq)))
		(unless bit
		  (x-type-error key key-type))
		(setq mask (logior mask (ash 1 bit)))))))))

(defun decode-mask (key-vector mask)
  (declare (type (simple-array keyword (*)) key-vector)
	   (type mask32 mask))
  (declare (clx-values list))
  (do ((m mask (ash m -1))
       (bit 0 (1+ bit))
       (len (length key-vector))
       (result nil))       
      ((or (zerop m) (>= bit len)) result)
    (declare (type mask32 m)
	     (fixnum bit len)
	     (list result))
    (when (oddp m)
      (push (aref key-vector bit) result))))

(defun encode-event-mask (event-mask)
  (declare (type event-mask event-mask))
  (declare (clx-values mask32))
  (or (encode-mask +event-mask-vector+ event-mask 'event-mask-class)
      (x-type-error event-mask 'event-mask)))

(defun make-event-mask (&rest keys)
  ;; This is only defined for core events.
  ;; Useful for constructing event-mask, pointer-event-mask, device-event-mask.
  (declare (type (clx-list event-mask-class) keys))
  (declare (clx-values mask32))
  (encode-mask +event-mask-vector+ keys 'event-mask-class))

(defun make-event-keys (event-mask)
  ;; This is only defined for core events.
  (declare (type mask32 event-mask))
  (declare (clx-values (clx-list event-mask-class)))
  (decode-mask +event-mask-vector+ event-mask))

(defun encode-device-event-mask (device-event-mask)
  (declare (type device-event-mask device-event-mask))
  (declare (clx-values mask32))
  (or (encode-mask +device-event-mask-vector+ device-event-mask
		   'device-event-mask-class)
      (x-type-error device-event-mask 'device-event-mask)))

(defun encode-modifier-mask (modifier-mask)
  (declare (type modifier-mask modifier-mask))
  (declare (clx-values mask16))
  (or (and (eq modifier-mask :any) #x8000)
      (encode-mask +state-mask-vector+ modifier-mask 'modifier-key)
      (x-type-error modifier-mask 'modifier-mask)))

(defun encode-state-mask (state-mask)
  (declare (type (or mask16 (clx-list state-mask-key)) state-mask))
  (declare (clx-values mask16))
  (or (encode-mask +state-mask-vector+ state-mask 'state-mask-key)
      (x-type-error state-mask '(or mask16 (clx-list state-mask-key)))))

(defun make-state-mask (&rest keys)
  ;; Useful for constructing modifier-mask, state-mask.
  (declare (type (clx-list state-mask-key) keys))
  (declare (clx-values mask16))
  (encode-mask +state-mask-vector+ keys 'state-mask-key))

(defun make-state-keys (state-mask)
  (declare (type mask16 state-mask))
  (declare (clx-values (clx-list state-mask-key)))
  (decode-mask +state-mask-vector+ state-mask))

(defun encode-pointer-event-mask (pointer-event-mask)
  (declare (type pointer-event-mask pointer-event-mask))
  (declare (clx-values mask32))
  (or (encode-mask +pointer-event-mask-vector+ pointer-event-mask
		   'pointer-event-mask-class)
      (x-type-error pointer-event-mask 'pointer-event-mask)))
