;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XLIB; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: RandR Extension
;;;   Created: 2014-11-17
;;;    Author: Johannes Martinez <johannes.martinez@gmail.com>
;;; ---------------------------------------------------------------------------
;;;
;;; (c) copyright 2014 by Johannes Martinez
;;; (c) copyright 2022 Jan Moringen
;;;
;;; Permission is granted to any individual or institution to use,
;;; copy, modify, and distribute this software, provided that this
;;; complete copyright and permission notice is maintained, intact, in
;;; all copies and supporting documentation.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(defpackage #:xlib/xrandr
  (:use
   #:cl
   #:xlib)

  (:shadow
   #:mode-info ; unfortunately exported by xvidmode extension
   #:make-mode-info)

  (:shadowing-import-from #:xlib
   #:defconstant)

  (:import-from #:xlib
   #:clx-values
   #:def-clx-class
   #:define-accessor

   #:clx-list
   #:clx-sequence

   #:index+ #:index*

   #:+replysize+
   #:with-buffer-request
   #:with-buffer-request-and-reply
   #:int16-get           #:int16-put
   #:card8-get
   #:card16-get          #:card16-put
   #:card32-get          #:card32-put
   #:member8-vector-get
   #:member16-vector-get
   #:boolean-get
   #:sequence-get
   #:string-get          #:string-put
   #:window-get
   #:decode-mask         #:encode-mask)

  (:export
   #:query-version
   #:get-screen-info
   #:set-screen-config)

  ;; 1.2
  (:export
   #:get-screen-size-range
   #:set-screen-size
   #:get-screen-resources

   #:mode-info
   #:make-mode-info
   #:mode-info-name
   #:mode-info-id
   #:mode-info-width
   #:mode-info-height
   #:mode-info-dot-clock
   #:mode-info-h-sync-start
   #:mode-info-h-sync-end
   #:mode-info-h-sync-total
   #:mode-info-h-sync-skew
   #:mode-info-v-sync-start
   #:mode-info-v-sync-end
   #:mode-info-v-total
   #:mode-info-mode-flags

   #:get-output-info
   #:list-output-properties
   #:query-output-property
   #:configure-output-property
   #:change-output-property
   #:delete-output-property
   #:get-output-property

   #:create-mode
   #:destroy-mode
   #:add-output-mode
   #:delete-output-mode

   #:get-crtc-info
   #:set-crtc-config
   #:get-crtc-gamma-size
   #:get-crtc-gamma
   #:set-crtc-gamma)

  ;; 1.3
  (:export
   #:get-screen-resources-current

   #:make-transform
   #:transform-x #:transform-y #:transform-z
   #:transform-i #:transform-j #:transform-k
   #:transform-d #:transform-e #:transform-f
   #:get-crtc-transform
   #:set-crtc-transform

   #:panning
   #:make-panning
   #:panning-top
   #:panning-left
   #:panning-width
   #:panning-height
   #:panning-track-top
   #:panning-track-left
   #:panning-track-width
   #:panning-track-height
   #:panning-border-left
   #:panning-border-top
   #:panning-border-bottom
   #:panning-border-right
   #:get-panning
   #:set-panning

   #:get-output-primary
   #:set-output-primary)

  ;; 1.4
  (:export
   #:get-providers
   #:get-provider-info
   #:set-provider-output-source
   #:set-provider-offload-sink
   #:list-provider-properties

   #:make-select-keys
   #:make-select-mask
   #:select-input

   #:make-mode-flag-keys
   #:make-mode-flag-mask

   #:make-rotation-keys
   #:make-rotation-mask)

  ;; Convenience functions
  (:export
   #:update-screens))

(in-package #:xlib/xrandr)

(pushnew :clx-ext-randr *features*)

(define-extension "RANDR"
  :events (:rr-screen-change-notify
           :rr-notify
           ; :rr-crtc-change-notify
           ; :rr-output-change-notify
           ; :rr-output-property-notify
           )
  :errors (output
           crtc
           mode))

(defun randr-opcode (display)
  (extension-opcode display "RANDR"))

(defconstant +rr-major+ 1)
(defconstant +rr-minor+ 4)

(defconstant +rr-QueryVersion+               0) ; we skip 1 to make old clients fail pretty immediately

(defconstant +rr-SetScreenConfig+            2)
(defconstant +rr-OldScreenChangeSelectInput+ 3) ; 3 used to be ScreenChangeSelectInput; deprecated

(defconstant +rr-SelectInput+                4)
(defconstant +rr-GetScreenInfo+              5)

;;; V1.2 additions
(defconstant +rr-GetScreenSizeRange+       6)
(defconstant +rr-SetScreenSize+            7)
(defconstant +rr-GetScreenResources+       8)
(defconstant +rr-GetOutputInfo+            9)
(defconstant +rr-ListOutputProperties+    10)
(defconstant +rr-QueryOutputProperty+     11)
(defconstant +rr-ConfigureOutputProperty+ 12)
(defconstant +rr-ChangeOutputProperty+    13)
(defconstant +rr-DeleteOutputProperty+    14)
(defconstant +rr-GetOutputProperty+       15)
(defconstant +rr-CreateMode+              16)
(defconstant +rr-DestroyMode+             17)
(defconstant +rr-AddOutputMode+           18)
(defconstant +rr-DeleteOutputMode+        19)
(defconstant +rr-GetCrtcInfo+             20)
(defconstant +rr-SetCrtcConfig+           21)
(defconstant +rr-GetCrtcGammaSize+        22)
(defconstant +rr-GetCrtcGamma+            23)
(defconstant +rr-SetCrtcGamma+            24)

;;; V1.3 additions
(defconstant +rr-GetScreenResourcesCurrent+ 25)
(defconstant +rr-SetCrtcTransform+          26)
(defconstant +rr-GetCrtcTransform+          27)
(defconstant +rr-GetPanning+                28)
(defconstant +rr-SetPanning+                29)
(defconstant +rr-SetOutputPrimary+          30)
(defconstant +rr-GetOutputPrimary+          31)

;;; V1.4 Additions
(defconstant +rr-GetProviders+              32)
(defconstant +rr-GetProviderInfo+           33)
(defconstant +rr-SetProviderOffloadSink+    34)
(defconstant +rr-SetProviderOutputSource+   35)
(defconstant +rr-ListProviderProperties+    36)
(defconstant +rr-QueryProviderProperty+     37)
(defconstant +rr-ConfigureProviderProperty+ 38)
(defconstant +rr-ChangeProviderProperty+    39)
(defconstant +rr-DeleteProviderProperty+    40)
(defconstant +rr-GetProviderProperty+       41)

;;; Status returns

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +config-status+
    #(:success :invalid-config-time :invalid-time :failed))

  (defconstant +connection+
    #(:connected :disconnected :unknown-connection))

  ;; temporarily here since not in xrender.lisp
  (defconstant +render-subpixel-order+
    #(:unknown :horizontal-rgb :horizontal-bgr :vertical-rgb :vertical-bgr :none)))

;;; Mask-vectors and types

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun xintern (&rest parts)
    (intern (apply #'concatenate 'string (mapcar #'string parts))
            '#:xlib/xrandr)))

(macrolet ((define (name size &rest values)
             (let ((mask-type       (ecase size
                                      (8  'mask8)
                                      (16 'mask16)
                                      (32 'mask32)))
                   (card-type       (ecase size
                                      (8  'card8)
                                      (16 'card16)
                                      (32 'card32)))
                   (vector-name     (xintern '+ name '-vector+))
                   (class-type-name (xintern name '-class))
                   (type-name       (xintern name '-mask))
                   (encode-name     (xintern 'make- name '-mask))
                   (decode-name     (xintern 'make- name '-keys)))
               `(progn
                  (eval-when (:compile-toplevel :load-toplevel :execute)
                    (defconstant ,vector-name
                      #(,@values)))

                  (deftype ,class-type-name ()
                    `(member ,@(coerce ,vector-name 'list)))

                  (deftype ,type-name ()
                    '(or ,mask-type (clx-list ,class-type-name)))

                  (defun ,encode-name (key-list)
                    (encode-mask ,vector-name key-list ',card-type))

                  (defun ,decode-name (bit-mask)
                    (declare (type ,card-type bit-mask))
                    (declare (clx-values (clx-list ,card-type)))
                    (decode-mask ,vector-name bit-mask))))))

  (define rotation 16
    :rotate-0 :rotate-90 :rotate-180 :rotate-270 :reflect-x :reflect-y)

  (define select 8
    :screen-change-notify-mask :crtc-change-notify-mask
    :output-change-notify-mask :output-property-notify-mask)

  (define mode-flag 32
    :hsync-positive :hsync-negative :vsync-positive :vsync-negative
    :interlace :double-scan :csync :csync-positive :csync-negative
    :hskew-present :b-cast :pixel-multiplex :double-clock :clock-divide-by-2)

  (define provider-capabilities 32
    :source-output :sink-output :source-offload :sink-offload))

;; (defconstant  +RRTransformUnit       (1L+ << 0))
;; (defconstant  +RRTransformScaleUp    (1L+ << 1))
;; (defconstant  +RRTransformScaleDown  (1L+ << 2))
;; (defconstant  +RRTransformProjective (1L+ << 3))

;;; Types

(deftype size-id     () 'card16)
(deftype mode-id     () '(or null resource-id))
(deftype crtc-id     () 'resource-id)
(deftype output-id   () 'resource-id)
(deftype provider-id () 'resource-id)

;;; Structs

(def-clx-class (screen-size
                (:constructor make-screen-size (width-in-pixels
                                                height-in-pixels
                                                width-in-mm
                                                height-in-mm)))
  (width-in-pixels  0 :type card16)
  (height-in-pixels 0 :type card16)
  (width-in-mm      0 :type card16)
  (height-in-mm     0 :type card16))

(def-clx-class (mode-info
                (:constructor make-mode-info (name id width height dot-clock
                                              h-sync-start h-sync-end h-sync-total h-sync-skew
                                              v-sync-start v-sync-end v-total mode-flags)))
  ;; Internally, the NAME slot is temporarily used to store the name
  ;; length. In such cases, the length is replaced by the name string
  ;; before the object is returned to user code.
  (name         "" :type (or card16 string))
  (id           0  :type card32)
  (width        0  :type card16)
  (height       0  :type card16)
  (dot-clock    0  :type card32)
  (h-sync-start 0  :type card16)
  (h-sync-end   0  :type card16)
  (h-sync-total 0  :type card16)
  (h-sync-skew  0  :type card16)
  (v-sync-start 0  :type card16)
  (v-sync-end   0  :type card16)
  (v-total      0  :type card16)
  (mode-flags   0  :type mode-flag-mask))

(define-accessor rr-mode-info (32) ; interns in package xlib :(
  ((index)
   `(make-mode-info
     (card16-get (+ ,index 26))
     (card32-get ,index)
     (card16-get (+ ,index  4))
     (card16-get (+ ,index  6))
     (card32-get (+ ,index  8))
     (card16-get (+ ,index 12))
     (card16-get (+ ,index 14))
     (card16-get (+ ,index 16))
     (card16-get (+ ,index 18))
     (card16-get (+ ,index 20))
     (card16-get (+ ,index 22))
     (card16-get (+ ,index 24))
     (card32-get (+ ,index 28))))
  ((index thing)
   `(let ((name (mode-info-name ,thing)))
      (card32-put ,index             (mode-info-id           ,thing))
      (card16-put (index+ ,index  4) (mode-info-width        ,thing))
      (card16-put (index+ ,index  6) (mode-info-height       ,thing))
      (card32-put (index+ ,index  8) (mode-info-dot-clock    ,thing))
      (card16-put (index+ ,index 12) (mode-info-h-sync-start ,thing))
      (card16-put (index+ ,index 14) (mode-info-h-sync-end   ,thing))
      (card16-put (index+ ,index 16) (mode-info-h-sync-total ,thing))
      (card16-put (index+ ,index 18) (mode-info-h-sync-skew  ,thing))
      (card16-put (index+ ,index 20) (mode-info-v-sync-start ,thing))
      (card16-put (index+ ,index 22) (mode-info-v-sync-end   ,thing))
      (card16-put (index+ ,index 24) (mode-info-v-total      ,thing))
      (card16-put (index+ ,index 26) (length name))
      (card32-put (index+ ,index 28) (mode-info-mode-flags   ,thing))
      (string-put (index+ ,index 32) name :appending t))))

(def-clx-class (panning)
  (left          0 :type card16)
  (top           0 :type card16)
  (width         0 :type card16)
  (height        0 :type card16)
  (track-left    0 :type card16)
  (track-top     0 :type card16)
  (track-width   0 :type card16)
  (track-height  0 :type card16)
  (border-left   0 :type int16)
  (border-top    0 :type int16)
  (border-right  0 :type int16)
  (border-bottom 0 :type int16))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-accessor rr-panning (24) ; interns in package xlib :(
    ((index)
     `(make-panning :left          (card16-get ,index)
                    :top           (card16-get (index+ ,index  2))
                    :width         (card16-get (index+ ,index  4))
                    :height        (card16-get (index+ ,index  6))
                    :track-left    (card16-get (index+ ,index  8))
                    :track-top     (card16-get (index+ ,index 10))
                    :track-width   (card16-get (index+ ,index 12))
                    :track-height  (card16-get (index+ ,index 14))
                    :border-left   (int16-get  (index+ ,index 16))
                    :border-top    (int16-get  (index+ ,index 18))
                    :border-right  (int16-get  (index+ ,index 20))
                    :border-bottom (int16-get  (index+ ,index 22))))
    ((index thing)
     `(progn
        (card16-put (index+ ,index  0) (panning-left          ,thing))
        (card16-put (index+ ,index  2) (panning-top           ,thing))
        (card16-put (index+ ,index  4) (panning-width         ,thing))
        (card16-put (index+ ,index  6) (panning-height        ,thing))
        (card16-put (index+ ,index  8) (panning-track-left    ,thing))
        (card16-put (index+ ,index 10) (panning-track-top     ,thing))
        (card16-put (index+ ,index 12) (panning-track-width   ,thing))
        (card16-put (index+ ,index 14) (panning-track-height  ,thing))
        (int16-put  (index+ ,index 16) (panning-border-left   ,thing))
        (int16-put  (index+ ,index 18) (panning-border-top    ,thing))
        (int16-put  (index+ ,index 20) (panning-border-right  ,thing))
        (int16-put  (index+ ,index 22) (panning-border-bottom ,thing))))))

(defstruct (transform (:type vector) :named) ; TODO should all be fixed32
  (x 0 :type card32)
  (y 0 :type card32)
  (z 0 :type card32)
  (i 0 :type card32)
  (j 0 :type card32)
  (k 0 :type card32)
  (d 0 :type card32)
  (e 0 :type card32)
  (f 0 :type card32))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-accessor rr-transform (36) ; interns in package xlib :(
    ((index) `(make-transform :x (card32-get (index+ ,index 0))
                              :y (card32-get (index+ ,index 4))
                              :z (card32-get (index+ ,index 8))
                              :i (card32-get (index+ ,index 12))
                              :j (card32-get (index+ ,index 16))
                              :k (card32-get (index+ ,index 20))
                              :d (card32-get (index+ ,index 24))
                              :e (card32-get (index+ ,index 28))
                              :f (card32-get (index+ ,index 32))))
    ((index thing) `(xlib::sequence-put ,index ,thing :start 1))))

;;; Events

(declare-event :rr-screen-change-notify
  ((data  (member8 +rotation-mask-vector+)))
  (card16 sequence)
  (card32 timestamp config-timestamp)
  ;; (card32 config-timestamp)
  (window root-window request-window)
  ;; (window request-window)
  (card16 size-id sub-pixel-order width height width-in-mm height-in-mm))

;;; Used only in declare-event
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-accessor rr-notify-body (0)
    ((index)
     `(let ((offset 4))
        (assert (= ,index offset))
        (ecase (card8-get 1)
          (0 ; crtc-change-notify
           (list :timestamp (card32-get (index+ offset  0))
                 :window    (window-get (index+ offset  4))
                 :crtc      (card32-get (index+ offset  8))
                 :mode      (card32-get (index+ offset 12))
                 :rotation  (card16-get (index+ offset 16))
                 :x         (card16-get (index+ offset 20))
                 :y         (card16-get (index+ offset 22))
                 :width     (card16-get (index+ offset 24))
                 :height    (card16-get (index+ offset 26))))
          ((1 2)))))
    ((index value)
     (declare (ignore index))
     `(card32-put 0 (first ,value)))))

(declare-event :rr-notify
  ((data (card8)) sub-code) ; output-change-notify, output-property-notify
  (card16         sequence)
  (rr-notify-body body))

;;; Helpers

(declaim (ftype (function (card32 card32) (values boolean &optional))
                has-rates-p))
(defun has-rates-p (major minor)
  (or (> major 1)
      (and (= major 1) (>= minor 1))))

(defun ensure-atom (keyword-or-atom display)
  (if (typep keyword-or-atom 'keyword)
      (find-atom display keyword-or-atom)
      keyword-or-atom))

;;; Requests

(declaim (ftype (function (display) (values card32 card32 &optional))
                query-version))
(defun query-version (display)
  "Execute the RRQueryVersion request and return its result as multiple
values consisting of the server's major and minor protocol versions."
  (with-buffer-request-and-reply (display (randr-opcode display) nil
                                  :sizes (32))
                                 ((data   +rr-QueryVersion+)
                                  (card32 +rr-major+)
                                  (card32 +rr-minor+))
    (values (card32-get 8) (card32-get 12))))

;;; Unexported
(declaim (ftype (function (display (or null card32) (or null card32))
                          (values card32 card32 &optional))
                maybe-query-version))
(defun maybe-query-version (display major minor)
  "Return MAJOR and MINOR as multiple values, substituting 0 for NIL,
unless they are both NIL, in which case call QUERY-VERSION and return
its values.

Some requests (e.g., RRGetScreenInfo) behave differently after a version
query (only the first query has any effect on these requests).
In order that the functions executing such requests be able to skip
subsequent (redundant) queries, have them accept MAJOR and MINOR keyword
arguments and call this function with those arguments instead of calling
QUERY-VERSION."
  (if (or major minor)
      (values (or major 0) (or minor 0))
      (query-version display)))

(declaim (ftype (function (window &key
                                  (:major (or null card32))
                                  (:minor (or null card32))
                                  (:result-type t))
                          (values window timestamp timestamp
                                  (clx-list rotation-mask-class) screen-size            (or null card16)
                                  (clx-list rotation-mask-class) (clx-list screen-size) (clx-sequence card16)
                                  &optional))
                get-screen-info))
(defun get-screen-info (window &key major minor (result-type 'list))
  "Execute the RRGetScreenInfo request and return its result as multiple
values consisting of:

1. List of possible rotations and reflections
2. Root window
3. Timestamp
4. Configuration timestamp
5. Current screen size index (in the list of possible screen sizes)
6. Current rotation and reflection
7. List of possible screen sizes
8. Current refresh rate (non-NIL only if server's protocol version is
   1.1 or later)
9. Sequence of refresh rate information (non-NIL only if server's
   protocol version is 1.1 or later)

Each screen size has in the refresh rate information sequence a
corresponding refresh rate count followed by that number of possible
refresh rates.
For example, '(2 120 60 1 60) means that the first screen size has the
two refresh rates 120 and 60, and that the second screen size has the
single refresh rate 60.

If MAJOR and MINOR, which comprise the server's protocol version, are
missing, this function executes the RRQueryVersion request before
RRGetScreenInfo in order to, first, potentially ask the server to
include, if it can, the current refresh rate and the refresh rate
information sequence in its reply to the latter request, and second,
determine whether this information is forthcoming.
Otherwise, this function assumes MAJOR and MINOR are the result of
QUERY-VERSION -- failing which it will behave unreliably -- and it
skips executing the RRQueryVersion request."
  (let ((display (window-display window)))
    (declare (type display display))
    (multiple-value-bind (major minor)
        (maybe-query-version display major minor)
      (with-buffer-request-and-reply (display (randr-opcode display) nil
                                              :sizes (8 16 32))
                                     ((data   +rr-GetScreenInfo+)
                                      (window window))
        (let* ((num-screens        (card16-get 20))
               (rate-info-length   (card16-get 28))
               (screen-start       +replysize+)
               (rate-info-start    (index+ screen-start (index* num-screens 8)))
               (has-rates          (has-rates-p major minor))
               ;; Possible rotations and reflections
               (rotations          (make-rotation-keys (card16-get 1)))
               (root-window        (window-get 8))
               (timestamp          (card32-get 12))
               (config-timestamp   (card32-get 16))
               (current-size-index (card16-get 22))
               (current-rotation   (make-rotation-keys (card16-get 24)))
               (sizes              (loop :repeat num-screens
                                         :for offset fixnum = screen-start :then (+ offset 8)
                                         :collect (make-screen-size
                                                   (card16-get offset)
                                                   (card16-get (index+ offset 2))
                                                   (card16-get (index+ offset 4))
                                                   (card16-get (index+ offset 6)))))
               ;; Some servers (e.g., X.Org) always reply with the
               ;; current refresh rate if they support it, even before
               ;; receiving any version query.
               ;; However, the refresh rate information is available
               ;; only after querying the version (when providing an
               ;; appropriate client version).
               (current-rate       (when has-rates
                                     (card16-get 26)))
               (rates              (when has-rates
                                     (sequence-get :format      card16
                                                   :index       rate-info-start
                                                   :length      rate-info-length
                                                   :result-type result-type))))
          (values root-window timestamp config-timestamp
                  current-rotation (nth current-size-index sizes) current-rate
                  rotations sizes rates))))))

(defun set-screen-config (window timestamp config-timestamp size-id rotation refresh)
  "Set the current screen to which WINDOW belongs.
Timestamps are obtained from `get-screen-info'.
Rotation can be a list of rotation keys or a rotation mask.
Returns timestamp, config timestamp, the root window of the screen and
sub-pixel order."
  (declare (type window window)
           (type card16 size-id refresh)
           (type card32 timestamp config-timestamp))
  (let ((display  (window-display window))
        (rot-mask (if (consp rotation)
                      (make-rotation-mask rotation)
                      rotation)))
    (declare (type display display)
             (type card16  rot-mask))
    (with-buffer-request-and-reply (display (randr-opcode display) nil
                                    :sizes (16 32))
                                   ((data   +rr-SetScreenConfig+)
                                    (window window)
                                    (card32 timestamp)
                                    (card32 config-timestamp)
                                    (card16 size-id)
                                    (card16 rot-mask)
                                    (card16 refresh)
                                    (pad16))
      (let ((status           (member8-vector-get   1 +config-status+))
            (timestamp        (card32-get           8))
            (config-timestamp (card32-get          12))
            (root-window      (window-get          16))
            (sub-pixel-order  (member16-vector-get 20 +render-subpixel-order+)))
        (values status timestamp config-timestamp root-window sub-pixel-order)))))

(defun select-input (window enable)
  "Enable XRandR event reception for WINDOW.
ENABLE may be a select-mask or list of select-keys."
  (declare (type window window))
  (let ((display     (window-display window))
        (select-mask (if (consp enable) (make-select-mask enable) enable)))
    (declare (type display display)
             (type card16  select-mask))
    (with-buffer-request (display (randr-opcode display))
      (data   +rr-selectinput+)
      (window window)
      (card16 select-mask)
      (pad16  pad))))

;;; Version 1.2

(defun get-screen-size-range (window)
  "Return minimum and maximum dimensions for the display containing WINDOW.
Return four values: 1) minimum width 2) minimum height 3) maximum
width 4) maximum height."
  (let ((display (window-display window)))
    (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (16))
                                   ((data   +rr-getscreensizerange+)
                                    (window window))
      (values-list (sequence-get :format      card16
                                 :index       8
                                 :length      4
                                 :result-type 'list)))))

;; doesn't work, asynchronous match error. set screen config works fine.

(defun set-screen-size (window width height width-mm height-mm)
  ""
  (declare (type window window)
           (type card16 width height)
           (type card32 width-mm height-mm))
  (let ((display (window-display window)))
    (declare (type display display))
    (with-buffer-request (display (randr-opcode display))
      (data   +rr-setscreensize+)
      (window window)
      (card16 width)
      (card16 height)
      (card32 width-mm)
      (card32 height-mm))))

(defun split-mode-names (mode-names-string mode-infos)
  (loop :for mode-info :in mode-infos
        :for start     =   0 :then end
        :for end       =   (+ start (mode-info-name mode-info))
        :for name      =   (subseq mode-names-string start end)
        :do (setf (mode-info-name mode-info) name)))

;;; This is shared between `get-screen-resources{,-current}'.
(defmacro decode-screen-resources (result-type)
  `(let* ((timestamp        (card32-get 8))
          (config-timestamp (card32-get 12))
          (crtc-count       (card16-get 16))
          (output-count     (card16-get 18))
          (mode-count       (card16-get 20))
          (name-bytes       (card16-get 22))
          (crtcs            (sequence-get :format      card32
                                          :index       32
                                          :length      crtc-count
                                          :result-type ,result-type))
          (output-start     (index+ +replysize+ (index* crtc-count 4)))
          (outputs          (sequence-get :format      card32
                                          :index       output-start
                                          :length      output-count
                                          :result-type ,result-type))
          (mode-start       (index+ output-start (index* output-count 4)))
          (modes            (loop :for i :of-type fixnum :from 1 :to mode-count
                                  :for offset :of-type fixnum := mode-start :then (+ offset 32)
                                  :collect (xlib::rr-mode-info-get offset)))
          (name-start       (index+ mode-start (index* mode-count 32)))
          (mode-names       (string-get name-bytes name-start)))
     (split-mode-names mode-names modes)
     (values timestamp config-timestamp crtcs outputs modes)))

(defun get-screen-resources (window &key (result-type 'list))
  "Poll hardware for changes and return screen resources for WINDOW.

Return six values:
1. the timestamp?
2. the configuration timestamp?
3. a sequence of type RESULT-TYPE of ids of available CRTCs.
4. a sequence of type RESULT-TYPE of ids of available OUTPUTS.
5. a sequence of type RESULT-TYPE of `mode-info' objects which describe
   the available modes.
6. a list of strings naming the modes returned as the fifth values."
  (let ((display (window-display window)))
    (declare (type display display)
             (type window window))
    (with-buffer-request-and-reply (display (randr-opcode display) nil
                                            :sizes (8 16 32))
                                   ((data   +rr-getscreenresources+)
                                    (window window))
      (decode-screen-resources result-type))))

(defun get-output-info (display output config-timestamp
                        &key (result-type 'list))
  "Return information for the output OUTPUT on DISPLAY.

OUTPUT is the id of an output.

CONFIG-TIMESTAMP is the ?.

Return 11 values:
 1. the status of OUTPUT
 2. the timestamp ?
 3. the name of OUTPUT as a string
 4. the id of the CRTC currently using OUTPUT
 5. the physical width of OUTPUT in millimeters
 6. the physical height of OUTPUT in millimeters
 7. the connection state of OUTPUT
 8. the sub-pixel order of OUTPUT
 9. a sequence of type RESULT-TYPE of ids of CRTCs which could use
    OUTPUT(?)
10. a sequence of type RESULT-TYPE of ids of modes which OUTPUT could
    use(?)
11. a sequence of type RESULT-TYPE of ids of ?"
  (declare (type display   display)
           (type output-id output))
  (with-buffer-request-and-reply (display (randr-opcode display) nil
                                  :sizes (8 16 32))
      ((data   +rr-getoutputinfo+)
       (card32 output)
       (card32 config-timestamp))
    (let* ((status          (member8-vector-get 1 +config-status+))
           (timestamp       (card32-get 8))
           (current-crtc    (card32-get 12))
           (width-in-mm     (card32-get 16))
           (height-in-mm    (card32-get 20))
           (connection      (member8-vector-get 24 +connection+))
           (sub-pixel-order (member8-vector-get 25 +render-subpixel-order+))
           (crtcs-num       (card16-get 26))
           (modes-num       (card16-get 28))
           (clones-num      (card16-get 32))
           (name-length     (card16-get 34))
           (crtcs-start     36)
           (crtcs           (sequence-get :result-type result-type
                                          :length      crtcs-num
                                          :index       crtcs-start))
           (modes-start     (index+ crtcs-start (index* crtcs-num 4)))
           (modes           (sequence-get :result-type result-type
                                          :length      modes-num
                                          :index       modes-start))
           (clones-start    (index+ modes-start (index* modes-num 4)))
           (clones          (sequence-get :result-type result-type
                                          :length      clones-num
                                          :index       clones-start))
           (name-start      (index+ clones-start (index* clones-num 4)))
           (name            (string-get name-length name-start)))
      (values status timestamp name current-crtc
              width-in-mm height-in-mm connection sub-pixel-order
              crtcs modes clones))))

(defun list-output-properties (display output &key (result-type 'list))
  "Return a list of atom properties for OUTPUT on DISPLAY.
?keep it simple and return id's or atom-names?"
  (declare (type display   display)
           (type output-id output))
  (with-buffer-request-and-reply (display (randr-opcode display) nil
                                  :sizes (8 16 32))
                                 ((data   +rr-listoutputproperties+)
                                  (card32 output))
    (let ((atom-count (card16-get 8)))
      (sequence-get :format      card32
                    :index       +replysize+
                    :length      atom-count
                    :result-type result-type
                    :transform   (lambda (id) (atom-name display id))))))

(defun query-output-property (display output atom &key (result-type 'list))
  "Queries the current properties of an atom.
ATOM may be referenced by either id or keyword"
  (declare (type display   display)
           (type output-id output))
  (let ((atom (ensure-atom atom display)))
    (declare (type card32 atom))
    (with-buffer-request-and-reply (display (randr-opcode display) nil
                                            :sizes (8 16 32))
        ((data   +rr-queryoutputproperty+)
         (card32 output)
         (card32 atom))
      (let ((pending   (boolean-get 8))
            (range     (boolean-get 9))
            (immutable (boolean-get 10))
            (value     (sequence-get :result-type result-type
                                     :index       +replysize+
                                     :length      (card32-get 4))))
        (values value immutable range pending)))))

(defun configure-output-property (display output atom value-list
                                  &key pending range)
  "ATOM can be specified by either id or keyword"
  (declare (type display   display)
           (type output-id output)
           (type boolean   pending range))
  (let ((atom (ensure-atom atom display))
        (seq  (coerce value-list 'vector)))
    (with-buffer-request (display (randr-opcode display))
      (data                      +rr-configureoutputproperty+)
      (card32                    output)
      (card32                    atom)
      (boolean                   pending range)
      ((sequence :format card32) seq))))

;;; Spec says type is not interpreted, what use?  shit, are certain
;;; property types tied to certain formats?  change if necessary after
;;; get-output-property
;;;
;;; FIXME asynchronous match error
(defun change-output-property (display output atom mode data &key (atom-type 0))
  "Mode may be 0-replace 1-prepend 2-append. atom-type is obtained by calling get-output-property "
  (declare (type display   display)
           (type output-id output))
  (let ((atom        (ensure-atom atom display))
        (data-length (length data))
        (seq         (coerce data 'vector)))
    (with-buffer-request (display (randr-opcode display))
      (data                      +rr-changeoutputproperty+)
      (card32                    output)
      (card32                    atom)
      (card32                    atom-type)
      (card8                     32) ; should we be concerned about extra bytes for small values?
      (card8                     mode)
      (pad16)
      (card32                    data-length)
      ((sequence :format card32) seq))))

(defun delete-output-property (display output property)
  "Delete PROPERTY from OUTPUT on DISPLAY. "
  (declare (type display   display)
           (type output-id output))
  (let ((atom (ensure-atom property display)))
    (with-buffer-request (display (randr-opcode display))
      (data   +rr-deleteoutputproperty+)
      (card32 output)
      (card32 atom))))

;;; TODO almost identical to XLIB:GET-PROPERTY
(defun get-output-property (display output property
                            &key (type 0) (delete 0) (pending 0)
                                 (result-type 'list))
  "Return the value and type of PROPERTY on OUTPUT."
  (declare (type display   display)
           (type output-id output))
  (let ((atom (ensure-atom property display)))
    (with-buffer-request-and-reply (display (randr-opcode display) nil
                                    :sizes (8 16 32))
                                   ((data   +rr-getoutputproperty+)
                                    (card32 output)
                                    (card32 atom)
                                    (card32 type)
                                    (card32 0)      ; offset
                                    (card32 #xffff) ; length
                                    (card8  delete)
                                    (card8  pending)
                                    (pad16))
      (let* ((format       (card8-get   1))
             (type         (card8-get   8))
             ;; (bytes-after  (card32-get 12))
             (value-length (card32-get 16))
             (value        (unless (zerop value-length)
                             (case format
                               (8  (sequence-get :format      card8
                                                 :index       +replysize+
                                                 :length      value-length
                                                 :result-type result-type))
                               (16 (sequence-get :format      card16
                                                 :index       +replysize+
                                                 :length      value-length
                                                 :result-type result-type))
                               (32 (sequence-get :format      card32
                                                 :index       +replysize+
                                                 :length      value-length
                                                 :result-type result-type)))))
             (value        (case type ; 4 is atom
                             (4 (map result-type (lambda (id)
                                                   (atom-name display id))
                                     value))
                             (t value))))
        (values value type)))))

(defun create-mode (window mode-info)
  "Create a mode described by MODE-INFO and return its id."
  (declare (type window    window)
           (type mode-info mode-info))
  (let ((display (window-display window)))
    (with-buffer-request-and-reply (display (randr-opcode display) nil
                                    :sizes (8 16 32))
                                   ((data   +rr-createmode+)
                                    (window window)
                                    ;; `rr-mode-info-put' writes the
                                    ;; variable length name after the
                                    ;; fixed fields, therefore the
                                    ;; automatic request offset/length
                                    ;; tracking does not work.
                                    (progn
                                      (xlib::rr-mode-info-put 8 mode-info)
                                      (let ((size (+ 40 (length (mode-info-name mode-info)))))
                                        ;; Write request size and bump
                                        ;; buffer pointer.
                                        (card16-put 2 (ceiling (xlib::lround size) 4))
                                        (setf (xlib::buffer-boffset xlib::%buffer)
                                              (index+ xlib::buffer-boffset size)))))
      (let ((mode (card32-get 8)))
        mode))))

(defun destroy-mode (display mode)
  "Destroy mode with id MODE on DISPLAY."
  (declare (type display display)
           (type mode-id mode))
  (with-buffer-request (display (randr-opcode display))
    (data   +rr-destroymode+)
    (card32 mode)))

(defun add-output-mode (display output mode)
  "Add mode with id MODE to the available modes of output with id OUTPUT on DISPLAY."
  (declare (type display   display)
           (type output-id output)
           (type mode-id   mode))
  (with-buffer-request (display (randr-opcode display))
    (data   +rr-addoutputmode+)
    (card32 output)
    (card32 mode)))

(defun delete-output-mode (display output mode)
  "Remove mode with id MODE from the available modes of output with id OUTPUT on DISPLAY."
  (declare (type display   display)
           (type output-id output)
           (type mode-id   mode))
  (with-buffer-request (display (randr-opcode display))
    (data   +rr-deleteoutputmode+)
    (card32 output)
    (card32 mode)))

(defun get-crtc-info (display crtc config-timestamp &key (result-type 'list))
  "Return information for the CRTC CRTC on DISPLAY.

CRTC is the id of a CRTC.

CONFIG-TIMESTAMP is the ?.

Return 11 values:
 1. the status of CRTC
 2. the timestamp ?
 3. the position of the left edge of CRTC in pixels
 4. the position of the top edge of CRTC in pixels
 5. the width of CRTC in pixels
 6. the height of CRTC in pixels
 7. the id of the mode currently used by CRTC.
 8. a list of symbols indicating the rotations currently configured for CRTC.
 9. a list of symbols indicating the rotations available for CRTC.
10. a sequence of type RESULT-TYPE of ids of outputs currently used by
    CRTC(?).
11. a sequence of type RESULT-TYPE of ids of outputs which CRTC could
    use(?)."
  (declare (type display display)
           (type crtc-id crtc))
  (with-buffer-request-and-reply (display (randr-opcode display) nil
                                  :sizes (8 16 32))
                                 ((data   +rr-getcrtcinfo+)
                                  (card32 crtc)
                                  (card32 config-timestamp))
    (let* ((status                (member8-vector-get 1 +config-status+))
           (timestamp             (card32-get 8))
           (x                     (int16-get 12))
           (y                     (int16-get 14))
           (width                 (card16-get 16))
           (height                (card16-get 18))
           (mode                  (card32-get 20))
           (current-rotation      (make-rotation-keys (card16-get 24)))
           (possible-rotations    (make-rotation-keys (card16-get 26)))
           (output-count          (card16-get 28))
           (possible-output-count (card16-get 30))
           (possible-output-start (index+ +replysize+ (index* output-count 4)))
           (outputs               (sequence-get :result-type result-type
                                                :index       +replysize+
                                                :length      output-count))
           (possible-outputs      (sequence-get :result-type result-type
                                                :index       possible-output-start
                                                :length      possible-output-count)))
      (values status timestamp x y width height mode
              current-rotation possible-rotations
              outputs possible-outputs))))

(defun set-crtc-config (display crtc timestamp config-timestamp
                        x y mode rotation output-list)
  "Rotation can be a rotation mask or list of rotation keys."
  (declare (type display display)
           (type crtc-id crtc))
  (let ((rot-mask (if (consp rotation) (make-rotation-mask rotation) rotation))
        (seq      (coerce output-list 'vector)))
    (with-buffer-request-and-reply (display (randr-opcode display) nil
                                    :sizes (8 16 32))
                                   ((data                      +rr-setcrtcconfig+)
                                    (card32                    crtc)
                                    (card32                    timestamp)
                                    (card32                    config-timestamp)
                                    (card16                    x)
                                    (card16                    y)
                                    (card32                    mode)
                                    (card16                    rot-mask)
                                    (pad16)
                                    ((sequence :format card32) seq))
      (let ((status        (member8-vector-get 1 +config-status+))
            (new-timestamp (card32-get 8)))
        (values status new-timestamp)))))

(defun get-crtc-gamma-size (display crtc)
  "Used to determine length of gamma ramps to submit in `set-crtc-gamma'."
  (declare (type display display)
           (type crtc-id crtc))
  (with-buffer-request-and-reply (display (randr-opcode display) nil
                                  :sizes (8 16 32))
                                 ((data   +rr-getcrtcgammasize+)
                                  (card32 crtc))
    (values (card16-get 8))))

(defun get-crtc-gamma (display crtc &key (result-type 'list))
  "Get current gamma ramps, returns 3 sequences for red, green, blue."
  (declare (type display display)
           (type crtc-id crtc))
  (with-buffer-request-and-reply (display (randr-opcode display) nil
                                  :sizes (8 16 32))
                                 ((data   +rr-getcrtcgamma+)
                                  (card32 crtc))
    (let* ((size        (card16-get 8))
           (green-start (index+ +replysize+ (index* 2 size)))
           (blue-start  (index+ green-start (index* 2 size)))
           (red         (sequence-get :format      card16
                                      :index       +replysize+
                                      :length      size
                                      :result-type result-type))
           (green       (sequence-get :format      card16
                                      :index       green-start
                                      :length      size
                                      :result-type result-type))
           (blue        (sequence-get :format      card16
                                      :index       blue-start
                                      :length      size
                                      :result-type result-type)))
      (values red green blue))))

(defun set-crtc-gamma (display crtc red green blue)
  "gamma values must be lists and must be the same length as returned by get-crtc-gamma-size"
  (declare (type display display)
           (type crtc-id crtc)
           (type cons    red green blue))
  (let ((size (length blue))
        (seq  (coerce (append red green blue) 'vector)))
    (declare (type vector  seq)
             (type card16  size))
    (with-buffer-request (display (randr-opcode display))
      (data                      +rr-setcrtcgamma+)
      (card32                    crtc)
      (card16                    size)
      (pad16) ; TODO (pad16 pad)
      ((sequence :format card16) seq))))

;;; Version 1.3

(defun get-screen-resources-current (window &key (result-type 'list))
  "Return screen resources for WINDOW as multiple values.

Return six values:
1. the timestamp?
2. the configuration timestamp?
3. a sequence of type RESULT-TYPE of ids of available CRTCs.
4. a sequence of type RESULT-TYPE of ids of available OUTPUTS.
5. a sequence of type RESULT-TYPE of `mode-info' structure which
   describe the available modes.
6. a list of strings naming the modes returned as the fifth value.

Unlike `get-screen-resources', this merely returns the current
configuration, and does not poll for hardware changes."
  (declare (type window window))
  (let ((display (window-display window)))
    (with-buffer-request-and-reply (display (randr-opcode display) nil
                                    :sizes (8 16 32))
                                   ((data   +rr-getscreenresourcescurrent+)
                                    (window window))
      (decode-screen-resources result-type))))

(defun get-crtc-transform (display crtc &key (result-type 'list))
  ""
  (declare (type display display)
           (type crtc-id crtc))
  (with-buffer-request-and-reply (display (randr-opcode display) nil
                                  :sizes (8 16 32))
                                 ((data   +rr-getcrtctransform+)
                                  (card32 crtc))
    (let* ((pend-name       (card16-get 88))
           (pend-num-params (card16-get 90))
           (pad-pend        (- 4 (mod pend-name 4)))
           (pad-pend-start  (index+ 96 pend-name pad-pend))
           (cur-name        (card16-get 92))
           (cur-num-params  (card16-get 94))
           (pad-cur         (- 4 (mod cur-name 4)))
           (cur-name-start  (index+ pad-pend-start (index* 4 pend-num-params)))
           (cur-param-start (index+ cur-name-start cur-name pad-cur))
           (transform       (xlib::rr-transform-get 8)))
      (declare (type card16 pend-name cur-name))
      (values
       transform
       ;; (sequence-get :result-type result-type :length 9 :index 8)
       (card8-get 44)
       (sequence-get :result-type result-type :length 9 :index 48)
       (string-get pend-name 96)
       (sequence-get :result-type result-type :length pend-num-params :index pad-pend-start)
       (string-get cur-name cur-name-start)
       (sequence-get :result-type result-type :length cur-num-params :index cur-param-start)))))

(defun set-crtc-transform (display crtc transform
                           &key (filter-name "") filter-parameters)
  "FIXME:Transform may be a list or vector of length 9.  ?perhaps allow length 6?"
  (declare (type display display)
           (type crtc-id crtc)
           (type string  filter-name))
  (error "not implemented")
  (let* ((seq          (if filter-parameters
                           (coerce filter-parameters 'vector)
                           #()))
         ;; (param-length (length seq))
         (name-length  (length filter-name)))
    (declare (type vector seq)
             (type card16 param-length))
    (with-buffer-request (display (randr-opcode display))
      (data                      +rr-setcrtctransform+)
      (card32                    crtc)
      (rr-transform              transform)
      (card16                    name-length)
      (pad16)
      ((string :appending t)     filter-name) ; appending to not store string length again
      ;; ((sequence :format card32) seq) TODO does not work after variable-length field; look at `create-mode' for inspiration
      )))

(defun get-panning (display crtc)
  "Return panning information for CRTC on DISPLAY."
  (declare (type display display)
           (type crtc-id crtc))
  (with-buffer-request-and-reply (display (randr-opcode display) nil
                                          :sizes (8 16 32))
      ((data   +rr-GetPanning+)
       (card32 crtc))
    (let ((status    (member8-vector-get    1 +config-status+))
          (timestamp (card32-get            8))
          (panning   (xlib::rr-panning-get 12)))
      (values status timestamp panning))))

(defun set-panning (display crtc timestamp panning)
  ""
  (declare (type display display)
           (type crtc-id crtc)
           (type panning panning))
  (with-buffer-request-and-reply (display (randr-opcode display) nil
                                  :sizes (8 16 32))
                                 ((data             +rr-SetPanning+)
                                  (card32           crtc)
                                  (card32           timestamp)
                                  (xlib::rr-panning panning))
    (let ((status        (member8-vector-get 1 +config-status+))
          (new-timestamp (card32-get         8)))
      (values status new-timestamp))))

(defun get-output-primary (window)
  "Return the id of the primary output of the display containing WINDOW."
  (declare (type window window))
  (let ((display (window-display window)))
    (with-buffer-request-and-reply (display (randr-opcode display) nil
                                            :sizes (8 16 32))
        ((data   +rr-getoutputprimary+)
         (window window))
      (values (card32-get 8)))))

(defun set-output-primary (window output)
  "Set OUTPUT as the primary out of the display containing WINDOW."
  (declare (type window    window)
           (type output-id output))
  (let ((display (window-display window)))
    (with-buffer-request (display (randr-opcode display))
      (data   +rr-setoutputprimary+)
      (window window)
      (card32 output))))

;;; Version 1.4

(defun get-providers (window)
  "Return a list of provider ids for the display containing WINDOW."
  (declare (type window window))
  (let ((display (window-display window)))
    (with-buffer-request-and-reply (display (randr-opcode display) nil
                                            :sizes (8 16 32))
        ((data   +rr-getproviders+)
         (window window))
      (let* ((timestamp      (card32-get 8))
             (provider-count (card16-get 12))
             (provider-ids   (sequence-get :format      card32
                                           :index       (+ 14 2 4 4 4 4) ; padding
                                           :length      provider-count
                                           :result-type 'list)))
        (values provider-ids timestamp)))))

(defun get-provider-info (display provider config-timestamp
                          &key (result-type 'list))
  "Return information for the provider with id PROVIDER on DISPLAY.

Return seven values:
1. timestamp
2. a list of capabilities
3. a sequence of type RESULT-TYPE of ids of available CRTCs.
4. a sequence of type RESULT-TYPE of ids of available outputs.
5. a sequence of type RESULT-TYPE of ids of associated providers.
6. a sequence of type RESULT-TYPE of associated capabilities.
7. the name of the provider"
  (declare (type display     display)
           (type provider-id provider))
  (with-buffer-request-and-reply (display (randr-opcode display) nil
                                  :sizes (8 16 32))
                                 ((data   +rr-GetProviderInfo+)
                                  (card32 provider)
                                  (card32 config-timestamp))
    (let* ((timestamp                     (card32-get 8))
           (capabilities                  (make-provider-capabilities-keys
                                           (card32-get 12)))
           (crtc-count                    (card16-get 16))
           (output-count                  (card16-get 18))
           (associated-provider-count     (card16-get 20))
           (name-length                   (card16-get 22))
           (crtcs-start                   (+ 24 8)) ; two card32 of padding after name length
           (crtcs                         (sequence-get :index       crtcs-start
                                                        :length      crtc-count
                                                        :result-type result-type))
           (outputs-start                 (index+ crtcs-start (index* crtc-count 4)))
           (outputs                       (sequence-get :index       outputs-start
                                                        :length      output-count
                                                        :result-type result-type))
           (associated-providers-start    (index+ outputs-start (index* output-count 4)))
           (associated-providers          (sequence-get :index       associated-providers-start
                                                        :length      associated-provider-count
                                                        :result-type result-type))
           (associated-capabilities-start (index+ associated-providers-start
                                                  (index* associated-provider-count 4)))
           (associated-capabilities       (sequence-get :index       associated-capabilities-start ; TODO map make-provider-capabilities-keys over this?
                                                        :length      associated-provider-count
                                                        :result-type result-type))
           (name-start                    (index+ associated-capabilities-start
                                                  (index* associated-provider-count 4)))
           (name                          (string-get name-length name-start)))
      (values timestamp capabilities crtcs outputs
              associated-providers associated-capabilities name))))

(defun set-provider-output-source (display provider source-provider config-timestamp)
  (declare (type display     display)
           (type provider-id provider source-provider))
  (with-buffer-request (display (randr-opcode display))
    (data   +rr-setprovideroutputsource+)
    (card32 provider)
    (card32 source-provider)
    (card32 config-timestamp)))

(defun set-provider-offload-sink (display provider sink-provider config-timestamp)
  (declare (type display     display)
           (type provider-id provider sink-provider))
  (with-buffer-request (display (randr-opcode display))
    (data   +rr-setprovideroffloadsink+)
    (card32 provider)
    (card32 sink-provider)
    (card32 config-timestamp)))

;;; TODO same as {list,query,...}-output-propert{ies,y}
(defun list-provider-properties (display provider &key (result-type 'list))
  ""
  (declare (type display     display)
           (type provider-id provider))
  (with-buffer-request-and-reply (display (randr-opcode display) nil
                                  :sizes (8 16 32))
                                 ((data   +rr-listproviderproperties+)
                                  (card32 provider))
    (let ((atom-count (card16-get 8)))
      (sequence-get :format      card32
                    :index       +replysize+
                    :length      atom-count
                    :result-type result-type
                    :transform   (lambda (id) (atom-name display id))))))

;; (defun query-provider-property (display provider atom)
;; "untested"
;;   (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (8 16 32))
;;                               ((data +rr-queryproviderproperty+)
;;                                (card32 provider)
;;                                (card32 atom))
;;     (values
;;      (boolean-get 8)
;;      (boolean-get 9)
;;      (boolean-get 10))))

;; (defun  (display)
;; ""
;;   (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (8 16 32))
;;                               ((data))
;;     (values
;;      ())))

;; (defun  (display)
;; ""
;;   (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (8 16 32))
;;                               ((data))
;;     (values
;;      ())))
