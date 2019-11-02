;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XLIB; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: RandR Extension
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

(export '(rr-query-version
          rr-get-screen-info
          rr-set-screen-config

          ;;  1.2

          rr-get-screen-size-range
          rr-set-screen-size
          rr-get-screen-resources
          rr-get-output-info
          rr-list-output-properties
          rr-query-output-property
          rr-configure-output-property
          rr-change-output-property
          rr-delete-output-property
          rr-get-output-property
          rr-create-mode
          rr-destroy-mode
          rr-add-output-mode
          rr-delete-output-mode
          rr-get-crtc-info
          rr-get-crtc-gamma-size
          rr-get-crtc-gamma
          rr-set-crtc-gamma

          ;;  1.3

          rr-get-screen-resources-current
          rr-set-crtc-transform
          rr-get-crtc-transform
          rr-get-panning
          rr-set-panning
          rr-set-output-primary
          rr-get-output-primary

          ;;  1.4

          rr-get-providers
          rr-get-provider-info
          rr-set-provider-output-source
          rr-set-provider-offload-sink
          rr-list-provider-properties
          rr-select-input

          ;; mask related
          make-mode-flag-keys
          make-mode-flag-mask
          make-rr-select-mask
          make-rr-select-keys
          make-rotation-keys
          make-rotation-mask

          ;; struct related
          rr-panning-top
          rr-panning-left
          rr-panning-width
          rr-panning-height
          rr-panning-track-top
          rr-panning-track-left
          rr-panning-track-width
          rr-panning-track-height
          rr-panning-border-left
          rr-panning-border-top
          rr-panning-border-bottom
          rr-panning-border-right
          rr-panning
          make-rr-transform
          ))

(pushnew :clx-ext-randr *features*)

(define-extension "RANDR"
  :events (:rr-screen-change-notify
           :rr-crtc-change-notify
           :rr-output-change-notify
           :rr-output-property-notify)
  :errors (output
           crtc
           mode))

(defun randr-opcode (display)
  (extension-opcode display "RANDR"))


(defconstant +rr-major+			1)
(defconstant +rr-minor+			4)

(defconstant  +rr-QueryVersion+                 0)
  ;; we skip 1 to make old clients fail pretty immediately */


(defconstant  +rr-SetScreenConfig+              2)
(defconstant  +rr-OldScreenChangeSelectInput+	3) ;; 3 used to be ScreenChangeSelectInput; deprecated */

(defconstant  +rr-SelectInput+                  4)
(defconstant  +rr-GetScreenInfo+                5)

  ;; * V1.2 additions */

(defconstant  +rr-GetScreenSizeRange+           6)
(defconstant  +rr-SetScreenSize+                7)
(defconstant  +rr-GetScreenResources+           8)
(defconstant  +rr-GetOutputInfo+                9)
(defconstant  +rr-ListOutputProperties+        10)
(defconstant  +rr-QueryOutputProperty+	       11)
(defconstant  +rr-ConfigureOutputProperty+     12)
(defconstant  +rr-ChangeOutputProperty+        13)
(defconstant  +rr-DeleteOutputProperty+    14)
(defconstant  +rr-GetOutputProperty+	    15)
(defconstant  +rr-CreateMode+		    16)
(defconstant  +rr-DestroyMode+		    17)
(defconstant  +rr-AddOutputMode+	    18)
(defconstant  +rr-DeleteOutputMode+	    19)
(defconstant  +rr-GetCrtcInfo+		    20)
(defconstant  +rr-SetCrtcConfig+	    21)
(defconstant  +rr-GetCrtcGammaSize+	    22)
(defconstant  +rr-GetCrtcGamma+	    23)
(defconstant  +rr-SetCrtcGamma+	    24)

  ;; /* V1.3 additions */

(defconstant  +rr-GetScreenResourcesCurrent+	25)
(defconstant  +rr-SetCrtcTransform+	    26)
(defconstant  +rr-GetCrtcTransform+	    27)
(defconstant  +rr-GetPanning+		    28)
(defconstant  +rr-SetPanning+		    29)
(defconstant  +rr-SetOutputPrimary+	    30)
(defconstant  +rr-GetOutputPrimary+	    31)

  ;; 1.4 additions


(defconstant  +rr-GetProviders+	      32)
(defconstant  +rr-GetProviderInfo+	      33)
(defconstant  +rr-SetProviderOffloadSink+    34)
(defconstant  +rr-SetProviderOutputSource+   35)
(defconstant  +rr-ListProviderProperties+    36)
(defconstant  +rr-QueryProviderProperty+     37)
(defconstant  +rr-ConfigureProviderProperty+ 38)
(defconstant  +rr-ChangeProviderProperty+    39)
(defconstant  +rr-DeleteProviderProperty+    40)
(defconstant  +rr-GetProviderProperty+	      41)

;;; status returns


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +rr-config-status+ '#(:success :invalid-config-time :invalid-time :failed))
  (defconstant +rr-connection+ '#(:connected :disconnected :unknown-connection)))

;;; mask-vectors and types

  ;; Rotation


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +rotation-mask-vector+
    '#(:rotate-0 :rotate-90 :rotate-180 :rotate-270 :reflect-x :reflect-y)))

(deftype rotation-mask-class ()
  '(member :rotate-0 :rotate-90 :rotate-180 :rotate-270 :reflect-x :reflect-y))

(deftype rotation-mask ()
  '(or mask16 (clx-list event-mask-class)))

  ;; Select


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +rr-select-mask-vector+
    '#(:screen-change-notify-mask :crtc-change-notify-mask :output-change-notify-mask :output-property-notify-mask)))

(deftype rr-select-mask-class ()
  '(member :screen-change-notify-mask :crtc-change-notify-mask :output-change-notify-mask :output-property-notify-mask))

(deftype rr-select-mask ()
  '(or mask8 (clx-list rr-select-mask-class)))

  ;; Mode-flag

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +mode-flag-mask-vector+
    '#(:hsync-positive :hsync-negative :vsync-positive :vsync-negative :interlace :double-scan :csync
       :csync-positive :csync-negative :hskew-present :b-cast :pixel-multiplex :double-clock :clock-divide-by-2)))

(deftype mode-flag-mask-class ()
  '(member :hsync-positive :hsync-negative :vsync-positive :vsync-negative :interlace :double-scan
    :csync :csync-positive :csync-negative :hskew-present :b-cast :pixel-multiplex :double-clock
    :clock-divide-by-2))

(deftype mode-flag-mask ()
  '(or mask32 (clx-list mode-flag-mask-class)))

  ;; temporarily here since not in xrender.lisp


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +render-subpixel-order+
    '#(:unknown :horizontal-RGB :horizontal-BGR :vertical-RGB :vertical-BGR :none)))

  ;; mask encode-decode functions

  ;; (defun make-mode-flag-mask (key-list)
  ;;   (encode-mask +mode-flag-mask-vector+ key-list 'mode-flag-mask))

  ;; (defun make-mode-flag-keys (mode-flag-mask)
  ;;   (declare (type mask32 mode-flag-mask))
  ;;   (declare (clx-values (clx-list mode-flag-mask)))
  ;;   (decode-mask +mode-flag-mask-vector+ mode-flag-mask))

  ;; (defun make-rotation-mask (key-list)
  ;;   (encode-mask +rotation-mask-vector+ key-list ))



(defmacro define-mask-fns (name mask-size mask-vector mask-type)
  (let ((encode-fn (xintern 'make- name '-mask))
        (decode-fn (xintern 'make- name '-keys)))
    `(progn
       (defun ,encode-fn (key-list)
         (encode-mask ,mask-vector key-list ',mask-type))
       (defun ,decode-fn (bit-mask)
         (declare (type ,mask-size bit-mask))
         (declare (clx-values (clx-list ,mask-type)))
         (decode-mask ,mask-vector bit-mask))
       )))

(define-mask-fns mode-flag card32 +mode-flag-mask-vector+ mode-flag-mask)
(define-mask-fns rr-select card8 +rr-select-mask-vector+ rr-select-mask)
(define-mask-fns rotation card16 +rotation-mask-vector+ rotation-mask)

;; (defconstant  +RRTransformUnit		    (1L+ << 0))
;; (defconstant  +RRTransformScaleUp	    (1L+ << 1))
;; (defconstant  +RRTransformScaleDown	    (1L+ << 2))
;; (defconstant  +RRTransformProjective	    (1L+ << 3))

;; types

(deftype size-id () 'card16)
(deftype rr-mode () '(or null resource-id))
(deftype output () 'resource-id)
(deftype connection () '(or +connected+ +disconnected+ +unknown-connection+))

;; structs

(def-clx-class (screen-size (:constructor make-screen-size (width-in-pixels
                                                            height-in-pixels
                                                            width-in-mm
                                                            height-in-mm)))
  (width-in-pixels 0 :type card16)
  (height-in-pixels 0 :type card16)
  (width-in-mm 0 :type card16)
  (height-in-mm 0 :type card16))

(def-clx-class (rr-mode-info
                (:constructor make-rr-mode-info (id width height dot-clock
                                                 h-sync-start h-sync-end h-sync-total h-sync-skew
                                                 v-sync-start v-sync-end v-total name-length mode-flags)))
  (id  0 :type card32)
  (width  0 :type card16)
  (height  0 :type card16)
  (dot-clock  0 :type card32)
  (h-sync-start  0 :type card16)
  (h-sync-end  0 :type card16)
  (h-sync-total  0 :type card16)
  (h-sync-skew  0 :type card16)
  (v-sync-start  0 :type card16)
  (v-sync-end  0 :type card16)
  (v-total  0 :type card16)
  (name-length  0 :type card16)
  (mode-flags  0 :type mode-flag-mask))


(def-clx-class (rr-panning)
  (left 0 :type card16)
  (top 0 :type card16)
  (width 0 :type card16)
  (height 0 :type card16)
  (track-left 0 :type card16)
  (track-top 0 :type card16)
  (track-width 0 :type card16)
  (track-height 0 :type card16)
  (border-left 0 :type int16)
  (border-top 0 :type int16)
  (border-right 0 :type int16)
  (border-bottom 0 :type int16))

(defstruct (rr-transform (:type vector) :named)
  (x 0 :type card32)
  (y 0 :type card32)
  (z 0 :type card32)
  (i 0 :type card32)
  (j 0 :type card32)
  (k 0 :type card32)
  (d 0 :type card32)
  (e 0 :type card32)
  (f 0 :type card32))

;; accessors
;; fricken macroexpansions !!! figure it out!!

(define-accessor rr-transform (36)
  ((index) `(make-rr-transform :x (card32-get (index+ ,index 0))
                               :y (card32-get (index+ ,index 4))
                               :z (card32-get (index+ ,index 8))
                               :i (card32-get (index+ ,index 12))
                               :j (card32-get (index+ ,index 16))
                               :k (card32-get (index+ ,index 20))
                               :d (card32-get (index+ ,index 24))
                               :e (card32-get (index+ ,index 28))
                               :f (card32-get (index+ ,index 32))))
  ((index thing) `(sequence-put ,index ,thing :start 1)))

;; (define-accessor rr-panning (24)
;;   ((index) `(make-rr-panning :left (card16-get ,index)
;;                           :top (card16-get (index+ ,index 2))
;;                           :width (card16-get (index+ ,index 4))
;;                           :height (card16-get (index+ ,index 6))
;;                           :track-left (card16-get (index+ ,index 8))
;;                           :track-top (card16-get (index+ ,index 10))
;;                           :track-width (card16-get (index+ ,index 12))
;;                           :track-height (card16-get (index+ ,index 14))
;;                           :border-left (int16-get (index+ ,index 16))
;;                           :border-top (int16-get (index+ ,index 18))
;;                           :border-right (int16-get (index+ ,index 20))
;;                           :border-bottom (int16-get (index+ ,index 22))))
;;   ;; put doesn't work
;;   ((index thing)  `(progn ,`(write-card16 (index+ ,index 0)(rr-panning-left ,thing))
;;                        , `(write-card16 (index+ ,index 2)(rr-panning-top ,thing))
;;                        , `(write-card16 (index+ ,index 4)(rr-panning-width ,thing))
;;                        , `(write-card16 (index+ ,index 6)(rr-panning-height ,thing))
;;                        , `(write-card16 (index+ ,index 8)(rr-panning-track-left ,thing))
;;                        , `(write-card16 (index+ ,index 10)(rr-panning-track-top ,thing))
;;                        , `(write-card16 (index+ ,index 12)(rr-panning-track-width ,thing))
;;                        , `(write-card16 (index+ ,index 14)(rr-panning-track-height ,thing))
;;                        , `(write-int16 (index+ ,index 16)(rr-panning-border-left ,thing))
;;                        , `(write-int16 (index+ ,index 18)(rr-panning-border-top ,thing))
;;                        , `(write-int16 (index+ ,index 20)(rr-panning-border-right ,thing))
;;                        , `(write-int16(index+ ,index 22)(rr-panning-border-bottom ,thing)))
;;                     ))

;; (defmacro pan-put ())

(define-accessor rr-mode-info (32)
  ((index)
   `(make-rr-mode-info
     (card32-get ,index)
     (card16-get (+ ,index 4))
     (card16-get (+ ,index 6))
     (card32-get (+ ,index 8))
     (card16-get (+ ,index 12))
     (card16-get (+ ,index 14))
     (card16-get (+ ,index 16))
     (card16-get (+ ,index 18))
     (card16-get (+ ,index 20))
     (card16-get (+ ,index 22))
     (card16-get (+ ,index 24))
     (card16-get (+ ,index 26))
     (card32-get (+ ,index 28))))
  ((index thing)
   `(progn (card32-put ,index (rr-mode-info-id ,thing))
     (card16-put (index+ ,index 4) (rr-mode-info-width ,thing))
     (card16-put (index+ ,index 6) (rr-mode-info-height ,thing))
     (card32-put (index+ ,index 8) (rr-mode-info-dot-clock ,thing))
     (card16-put (index+ ,index 12) (rr-mode-info-h-sync-start ,thing))
     (card16-put (index+ ,index 14) (rr-mode-info-h-sync-end ,thing))
     (card16-put (index+ ,index 16) (rr-mode-info-h-sync-total ,thing))
     (card16-put (index+ ,index 18) (rr-mode-info-h-sync-skew ,thing))
     (card16-put (index+ ,index 20) (rr-mode-info-v-sync-start ,thing))
     (card16-put (index+ ,index 22) (rr-mode-info-v-sync-end ,thing))
     (card16-put (index+ ,index 24) (rr-mode-info-v-total ,thing))
     (card16-put (index+ ,index 26) (rr-mode-info-name-length ,thing))
     (card32-put (index+ ,index 28) (rr-mode-info-mode-flags ,thing))
     )))

;; x-events

;; test!!

(declare-event :rr-screen-change-notify
  ((data (member8 +rotation-mask-vector+)))
  (card16 sequence)
  (card32 timestamp config-timestamp)
;  (card32 config-timestamp)
  (window root-window request-window)
 ; (window request-window)
  (card16 size-id sub-pixel-order width height width-in-mm height-in-mm))

(declare-event :rr-crtc-change-notify
  ((data (member8 +rotation-mask-vector+)))
  (card16 sequence)
  (card32 timestamp)
  (window request-window)
  (card32 crtc)
  (card32 mode)
  (card16 rotation)
  (pad16)
  (int16 x y)
  (card16 width height))

(declare-event :rr-output-change-notify
  (card16 sequence)
  (card32 timestamp config-timestamp)
  (window request-window)
  (card32 output crtc mode)
  (card16 rotation)
  (card8 connection)
  (card8 sub-pixel-order))

(declare-event :rr-output-property-notify
  (card16 sequence)
  (window window)
  (card32 output atom timestamp)
  (boolean state)
  )

;; x-requests

(defun rr-query-version (display)
"Returns version MAJOR and MINOR from server."
  (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (32))
                                 ((data +rr-QueryVersion+)
                                  (card32 +rr-major+)
                                  (card32 +rr-minor+))
    (values
     (card32-get 8)
     (card32-get 12))))

(defun rr-set-screen-config (window timestamp conf-timestamp size-id rotation refresh)
  "Sets the current screen to which the given window belongs.  Timestamps are obtained from rr-get-screen-info.  Rotation can be a list of rotation keys or a rotation mask.  Returns timestamp, config timestamp, the root window of the screen and sub-pixel order."
  (let ((display (window-display window))
        (rot-mask (if (consp rotation)
                      (make-rotation-mask rotation)
                      rotation)))
    (declare (type display display)
             (type window window)
             (type card16 size-id rot-mask refresh)
             (type card32 timestamp conf-timestamp))
    (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (16 32))
                                   ((data +rr-SetScreenConfig+)
                                    (window window)
                                    (card32 timestamp)
                                    (card32 conf-timestamp)
                                    (card16 size-id)
                                    (card16 rot-mask)
                                    (card16 refresh)
                                    (pad16))
      (values
       (member8-vector-get 1 +rr-config-status+)
       (card32-get 8)  ;; timestamp
       (card32-get 12) ;; config timestamp
       (window-get 16) ;; root window
       (member16-vector-get 20 +render-subpixel-order+) ;; sub pixel order
       ))))

(defun rr-select-input (window enable)
"Enables event reception for given window.  Enable may be a select-mask or list of select-keys "
  (let ((display (window-display window))
        (select-mask (if (consp enable) (make-rr-select-mask enable) enable)))
    (declare (type display display)
             (type window window)
             (type card16 select-mask))
    (with-buffer-request (display (randr-opcode display))
      (data +rr-selectinput+)
      (window window)
      (card16 select-mask)
      (pad16))))

(defun rr-get-screen-info (window &optional (result-type 'list))
"Returns rotations, root-window, timestamp, config-timestamp, current-size-id, current rotation, current rate, a list of screen-size structures, and last a sequence of refresh-rates"
  (let ((display (window-display window)))
    (declare (type display display)
             (type window window))
    (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (8 16 32))
                                   ((data +rr-GetScreenInfo+ )
                                    (window window))
      (let ((num-screens (card16-get 20))
              (num-rates (card16-get 28))
            (rates-location 0))
        (declare (type fixnum rates-location num-rates))
          (values
           (make-rotation-keys (card16-get 1)) ; possible rotations, using card16, not card8 from spec.
           (window-get 8) ;root window
           (card32-get 12) ;timestamp
           (card32-get 16) ;config-timestamp
           (card16-get 22) ;size-id
           (make-rotation-keys (card16-get 24)) ;current rotation
           (card16-get 26) ; current rate
           (loop :for x fixnum :from 1 :to num-screens
                 :for offset fixnum := 32 :then (+ offset 8)
                 :collect (make-screen-size (card16-get offset)
                                            (card16-get (index+ offset 2))
                                            (card16-get (index+ offset 4))
                                            (card16-get (index+ offset 6)))
                 :finally (setf rates-location (+ offset 8 2)))
           (sequence-get :format card16 :length num-rates :index rates-location :result-type result-type))))))


;; Version 1.2

(defun rr-get-screen-size-range (window &optional (result-type 'list))
"Returns a sequence of minimum width, minimum height, max width, max height."
  (let ((display (window-display window)))
   (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (16))
                                  ((data +rr-getscreensizerange+)
                                   (window window))
     (values
      (sequence-get :format card16 :length 4 :index 8 :result-type result-type)))))


;; doesn't work, asynchronous match error. set screen config works fine.

(defun rr-set-screen-size (window width height width-mm height-mm)
  ""
  (let ((display (window-display window)))
    (declare (type display display)
             (type window window)
             (type card16 width height)
             (type card32 width-mm height-mm))
    (with-buffer-request (display (randr-opcode display))
      (data +rr-setscreensize+)
      (window window)
      (card16 width)
      (card16 height)
      (card32 width-mm)
      (card32 height-mm))))

(defun rr-get-screen-resources (window &optional (result-type 'list))
  ""
  (let ((display (window-display window)))
    (declare (type display display)
             (type window window))
    (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (8 16 32))
                                   ((data +rr-getscreenresources+)
                                    (window window))
      (let* ((num-crtcs (card16-get 16))
             (num-outputs (card16-get 18))
             (output-start (index+ +replysize+ (index* num-crtcs 4)))
             (num-modeinfos (card16-get 20))
             (name-bytes (card16-get 22))
             (mode-start (index+ output-start (index* num-outputs 4)))
             (name-start (index+ mode-start (index* num-modeinfos 32))))
        (values
         (card32-get 8)			; timestamp
         (card32-get 12)		; config-timestamp
         (sequence-get :format card32 :result-type result-type :index 32 :length num-crtcs)
         (sequence-get :format card32 :result-type result-type :index output-start :length num-outputs)
         (loop :for i fixnum :from 1 :to num-modeinfos
               :for offset fixnum := mode-start :then (+ offset 32)
               :collect (rr-mode-info-get offset))
         (string-get name-bytes name-start))
        ))))



(defun rr-get-output-info (display output config-timestamp &optional (result-type 'list))
"FIXME: indexes might be off, name not decoded properly"
  (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (8 16 32))
                                 ((data +rr-getoutputinfo+)
                                  (card32 output)
                                  (card32 config-timestamp))
    (let* ((num-crtcs (card16-get 26))
          (num-modes (card16-get 28))
          (num-clones (card16-get 32))
          (name-length (card16-get 34))
          (crtc-start 26)
          (mode-start (index+ crtc-start (index* num-crtcs 4)))
          (clone-start (index+ mode-start (index* num-modes 4)))
          (name-start (index+ clone-start (index* num-clones 4))))
      (values
        (member8-vector-get 1 +rr-config-status+)
        (card32-get 8)  ; timestamp
        (card32-get 12) ; current connected crtc
        (card32-get 16) ; width in mm
        (card32-get 20) ; height in mm
        (member8-vector-get 24 +rr-connection+)
        (member8-vector-get 25 +render-subpixel-order+)  ; sub-pixel-order
        (sequence-get :result-type result-type :length num-crtcs :index 26)
        (card16-get 30)
        (sequence-get :result-type result-type :length num-modes :index mode-start)
        (sequence-get :result-type result-type :length num-clones :index clone-start)
        ;(string-get name-length name-start )
        (sequence-get :result-type 'string :format card16 :length name-length :index name-start :transform #'code-char))
)))

(defun rr-list-output-properties (display output &optional (result-type 'list))
"Returns a list of atom properties for given display. ?keep it simple and return id's or atom-names?"
  (declare (type display display)
           (type card32 output))
  (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (8 16 32))
                                 ((data +rr-listoutputproperties+)
                                  (card32 output))
    (let ((num-atoms (card16-get 8)))
      (values
       (sequence-get :format card32 :result-type result-type :length num-atoms :index +replysize+ :transform #'(lambda (id) (atom-name display id)))))))

(defun rr-query-output-property (display output atom &optional (result-type 'list))
"Querys the current properties of an atom.  Atom may be referenced by either id or keyword"
  (let ((atom (if (typep atom 'keyword) (find-atom display atom) atom)))
    (declare (type display display)
             (type card32 atom))
    (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (8 16 32))
                                   ((data +rr-queryoutputproperty+)
                                    (card32 output)
                                    (card32 atom))
      (values
       (boolean-get 8)  ; pending
       (boolean-get 9)  ; range
       (boolean-get 10) ; immutable
       (sequence-get :result-type result-type :index +replysize+ :length (card32-get 4))))))

(defun rr-configure-output-property (display output atom value-list &optional (pending nil) (range nil))
  "Atom can be specified by either id or keyword"
  (let ((atom (if (typep atom 'keyword) (find-atom display atom) atom))
        (seq (coerce value-list 'vector)))
    (declare (type display display)
             (type card32 output value-list)
             (type boolean pending range))
    (with-buffer-request (display (randr-opcode display))
      (data +rr-configureoutputproperty+)
      (card32 output)
      (card32 atom)
      (boolean pending range)
      ((sequence :format card32) seq))))

;; Spec says type is not interpreted, what use?  shit, are certain property types tied to certain formats?  change if necessary after get-output-property

;; FIXME asynchronous match error
(defun rr-change-output-property (display output atom mode data &optional  (atom-type 0) )
"Mode may be 0-replace 1-prepend 2-append. atom-type is obtained by calling rr-get-output-property "
  (let ((atom (if (typep atom 'keyword) (find-atom display atom) atom))
        (data-length (length data))
        (seq (coerce data 'vector))
        )
    (with-buffer-request (display (randr-opcode display))
      (data +rr-changeoutputproperty+)
      (card32 output)
      (card32 atom)
      (card32 atom-type)
      (card8 32) ; should we be concerned about extra bytes for small values?
      (card8 mode)
      (pad16)
      (card32 data-length)
      ((sequence :format card32) seq))))

(defun rr-delete-output-property (display output property)
  ""
  (let ((atom (if (typep property 'keyword) (find-atom display property) property)))
    (with-buffer-request (display (randr-opcode display))
      (data +rr-deleteoutputproperty+)
      (card32 output)
      (card32 atom))))

(defun rr-get-output-property (display output property  &optional (type 0) (delete 0) (pending 0) (result-type 'list))
  ""
  (let ((atom (if (typep property 'keyword) (find-atom display property) property)))
    (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (8 16 32))
                                   ((data +rr-getoutputproperty+)
                                    (card32 output)
                                    (card32 atom)
                                    (card32 type)
                                    (card32 0) ; long-offset
                                    (card32 0) ; long-length
                                    (card8 delete)
                                    (card8 pending)
                                    (pad16))
      (let* ((bytes-after (card32-get 12))
             (value-length (card32-get 16))
             (byte-format (unless (eql value-length 0)
                            (if (eql bytes-after value-length)
                                'card8
                                (if (eql 2 (/ bytes-after value-length))
                                    'card16
                                    'card32)))))

        (values
         (card32-get 8)			; type
         value-length
         (when (not (eql value-length 0))
           (case byte-format
             (card8 (sequence-get :format card8 :index +replysize+
                                  :length value-length :result-type result-type))
             (card16 (sequence-get :format card16 :index +replysize+
                                   :length value-length :result-type result-type))
             (card32 (sequence-get :format card32 :index +replysize+
                                   :length value-length :result-type result-type)))))))))



(defun rr-create-mode (window mode-info name)
  "FIXME"
  (let ((display (window-display window)))
    (declare (type display display)
             (type window window)
             (type rr-mode-info mode-info)
             (type string name))
    (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (8 16 32))
                                   ((data +rr-createmode+)
                                    (window window)
                                    (progn (rr-mode-info-put 8 mode-info)
                                           (string-put 40 name)))
      (values
       (card32-get 8) ; mode
       ))))

(defun rr-destroy-mode (display mode)
""
 (with-buffer-request (display (randr-opcode display))
   (data +rr-destroymode+)
   (card32 mode)))

(defun rr-add-output-mode (display output mode)
""
 (with-buffer-request (display (randr-opcode display))
   (data +rr-addoutputmode+)
   (card32 output)
   (card32 mode)))

(defun rr-delete-output-mode (display output mode)
""
 (with-buffer-request (display (randr-opcode display))
   (data +rr-deleteoutputmode+)
   (card32 output)
   (card32 mode)))

(defun rr-get-crtc-info (display crtc config-timestamp &optional (result-type 'list))
  ""
  (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (8 16 32))
                                 ((data +rr-getcrtcinfo+)
                                  (card32 crtc)
                                  (card32 config-timestamp))
    (let* ((num-outputs (card16-get 28))
           (pos-outputs (card16-get 30))
           (pos-start (index+ +replysize+ (index* num-outputs 4))))
      (values
       (member8-vector-get 1 +rr-config-status+)
       (card32-get 8)                                   ; timestamp
       (int16-get 12)					; x
       (int16-get 14)					; y
       (card16-get 16)					; width
       (card16-get 18)					; height
       (card32-get 20)					; mode
       (make-rotation-keys (card16-get 24) )  ; current
       (make-rotation-keys (card16-get 26))  ; possible
       (sequence-get :result-type result-type :index +replysize+ :length num-outputs)
       (sequence-get :result-type result-type :index pos-start :length pos-outputs)))))

(defun rr-set-crtc-config (display crtc timestamp config-timestamp x y mode rotation output-list)
"Rotation can be a rotation mask or list of rotation keys."
  (let ((rot-mask (if (consp rotation) (make-rotation-mask rotation) rotation))
        (seq (coerce output-list 'vector)))
    (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (8 16 32))
                                   ((data +rr-setcrtcconfig+)
                                    (card32 crtc)
                                    (card32 timestamp)
                                    (card32 config-timestamp)
                                    (card16 x)
                                    (card16 y)
                                    (card32 mode)
                                    (card16 rot-mask)
                                    (pad16)
                                    ((sequence :format card32) seq))
      (values
       (member8-vector-get 1 +rr-config-status+)
       (card32-get 8) ; new timestamp
       ))))

(defun rr-get-crtc-gamma-size (display crtc)
"Used to determine length of gamma ramps to submit in set-crtc-gamma"
  (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (8 16 32))
                                 ((data +rr-getcrtcgammasize+)
                                  (card32 crtc))
    (values
     (card16-get 8))))

(defun rr-get-crtc-gamma (display crtc &optional (result-type 'list))
  "Get current gamma ramps, returns 3 sequences for red, green, blue."
  (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (8 16 32))
                                 ((data +rr-getcrtcgamma+)
                                  (card32 crtc))
    (let* ((size (card16-get 8))
          (green-start (index+ +replysize+ (index* 2 size)))
          (blue-start (index+ green-start (index* 2 size))))
      (values
       (sequence-get :format card16 :length size :index +replysize+ :result-type result-type)
       (sequence-get :format card16 :length size :index green-start :result-type result-type)
       (sequence-get :format card16 :length size :index blue-start :result-type result-type)))))

(defun rr-set-crtc-gamma (display crtc red green blue)
  "gamma values must be lists and must be the same length as returned by get-crtc-gamma-size"
  (declare (type cons red green blue))
  (let ((size (length blue))
        (seq (coerce (append red green blue) 'vector)))
    (declare (type vector seq)
             (type display display)
             (type card16 size))
    (with-buffer-request (display (randr-opcode display))
      (data +rr-setcrtcgamma+)
      (card32 crtc)
      (card16 size)
      (pad16)
      ((sequence :format card16) seq))))

;; version 1.3


 (defun rr-get-screen-resources-current (window &optional (result-type 'list ))
   "Unlike RRGetScreenResources, this merely returns the current configuration, and does not poll for hardware changes."
   (let ((display (window-display window)))
     (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (8 16 32))
                                    ((data +rr-getscreenresourcescurrent+)
                                     (window window))
       (let* ((num-crtcs (card16-get 16))
             (num-outputs (card16-get 18))
             (output-start (index+ +replysize+ (index* num-crtcs 4)))
             (num-modeinfos (card16-get 20))
             (name-bytes (card16-get 22))
             (mode-start (index+ output-start (index* num-outputs 4)))
             (name-start (index+ mode-start (index* num-modeinfos 32))))
        (values
         (card32-get 8)			; timestamp
         (card32-get 12)		; config-timestamp
         (sequence-get :format card32 :result-type result-type :index 32 :length num-crtcs)
         (sequence-get :format card32 :result-type result-type :index output-start :length num-outputs)
         (loop :for i fixnum :from 1 :to num-modeinfos
               :for offset fixnum := mode-start :then (+ offset 32)
               :collect (rr-mode-info-get offset))
         (string-get name-bytes name-start))))))


;; (defun rr-set-crtc-transform (display crtc transform &optional ( filter-name nil) ( filter-parameters nil))
;;   "FIXME:Transfrom may be a list or vector of length 9.  ?perhaps allow length 6?"
;;   (let ((seq (if filter-parameters (coerce filter-parameters 'vector) nil ))
;;      (param-length (length filter-parameters))
;;      (name-length (length filter-name)))
;;     (declare ;(type vector seq)
;;           (type card16 param-length)
;;           (type display display)
;;           (type string filter-name))
;;     (with-buffer-request (display (randr-opcode display))
;;       (data +rr-setcrtctransform+)
;;       (card32 crtc)
;;       (card16 param-length)
;;       (pad16)
;;       (rr-transform transform)
;;       (card16 name-length)
;;       (pad16)
;;       (string filter-name)
;; ;      ((sequence :format card32) seq)


;;       ;((sequence :format card32) seq)
;;       )))


(defun rr-get-crtc-transform (display crtc &optional (result-type 'list))
  ""
  (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (8 16 32))
                                 ((data +rr-getcrtctransform+)
                                  (card32 crtc))
    (let* ((pend-name        (card16-get 88))
           (pend-num-params (card16-get 90))
           (pad-pend (- 4 (mod pend-name 4)))
           (pad-pend-start (index+ 96 pend-name pad-pend))
           (cur-name         (card16-get 92))
           (cur-num-params   (card16-get 94))
           (pad-cur (- 4 (mod cur-name 4)))
           (cur-name-start (index+ pad-pend-start (index* 4 pend-num-params)))
           (cur-param-start (index+ cur-name-start cur-name pad-cur))
           )
      (declare (type card16 pend-name cur-name))
      (values
       (rr-transform-get 8)
       ;(sequence-get :result-type result-type :length 9 :index 8)
       (card8-get 44)
       (sequence-get :result-type result-type :length 9 :index 48)
       (string-get pend-name 96)
       (sequence-get :result-type result-type :length pend-num-params :index pad-pend-start)
       (string-get cur-name cur-name-start)
       (sequence-get :result-type result-type :length cur-num-params :index cur-param-start )
  ))))


;; (defun rr-get-panning (display crtc)
;;   ""
;;   (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (8 16 32))
;;                               ((data +rr-getpanning+)
;;                                (card32 crtc))
;;     (values
;;      (member8-vector-get 1 +rr-config-status+)
;;      (card32-get 8) ; timestamp
;;      (rr-panning-get 12)
;;                                      ;(sequence-get :length 8 :format card16 :index 12 :result-type result-type)
;;      ;(sequence-get :length 4 :format int16 :index 28 :result-type result-type)
;;      )))



;; (defun rr-set-panning (display crtc timestamp rr-panning)
;;   ""
;;   (declare (type rr-panning rr-panning))
;;   (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (8 16 32))
;;                               ((data +rr-setpanning+)
;;                                (card32 crtc)
;;                                (card32 timestamp)
;;                               ;				  (progn ()
;;                                (rr-panning rr-panning))

;;     (values
;;      (member8-vector-get 1 +rr-config-status+)
;;                                      ;  (card32-get 8) ; new timestamp
;;      )))

(defun rr-set-output-primary (window output)
  ""
  (let ((display (window-display window)))
    (with-buffer-request (display (randr-opcode display))
      (data +rr-setoutputprimary+)
      (window window)
      (card32 output))))

(defun rr-get-output-primary (window)
  ""
  (let ((display (window-display window)))
    (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (8 16 32))
                                   ((data +rr-getoutputprimary+)
                                    (window window))
      (values
       (card32-get 8)
       ))))




(defun rr-get-providers (window)
""
  (let ((display (window-display window)))
      (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (8 16 32))
                                     ((data +rr-getproviders+)
                                      (window window))
        (values
         (card32-get 8)  ; timestamp
         (card16-get 12) ; num providers
        ; (string-get 1256 14) ; checking if this is supposed to return anything besides just num
        ; (sequence-get :index 46 :length (card16-get 12) :format card8 :result-type 'list )
         ))))

(defun rr-get-provider-info (display provider config-timestamp)
""
  (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (8 16 32))
                                 ((data +rr-getproviderinfo+)
                                  (card32 provider)
                                  (card32 config-timestamp))
    (values
     (card32-get 8)  ;timestamp
     (card32-get 12) ; capabilities
     (card16-get 16) ; num crtcs
     (card16-get 18) ; num outputs
     (card16-get 20) ; num associated providers
     (string-get (card16-get 22) 56))))

(defun rr-set-provider-output-source (display provider source-provider config-timestamp)
  (with-buffer-request (display (randr-opcode display))
    (data +rr-setprovideroutputsource+)
    (card32 provider)
    (card32 source-provider)
    (card32 config-timestamp)))

(defun rr-set-provider-offload-sink (display provider sink-provider config-timestamp)
  (with-buffer-request (display (randr-opcode display))
    (data +rr-setprovideroffloadsink+)
    (card32 provider)
    (card32 sink-provider)
    (card32 config-timestamp)))



(defun rr-list-provider-properties (display provider)
""
  (with-buffer-request-and-reply (display (randr-opcode display) nil :sizes (8 16 32))
                                 ((data +rr-listproviderproperties+)
                                  (card32 provider))
    (values
     (card32-get 4)
     (card16-get 8))))


;; (defun rr-query-provider-property (display provider atom)
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
