;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XLIB; -*-
;;; --------------------------------------------------------------------------
;;;    Title: The X Keyboard Extension
;;;    Created: 2018-05-16
;;;    Authors: David Bjergaard <dbjergaard@gmail.com>, Michael
;;;    Filonenko <filonenko.mikhail at gmail.com>, Eric Wolf <eric at boese-wolf.eu>"
;;; ---------------------------------------------------------------------------
;;;
;;; (c) copyright 2018 by David Bjergaard
;;; (c) copyright 2014 by Mikhail Filonenko
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

;;; NOTE: we need to watch maximum request sizes and somehow work
;;; around them. Sometimes e.g. in AddGlyphs this is not possible,
;;; which is a design failure.

(in-package :xlib)

(pushnew :clx-ext-xkeyboard *features*)

(define-extension "XKEYBOARD"
    :events ()
    :errors (xkeyboard-error))

(export '(+use-core-kbd+
	  +use-core-ptr+

	  +xkbkeysymdb+

	  xkeyboard-error
	  make-device-state
	  device-state-p
	  copy-device-state
	  device-state-device-id
	  device-state-mods
	  device-state-base-mods
	  device-state-latched-mods
	  device-state-locked-mods
	  device-state-group
	  device-state-locked-group
	  device-state-base-group
	  device-state-latched-group
	  device-state-compat-state
	  device-state-grab-mods
	  device-state-compat-grab-mods
	  device-state-lookup-mods
	  device-state-compat-lookup-mods
	  device-state-ptr-btn-state

	  get-state
	  latch-lock-state
	  lock-group

	  get-map
	  transform-xkb-keymap-to-client-mapping
	  keyevent->keysym
	  xkb/keysym->character

	  client-mapping
	  process-leftover-modifiers
	  client-mapping-symmaps
	  effective-group
	  corestate->group
	  client-keysymmap-num-groups
	  client-keysymmap-groups-wrap
	  client-keysymmap-redirect-group
	  client-keysymmap-keytypes
	  shiftlevel/leftover-modifiers
	  corestate->mask
	  ))

(define-condition xkeyboard-error (request-error) ())

(define-error xkeyboard-error decode-core-error)

(defmacro xkeyboard-opcode (display)
  `(extension-opcode ,display "XKEYBOARD"))

(defun enable-xkeyboard (display &optional (major +major-version+) (minor +minor-version+))
  (declare (type display display))
  (with-buffer-request-and-reply (display (xkeyboard-opcode display) nil)
    ((data +use-extension+)
     (card16 major)
     (card16 minor))
    (values (boolean-get 1)
	    (card16-get 8))))

(export 'enable-xkeyboard)

;;;;;;;;;;;;;;;
;; Requests  ;;
;;;;;;;;;;;;;;;

;; Bell request
(defun xkb-bell (display &key (device +use-core-kbd+)
			   bell-class
			   id
			   percent
			   force-sound
			   event-only
			   pitch
			   duration
			   name
			   window)
  (declare (type display display)
	   (type devicespec device)
	   (type window window))
  (with-buffer-request (display (xkeyboard-opcode display))
    (data +bell+)
    (devicespec device)
    (bellclassspec bell-class)
    (idspec id)
    (card8 percent)
    (boolean force-sound)
    (boolean event-only)
    (pad8 0)
    (int16 pitch)
    (int16 duration)
    (pad16 0)
    (keyword name)
    (window window)))



(defun get-state (display &optional (device +use-core-kbd+))
  (declare (type display display))
  (with-buffer-request-and-reply (display (xkeyboard-opcode display) nil)
    ((data +get-state+)
     (devicespec device)
     (pad16 0))
    (make-device-state
     :device-id (card8-get 1)
     :mods (keymask-get 8)
     :base-mods (keymask-get 9)
     :latched-mods (keymask-get 10)
     :locked-mods (keymask-get 11)
     :group (group-get 12)
     :locked-group (group-get 13)
     :base-group (int16-get 14)
     :latched-group (int16-get 16)
     :compat-state (keymask-get 18)
     :lookup-mods (keymask-get 19)
     :compat-lookup-mods (keymask-get 20)
     :ptr-btn-state (butmask-get 22))))

;; LatchLockState
(defun latch-lock-state (display &key (device +use-core-kbd+)
				   affect-mod-locks
				   mod-locks
				   lock-group
				   group-lock
				   affect-mod-latches
				   mod-latches
				   latch-group
				   group-latch)
  (declare (type display display))
  (with-buffer-request (display (xkeyboard-opcode display))
    (data +latch-lock-state+)
    (devicespec device)
    (keymask affect-mod-locks)
    (keymask mod-locks)
    (boolean lock-group)
    (group group-lock)
    (keymask affect-mod-latches)
    (keymask mod-latches)
    (pad8 0)
    (boolean latch-group)
    (int16 group-latch)))

(defun lock-group (display &key (device +use-core-kbd+) group)
  (latch-lock-state display :device device
		    :affect-mod-locks 0
		    :mod-locks 0
		    :lock-group t
		    :group-lock group
		    :affect-mod-latches 0
		    :mod-latches 0
		    :latch-group nil
		    :group-latch 0))
;; https://www.x.org/releases/current/doc/kbproto/xkbproto.html#Querying_and_Changing_Keyboard_Controls
;; (defun get-controls (device-spec)
;;   )


;; https://www.x.org/releases/current/doc/kbproto/xkbproto.html#Querying_and_Changing_Keyboard_Controls
;; (defun set-controls (device-spec
;; 		     affect-internal-real-mods
;; 		     internal-real-mods
;; 		     affect-ignore-lock-real-mods
;; 		     ignore-lock-real-mods
;; 		     affect-internal-virtual-mods
;; 		     internal-virtual-mods
;; 		     affect-ignore-lock-virtual-mods
;; 		     ignore-lock-virtual-mods
;; 		     mouse-keys-dflt-btn
;; 		     groups-wrap
;; 		     access-x-options
;; 		     affect-enabled-controls
;; 		     enabled-controls
;; 		     change-controls
;; 		     repeat-delay
;; 		     repeat-interval
;; 		     slow-keys-delay
;; 		     debounce-delay
;; 		     mouse-keys-delay
;; 		     mouse-keys-interval
;; 		     mouse-keys-time-to-max
;; 		     mouse-keys-max-speed
;; 		     mouse-keys-curve
;; 		     access-x-timeout
;; 		     access-x-timeout-mask
;; 		     access-x-timeout-values
;; 		     access-x-timeout-options-mask
;; 		     access-x-timeout-options-values
;; 		     '(per-key-repeat 32)))

;; https://www.x.org/releases/current/doc/kbproto/xkbproto.html#Querying_and_Changing_the_Keyboard_Mapping
;; (defun get-map (device-spec
;; 		full
;; 		partial
;; 		first-type
;; 		n-types
;; 		first-key-sym
;; 		n-key-syms
;; 		first-key-action
;; 		n-key-actions
;; 		first-key-behavior
;; 		n-key-behaviors
;; 		virtual-mods
;; 		first-key-explicit
;; 		n-key-explicit
;; 		first-mod-map-key
;; 		n-mod-map-keys
;; 		first-v-mod-map-key
;; 		n-v-mod-map-keys))

;; (defun set-map (device-spec
;; 		present
;; 		flags
;; 		min-key-code
;; 		max-key-code
;; 		first-type
;; 		n-types
;; 		first-key-sym
;; 		n-key-syms
;; 		total-syms
;; 		first-key-action
;; 		n-key-actions
;; 		total-actions
;; 		first-key-behavior
;; 		n-key-behaviors
;; 		total-key-behaviors
;; 		first-key-explicit
;; 		n-key-explicit
;; 		total-key-explicit
;; 		first-mod-map-key
;; 		n-mod-map-keys
;; 		total-mod-map-keys
;; 		first-v-mod-map-key
;; 		n-v-mod-map-keys
;; 		total-v-mod-map-keys
;; 		virtual-mods))

;; (defun get-compat-map (device-spec
;; 		       groups
;; 		       get-all-si
;; 		       first-si
;; 		       n-si))
;; (defun set-compat-map (device-spec
;; 		       recompute-actions
;; 		       truncate-si
;; 		       groups
;; 		       first-si
;; 		       n-si))

;; (defun get-indicator-state (device-spec))

;; (defun get-indicator-map (device-spec which))

;; (defun set-indicator-map (device-spec which maps))

;; (defun get-named-indicator (device-spec led-class
;; led-id indicator))

;; (defun set-named-indicator (device-spec
;; 			    led-class
;; 			    led-id
;; 			    indicator
;; 			    set-state
;; 			    on
;; 			    set-map
;; 			    create-map
;; 			    map-flags
;; 			    map-which-groups
;; 			    map-groups
;; 			    map-which-mods
;; 			    map-real-mods
;; 			    map-vmods
;; 			    map-ctrls))

;; (defun get-names (device-spec which))

;; (defun set-names (device-spec
;; 		  virtual-mods
;; 		  which
;; 		  first-type
;; 		  n-types
;; 		  first-kt-levelt
;; 		  n-kt-levels
;; 		  indicators
;; 		  group-names
;; 		  n-radio-groups
;; 		  first-key
;; 		  n-keys
;; 		  n-key-aliases
;; 		  total-kt-level-names
;; 		  values))

;; (defun get-geometry (device-spec name))

;; (defun set-geometry (device-spec
;; 		     n-shapes
;; 		     n-sections
;; 		     name
;; 		     width-mm
;; 		     height-mm
;; 		     n-properties
;; 		     n-colors
;; 		     n-doodads
;; 		     n-key-aliases
;; 		     base-color-ndx
;; 		     label-color-ndx
;; 		     label-font
;; 		     properties		;; list
;; 		     colors		;; list
;; 		     shapes		;; list
;; 		     sections		;; list
;; 		     doodads		;; list
;; 		     key-aliases))	;; list

;; (defun per-client-flags (device-spec
;; 			 change
;; 			 value
;; 			 ctrls-to-change
;; 			 auto-ctrls
;; 			 auto-ctrls-values))

;; (defun list-components (device-spec
;; 			max-names
;; 			keymaps-spec-len
;; 			keymaps-spec	;list
;; 			keycodes-spec-len
;; 			keycodes-spec	;list
;; 			types-spec-len
;; 			types-spec	;list
;; 			compat-map-spec-len
;; 			compat-map-spec ;list
;; 			symbols-spec-len
;; 			symbols-spec	;list
;; 			geometry-spec-len
;; 			geometry-spec)) ;list

;; (defun get-kbd-by-name (device-spec
;; 			need
;; 			want
;; 			load
;; 			keymaps-spec-len
;; 			keymaps-spec
;; 			keycodes-spec-len
;; 			keycodes-spec
;; 			types-spec-len
;; 			types-spec
;; 			compat-map-spec-len
;; 			compat-map-spec
;; 			symbols-spec-len
;; 			symbols-spec
;; 			geometry-spec-len
;; 			geometry-spec))

;; (defun get-device-info  (device-spec
;; 			 wanted
;; 			 all-buttons
;; 			 first-button
;; 			 n-buttons
;; 			 led-class
;; 			 led-id))

;; (defun set-device-info (device-spec
;; 			first-btn
;; 			n-btns
;; 			change
;; 			n-device-led-f-bs
;; 			btn-actions
;; 			leds))

;; (defun set-debugging-flags (msg-length
;; 			    affect-flags
;; 			    flags
;; 			    affect-ctrls
;; 			    ctrls
;; 			    message))

;;;;;;;;;;;;
;; Events ;;
;;;;;;;;;;;;




(defun contained-in-mask (const mask)
  (plusp (logand const mask)))

(defmacro moddef-get (indexsym)
  `(prog1 (make-moddef
	   :mask (keymask-get ,indexsym)
	   :real-mods (keymask-get (index-incf ,indexsym 1))
	   :vmods (vmodmask-get (index-incf ,indexsym 1)))
     (index-incf ,indexsym 2)))

(defmacro modmap-get (indexsym)
  `(prog1 (make-modmap
	   :keycode (keycode-get ,indexsym)
	   :mods (keymask-get (index-incf ,indexsym 1)))
     (index-incf ,indexsym 1)))

(defmacro vmodmap-get (indexsym)
  `(prog1 (make-vmodmap
	   :keycode (keycode-get ,indexsym)
	   :vmods   (vmodmask-get (index-incf ,indexsym 2)))
     (index-incf ,indexsym 2)))

(defmacro behaviormap-get (indexsym)
  `(prog1 (make-behaviormap
	   :keycode (keycode-get ,indexsym)
	   :behavior (behavior-get (index-incf ,indexsym 1)))
     (index-incf ,indexsym 3)))

(defmacro explicitmap-get (indexsym)
  `(prog1 (make-explicitmap
	   :keycode (keycode-get ,indexsym)
	   :explicit (explicit-get (index-incf ,indexsym 1)))
     (index-incf ,indexsym 1)))

(defmacro keytype-mapentry-get (indexsym)
  `(prog1 (make-keytype-mapentry
	   :active (boolean-get ,indexsym)
	   :mask (keymask-get (index-incf ,indexsym 1))
	   :level (card8-get (index-incf ,indexsym 1))
	   :mods (keymask-get (index-incf ,indexsym 1))
	   :vmods (vmodmask-get (index-incf ,indexsym 1)))
     (index-incf ,indexsym 4)))

(defmacro keytype-get (indexsym)
  (let ((n-map-entries-sym (gensym "n-map-entries"))
	(preserve-p-sym (gensym "preserve-p")))
    `(let (,n-map-entries-sym ,preserve-p-sym)
       (make-keytype
	:mask (keymask-get ,indexsym)
	:mods (keymask-get (index-incf ,indexsym 1))
	:vmods (vmodmask-get (index-incf ,indexsym 1))
	:levels (card8-get (index-incf ,indexsym 2))
	:map-entries (setf ,n-map-entries-sym (card8-get (index-incf ,indexsym 1)))
	:preserve-p (setf ,preserve-p-sym (boolean-get (index-incf ,indexsym 1)))
	:map (progn (index-incf ,indexsym 2)
		    (loop repeat ,n-map-entries-sym
		       collect (keytype-mapentry-get ,indexsym)))
	:preserve (when ,preserve-p-sym
		    (loop repeat ,n-map-entries-sym
		       collect (moddef-get ,indexsym)))))))

(defmacro keysymmap-get (indexsym)
  (let ((n-sym (gensym)))
    `(let (,n-sym)
       (make-keysymmap
	:kt-index (coerce (loop repeat 4
			     collect (prog1 (card8-get ,indexsym)
				       (index-incf ,indexsym 1)))
			  '(vector card8 4))
	:group (prog1 (card8-get ,indexsym)
		 (index-incf ,indexsym 1))
	:width (prog1 (card8-get ,indexsym)
		 (index-incf ,indexsym 1))
	:n (prog1 (setf ,n-sym (card16-get ,indexsym)) ;depending on
					;side effects
					;in this way
					;makes me
					;feeling dirty
	     (index-incf ,indexsym 2))
	:keysyms (loop repeat ,n-sym
		    collect (prog1 (card32-get ,indexsym)
			      (index-incf ,indexsym 4)))))))

(defmacro xkb-keymap-get (indexsym)
  (let ((minKeycodeSym		(gensym))
	(maxKeycodeSym		(gensym))
	(mappartMaskSym		(gensym))
	(firstTypeSym		(gensym))
	(nTypesSym		(gensym))
	(totalTypesSym		(gensym))
	(firstKeysymSym		(gensym))
	(totalSymsSym		(gensym))
	(nKeysymsSym		(gensym))
	(firstKeyactionSym	(gensym))
	(totalActionsSym	(gensym))
	(nKeyactionsSym		(gensym))
	(firstKeybehaviorSym	(gensym))
	(nKeybehaviorSym	(gensym))
	(totalKeybehaviorsSym	(gensym))
	(firstKeyexplicitSym	(gensym))
	(nKeyexplicitSym	(gensym))
	(totalKeyexplicitSym	(gensym))
	(firstModMapKeySym	(gensym))
	(nModMapKeySym		(gensym))
	(totalModMapKeySym	(gensym))
	(firstVModMapKeySym	(gensym))
	(nVModMapKeySym		(gensym))
	(totalVModMapKeySym	(gensym))
	(virtualModsSym		(gensym)))
    `(let ((,minKeycodeSym		(card8-get ,indexsym))
	   (,maxKeycodeSym		(card8-get (index-incf ,indexsym 1)))
	   (,mappartMaskSym		(card16-get (index-incf ,indexsym 1)))
	   (,firstTypeSym		(card8-get (index-incf ,indexsym 2)))
	   (,nTypesSym			(card8-get (index-incf ,indexsym 1)))
	   (,totalTypesSym		(card8-get (index-incf ,indexsym 1)))
	   (,firstKeysymSym		(card8-get (index-incf ,indexsym 1)))
	   (,totalSymsSym		(card16-get (index-incf ,indexsym 1)))
	   (,nKeysymsSym		(card8-get (index-incf ,indexsym 2)))
	   (,firstKeyactionSym		(card8-get (index-incf ,indexsym 1)))
	   (,totalActionsSym		(card16-get (index-incf ,indexsym 1)))
	   (,nKeyactionsSym		(card8-get (index-incf ,indexsym 2)))
	   (,firstKeybehaviorSym	(card8-get (index-incf ,indexsym 1)))
	   (,nKeybehaviorSym		(card8-get (index-incf ,indexsym 1)))
	   (,totalKeybehaviorsSym	(card8-get (index-incf ,indexsym 1)))
	   (,firstKeyexplicitSym	(card8-get (index-incf ,indexsym 1)))
	   (,nKeyexplicitSym		(card8-get (index-incf ,indexsym 1)))
	   (,totalKeyexplicitSym	(card8-get (index-incf ,indexsym 1)))
	   (,firstModMapKeySym		(card8-get (index-incf ,indexsym 1)))
	   (,nModMapKeySym		(card8-get (index-incf ,indexsym 1)))
	   (,totalModMapKeySym		(card8-get (index-incf ,indexsym 1)))
	   (,firstVModMapKeySym		(card8-get (index-incf ,indexsym 1)))
	   (,nVModMapKeySym		(card8-get (index-incf ,indexsym 1)))
	   (,totalVModMapKeySym		(card8-get (index-incf ,indexsym 1)))
	   (,virtualModsSym		(card16-get (index-incf ,indexsym 2))))
       (declare (ignore ,nVModMapKeySym))
       (index-incf ,indexsym 2)
       (make-xkb-keymap
	:min-keycode ,minKeycodeSym
	:max-keycode ,maxKeycodeSym
	:mappart-mask ,mappartMaskSym
	:types (when (contained-in-mask +KEYTYPES+ ,mappartMaskSym)
		 (make-xkb-keymap-part
		  :first ,firstTypeSym
		  :n ,nTypesSym
		  :total ,totalTypesSym
		  :list (loop repeat ,nTypesSym
			   collect (keytype-get ,indexsym))))
	:syms (when (contained-in-mask +KEYSYMS+ ,mappartMaskSym)
		(make-xkb-keymap-part
		 :first ,firstKeysymSym
		 :n ,nKeysymsSym
		 :total ,totalSymsSym
		 :list (loop repeat ,nKeysymsSym
			  collect (keysymmap-get ,indexsym))))
	:actions (when (contained-in-mask +KEYACTIONS+ ,mappartMaskSym)
		   (make-xkb-keymap-part
		    :first ,firstKeyactionSym
		    :n ,nKeyactionsSym
		    :total ,totalActionsSym
		    :list (cons
			   (loop repeat ,nKeyactionsSym
			      collect (prog1 (card8-get ,indexsym)
					(index-incf ,indexsym 1))
			      finally (setf ,indexsym (lround ,indexsym)))
			   (loop repeat ,totalActionsSym
			      collect (loop repeat 8
					 collect (prog1 (card8-get ,indexsym)
						   (index-incf ,indexsym 1)))))))
	:behaviors (when (contained-in-mask +KEYBEHAVIORS+ ,mappartMaskSym)
		     (make-xkb-keymap-part
		      :first ,firstKeybehaviorSym
		      :n ,nKeybehaviorSym
		      :total ,totalKeybehaviorsSym
		      :list (loop repeat ,totalKeybehaviorsSym
			       collect (behaviormap-get ,indexsym))))
	:virtualmods (when (contained-in-mask +VIRTUALMODS+ ,mappartMaskSym)
		       (make-virtual-modifier-bindings
			:virtual-modifiers ,virtualModsSym
			:real-modifiers-per-virtual-modifier
			(loop for i from 0 upto 15 when (= (ldb (byte 1 i) ,virtualModsSym) 1)
			   collect (prog1 (keymask-get ,indexsym)
				     (index-incf ,indexsym 1)))))
	:explicits (when (contained-in-mask +EXPLICITCOMPONENTS+ ,mappartMaskSYm)
		     (prog1 (make-xkb-keymap-part
			     :first ,firstKeyexplicitSym
			     :n ,nKeyexplicitSym
			     :total ,totalKeyexplicitSym
			     :list (loop repeat ,totalKeyexplicitSym
				      collect (explicitmap-get ,indexsym)))
		       (setf ,indexsym (lround ,indexsym))))
	:modmapkeys (when (contained-in-mask +MODIFIERMAP+ ,mappartMaskSym)
		      (prog1 (make-xkb-keymap-part
			      :first ,firstModMapKeySym
			      :n ,nModMapKeySym
			      :total ,totalModMapKeySym
			      :list (loop repeat ,totalModMapKeySym
				       collect (modmap-get ,indexsym)))
			(setf ,indexsym (lround ,indexsym))))
	:vmodmapkeys (when (contained-in-mask +VIRTUALMODMAP+ ,mappartMaskSym)
		       (make-xkb-keymap-part
			:first ,firstVModMapKeySym
			:n 0
			:total ,totalVModMapKeySym
			:list (loop repeat ,totalVModMapKeySym
				 collect (vmodmap-get ,indexsym))))))))

(defun get-map (display devicespec mappart-mask-full)
  (with-buffer-request-and-reply (display (xkeyboard-opcode display) nil)
    ((data +get-map+)
     (devicespec devicespec)
     (mappart mappart-mask-full)	;full
     (mappart 0)			;partial
     (card8 0)			;firstType
     (card8 0)			;nTypes
     (keycode 0)			;firstKeySym
     (card8 0)			;nKeySyms
     (keycode 0)			;firstKeyAction
     (card8 0)			;nKeyActions
     (keycode 0)			;firstKeyBehavior
     (card8 0)			;nKeyBehavior
     (vmodmask 0)			;virtualMods
     (keycode 0)			;firstKeyExplicit
     (card8 0)			;nKeyExplicit
     (keycode 0)			;firstModMapKey
     (card8 0)			;nModMapKeys
     (keycode 0)			;firstVModMapKey
     (card8 0)			;nVModMapKeys
     (pad16 nil))
    (let ((index 10))
      (xkb-keymap-get index))))

(defun construct-keytype-map-entry (entry preserve)
  (make-client-keytype-mapentry
   :preserve preserve
   :active (keytype-mapentry-active entry)
   :level (keytype-mapentry-level entry)))

(defun construct-type (type)
  (let ((result (make-client-keytype
		 :mask (keytype-mask type))))
    (loop for i from 0 below (keytype-map-entries type)
       for mapentry in (keytype-map type)
       for preserve = (if (keytype-preserve-p type)
			  (moddef-mask (nth i (keytype-preserve type))) 0)
       do (push (construct-keytype-map-entry mapentry preserve)
		(gethash (keytype-mapentry-mask mapentry)
			 (client-keytype-map result))))
    result))

(defun construct-type-vector (typevector types)
  (loop for i from 0
     repeat (xkb-keymap-part-n types)
     do (setf (svref typevector (+ i (xkb-keymap-part-first types)))
	      (construct-type (nth i (xkb-keymap-part-list types))))))

(defun symbolize-groups-wrap (groups-wrap-number)
  (or (case groups-wrap-number
	(0 :wrap)
	(2 :clamp)
	(4 :redirect))
      (error "wrong-value-in-groups-wrap")))

(defun construct-symmap (symmap typevector)
  (let ((groupinfo (keysymmap-group symmap)))
    (make-client-keysymmap
     :num-groups (ldb (byte 4 0) groupinfo)
     :groups-wrap (symbolize-groups-wrap (ldb (byte 2 6) groupinfo))
     :redirect-group (ldb (byte 2 4) groupinfo)
     :keysyms (coerce (keysymmap-keysyms symmap) 'vector)
     :width (keysymmap-width symmap)
     :keytypes (coerce (loop for i from 0 below 4
			  collect (svref
				   typevector
					;svref better? but sbcl complains about types.
				   (aref (keysymmap-kt-index symmap) i)))
		       'vector))))

(defun transform-xkb-keymap-to-client-mapping (info)
  (let*
      ((types (xkb-keymap-types info))
       (symmaps (xkb-keymap-syms info))
       (symmaps-array (make-array (+ (xkb-keymap-part-first symmaps)
				     (xkb-keymap-part-n symmaps))))
       (typevector (make-array (+ (xkb-keymap-part-first types)
				  (xkb-keymap-part-n types)))))
    (construct-type-vector typevector types)
    (loop for i from 0
       repeat (xkb-keymap-part-n symmaps)
       do (setf (svref symmaps-array (+ (xkb-keymap-part-first symmaps) i))
		(construct-symmap (nth i (xkb-keymap-part-list symmaps))
				  typevector)))
    (make-client-mapping
     :symmaps symmaps-array)))

(defun corestate->group (corestate)
  (ldb (byte 2 13) corestate))

(defun corestate->mask (corestate)
  (ldb (byte 8 0) corestate))

(defun sanitize-redirect-group (num-groups redirect-group)
  (if (>= redirect-group num-groups)
      0 redirect-group))

(defun effective-group (event-group num-groups groups-wrap redirect-group)
  (cond
    ((= num-groups 0) 0)
    ((<= 0 event-group (1- num-groups)) event-group)
    (t
     (case groups-wrap
       (:wrap (mod event-group num-groups))
       (:clamp (1- num-groups))
       (:redirect (sanitize-redirect-group num-groups redirect-group))))))
(defun calculate-leftover-modifiers (mask entry keytype-mask)
  (logior (client-keytype-mapentry-preserve entry)
	  (logand mask (lognot keytype-mask))))

(defun shiftlevel/leftover-modifiers (keytype mask)
  (let ((keytype-mask (client-keytype-mask keytype)))
    (loop for entry in (gethash (logand mask keytype-mask)
				(client-keytype-map keytype))
       when (client-keytype-mapentry-active entry)
       return (values (client-keytype-mapentry-level entry)
		      (calculate-leftover-modifiers mask entry keytype-mask))
       finally (return (values 0 (logand mask (lognot keytype-mask)))))))

(defun keyevent->keysym (mapping keycode corestate)
  (let* ((group (corestate->group corestate))
	 (mask (corestate->mask corestate))
	 (symmap (svref (client-mapping-symmaps mapping) keycode))
	 (effective-group (effective-group group
					   (client-keysymmap-num-groups symmap)
					   (client-keysymmap-groups-wrap symmap)
					   (client-keysymmap-redirect-group symmap)))
	 (keytype (svref (client-keysymmap-keytypes symmap)
			 effective-group)))
    (if (= (length (client-keysymmap-keysyms symmap)) 0)
	nil
	(multiple-value-bind (shiftlevel leftover-modifiers)
	    (shiftlevel/leftover-modifiers keytype mask)
	  (values
	   (svref (client-keysymmap-keysyms symmap)
		  (+ (* effective-group (client-keysymmap-width symmap)) shiftlevel))
	   leftover-modifiers)))))

(defun xkb/keysym->character (keysym keysymdb) ;keysymdb would be normally +xkbkeysymdb+
  (cond
    ((or (<= #x0020 keysym #x007E) (<= #x00A0 keysym #x00FF))
     (code-char keysym))
    ((<= #x01000100 keysym #x0110FFFF)
     (code-char (- keysym #x01000000)))
    (t
     (let ((codepoint (cadr (assoc :unicode (gethash keysym keysymdb)))))
       (if codepoint (code-char codepoint))))))

(defun upcase-keysym (keysym)
  ;; For now, we only upcase a-z for the time being,
  ;; To improve this, we need to implement the tables here:
  ;; https://www.x.org/releases/current/doc/kbproto/xkbproto.html#Locale_Sensitive_Capitalization
  (cond
    ((<= #x0061 keysym #x007A) (- keysym #x0020))
    (t keysym)))

(defun control-character (keysym keysymdb)
  (cond
    ((<= #x0040 keysym #x005F) (code-char (- keysym #x0040))) ;@A-Z[\]^_
    ((<= #x0061 keysym #x007A) (code-char (- keysym #x0060))) ;a-z
    (t (xkb/keysym->character keysym keysymdb))))

(defun process-leftover-modifiers (keysym leftover-modifiers keysymdb)
  (cond
    ((contained-in-mask +control+ leftover-modifiers)
     (values keysym (string (control-character keysym keysymdb))))
    ((contained-in-mask +lock+ leftover-modifiers)
     (values (upcase-keysym keysym)
	     (string (xkb/keysym->character (upcase-keysym keysym) keysymdb))))
    (t
     (values keysym
	     (string (xkb/keysym->character keysym keysymdb))))))
