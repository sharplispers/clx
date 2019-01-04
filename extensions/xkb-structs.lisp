(in-package :xlib)

(defstruct moddef
  (mask 0 :type keymask)
  (real-mods 0 :type keymask)
  (vmods 0 :type vmodmask))

;; TODO
(defstruct action
  (type 0 :type card8))

(defstruct device-state
  (device-id 0 :type card8)
  (mods 0 :type keymask)
  (base-mods 0 :type keymask)
  (latched-mods 0 :type keymask)
  (locked-mods 0 :type keymask)
  (group 0 :type group)
  (locked-group 0 :type group)
  (base-group 0 :type int16)
  (latched-group 0 :type int16)
  (compat-state 0 :type keymask)
  (grab-mods 0 :type keymask)
  (compat-grab-mods 0 :type keymask)
  (lookup-mods 0 :type keymask)
  (compat-lookup-mods 0 :type keymask)
  (ptr-btn-state 0 :type butmask))

(defstruct modmap
  (keycode 0 :type keycode)
  (mods 0 :type keymask))

(defstruct vmodmap
  (keycode 0 :type keycode)
  (vmods 0 :type vmodmask))

(defstruct behaviormap ;defstruct introduces a new type, so we have to
					;distinguish it from the existing behavior.
  (keycode 0 :type keycode)
  (behavior 0 :type behavior))

(defstruct explicitmap ;same as behavior/behaviormap
  (keycode 0 :type keycode)
  (explicit 0 :type explicit))

(defstruct keytype-mapentry
  (active nil :type (member nil t))
  (mask 0 :type keymask)
  (level 0 :type card8)
  (mods 0 :type keymask)
  (vmods 0 :type vmodmask))

(defstruct keytype
  (mask 0 :type keymask)
  (mods 0 :type keymask)
  (vmods 0 :type vmodmask)
  (levels 0 :type card8)
  (map-entries 0 :type card8)
  (preserve-p nil :type (member nil t))
  (map nil :type list)
  (preserve nil :type list))

(defstruct keysymmap
  (kt-index (make-array 4 :element-type 'card8
			:initial-element 0)
	    :type (vector card8 4))
  (group 0 :type card8)
  (width 0 :type card8)
  (n 0 :type card16)
  (keysyms nil :type list))

(defstruct virtual-modifier-bindings
  (virtual-modifiers 0 :type vmodmask)
  (real-modifiers-per-virtual-modifier nil :type list))

(defstruct xkb-keymap-part
  (first 0 :type (or card8 keycode)) ;doesn't matter as card8 = keycode, but logically
  (n 0 :type card8)
  (total 0 :type card16)
  (list nil :type list))

(defstruct xkb-keymap
  (min-keycode 0 :type keycode)
  (max-keycode 0 :type keycode)
  (mappart-mask 0 :type mappart)
  (types nil :type (or null xkb-keymap-part))
  (syms nil :type (or null xkb-keymap-part))
  (actions nil :type (or null xkb-keymap-part))
  (behaviors nil :type (or null xkb-keymap-part))
  (explicits nil :type (or null xkb-keymap-part))
  (modmapkeys nil :type (or null xkb-keymap-part))
  (vmodmapkeys nil :type (or null xkb-keymap-part))
  (virtualmods nil :type (or null virtual-modifier-bindings)))

(defstruct client-keytype-mapentry
  (preserve 0 :type keymask)
  (active nil :type (member nil t))
  (level 0 :type card8))

(defstruct client-keytype
  (mask 0 :type keymask)
  (map (make-hash-table) :type hash-table))

(defstruct client-keysymmap
  (num-groups 0 :type card8)
  (groups-wrap nil :type symbol)
  (redirect-group 0 :type card8)
  (keysyms (make-array 0) :type vector)
  (width 0 :type card8)
  (keytypes (make-array 0) :type vector))

(defstruct client-mapping
  (symmaps (make-array 0) :type vector))

