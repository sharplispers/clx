;;; This file contains various graphics hacks written and ported over the
;;; years by various and numerous persons.
;;;
;;; This file should be portable to any valid Common Lisp with CLX -- DEC 88.
;;;
;;; CMUCL MP support by Douglas Crosher 1998.
;;; Enhancements including the CLX menu, rewrite of the greynetic
;;; demo, and other fixes by Fred Gilham 1998.
;;;
;;; Backported some changes found in CMUCL repository -- jd 2018-12-29.

(defpackage #:xlib-demo/demos
  (:use :common-lisp)
  (:export #:demo))

(in-package :xlib-demo/demos)


;;;; Graphic demos wrapper macro.

;;; This wrapper macro should be reconsidered with respect to its property
;;; list usage.  Possibly a demo structure should be used with *demos*
;;; pointing to these instead of function names.  Also, something should
;;; be done about a title window that displays the name of the demo while
;;; it is running.

(defparameter *demos* nil)
(defparameter *delay* 0.5)

(defvar *display* nil)
(defvar *screen* nil)
(defvar *root* nil)
(defvar *black-pixel* nil)
(defvar *white-pixel* nil)
(defvar *window* nil)

(defmacro defdemo (fun-name demo-name args x y width height doc &rest forms)
  `(progn
     (defun ,fun-name ,args
       ,doc
       (let* ((*display* (or *display*
                             (xlib:open-default-display)
                             (xlib:open-display (machine-instance))))
              (*screen* (xlib:display-default-screen *display*))
              (*root* (xlib:screen-root *screen*))
              (*black-pixel* (xlib:screen-black-pixel *screen*))
              (*white-pixel* (xlib:screen-white-pixel *screen*))
              (*window* (xlib:create-window :parent *root*
                                            :x ,x :y ,y
                                            :event-mask '(:visibility-change)
                                            :width ,width :height ,height
                                            :background *white-pixel*
                                            :border *black-pixel*
                                            :border-width 2
                                            :override-redirect :off)))
         (xlib:set-wm-properties *window*
				 :name ,demo-name
				 :icon-name ,demo-name
				 :resource-name ,demo-name
				 :x ,x :y ,y :width ,width :height ,height
				 :user-specified-position-p t
				 :user-specified-size-p t
				 :min-width ,width :min-height ,height
				 :width-inc nil :height-inc nil)
	 (xlib:map-window *window*)
	 ;; Wait until we get mapped before doing anything.
         (xlib:display-finish-output *display*)
	 (unwind-protect (progn ,@forms)
           (xlib:display-finish-output *display*)
	   (xlib:unmap-window *window*))))
    (setf (get ',fun-name 'demo-name) ',demo-name)
    (setf (get ',fun-name 'demo-doc) ',doc)
    (pushnew ',fun-name *demos*)
    ',fun-name))


;;; DEMO

(defvar *name-to-function* (make-hash-table :test #'eq))
(defvar *keyword-package* (find-package "KEYWORD"))
(defvar *demo-names* nil)

(defun demo ()
  (let ((*demo-names* '("Quit")))
    (dolist (d *demos*)
      (setf (gethash (intern (string-upcase (get d 'demo-name))
			     *keyword-package*)
		     *name-to-function*)
	    d)
      (push (get d 'demo-name) *demo-names*))
  
    (let* ((display (xlib:open-default-display))
           (screen (xlib:display-default-screen display))
           (fg-color (xlib:screen-white-pixel screen))
           (bg-color (xlib:screen-black-pixel screen))
           (nice-font (xlib:open-font display "fixed")))
      
      (let ((a-menu (xlib::create-menu
                     (xlib::screen-root screen) ;the menu's parent
                     fg-color bg-color nice-font)))
        
        (setf (xlib::menu-title a-menu) "Please pick your favorite demo:")
        (xlib::menu-set-item-list a-menu *demo-names*)
        (ignore-errors ;; closing window is not handled properly in menu.
          (unwind-protect
               (do ((choice (xlib::menu-choose a-menu 100 100)
                            (xlib::menu-choose a-menu 100 100)))
                   ((and choice (string-equal "Quit" choice)))
                 (let* ((demo-choice (intern (string-upcase choice)
                                             *keyword-package*))
                        (fun (gethash demo-choice *name-to-function*)))
                   (setf choice nil)
                   (when fun
                     (ignore-errors (funcall fun)))))
            (xlib:display-finish-output display)
            (xlib:close-display display)))))))


;;;; Shared demo utilities.







;;;; Plaid

;;; 
;;; Translated from the X11 Plaid Demo written in C by Christopher Hoover.
;;; 

(defmacro rect-x (rects n)
  `(svref ,rects (ash ,n 2)))
(defmacro rect-y (rects n)
  `(svref ,rects (+ (ash ,n 2) 1)))
(defmacro rect-width (rects n)
  `(svref ,rects (+ (ash ,n 2) 2)))
(defmacro rect-height (rects n)
  `(svref ,rects (+ (ash ,n 2) 3)))

(defun plaid (display window &optional (num-iterations 10000) (num-rectangles 10))
  (let ((gcontext (xlib:create-gcontext :drawable window
					:function boole-c2
					:plane-mask (logxor *white-pixel*
							    *black-pixel*)
					:background *black-pixel*
					:foreground *white-pixel*
					:fill-style :solid))
	(rectangles (make-array (* 4 num-rectangles)
				:element-type 'number
				:initial-element 0)))
    (multiple-value-bind (width height) (full-window-state window)
      (let ((center-x (ash width -1))
	    (center-y (ash height -1))
	    (x-dir -2)
	    (y-dir -2)
	    (x-off 2)
	    (y-off 2))
	(dotimes (iter (truncate num-iterations num-rectangles))
	  (dotimes (i num-rectangles)
	    (setf (rect-x rectangles i) (- center-x x-off))
	    (setf (rect-y rectangles i) (- center-y y-off))
	    (setf (rect-width rectangles i) (ash x-off 1))
	    (setf (rect-height rectangles i) (ash y-off 1))
	    (incf x-off x-dir)
	    (incf y-off y-dir)
	    (when (or (<= x-off 0) (>= x-off center-x))
	      (decf x-off (ash x-dir 1))
	      (setf x-dir (- x-dir)))
	    (when (or (<= y-off 0) (>= y-off center-y))
	      (decf y-off (ash y-dir 1))
	      (setf y-dir (- y-dir))))
	  (xlib:draw-rectangles window gcontext rectangles t)
	  (sleep *delay*)
	  (xlib:display-force-output display))))
    (xlib:free-gcontext gcontext)))

(defdemo plaid-demo "Plaid" (&optional (iterations 10000) (num-rectangles 10))
  10 10 101 201
  "Plaid, man."
  (plaid *display* *window* iterations num-rectangles))


;;;; Bball demo

;;; 
;;; Ported to CLX by Blaine Burks
;;; 

(defvar *ball-size-x* 36)
(defvar *ball-size-y* 34)

(defun xor-ball (pixmap window gcontext x y)
  (xlib:copy-plane pixmap gcontext 1
		  0 0
		  *ball-size-x* *ball-size-y*
		  window
		  x y))

(defconstant bball-gravity 1)
(defconstant maximum-x-drift 7)

(defvar *max-bball-x*)
(defvar *max-bball-y*)

(defstruct ball
  (x (random (- *max-bball-x* *ball-size-x*)))
  (y (random (- *max-bball-y* *ball-size-y*)))
  (dx (if (zerop (random 2)) (random maximum-x-drift)
	  (- (random maximum-x-drift))))
  (dy 0))

(defun get-bounce-image ()
  "Returns the pixmap to be bounced around the screen."
  (xlib::bitmap-image   #*000000000000000000000000000000000000
			#*000000000000000000000000000000000000
			#*000000000000000000001000000010000000
			#*000000000000000000000000000100000000
			#*000000000000000000000100001000000000
			#*000000000000000010000000010000000000
			#*000000000000000000100010000000000000
			#*000000000000000000001000000000000000
			#*000000000001111100000000000101010000
			#*000000000010000011000111000000000000
			#*000000000100000000111000000000000000
			#*000000000100000000000000000100000000
			#*000000000100000000001000100010000000
			#*000000111111100000010000000001000000
			#*000000111111100000100000100000100000
			#*000011111111111000000000000000000000
			#*001111111111111110000000100000000000
			#*001111111111111110000000000000000000
			#*011111111111111111000000000000000000
			#*011111111111111111000000000000000000
			#*111111111111110111100000000000000000
			#*111111111111111111100000000000000000
			#*111111111111111101100000000000000000
			#*111111111111111101100000000000000000
			#*111111111111111101100000000000000000
			#*111111111111111111100000000000000000
			#*111111111111110111100000000000000000
			#*011111111111111111000000000000000000
			#*011111111111011111000000000000000000
			#*001111111111111110000000000000000000
			#*001111111111111110000000000000000000
			#*000011111111111000000000000000000000
			#*000000111111100000000000000000000000
			#*000000000000000000000000000000000000))


(defun bounce-1-ball (pixmap window gcontext ball)
  (let ((x (ball-x ball))
	(y (ball-y ball))
	(dx (ball-dx ball))
	(dy (ball-dy ball)))
    (xor-ball pixmap window gcontext x y)
    (setq x (+ x dx))
    (setq y (+ y dy))
    (if (or (< x 0) (> x (- *max-bball-x* *ball-size-x*)))
	(setq x (- x dx)
	      dx (- dx)))
    (if (> y (- *max-bball-y* *ball-size-y*))
	(setq y (- y dy)
	      dy (- dy)))
    (setq dy (+ dy bball-gravity))
    (setf (ball-x ball) x)
    (setf (ball-y ball) y)
    (setf (ball-dx ball) dx)
    (setf (ball-dy ball) dy)
    (xor-ball pixmap window gcontext x y)))

(defun bounce-balls (display window how-many duration)
  (xlib:clear-area window)
  (xlib:display-finish-output display)
  (multiple-value-bind (*max-bball-x* *max-bball-y*) (full-window-state window)
    (let* ((balls (do ((i 0 (1+ i))
		       (list () (cons (make-ball) list)))
		      ((= i how-many) list)))
	   (gcontext (xlib:create-gcontext :drawable window
					   :foreground *white-pixel*
					   :background *black-pixel*
					   :function boole-xor
					   :exposures :off))
	   (bounce-pixmap (xlib:create-pixmap :width 38 :height 34 :depth 1
					      :drawable window))
	   (pixmap-gc (xlib:create-gcontext :drawable bounce-pixmap
					    :foreground *white-pixel*
					    :background *black-pixel*)))
      (xlib:put-image bounce-pixmap pixmap-gc (get-bounce-image)
		      :x 0 :y 0 :width 38 :height 34)
      (xlib:free-gcontext pixmap-gc)
      (dolist (ball balls)
	(xor-ball bounce-pixmap window gcontext (ball-x ball) (ball-y ball)))
      (xlib:display-finish-output display)
      (dotimes (i duration)
	(dolist (ball balls)
	  (bounce-1-ball bounce-pixmap window gcontext ball)
          (xlib:display-finish-output display))
	(sleep (/ *delay* 50.0)))
      (xlib:free-pixmap bounce-pixmap)
      (xlib:free-gcontext gcontext))))

(defdemo bouncing-ball-demo "Bouncing-Ball" (&optional (how-many 5) (duration 500))
  36 34 700 500
  "Bouncing balls in space."
  (bounce-balls *display*  *window* how-many duration))
