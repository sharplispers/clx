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
