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
  (:export #:demo
           #:*display* #:*screen* #:*root* :*colormap*
           #:*black-pixel* #:*white-pixel* #:*font*
           #:*font* #:*window*
           #:*demos* #:*delay*

           #:make-demo
           #:with-x11-context
           #:full-window-state))

(in-package :xlib-demo/demos)

(defparameter *display* nil
  "The current X11 display connection.")

(defparameter *screen* nil
  "The current default screen of the current display.")

(defparameter *root* nil
  "The root window of the current screen.")

(defparameter *colormap* nil
  "The default colormap for the current screen.")

(defparameter *black-pixel* nil
  "The pixel value that represents black on the current screen.")

(defparameter *white-pixel* nil
  "The pixel value that represents white on the current screen.")

(defparameter *font* nil
  "The default font used for text rendering in demos.")

(defparameter *window* nil
  "The current demo window.")

(defvar *demos* '()  "Registry of available demos.")
(declaim (type list *demos*))

(defparameter *delay* 0.5)

(defstruct demo
  (name "" :type string )
  (function nil :type function))

(defun open-default-font (display)
  (xlib:open-font display
                  (or (first
                       (xlib:list-font-names
                        display
                        "-adobe-courier-medium-r-*--48-0-0-0-m-0-*-*"
                        :max-fonts 1))
                      "fixed")))

(defmacro with-x11-context (() &body body)
  `(let* ((*display* (or (xlib:open-default-display)
                         (xlib:open-display (machine-instance))))
          (*screen* (xlib:display-default-screen *display*))
          (*root* (xlib:screen-root *screen*))
          (*colormap* (xlib:screen-default-colormap *screen*))
          (*black-pixel* (xlib:screen-black-pixel *screen*))
          (*white-pixel* (xlib:screen-white-pixel *screen*))
          (*font* (open-default-font *display*)))
     (setf (xlib:display-report-asynchronous-errors *display*)
           '(:after-finish-output))
     (unwind-protect
          (progn ,@body)
       (xlib:close-display *display*))))

(defun full-window-state (w)
  (xlib:with-state (w)
    (values (xlib:drawable-width w) (xlib:drawable-height w)
            (xlib:drawable-x w) (xlib:drawable-y w)
            (xlib:window-map-state w))))

(defun start-in-thread (function name &optional args)
  #+sbcl
  (sb-thread:make-thread
   (lambda () (apply function args))
   :name name)

  #+(and cmu mp)
  (mp:make-process
   (lambda () (apply function args))
   :name name)

  #+(and ecl threads)
  (mp:process-run-function
   name
   (lambda () (apply function args)))

  #+(and clasp threads)
  (mp:process-run-function
   name
   (lambda () (apply function args)))

  #+abcl
  (threads:make-thread
   (lambda () (apply function args))
   :name name)

  #-(or sbcl (and cmu mp) (and ecl threads) (and clasp threads) abcl)
  (progn
    (warn "Threading not supported on this Lisp implementation.")
    (apply function args)))

(defun demo ()
  (with-x11-context ()
    (let* ((menu (xlib::create-menu *root* *white-pixel* *black-pixel*
                                    *font*))
           (menu-window (xlib::menu-window menu)))
      (setf (xlib:window-event-mask (xlib::menu-window menu))
            (xlib:make-event-mask :structure-notify :leave-window
                                  :exposure)

            (xlib::menu-title menu)
            "Please pick your favorite demo:")
      (xlib::menu-set-item-list
       menu
       (append (mapcar #'demo-name *demos*) '("Quit")))
      (xlib::menu-present menu 0 0)

      (loop
        with quit-requested = nil
        with items of-type list = (xlib::menu-item-alist menu)
        until (or quit-requested (xlib::display-dead *display*)) do
          (xlib:event-case (*display* :timeout 0.01 :force-output-p t)
            (:destroy-notify
             (event-window)
             (when (xlib:window-equal event-window menu-window)
               (setf quit-requested t)
               t))
            (:exposure
             (event-window count)
             (when (xlib:window-equal event-window menu-window)
               (locally (declare (type xlib:card8 count))
                 ;; Only refresh on final exposure event
                 (when (zerop count)
                   (xlib::menu-refresh menu)))
               t))
            (:button-release
             (event-window)
             (let ((item-name (second (assoc event-window items))))
               (when item-name
                 (if (string-equal "Quit" item-name)
                     (setf quit-requested t)
                     (let ((demo (find item-name
                                       *demos*
                                       :key #'demo-name
                                       :test #'string-equal)))
                       (start-in-thread
                        (lambda ()
                          (handler-case
                              (funcall (demo-function demo))
                            (serious-condition (c)
                              (format *debug-io* "~s error:~%~a" item-name c))))
                        (demo-name demo))))))
             t)
            (:enter-notify
             (window)
             (locally (declare (type xlib:window window))
               (let ((position (position window items :key #'first)))
                 (when position
                   (xlib::menu-highlight-item menu position))))
             t)
            (:leave-notify
             (window)
             (locally (declare (type xlib:window window))
               (let ((position (position window items :key #'first)))
                 (when position
                   (xlib::menu-unhighlight-item menu position))))
             t)
            (otherwise
             ()
             t))))))

#+ (or)
(demo)

(with-standard-io-syntax
  (format t "~&To start CLX demos run: ~s" '(xlib-demo/demos:demo)))
