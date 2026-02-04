(defpackage #:xlib-demo/bouncing-balls
  (:use :common-lisp :xlib :xlib-demo/demos)
  (:export #:bouncing-balls))

(in-package :xlib-demo/bouncing-balls)



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

(defun bounce-balls (&optional (how-many 5) (duration 500))
  (with-x11-context ()
    (let ((window (create-window
                   :parent (screen-root *screen*)
                   :x 36 :y 34 :width 700 :height 500
                   :background *white-pixel*
                   :event-mask '(:structure-notify))))
      (xlib:set-wm-properties window :name "Bouncing balls")
      (xlib:map-window window)
      (xlib:clear-area window)
      (xlib:display-finish-output *display*)
      (do ((attempts 0 (1+ attempts)))
          ((or (eq (xlib:window-map-state window) :viewable)
               (>= attempts 100)))  ; wait 1 sec before giving up
        (sleep 0.01))
      (multiple-value-bind (*max-bball-x* *max-bball-y*)
          (full-window-state window)
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
                                                :background *black-pixel*))
               (runningp t))
          (xlib:put-image bounce-pixmap pixmap-gc (get-bounce-image)
                          :x 0 :y 0 :width 38 :height 34)
          (xlib:free-gcontext pixmap-gc)
          (dolist (ball balls)
            (xor-ball bounce-pixmap window gcontext (ball-x ball) (ball-y ball)))
          (xlib:display-finish-output *display*)
          (dotimes (i duration)
            (unless runningp (return))
            (xlib:event-case (*display* :timeout 0 :discard-p t)
              (:destroy-notify
               (event-window)
               (when (xlib:window-equal event-window window)
                 (setf runningp nil)
                 t))
              (otherwise
               (event-window)
               (when (xlib:window-equal event-window window)
                 t)))
            (when runningp
              (ignore-errors
               (dolist (ball balls)
                 (bounce-1-ball bounce-pixmap window gcontext ball)
                 (xlib:display-finish-output *display*))))
            (sleep (/ *delay* 50.0)))
          (xlib:free-pixmap bounce-pixmap)
          (xlib:free-gcontext gcontext))))))

(push (make-demo :name "Bouncing Balls" :function #'bounce-balls) *demos*)
