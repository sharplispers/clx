(defpackage #:xlib-demo/plaid
  (:use :common-lisp :xlib :xlib-demo/demos-new)
  (:export #:plaid))

(in-package :xlib-demo/plaid)



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

(defun plaid (&optional (num-iterations 10000) (num-rectangles 10))
  (with-x11-context ()
    (let* ((window (create-window
                    :parent (screen-root *screen*)
                    :x 10 :y 10 :width 700 :height 700
                    :background *white-pixel*))
           (gcontext (xlib:create-gcontext :drawable window
                                           :function boole-c2
                                           :plane-mask (logxor *white-pixel*
                                                               *black-pixel*)
                                           :background *black-pixel*
                                           :foreground *white-pixel*
                                           :fill-style :solid))
           (rectangles (make-array (* 4 num-rectangles)
                                   :element-type 'number
                                   :initial-element 0)))
      (xlib:set-wm-properties window :name "Plaid")
      (xlib:map-window window)
      (do ((attempts 0 (1+ attempts)))
          ((or (eq (xlib:window-map-state window) :viewable)
               (>= attempts 100)))  ; wait 1 sec before giving up
        (sleep 0.01))
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
            (xlib:display-force-output *display*))))
      (xlib:free-gcontext gcontext))))

(push (make-demo :name "Plaid" :function #'plaid) *demos*)

