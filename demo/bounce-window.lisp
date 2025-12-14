(defpackage #:xlib-demo/bounce-window
  (:use :common-lisp :xlib :xlib-demo/demos)
  (:export #:bounce-window))

(in-package :xlib-demo/bounce-window)


;;;; Bounce window.

;;; BOUNCE-WINDOW takes a window and seemingly drops it to the bottom of
;;; the screen.  Optionally, the window can have an initial x velocity,
;;; screen border elasticity, and gravity value.  The outer loop is
;;; entered the first time with the window at its initial height, but
;;; each iteration after this, the loop starts with the window at the
;;; bottom of the screen heading upward.  The inner loop, except for the
;;; first execution, carries the window up until the negative velocity
;;; becomes positive, carrying the window down to bottom when the
;;; velocity is positive.  Due to number lossage, ROUND'ing and
;;; TRUNC'ing when the velocity gets so small will cause the window to
;;; head upward with the same velocity over two iterations which will
;;; cause the window to bounce forever, so we have prev-neg-velocity and
;;; number-problems to check for this.  This is not crucial with the x
;;; velocity since the loop terminates as a function of the y velocity.
;;;
(defun bounce-window (&key (x 100) (y 100) (width 300) (height 300)
                        (x-velocity 0) (elasticity 0.85) (gravity 2))
  (unless (< 0 elasticity 1)
    (error "Elasticity must be between 0 and 1."))
  (unless (plusp gravity)
    (error "Gravity must be positive."))
  (with-x11-context ()
    (let ((window (create-window
                   :parent (screen-root *screen*)
                   :x x :y y :width width :height height
                   :background *white-pixel*)))
      (xlib:set-wm-properties window
                              :name "Bounce Window"
                              :x x :y y
                              :width width :height height
                              :user-specified-position-p t
                              :user-specified-size-p t
                              :min-width width :min-height height
                              :max-width width :max-height height)
      (xlib:map-window window)
      (xlib:display-force-output *display*)
      (do ((attempts 0 (1+ attempts)))
          ((or (eq (xlib:window-map-state window) :viewable)
               (>= attempts 100)))  ; wait 1 sec before giving up
        (sleep 0.01))
      (let ((top-of-window-at-bottom (- (xlib:drawable-height *root*) height))
            (left-of-window-at-right (- (xlib:drawable-width *root*) width))
            (y-velocity 0)
            (prev-neg-velocity most-negative-fixnum)
            (number-problems nil))
        (declare (fixnum top-of-window-at-bottom left-of-window-at-right
                         y-velocity))
        (loop
          (when (= prev-neg-velocity 0) (return t))
          (let ((negative-velocity (minusp y-velocity)))
            (loop
              (let ((next-y (+ y y-velocity))
                    (next-y-velocity (+ y-velocity gravity)))
                (declare (fixnum next-y next-y-velocity))
                (when (> next-y top-of-window-at-bottom)
                  (cond
                    (number-problems
                     (setf y-velocity (incf prev-neg-velocity)))
                    (t
                     (setq y-velocity
                           (- (truncate (* elasticity y-velocity))))
                     (when (= y-velocity prev-neg-velocity)
                       (incf y-velocity)
                       (setf number-problems t))
                     (setf prev-neg-velocity y-velocity)))
                  (setf y top-of-window-at-bottom)
                  (setf (xlib:drawable-x window) x
                        (xlib:drawable-y window) y)
                  (xlib:display-force-output *display*)
                  (return))
                (setq y-velocity next-y-velocity)
                (setq y next-y)
                (sleep (/ *delay* 100)))
              (when (and negative-velocity (>= y-velocity 0))
                (setf negative-velocity nil))
              (let ((next-x (+ x x-velocity)))
                (declare (fixnum next-x))
                (when (or (> next-x left-of-window-at-right)
                          (< next-x 0))
                  (setq x-velocity (- (truncate (* elasticity x-velocity)))))
                (setq x next-x))
              (setf (xlib:drawable-x window) x
                    (xlib:drawable-y window) y)
              (xlib:display-force-output *display*))))))))

(push (make-demo :name "Shove-bounce"
                 :function (lambda () (bounce-window :x-velocity 3)))
      *demos*)

(push (make-demo :name "Bounce" :function #'bounce-window) *demos*)

