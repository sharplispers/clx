(defpackage #:xlib-demo/recurrence
  (:use :common-lisp :xlib :xlib-demo/demos-new)
  (:export #:recurrence))

(in-package :xlib-demo/recurrence)



;;;; Recurrence Demo

;;; Copyright (C) 1988 Michael O. Newton (newton@csvax.caltech.edu)

;;; Permission is granted to any individual or institution to use, copy,
;;; modify, and distribute this software, provided that this complete
;;; copyright and permission notice is maintained, intact, in all copies and
;;; supporting documentation.

;;; The author provides this software "as is" without express or
;;; implied warranty.

;;; This routine plots the recurrence
;;;      x <- y(1+sin(0.7x)) - 1.2(|x|)^.5
;;;      y <- .21 - x
;;; As described in a ?? 1983 issue of the Mathematical Intelligencer

(defun recurrence (&optional (point-count 10000))
  (with-x11-context ()
    (let* ((window (create-window
                    :parent (screen-root *screen*)
                    :x 10 :y 10 :width 700 :height 700
                    :background *white-pixel*))
           (gc (xlib:create-gcontext :drawable window
                                     :background *white-pixel*
                                     :foreground *black-pixel*)))
      (xlib:set-wm-properties window :name "Recurrence")
      (xlib:map-window window)
      (do ((attempts 0 (1+ attempts)))
          ((or (eq (xlib:window-map-state window) :viewable)
               (>= attempts 100)))  ; wait 1 sec before giving up
        (sleep 0.01))
      (multiple-value-bind (width height) (full-window-state window)
        (xlib:clear-area window)
        (draw-ppict window gc point-count 0.0 0.0 (* width 0.5) (* height 0.5))
        (xlib:display-finish-output *display*)
        (sleep 1))
      (xlib:free-gcontext gc))))

;;; Draw points.  X assumes points are in the range of width x height,
;;; with 0,0 being upper left and 0,H being lower left.
;;; hw and hh are half-width and half-height of screen

(defun draw-ppict (win gc count x y hw hh)
  "Recursively draw pretty picture"
  (unless (zerop count)
    (let ((xf (floor (* (+ 1.0 x) hw ))) ; These lines center the picture
          (yf (floor (* (+ 0.7 y) hh ))))
      (xlib:draw-point win gc xf yf)
      (draw-ppict win gc (1- count)
                  (- (* y (1+ (sin (* 0.7 x)))) (* 1.2 (sqrt (abs x))))
                  (- 0.21 x)
                  hw
                  hh))))

(push (make-demo :name "Recurrence" :function #'recurrence) *demos*)



