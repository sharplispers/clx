(defpackage #:xlib-demo/clclock
  (:use :common-lisp :xlib :xlib-demo/demos)
  (:export #:clock))

(in-package #:xlib-demo/clclock)

(defun romanize (arg)
  (if (zerop arg)
      "O"
      (format nil "~@R" arg)))

(defun clock-string ()
  (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
    (format nil "~a ~a ~a" (romanize h) (romanize m) (romanize s))))

(defun update-clockface (window gcontext background)
  (let ((string (clock-string)))
    (let ((string-width (xlib:text-width gcontext string)))
      (xlib:draw-rectangle window background
                           0 0
                           (xlib:drawable-width window)
                           (xlib:drawable-height window)
                           :fill-p)
      (xlib:draw-glyphs window gcontext
                        (- (truncate
                            (- (xlib:drawable-width window) string-width)
                            2)
                           10)
                        (- (xlib:drawable-height window) 10)
                        string)))
  (xlib:display-force-output *display*))

(defun clock ()
  (with-x11-context ()
    (multiple-value-bind (width ascent)
        (xlib:text-extents *font* "XVIIII XXXVIIII XXXVIIII")
      (let* ((midnightblue
               (xlib:alloc-color
                *colormap*
                (xlib:lookup-color *colormap* "midnightblue")))
             (window (xlib:create-window
                      :parent (xlib:screen-root *screen*)
                      :x 512
                      :y 512
                      :width (+ 20 width)
                      :height (+ 20 ascent)
                      :background midnightblue
                      :event-mask '(:structure-notify)))
             (gcontext (xlib:create-gcontext
                        :drawable window
                        :fill-style :solid
                        :background *white-pixel*
                        :foreground
                        (xlib:alloc-color
                         *colormap*
                         (xlib:lookup-color *colormap* "yellow"))
                        :font *font*))
             (background (xlib:create-gcontext
                          :drawable window
                          :fill-style :solid
                          :background *white-pixel*
                          :foreground midnightblue
                          :font *font*)))
        (xlib:map-window window)
        (loop with runningp = t
              while runningp do
                (xlib:event-case (*display* :timeout 1 :discard-p t)
                  (:destroy-notify
                   (event-window)
                   (when (xlib:window-equal event-window window)
                     (setf runningp nil)
                     t))
                  (otherwise
                   (event-window)
                   (when (and event-window window
                              (xlib:window-equal event-window window))
                     t)))
                (ignore-errors
                 (update-clockface window gcontext background)))))))

(push (make-demo :name "Clock" :function #'clock) *demos*)

