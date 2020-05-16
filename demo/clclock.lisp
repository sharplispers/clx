(defpackage #:xlib-demo/clclock
  (:use "CL")
  (:export "CLOCK"))

(in-package #:xlib-demo/clclock)

(declaim (special *display* *screen* *colormap* *font* *win* *gcontext*
                  *background* *palette* *black*))

(defun romanize (arg)
  (if (zerop arg)
      "O"
      (format nil "~@R" arg)))

(defun clock-string ()
  (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
    (format nil "~a ~a ~a" (romanize h) (romanize m) (romanize s))))

(defun update-clockface ()
  (let ((string (clock-string)))
    (let ((string-width (xlib:text-width *gcontext* string)))
      (xlib:draw-rectangle *win* *background*
                           0 0
                           (xlib:drawable-width *win*)
                           (xlib:drawable-height *win*)
                           :fill-p)
      (xlib:draw-glyphs *win* *gcontext*
                        (- (truncate
                            (- (xlib:drawable-width *win*) string-width)
                            2)
                           10)
                        (- (xlib:drawable-height *win*) 10)
                        string)))
  (xlib:display-force-output *display*))

(defun clock ()
  (let* ((*display* (xlib:open-default-display))
         (*screen* (xlib:display-default-screen *display*))
         (*colormap* (xlib:screen-default-colormap *screen*))

         (*font* (xlib:open-font *display* "fixed")))
    (multiple-value-bind (width ascent)
        (xlib:text-extents *font* "XVIIII XXXVIIII XXXVIIII")
      (setq *win* (xlib:create-window
                   :parent (xlib:screen-root *screen*)
                   :x 512
                   :y 512
                   :width (+ 20 width)
                   :height (+ 20 ascent)
                   :background (xlib:alloc-color *colormap*
                                                 (xlib:lookup-color *colormap*
                                                                    "midnightblue")))
            *gcontext* (xlib:create-gcontext
                        :drawable *win*
                        :fill-style :solid
                        :background (xlib:screen-white-pixel *screen*)
                        :foreground (xlib:alloc-color *colormap*
                                                      (xlib:lookup-color
                                                       *colormap*
                                                       "yellow"))
                        :font *font*)
            *background* (xlib:create-gcontext
                          :drawable *win*
                          :fill-style :solid
                          :background (xlib:screen-white-pixel *screen*)
                          :foreground (xlib:alloc-color *colormap*
                                                        (xlib:lookup-color *colormap*
                                                                           "midnightblue"))
                          :font *font*)
            *palette* nil
            *black* (xlib:screen-black-pixel *screen*)))
    (xlib:map-window *win*)
    (loop
      (update-clockface)
      (sleep 1))))
