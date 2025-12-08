(defpackage #:xlib-demo/greynetic
  (:use :common-lisp :xlib :xlib-demo/demos-new)
  (:export #:greynetic))

(in-package :xlib-demo/greynetic)

(defvar *pixmaps* nil)

(defun make-random-bitmap ()
  (let ((bitmap-data (make-array '(32 32) :initial-element 0
                                          :element-type 'xlib::bit)))
    (dotimes (i 4)
      (declare (fixnum i))
      (let ((nibble (random 16)))
        (setf nibble (logior nibble (ash nibble 4))
              nibble (logior nibble (ash nibble 8))
              nibble (logior nibble (ash nibble 12))
              nibble (logior nibble (ash nibble 16)))
        (dotimes (j 32)
          (let ((bit (if (logbitp j nibble) 1 0)))
            (setf (aref bitmap-data i j) bit
                  (aref bitmap-data (+ 4 i) j) bit
                  (aref bitmap-data (+ 8 i) j) bit
                  (aref bitmap-data (+ 12 i) j) bit
                  (aref bitmap-data (+ 16 i) j) bit
                  (aref bitmap-data (+ 20 i) j) bit
                  (aref bitmap-data (+ 24 i) j) bit
                  (aref bitmap-data (+ 28 i) j) bit)))))
    bitmap-data))

(defun make-random-pixmap ()
  (let ((image (xlib:create-image :depth 1 :data (make-random-bitmap))))
    (make-pixmap image 32 32)))

(defun make-pixmap (image width height)
  (let* ((pixmap (xlib:create-pixmap :width width :height height
                                     :depth 1 :drawable *root*))
         (gc (xlib:create-gcontext :drawable pixmap
                                   :background *black-pixel*
                                   :foreground *white-pixel*)))
    (xlib:put-image pixmap gc image :x 0 :y 0 :width width :height height)
    (xlib:free-gcontext gc)
    pixmap))


;;;
;;; This function returns one of the pixmaps in the *pixmaps* array.
(defun greynetic-pixmapper ()
  (aref *pixmaps* (random (length *pixmaps*))))

(defun greynetic (&optional (duration 300))
  (with-x11-context ()
    (let* ((window (create-window
                    :parent (screen-root *screen*)
                    :x 100 :y 100 :width 600 :height 600))
           (depth (xlib:drawable-depth window))
           (draw-gcontext (xlib:create-gcontext :drawable window
                                                :foreground *white-pixel*
                                                :background *black-pixel*))
           ;; Need a random state per process.
           (*random-state* (make-random-state t))
           (*pixmaps* (let ((pixmap-array (make-array 30)))
                        (dotimes (i 30)
                          (setf (aref pixmap-array i) (make-random-pixmap)))
                        pixmap-array)))
      (xlib:set-wm-properties window :name "Greynetic")
      (xlib:map-window window)
      (xlib:display-force-output *display*)
      (unwind-protect
           (multiple-value-bind (width height) (full-window-state window)
             (declare (fixnum width height))
             (let ((border-x (truncate width 20))
                   (border-y (truncate height 20)))
               (declare (fixnum border-x border-y))
               (dotimes (i duration)
                 (let ((pixmap (greynetic-pixmapper)))
                   (xlib:with-gcontext (draw-gcontext
                                        :foreground (random (ash 1 depth))
                                        :background (random (ash 1 depth))
                                        :stipple pixmap
                                        :fill-style
                                        :opaque-stippled)
                     (cond ((zerop (mod i 500))
                            (xlib:clear-area window)
                            (sleep .1))
                           (t
                            (sleep (/ *delay* 20))))
                     (if (< (random 3) 2)
                         (let* ((w (+ border-x
                                      (truncate (* (random (- width
                                                              (* 2 border-x)))
                                                   (random width))
                                                width)))
                                (h (+ border-y
                                      (truncate (* (random (- height
                                                              (* 2 border-y)))
                                                   (random height))
                                                height)))
                                (x (random (- width w)))
                                (y (random (- height h))))
                           (declare (fixnum w h x y))
                           (if (zerop (random 2))
                               (xlib:draw-rectangle window draw-gcontext
                                                    x y w h t)
                               (xlib:draw-arc window draw-gcontext
                                              x y w h 0 (* 2 pi) t)))
                         (let ((p1-x (+ border-x
                                        (random (- width (* 2 border-x)))))
                               (p1-y (+ border-y
                                        (random (- height (* 2 border-y)))))
                               (p2-x (+ border-x
                                        (random (- width (* 2 border-x)))))
                               (p2-y (+ border-y
                                        (random (- height (* 2 border-y)))))
                               (p3-x (+ border-x
                                        (random (- width (* 2 border-x)))))
                               (p3-y (+ border-y
                                        (random (- height (* 2 border-y))))))
                           (declare (fixnum p1-x p1-y p2-x p2-y p3-x p3-y))
                           (xlib:draw-lines window draw-gcontext
                                            (list p1-x p1-y p2-x p2-y p3-x p3-y)
                                            :relative-p nil
                                            :fill-p t
                                            :shape :convex)))
                     (xlib:display-force-output *display*))))))
        (dotimes (i (length *pixmaps*))
          (xlib:free-pixmap (aref *pixmaps* i)))
        (xlib:free-gcontext draw-gcontext)))))


(push (make-demo :name "Greynetic" :function #'greynetic) *demos*)

