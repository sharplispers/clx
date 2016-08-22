(defpackage #:xlib-demo/gl-test
  (:use :common-lisp :xlib :xlib/gl)
  (:export "TEST" "CLX-TEST"))

(in-package #:xlib-demo/gl-test)


(defun test (function &key (host "localhost") (display 1) (width 200) (height 200))
  (let* ((display (open-display host :display display))
         (screen (display-default-screen display))
         (root (screen-root screen))
         ctx)
    (unwind-protect
         (progn
           ;;; Inform the server about us.
           (xlib/glx::client-info display)
           (let* ((visual (xlib/glx:choose-visual screen '(:glx-rgba
                                                      (:glx-red-size 1)
                                                      (:glx-green-size 1)
                                                      (:glx-blue-size 1)
                                                      :glx-double-buffer)))
                  (colormap (create-colormap (xlib/glx:visual-id visual) root))
                  (window (create-window :parent root
                                         :x 10 :y 10 :width width :height height
                                         :class :input-output
                                         :background (screen-black-pixel screen)
                                         :border (screen-black-pixel screen)
                                         :visual (xlib/glx:visual-id visual)
                                         :depth 24
                                         :colormap colormap
                                         :event-mask '(:structure-notify :exposure)))
                  (gc (create-gcontext :foreground (screen-white-pixel screen)
                                       :background (screen-black-pixel screen)
                                       :drawable window
                                       :font (open-font display "fixed"))))
             (set-wm-properties window
                                :name "glx-test"
                                :resource-class "glx-test"
                                :command (list "glx-test")
                                :x 10 :y 10 :width width :height height
                                :min-width width :min-height height
                                :initial-state :normal)

             (setf ctx (xlib/glx:create-context screen (xlib/glx:visual-id visual)))
             (map-window window)
             (xlib/glx:make-current window ctx)

             (funcall function display window)

             (unmap-window window)
             (free-gcontext gc)))
      
      (when ctx (xlib/glx:destroy-context ctx))
      (close-display display))))


;;; Tests


(defun no-floats (display window)
  (declare (ignore display window))
  (color-3s #x7fff #x7fff 0)
  (begin +polygon+)
  (vertex-2s 0 0)
  (vertex-2s 1 0)
  (vertex-2s 1 1)
  (vertex-2s 0 1)
  (end)
  (xlib/glx:swap-buffers)
  (sleep 5))


(defun anim (display window)
  (declare (ignore display window))
  (ortho 0.0d0 1.0d0 0.0d0 1.0d0 -1.0d0 1.0d0)
  (clear-color 0.0s0 0.0s0 0.0s0 0.0s0)
  (line-width 2.0s0)
  (loop
     repeat 361
     for angle upfrom 0.0s0 by 1.0s0
     do (progn
          (clear +color-buffer-bit+)
          (push-matrix)
          (translate-f 0.5s0 0.5s0 0.0s0)
          (rotate-f angle 0.0s0 0.0s0 1.0s0)
          (translate-f -0.5s0 -0.5s0 0.0s0)
          (begin +polygon+ #-(and) +line-loop+)
          (color-3ub 255 0 0)
          (vertex-2f 0.25s0 0.25s0)
          (color-3ub 0 255 0)
          (vertex-2f 0.75s0 0.25s0)
          (color-3ub 0 0 255)
          (vertex-2f 0.75s0 0.75s0)
          (color-3ub 255 255 255)
          (vertex-2f 0.25s0 0.75s0)
          (end)
          (pop-matrix)
          (xlib/glx:swap-buffers)
          (sleep 0.02)))
  (sleep 3))


(defun anim/list (display window)
  (declare (ignore display window))
  (ortho 0.0d0 1.0d0 0.0d0 1.0d0 -1.0d0 1.0d0)
  (clear-color 0.0s0 0.0s0 0.0s0 0.0s0)
  (let ((list (gen-lists 1)))
    (new-list list +compile+)
    (begin +polygon+)
    (color-3ub 255 0 0)
    (vertex-2f 0.25s0 0.25s0)
    (color-3ub 0 255 0)
    (vertex-2f 0.75s0 0.25s0)
    (color-3ub 0 0 255)
    (vertex-2f 0.75s0 0.75s0)
    (color-3ub 255 255 255)
    (vertex-2f 0.25s0 0.75s0)
    (end)
    (xlib/glx:render)
    (end-list)

    (loop
       repeat 361
       for angle upfrom 0.0s0 by 1.0s0
       do (progn
            (clear +color-buffer-bit+)
            (push-matrix)
            (rotate-f angle 0.0s0 0.0s0 1.0s0)
            (call-list list)
            (pop-matrix)
            (xlib/glx:swap-buffers)
            (sleep 0.02))))
  
  (sleep 3))


;;; glxgears

(defconstant +pi+ (coerce pi 'single-float))
(declaim (type single-float +pi+))


(defun gear (inner-radius outer-radius width teeth tooth-depth)
  (let ((r0 inner-radius)
        (r1 (/ (- outer-radius tooth-depth) 2.0s0))
        (r2 (/ (+ outer-radius tooth-depth) 2.0s0))
        (da (/ (* 2.0s0 +pi+) teeth 4.0s0)))
    (shade-model +flat+)
    (normal-3f 0.0s0 0.0s0 1.0s0)

    ;; Front face.
    (begin +quad-strip+)
    (dotimes (i (1+ teeth))
      (let ((angle (/ (* i 2.0 +pi+) teeth)))
        (declare (type single-float angle))
        (vertex-3f (* r0 (cos angle))
                      (* r0 (sin angle))
                      (* width 0.5s0))
        (vertex-3f (* r1 (cos angle))
                      (* r1 (sin angle))
                      (* width 0.5s0))
        (when (< i teeth)
          (vertex-3f (* r0 (cos angle))
                        (* r0 (sin angle))
                        (* width 0.5s0))
          (vertex-3f (* r1 (cos (+ angle (* 3 da))))
                        (* r1 (sin (+ angle (* 3 da))))
                        (* width 0.5s0)))))
    (end)


    ;; Draw front sides of teeth.
    (begin +quads+)
    (setf da (/ (* 2.0s0 +pi+) teeth 4.0s0))
    (dotimes (i teeth)
      (let ((angle (/ (* i 2.0s0 +pi+) teeth)))
        (declare (type single-float angle))
        (vertex-3f (* r1 (cos angle))
                      (* r1 (sin angle))
                      (* width 0.5s0))
        (vertex-3f (* r2 (cos (+ angle da)))
                      (* r2 (sin (+ angle da)))
                      (* width 0.5s0))
        (vertex-3f (* r2 (cos (+ angle (* 2 da))))
                      (* r2 (sin (+ angle (* 2 da))))
                      (* width 0.5s0))
        (vertex-3f (* r1 (cos (+ angle (* 3 da))))
                      (* r1 (sin (+ angle (* 3 da))))
                      (* width 0.5s0))))
    (end)

    (normal-3f 0.0s0 0.0s0 -1.0s0)
                 
    ;; Draw back face.
    (begin +quad-strip+)
    (dotimes (i (1+ teeth))
      (let ((angle (/ (* i 2.0s0 +pi+) teeth)))
        (declare (type single-float angle))
        (vertex-3f (* r1 (cos angle))
                      (* r1 (sin angle))
                      (* width -0.5s0))
        (vertex-3f (* r0 (cos angle))
                      (* r0 (sin angle))
                      (* width -0.5s0))
        (when (< i teeth)
          (vertex-3f (* r1 (cos (+ angle (* 3 da))))
                        (* r1 (sin (+ angle (* 3 da))))
                        (* width -0.5s0))
          (vertex-3f (* r0 (cos angle))
                        (* r0 (sin angle))
                        (* width 0.5s0)))))
    (end)

    ;; Draw back sides of teeth.
    (begin +quads+)
    (setf da (/ (* 2.0s0 +pi+) teeth 4.0s0))
    (dotimes (i teeth)
      (let ((angle (/ (* i 2.0s0 +pi+) teeth)))
        (declare (type single-float angle))
        (vertex-3f (* r1 (cos (+ angle (* 3 da))))
                      (* r1 (sin (+ angle (* 3 da))))
                      (* width -0.5s0))
        (vertex-3f (* r2 (cos (+ angle (* 2 da))))
                      (* r2 (sin (+ angle (* 2 da))))
                      (* width -0.5s0))
        (vertex-3f (* r2 (cos (+ angle da)))
                      (* r2 (sin (+ angle da)))
                      (* width -0.5s0))
        (vertex-3f (* r1 (cos angle))
                      (* r1 (sin angle))
                      (* width -0.5s0))))
    (end)

    ;; Draw outward faces of teeth.
    (begin +quad-strip+)
    (dotimes (i teeth)
      (let ((angle (/ (* i 2.0s0 +pi+) teeth)))
        (declare (type single-float angle))
        (vertex-3f (* r1 (cos angle))
                      (* r1 (sin angle))
                      (* width 0.5s0))
        (vertex-3f (* r1 (cos angle))
                      (* r1 (sin angle))
                      (* width -0.5s0))
        (let* ((u (- (* r2 (cos (+ angle da))) (* r1 (cos angle))))
               (v (- (* r2 (sin (+ angle da))) (* r1 (sin angle))))
               (len (sqrt (+ (* u u) (* v v)))))
          (setf u (/ u len)
                v (/ v len))
          (normal-3f v u 0.0s0)
          (vertex-3f (* r2 (cos (+ angle da)))
                        (* r2 (sin (+ angle da)))
                        (* width 0.5s0))
          (vertex-3f (* r2 (cos (+ angle da)))
                        (* r2 (sin (+ angle da)))
                        (* width -0.5s0))
          (normal-3f (cos angle) (sin angle) 0.0s0)
          (vertex-3f (* r2 (cos (+ angle (* 2 da))))
                        (* r2 (sin (+ angle (* 2 da))))
                        (* width 0.5s0))
          (vertex-3f (* r2 (cos (+ angle (* 2 da))))
                        (* r2 (sin (+ angle (* 2 da))))
                        (* width -0.5s0))
          (setf u (- (* r1 (cos (+ angle (* 3 da)))) (* r2 (cos (+ angle (* 2 da)))))
                v (- (* r1 (sin (+ angle (* 3 da)))) (* r2 (sin (+ angle (* 2 da))))))
          (normal-3f v (- u) 0.0s0)
          (vertex-3f (* r1 (cos (+ angle (* 3 da))))
                        (* r1 (sin (+ angle (* 3 da))))
                        (* width 0.5s0))
          (vertex-3f (* r1 (cos (+ angle (* 3 da))))
                        (* r1 (sin (+ angle (* 3 da))))
                        (* width -0.5s0))
          (normal-3f (cos angle) (sin angle) 0.0s0))))

    (vertex-3f (* r1 (cos 0)) (* r1 (sin 0)) (* width 0.5s0))
    (vertex-3f (* r1 (cos 0)) (* r1 (sin 0)) (* width -0.5s0))

    (end)

    (shade-model +smooth+)
                 
    ;; Draw inside radius cylinder.
    (begin +quad-strip+)
    (dotimes (i (1+ teeth))
      (let ((angle (/ (* i 2.0s0 +pi+) teeth)))
        (declare (type single-float angle))
        (normal-3f (- (cos angle)) (- (sin angle)) 0.0s0)
        (vertex-3f (* r0 (cos angle)) (* r0 (sin angle)) (* width -0.5s0))
        (vertex-3f (* r0 (cos angle)) (* r0 (sin angle)) (* width 0.5s0))))
    (end)))


(defun draw (gear-1 gear-2 gear-3 view-rotx view-roty view-rotz angle)
  (clear (logior +color-buffer-bit+ +depth-buffer-bit+))

  (push-matrix)
  (rotate-f view-rotx 1.0s0 0.0s0 0.0s0)
  (rotate-f view-roty 0.0s0 1.0s0 0.0s0)
  (rotate-f view-rotz 0.0s0 0.0s0 1.0s0)

  (push-matrix)
  (translate-f -3.0s0 -2.0s0 0.0s0)
  (rotate-f angle 0.0s0 0.0s0 1.0s0)
  (call-list gear-1)
  (pop-matrix)

  (push-matrix)
  (translate-f 3.1s0 -2.0s0 0.0s0)
  (rotate-f (- (* angle -2.0s0) 9.0s0) 0.0s0 0.0s0 1.0s0)
  (call-list gear-2)
  (pop-matrix)

  (push-matrix)
  (translate-f -3.1s0 4.2s0 0.0s0)
  (rotate-f (- (* angle -2.s0) 25.0s0) 0.0s0 0.0s0 1.0s0)
  (call-list gear-3)
  (pop-matrix)

  (pop-matrix))


(defun reshape (width height)
  (viewport 0 0 width height)
  (let ((h (coerce (/ height width) 'double-float)))
    (matrix-mode +projection+)
    (load-identity)
    (frustum -1.0d0 1.0d0 (- h) h 5.0d0 60.0d0))

  (matrix-mode +modelview+)
  (load-identity)
  (translate-f 0.0s0 0.0s0 -40.0s0))

             
(defun init ()
  (let (gear-1 gear-2 gear-3)
    ;;(light-fv +light0+ +position+ '(5.0s0 5.0s0 10.0s0 0.0s0))
    ;;(enable +cull-face+)
    ;;(enable +lighting+)
    ;;(enable +light0+)
    ;;(enable +depth-test+)

    ;; Make the gears.
    (setf gear-1 (gen-lists 1))
    (new-list gear-1 +compile+)
    (material-fv +front+ +ambient-and-diffuse+ '(0.8s0 0.1s0 0.0s0 1.0s0))
    (gear 1.0s0 4.0s0 1.0s0 20 0.7s0)
    (end-list)

    (setf gear-2 (gen-lists 1))
    (new-list gear-2 +compile+)
    (material-fv +front+ +ambient-and-diffuse+ '(0.0s0 0.8s0 0.2s0 1.0s0))
    (gear 0.5s0 2.0s0 2.0s0 10 0.7s0)
    (end-list)

    (setf gear-3 (gen-lists 1))
    (new-list gear-3 +compile+)
    (material-fv +front+ +ambient-and-diffuse+ '(0.2s0 0.2s0 1.0s0 1.0s0))
    (gear 1.3s0 2.0s0 0.5s0 10 0.7s0)
    (end-list)

    ;;(enable +normalize+)

    (values gear-1 gear-2 gear-3)))


(defun gears* (display window)
  (declare (ignore display window))

  (enable +cull-face+)
  (enable +lighting+)
  (enable +light0+)
  (enable +normalize+)
  (enable +depth-test+)

  (reshape 300 300)

  ;;(light-fv +light0+ +position+ #(5.0s0 5.0s0 10.0s0 0.0s0))

  (let (list)
    (declare (ignore list))
    #-(and)
    (progn
      (setf list (gen-lists 1))
      (new-list list +compile+)
      ;;(material-fv +front+ +ambient-and-diffuse+ '(0.8s0 0.1s0 0.0s0 1.0s0))
      (gear 1.0s0 4.0s0 1.0s0 20 0.7s0)
      (xlib/glx:render)
      (end-list))


    (loop
       ;;for angle from 0.0s0 below 361.0s0 by 1.0s0
       with angle of-type single-float = 0.0s0
       with dt = 0.004s0
       repeat 2500
       do (progn

            (incf angle (* 70.0s0 dt))       ; 70 degrees per second
            (when (< 3600.0s0 angle)
              (decf angle 3600.0s0))

            (clear (logior +color-buffer-bit+ +depth-buffer-bit+))

            (push-matrix)
            (rotate-f 20.0s0 0.0s0 1.0s0 0.0s0)


            (push-matrix)
            (translate-f -3.0s0 -2.0s0 0.0s0)
            (rotate-f angle 0.0s0 0.0s0 1.0s0)
            (material-fv +front+ +ambient-and-diffuse+ '(0.8s0 0.1s0 0.0s0 1.0s0))
            (gear 1.0s0 4.0s0 1.0s0 20 0.7s0)
            (pop-matrix)

            
            (push-matrix)
            (translate-f 3.1s0 -2.0s0 0.0s0)
            (rotate-f (- (* angle -2.0s0) 9.0s0) 0.0s0 0.0s0 1.0s0)
            (material-fv +front+ +ambient-and-diffuse+ '(0.0s0 0.8s0 0.2s0 1.0s0))
            (gear 0.5s0 2.0s0 2.0s0 10 0.7s0)
            (pop-matrix)


            (push-matrix)
            (translate-f -3.1s0 4.2s0 0.0s0)
            (rotate-f (- (* angle -2.s0) 25.0s0) 0.0s0 0.0s0 1.0s0)
            (material-fv +front+ +ambient-and-diffuse+ '(0.2s0 0.2s0 1.0s0 1.0s0))
            (gear 1.3s0 2.0s0 0.5s0 10 0.7s0)
            (pop-matrix)


            (pop-matrix)

            (xlib/glx:swap-buffers)
            ;;(sleep 0.025)
            )))
  

  ;;(sleep 3)
  )


(defun gears (display window)
  (declare (ignore window))
  (let ((view-rotx 20.0s0)
        (view-roty 30.0s0)
        (view-rotz 0.0s0)
        (angle 0.0s0)
        (frames 0)
        (dt 0.004s0)                ; *** This is dynamically adjusted
        ;;(t-rot-0 -1.0d0)
        ;;(t-rate-0 -1.d0)
        gear-1 gear-2 gear-3)

    (multiple-value-setq (gear-1 gear-2 gear-3)
      (init))

    (loop
       (event-case (display :timeout 0.01 :force-output-p t)
         (configure-notify (width height)
                           (reshape width height)
                           t)
         (key-press (code)
                    (format t "Key pressed: ~S~%" code)
                    (return-from gears t)))

       (incf angle (* 70.0s0 dt))       ; 70 degrees per second
       (when (< 3600.0s0 angle)
         (decf angle 3600.0s0))

       (draw gear-1 gear-2 gear-3 view-rotx view-roty view-rotz angle)
       (xlib/glx:swap-buffers)
       
       (incf frames)

       ;; FPS calculation goes here
       )))
