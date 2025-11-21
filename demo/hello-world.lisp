(defpackage #:xlib-demo/hello-world
  (:use :common-lisp :xlib :xlib-demo/demos-new)
  (:export #:hello-world))

(in-package :xlib-demo/hello-world)

(defun hello-world ()
  (with-x11-context ()
    (let* ((string "Hello, World!")
           (border 1)                    ; Minimum margin around the text
           (width (+ (text-width *font* string) (* 2 border)))
           (height (+ (max-char-ascent *font*)
                      (max-char-descent *font*)
                      (* 2 border)))
           (x (truncate (- (screen-width *screen*) width) 2))
           (y (truncate (- (screen-height *screen*) height) 2))
           (window (create-window
                    :parent (screen-root *screen*)
                    :x x :y y :width width :height height
                    :background *black-pixel*
                    :border *white-pixel*
                    :border-width 1
                    :colormap (screen-default-colormap *screen*)
                    :bit-gravity :center
                    :event-mask '(:exposure :button-press)))
           (gcontext (create-gcontext :drawable window
                                      :background *black-pixel*
                                      :foreground *white-pixel*
                                      :font *font*)))
      ;; Set window manager hints
      (set-wm-properties window
                         :name 'hello-world
                         :icon-name string
                         :resource-name string
                         :resource-class 'hello-world
                         ;; :command (list 'hello-world)
                         :x x :y y :width width :height height
                         :min-width width :min-height height
                         :input :off :initial-state :normal)
      (map-window window)                ; Map the window
      ;; Handle events
      (event-case (*display* :discard-p t :force-output-p t)
        (exposure ;; Come here on exposure events
         (window count)
         (locally (declare (type xlib:card8 count))
           (when (zerop count) ;; Ignore all but the last exposure event
             (with-state (window)
               (let ((x (truncate (- (drawable-width window) width) 2))
                     (y (truncate (- (+ (drawable-height window)
                                        (max-char-ascent *font*))
                                     (max-char-descent *font*))
                                  2)))
                 ;; Draw text centered in widnow
                 (clear-area window)
                 (draw-glyphs window gcontext x y string)))
             ;; Returning non-nil causes event-case to exit
             nil)))
        ;; Pressing any mouse-button exits
        (button-press () t)))))

(push (make-demo :name "Hello, World!" :function #'hello-world)
      *demos*)
