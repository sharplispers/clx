(defpackage #:xlib-demo/qix
  (:use :common-lisp :xlib :xlib-demo/demos)
  (:export #:qix))

(in-package :xlib-demo/qix)

(defstruct qix
  buffer
  (dx1 5)
  (dy1 10)
  (dx2 10)
  (dy2 5))

(defun construct-qix (length)
  (let ((qix (make-qix)))
    (setf (qix-buffer qix) (make-circular-list length))
    qix))

(defun make-circular-list (length)
  (let ((l (make-list length)))
    (rplacd (last l) l)))


(defun qix (&optional (lengths '(30 30)) (duration 2000))
  "Each length is the number of lines to put in a qix, and that many qix
  (of the correct size) are put up on the screen.  Lets the qix wander around
  the screen for Duration steps."
  (with-x11-context ()
    (let ((window (create-window
                   :parent (screen-root *screen*)
                   :x 0 :y 0 :width 700 :height 700
                   :background *white-pixel*))
          (histories (mapcar #'construct-qix lengths)))
      (xlib:set-wm-properties window
                              :name "Qix: hypnotic wandering lines")
      (xlib:map-window window)
      (xlib:display-force-output *display*)
      (multiple-value-bind (width height) (full-window-state window)
        (declare (fixnum width height))
        (xlib:clear-area window)
        (xlib:display-force-output *display*)
        (do ((h histories (cdr h))
             (l lengths (cdr l)))
            ((null h))
          (do ((x (qix-buffer (car h)) (cdr x))
               (i 0 (1+ i)))
              ((= i (car l)))
            (rplaca x (make-array 4))))
        ;; Start each qix at a random spot on the screen.
        (dolist (h histories)
          (let ((x (random width))
                (y (random height)))
            (rplaca (qix-buffer h)
                    (make-array 4 :initial-contents (list x y x y)))))
        (rplacd (last histories) histories)
        (let ((x1 0) (y1 0) (x2 0) (y2 0)
              (dx1 0) (dy1 0) (dx2 0) (dy2 0)
              tem line next-line qix
              (gc (xlib:create-gcontext :drawable window
                                        :foreground *white-pixel*
                                        :background *black-pixel*
                                        :line-width 0 :line-style :solid
                                        :function boole-c2)))
          (declare (fixnum x1 y1 x2 y2 dx1 dy1 dx2 dy2))
          (dotimes (i duration)
            ;; Line is the next line in the next qix. Rotate this qix and
            ;; the qix ring.
            (setq qix (car histories))
            (setq line (car (qix-buffer qix)))
            (setq next-line (cadr (qix-buffer qix)))
            (setf (qix-buffer qix) (cdr (qix-buffer qix)))
            (setq histories (cdr histories))
            (setf x1 (svref line 0))
            (setf y1 (svref line 1))
            (setf x2 (svref line 2))
            (setf y2 (svref line 3))
            (xlib:draw-line window gc x1 y1 x2 y2)
            (setq dx1 (- (+ (qix-dx1 qix) (random 3)) 1))
            (setq dy1 (- (+ (qix-dy1 qix) (random 3)) 1))
            (setq dx2 (- (+ (qix-dx2 qix) (random 3)) 1))
            (setq dy2 (- (+ (qix-dy2 qix) (random 3)) 1))
            (cond ((> dx1 10) (setq dx1 10))
                  ((< dx1 -10) (setq dx1 -10)))
            (cond ((> dy1 10) (setq dy1 10))
                  ((< dy1 -10) (setq dy1 -10)))
            (cond ((> dx2 10) (setq dx2 10))
                  ((< dx2 -10) (setq dx2 -10)))
            (cond ((> dy2 10) (setq dy2 10))
                  ((< dy2 -10) (setq dy2 -10)))
            (cond ((or (>= (setq tem (+ x1 dx1)) width) (minusp tem))
                   (setq dx1 (- dx1))))
            (cond ((or (>= (setq tem (+ x2 dx2)) width) (minusp tem))
                   (setq dx2 (- dx2))))
            (cond ((or (>= (setq tem (+ y1 dy1)) height) (minusp tem))
                   (setq dy1 (- dy1))))
            (cond ((or (>= (setq tem (+ y2 dy2)) height) (minusp tem))
                   (setq dy2 (- dy2))))
            (setf (qix-dy2 qix) dy2)
            (setf (qix-dx2 qix) dx2)
            (setf (qix-dy1 qix) dy1)
            (setf (qix-dx1 qix) dx1)
            `   (when (svref next-line 0)
                  (xlib:draw-line window gc
                                  (svref next-line 0) (svref next-line 1)
                                  (svref next-line 2) (svref next-line 3)))
            (setf (svref next-line 0) (+ x1 dx1))
            (setf (svref next-line 1) (+ y1 dy1))
            (setf (svref next-line 2) (+ x2 dx2))
            (setf (svref next-line 3) (+ y2 dy2))
            (xlib:display-force-output *display*)
            (sleep (/ *delay* 100))))))))


(push (make-demo :name "Qix" :function #'qix) *demos*)

