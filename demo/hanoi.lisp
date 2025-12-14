(defpackage #:xlib-demo/hanoi
  (:use :common-lisp :xlib :xlib-demo/demos-new)
  (:export #:hanoi))

(in-package :xlib-demo/hanoi)

;;;; Hanoi.

;;; Random parameters:

(defparameter *disk-height* 15
  "The height of a disk in pixels.")
(defparameter *disk-spacing* (+ *disk-height* 3)
  "The amount of vertical space used by a disk on a needle.")
(defvar *horizontal-velocity* 20
  "The speed at which disks slide sideways.")
(defvar *vertical-velocity* 12
  "The speed at which disks move up and down.")

;;; These variables are bound by the main function.

(defvar *hanoi-window* nil
  "The window that Hanoi is happening on.")
(defvar *hanoi-window-height* nil
  "The height of the viewport Hanoi is happening on.")
(defvar *transfer-height* nil
  "The height at which disks are transferred.")
(defvar *hanoi-gcontext* nil
  "The graphics context for Hanoi under X11.")

;;; Needle Functions

(defstruct disk
  half-width)

(defstruct needle
  position
  (disk-stack ()))

(defun needle-top-height (needle)
  "Returns the height of the top disk on NEEDLE."
  (- *hanoi-window-height*
     (* *disk-spacing* (length (the list (needle-disk-stack needle))))))

;;; Graphic interface abstraction:

(defmacro invert-rectangle (x y half-width height)
  "Calls the CLX function draw-rectangle with FILL-P set to T."
  `(xlib:draw-rectangle *hanoi-window* *hanoi-gcontext*
                        ,x ,y ,half-width ,height t))

(defmacro update-screen ()
  "Forces the display output."
  `(xlib:display-force-output *display*))


;;;; Moving disks up and down

(defun slide-up (start-y end-y x disk-half-width)
  "Slides the image of a disk up from the coordinates (X, START-Y) to the
point (X, END-Y). DISK-HALF-WIDTH is the half-width of the disk to move. START-Y
must be greater than END-Y."
  (multiple-value-bind (number-moves pixels-left)
      (truncate (- start-y end-y) *vertical-velocity*)
    (do ((x (- x disk-half-width))
         (width (* disk-half-width 2))
         (old-y start-y (- old-y *vertical-velocity*))
         (new-y (- start-y *vertical-velocity*) (- new-y *vertical-velocity*))
         (number-moves number-moves (1- number-moves)))
        ((zerop number-moves)
         (when (plusp pixels-left)
           (invert-rectangle x (- old-y pixels-left) width *disk-height*)
           (invert-rectangle x old-y width *disk-height*)
           (update-screen)))
      ;; Loop body writes disk at new height & erases at old height.
      (invert-rectangle x old-y width *disk-height*)
      (invert-rectangle x new-y width *disk-height*)
      (update-screen)
      (sleep (/ *delay* 100)))))

(defun slide-down (start-y end-y x disk-half-width)
  "Slides the image of a disk down from the coordinates (X, START-Y) to the
point (X, END-Y). DISK-HALF-WIDTH is the half-width of the disk to move. START-Y
must be less than END-Y."
  (multiple-value-bind (number-moves pixels-left)
      (truncate (- end-y start-y) *vertical-velocity*)
    (do ((x (- x disk-half-width))
         (width (* disk-half-width 2))
         (old-y start-y (+ old-y *vertical-velocity*))
         (new-y (+ start-y *vertical-velocity*) (+ new-y *vertical-velocity*))
         (number-moves number-moves (1- number-moves)))
        ((zerop number-moves)
         (when (plusp pixels-left)
           (invert-rectangle x (+ old-y pixels-left) width *disk-height*)
           (invert-rectangle x old-y width *disk-height*)
           (update-screen)))
      ;; Loop body writes disk at new height & erases at old height.
      (invert-rectangle X old-y width *disk-height*)
      (invert-rectangle X new-y width *disk-height*)
      (update-screen)
      (sleep (/ *delay* 100)))))


;;;; Lifting and Droping Disks

(defun lift-disk (needle)
  "Pops the top disk off of needle and raises it up to the transfer height.
The disk is returned."
  (let* ((height (needle-top-height needle))
         (disk (pop (needle-disk-stack needle))))
    (slide-up height
              *transfer-height*
              (needle-position needle)
              (disk-half-width disk))
    disk))

(defun drop-disk (disk needle)
  "Drops DISK positioned over NEEDLE at the transfer height onto NEEDLE.
DISK is pushed onto NEEDLE disk stack."
  (push disk (needle-disk-stack needle))
  (slide-down *transfer-height*
              (needle-top-height needle)
              (needle-position needle)
              (disk-half-width disk))
  t)


(defun drop-initial-disk (disk needle)
  "The function is the same as DROP-DISK except that the disk is drawn once
before dropping."
  (let* ((half-width (disk-half-width disk))
         (lx (- (needle-position needle) half-width)))
    (invert-rectangle lx *transfer-height* (* half-width 2) *disk-height*)
    (push disk (needle-disk-stack needle))
    (slide-down *transfer-height*
                (needle-top-height needle)
                (needle-position needle)
                (disk-half-width disk))
    t))


;;;; Sliding Disks Right and Left

(defun slide-right (start-x end-x Y disk-half-width)
  "Slides the image of a disk located at (START-X, Y) to the position
(END-X, Y). DISK-HALF-WIDTH is the half-width of the disk. START-X is
less than END-X."
  (multiple-value-bind (number-moves pixels-left)
      (truncate (- end-x start-x) *horizontal-velocity*)
    (do ((right-x (+ start-x disk-half-width) (+ right-x *horizontal-velocity*))
         (left-x  (- start-x disk-half-width) (+ left-x  *horizontal-velocity*))
         (number-moves number-moves (1- number-moves)))
        ((zerop number-moves)
         (when (plusp pixels-left)
           (invert-rectangle right-x Y pixels-left *disk-height*)
           (invert-rectangle left-x  Y pixels-left *disk-height*)
           (update-screen)))
      ;; Loop body adds chunk *horizontal-velocity* pixels wide to right
      ;; side of disk, then chops off left side.
      (invert-rectangle right-x Y *horizontal-velocity* *disk-height*)
      (invert-rectangle left-x Y *horizontal-velocity* *disk-height*)
      (update-screen)
      (sleep (/ *delay* 100)))))

(defun slide-left (start-x end-x Y disk-half-width)
  "The same as SLIDE-RIGHT except that START-X is greater than END-X."
  (multiple-value-bind (number-moves pixels-left)
      (truncate (- start-x end-x) *horizontal-velocity*)
    (do ((right-x (- (+ start-x disk-half-width) *horizontal-velocity*)
                  (- right-x *horizontal-velocity*))
         (left-x  (- (- start-x disk-half-width) *horizontal-velocity*)
                  (- left-x  *horizontal-velocity*))
         (number-moves number-moves (1- number-moves)))
        ((zerop number-moves)
         (when (plusp pixels-left)
           (setq left-x  (- (+ left-x  *horizontal-velocity*) pixels-left))
           (setq right-x (- (+ right-x *horizontal-velocity*) pixels-left))
           (invert-rectangle left-x  Y pixels-left *disk-height*)
           (invert-rectangle right-x Y pixels-left *disk-height*)
           (update-screen)))
      ;; Loop body adds chunk *horizontal-velocity* pixels wide to left
      ;; side of disk, then chops off right side.
      (invert-rectangle left-x  Y *horizontal-velocity* *disk-height*)
      (invert-rectangle right-x Y *horizontal-velocity* *disk-height*)
      (update-screen)
      (sleep (/ *delay* 100)))))


;;;; Transferring Disks

(defun transfer-disk (disk start-needle end-needle)
  "Slides a disk at the transfer height from a position over START-NEEDLE
to a position over END-NEEDLE. Modified disk is returned."
  "Moves DISK from a position over START-NEEDLE to a position over END-NEEDLE."
  (let ((start (needle-position start-needle))
        (end (needle-position end-needle)))
    (if (< start end)
        (slide-right start end *transfer-height* (disk-half-width disk))
        (slide-left start end *transfer-height* (disk-half-width disk)))
    disk))


(defun move-one-disk (start-needle end-needle)
  "Moves the top disk from START-NEEDLE to END-NEEDLE."
  "Moves the disk on top of START-NEEDLE to the top of END-NEEDLE."
  (drop-disk (transfer-disk (lift-disk start-needle)
                            start-needle
                            end-needle)
             end-needle)
  t)

(defun move-n-disks (n start-needle end-needle temp-needle)
  "Moves the top N disks from START-NEEDLE to END-NEEDLE obeying the rules
of the towers of hannoi problem. To move the disks, a third needle,
TEMP-NEEDLE, is needed for temporary storage."
  (cond ((= n 1)
         (move-one-disk start-needle end-needle))
        (t
         (move-n-disks (1- n) start-needle temp-needle end-needle)
         (move-one-disk start-needle end-needle)
         (move-n-disks (1- n) temp-needle end-needle start-needle)))
  t)


;;;; Hanoi itself.

(defun hanoi (n title x y width height)
  (with-x11-context ()
    (let ((window (create-window
                   :parent (screen-root *screen*)
                   :x x :y y :width width :height height
                   :background *white-pixel*)))
      (multiple-value-bind (width height) (full-window-state window)
        (declare (ignore width))
        (let* ((*hanoi-window* window)
               (*hanoi-window-height* height)
               (*transfer-height* (- height (* *disk-spacing* n) 100))
               (*hanoi-gcontext*
                 (xlib:create-gcontext :drawable *hanoi-window*
                                       :foreground *white-pixel*
                                       :fill-style :solid
                                       :function boole-xor)))
          (xlib:set-wm-properties *hanoi-window* :name title)
          (xlib:clear-area *hanoi-window*)
          (xlib:map-window *hanoi-window*)
          (xlib:display-force-output *display*)
          (let ((needle-1 (make-needle :position 184))
                (needle-2 (make-needle :position 382))
                (needle-3 (make-needle :position 584)))
            (do ((n n (1- n)))
                ((zerop n))
              (sleep *delay*)
              (drop-initial-disk (make-disk :half-width (* n 10)) needle-1))
            (sleep *delay*)
            (move-n-disks n needle-1 needle-3 needle-2)
            (sleep *delay*)
            t))))))

(push (make-demo
       :name "Slow towers of Hanoi"
       :function (lambda ()
                   (let ((*horizontal-velocity* 3)
                         (*vertical-velocity* 1))
                     (hanoi 4 "Slow towers of Hanoi" 0 100 768 300))))
      *demos*)

(push (make-demo
       :name "Fast towers of Hanoi"
       :function (lambda ()
                   (hanoi 7 "Fast towers of Hanoi" 100 200 768 300)))
      *demos*)

