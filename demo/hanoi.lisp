(defpackage #:xlib-demo/hanoi
  (:use :common-lisp :xlib :xlib-demo/demos-new)
  (:export #:hanoi))

(in-package :xlib-demo/hanoi)

;;;; Hanoi.

;;; Random parameters:

(defparameter disk-thickness 15 "The thickness of a disk in pixels.")
(defparameter disk-spacing (+ disk-thickness 3)
  "The amount of vertical space used by a disk on a needle.")
(defvar *horizontal-velocity* 20 "The speed at which disks slide sideways.")
(defvar *vertical-velocity* 12 "The speed at which disks move up and down.")

;;; These variables are bound by the main function.

(defvar *hanoi-window* () "The window that Hanoi is happening on.")
(defvar *hanoi-window-height* () "The height of the viewport Hanoi is happening on.")
(defvar *transfer-height* () "The height at which disks are transferred.")
(defvar *hanoi-gcontext* () "The graphics context for Hanoi under X11.")

;;; Needle Functions

(defstruct disk
  size)

(defstruct needle
  position
  disk-stack)

;;; Needle-Top-Height returns the height of the top disk on NEEDLE.

(defun needle-top-height (needle)
  (- *hanoi-window-height*
     (* disk-spacing (length (the list (needle-disk-stack needle))))))

(defvar available-disks
  (do ((i 10 (+ i 10))
       (dlist () (cons (make-disk :size i) dlist)))
      ((> i 80) dlist)))

(defvar needle-1 (make-needle :position 184))
(defvar needle-2 (make-needle :position 382))
(defvar needle-3 (make-needle :position 584))

;;; Graphic interface abstraction:

;;; Invert-Rectangle calls the CLX function draw-rectangle with "fill-p"
;;; set to T.  Update-Screen forces the display output.
;;;
(defmacro invert-rectangle (x y height width)
  `(xlib:draw-rectangle *hanoi-window* *hanoi-gcontext*
                        ,x ,y ,width ,height t))

(defmacro update-screen ()
  `(xlib:display-force-output *display*))


;;;; Moving disks up and down

;;; Slide-Up slides the image of a disk up from the coordinates X,
;;; START-Y to the point X, END-Y.  DISK-SIZE is the size of the disk to
;;; move.  START-Y must be greater than END-Y

(defun slide-up (start-y end-y x disk-size)
  (multiple-value-bind (number-moves pixels-left)
      (truncate (- start-y end-y) *vertical-velocity*)
    (do ((x (- x disk-size))
         (width (* disk-size 2))
         (old-y start-y (- old-y *vertical-velocity*))
         (new-y (- start-y *vertical-velocity*) (- new-y *vertical-velocity*))
         (number-moves number-moves (1- number-moves)))
        ((zerop number-moves)
         (when (plusp pixels-left)
           (invert-rectangle x (- old-y pixels-left) disk-thickness width)
           (invert-rectangle x old-y disk-thickness width)
           (update-screen)))
      ;; Loop body writes disk at new height & erases at old height.
      (invert-rectangle x old-y disk-thickness width)
      (invert-rectangle x new-y disk-thickness width)
      (update-screen))))

;;; Slide-Down slides the image of a disk down from the coordinates X,
;;; START-Y to the point X, END-Y.  DISK-SIZE is the size of the disk to
;;; move.  START-Y must be less than END-Y.

(defun slide-down (start-y end-y x disk-size)
  (multiple-value-bind (number-moves pixels-left)
      (truncate (- end-y start-y) *vertical-velocity*)
    (do ((x (- x disk-size))
         (width (* disk-size 2))
         (old-y start-y (+ old-y *vertical-velocity*))
         (new-y (+ start-y *vertical-velocity*) (+ new-y *vertical-velocity*))
         (number-moves number-moves (1- number-moves)))
        ((zerop number-moves)
         (when (plusp pixels-left)
           (invert-rectangle x (+ old-y pixels-left) disk-thickness width)
           (invert-rectangle x old-y disk-thickness width)
           (update-screen)))
      ;; Loop body writes disk at new height & erases at old height.
      (invert-rectangle X old-y disk-thickness width)
      (invert-rectangle X new-y disk-thickness width)
      (update-screen))))


;;;; Lifting and Droping Disks

;;; Lift-disk pops the top disk off of needle and raises it up to the
;;; transfer height.  The disk is returned.

(defun lift-disk (needle)
  "Pops the top disk off of NEEDLE, Lifts it above the needle, & returns it."
  (let* ((height (needle-top-height needle))
         (disk (pop (needle-disk-stack needle))))
    (slide-up height
              *transfer-height*
              (needle-position needle)
              (disk-size disk))
    disk))

;;; Drop-disk drops a disk positioned over needle at the transfer height
;;; onto needle.  The disk is pushed onto needle.

(defun drop-disk (disk needle)
  "DISK must be positioned above NEEDLE.  It is dropped onto NEEDLE."
  (push disk (needle-disk-stack needle))
  (slide-down *transfer-height*
              (needle-top-height needle)
              (needle-position needle)
              (disk-size disk))
  t)


;;; Drop-initial-disk is the same as drop-disk except that the disk is
;;; drawn once before dropping.

(defun drop-initial-disk (disk needle)
  "DISK must be positioned above NEEDLE.  It is dropped onto NEEDLE."
  (let* ((size (disk-size disk))
         (lx (- (needle-position needle) size)))
    (invert-rectangle lx *transfer-height* disk-thickness (* size 2))
    (push disk (needle-disk-stack needle))
    (slide-down *transfer-height*
                (needle-top-height needle)
                (needle-position needle)
                (disk-size disk))
    t))


;;;; Sliding Disks Right and Left

;;; Slide-Right slides the image of a disk located at START-X, Y to the
;;; position END-X, Y.  DISK-SIZE is the size of the disk.  START-X is
;;; less than END-X.

(defun slide-right (start-x end-x Y disk-size)
  (multiple-value-bind (number-moves pixels-left)
      (truncate (- end-x start-x) *horizontal-velocity*)
    (do ((right-x (+ start-x disk-size) (+ right-x *horizontal-velocity*))
         (left-x  (- start-x disk-size) (+ left-x  *horizontal-velocity*))
         (number-moves number-moves (1- number-moves)))
        ((zerop number-moves)
         (when (plusp pixels-left)
           (invert-rectangle right-x Y disk-thickness pixels-left)
           (invert-rectangle left-x  Y disk-thickness pixels-left)
           (update-screen)))
      ;; Loop body adds chunk *horizontal-velocity* pixels wide to right
      ;; side of disk, then chops off left side.
      (invert-rectangle right-x Y disk-thickness *horizontal-velocity*)
      (invert-rectangle left-x Y disk-thickness *horizontal-velocity*)
      (update-screen))))

;;; Slide-Left is the same as Slide-Right except that START-X is greater
;;; than END-X.

(defun slide-left (start-x end-x Y disk-size)
  (multiple-value-bind (number-moves pixels-left)
      (truncate (- start-x end-x) *horizontal-velocity*)
    (do ((right-x (- (+ start-x disk-size) *horizontal-velocity*)
                  (- right-x *horizontal-velocity*))
         (left-x  (- (- start-x disk-size) *horizontal-velocity*)
                  (- left-x  *horizontal-velocity*))
         (number-moves number-moves (1- number-moves)))
        ((zerop number-moves)
         (when (plusp pixels-left)
           (setq left-x  (- (+ left-x  *horizontal-velocity*) pixels-left))
           (setq right-x (- (+ right-x *horizontal-velocity*) pixels-left))
           (invert-rectangle left-x  Y disk-thickness pixels-left)
           (invert-rectangle right-x Y disk-thickness pixels-left)
           (update-screen)))
      ;; Loop body adds chunk *horizontal-velocity* pixels wide to left
      ;; side of disk, then chops off right side.
      (invert-rectangle left-x  Y disk-thickness *horizontal-velocity*)
      (invert-rectangle right-x Y disk-thickness *horizontal-velocity*)
      (update-screen))))


;;;; Transferring Disks

;;; Transfer disk slides a disk at the transfer height from a position
;;; over START-NEEDLE to a position over END-NEEDLE.  Modified disk is
;;; returned.

(defun transfer-disk (disk start-needle end-needle)
  "Moves DISK from a position over START-NEEDLE to a position over END-NEEDLE."
  (let ((start (needle-position start-needle))
        (end (needle-position end-needle)))
    (if (< start end)
        (slide-right start end *transfer-height* (disk-size disk))
        (slide-left start end *transfer-height* (disk-size disk)))
    disk))


;;; Move-One-Disk moves the top disk from START-NEEDLE to END-NEEDLE.

(defun move-one-disk (start-needle end-needle)
  "Moves the disk on top of START-NEEDLE to the top of END-NEEDLE."
  (drop-disk (transfer-disk (lift-disk start-needle)
                            start-needle
                            end-needle)
             end-needle)
  (sleep *delay*)
  t)

;;; Move-N-Disks moves the top N disks from START-NEEDLE to END-NEEDLE
;;; obeying the rules of the towers of hannoi problem.  To move the
;;; disks, a third needle, TEMP-NEEDLE, is needed for temporary storage.

(defun move-n-disks (n start-needle end-needle temp-needle)
  "Moves the top N disks from START-NEEDLE to END-NEEDLE.
   Uses TEMP-NEEDLE for temporary storage."
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
               (*transfer-height* (- height (* disk-spacing n)))
               (*hanoi-gcontext* (xlib:create-gcontext :drawable *hanoi-window*
                                                       :foreground *white-pixel*
                                                       :background *black-pixel*
                                                       :fill-style :solid
                                                       :function boole-c2)))
          (xlib:set-wm-properties window :name title)
          (xlib:clear-area *hanoi-window*)
          (xlib:map-window window)
          (xlib:display-force-output *display*)
          (let ((needle-1 (make-needle :position 184))
                (needle-2 (make-needle :position 382))
                (needle-3 (make-needle :position 584)))
            (setf (needle-disk-stack needle-1) ())
            (setf (needle-disk-stack needle-2) ())
            (setf (needle-disk-stack needle-3) ())
            (do ((n n (1- n))
                 (available-disks available-disks (cdr available-disks)))
                ((zerop n))
              (drop-initial-disk (car available-disks) needle-1))
            (move-n-disks n needle-1 needle-3 needle-2)
            t)))))
  )

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

