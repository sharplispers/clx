(defpackage #:xlib-demo/petal
  (:use :common-lisp :xlib :xlib-demo/demos)
  (:export #:petal))

(in-package :xlib-demo/petal)


;;; Fast sine constants:

(defconstant d360 #o5500)
(defconstant d270 #o4160)
(defconstant d180 #o2640)
(defconstant d90 #o1320)
(defconstant vecmax 2880)

(defparameter sin-array
  '#(#o0 #o435 #o1073 #o1531 #o2166 #o2623 #o3260
     #o3714 #o4350 #o5003 #o5435 #o6066 #o6516 #o7145
     #o7573 #o10220 #o10644 #o11266 #o11706 #o12326
     #o12743 #o13357 #o13771 #o14401 #o15007 #o15414
     #o16016 #o16416 #o17013 #o17407 #o20000 #o20366
     #o20752 #o21333 #o21711 #o22265 #o22636 #o23204
     #o23546 #o24106 #o24443 #o24774 #o25323 #o25645
     #o26165 #o26501 #o27011 #o27316 #o27617 #o30115
     #o30406 #o30674 #o31156 #o31434 #o31706 #o32154
     #o32416 #o32654 #o33106 #o33333 #o33554 #o33771
     #o34202 #o34406 #o34605 #o35000 #o35167 #o35351
     #o35526 #o35677 #o36043 #o36203 #o36336 #o36464
     #o36605 #o36721 #o37031 #o37134 #o37231 #o37322
     #o37407 #o37466 #o37540 #o37605 #o37646 #o37701
     #o37730 #o37751 #o37766 #o37775 #o40000))

(defmacro psin (val)
  `(let* ((val ,val)
          neg
          frac
          sinlo)
     (if (>= val d180)
         (setq neg t
               val (- val d180)))
     (if (>= val d90)
         (setq val (- d180 val)))
     (setq frac (logand val 7))
     (setq val (ash val -3))
     ;;
     (setq sinlo (if (>= val 90)
                     (svref sin-array 90)
                     (svref sin-array val)))
     ;;
     (if (< val 90)
         (setq sinlo
               (+ sinlo (ash (* frac (- (svref sin-array (1+ val)) sinlo))
                             -3))))
     ;;
     (if neg
         (- sinlo)
         sinlo)))

(defmacro pcos (x)
  `(let ((tmp (- ,x d270)))
     (psin (if (minusp tmp) (+ tmp d360) tmp))))


;;;; Miscellaneous petal hackery.

(defmacro high-16bits-* (a b)
  `(let ((a-h (ash ,a -8))
         (b-h (ash ,b -8)))
     (+ (* a-h b-h)
        (ash (* a-h (logand ,b 255)) -8)
        (ash (* b-h (logand ,a 255)) -8))))

(defun complete (style petal)
  (let ((repnum 1)
        factor cntval needed)
    (dotimes (i 3)
      (case i
        (0 (setq factor 2 cntval 6))
        (1 (setq factor 3 cntval 2))
        (2 (setq factor 5 cntval 1)))
      (do ()
          ((or (minusp cntval) (not (zerop (rem style factor)))))
        (setq repnum (* repnum factor))
        (setq cntval (1- cntval))
        (setq style (floor style factor))))
    (setq needed (floor vecmax repnum))
    (if (and (not (oddp needed)) (oddp petal)) (floor needed 2) needed)))


;;;; Petal Parameters and Petal itself

(defparameter continuous t)
(defparameter styinc 2)
(defparameter petinc 1)
(defparameter scalfac-fac 8192)

(defun petal (&optional (how-many 10) (style 0) (petal 0))
  (with-x11-context ()
    (let* ((width 512)
           (height 512)
           (petal-window (create-window
                          :parent (screen-root *screen*)
                          :x 200 :y 200 :width width :height height
                          :background *white-pixel*)))
      (xlib:set-wm-properties petal-window
                              :name "Petal: flower-like display")
      (xlib:map-window petal-window)
      (xlib:clear-area petal-window)
      (xlib:display-force-output *display*)
      (let ((veccnt 0)
            (nustyle 722)
            (nupetal 3)
            (scalfac (1+ (floor scalfac-fac (min width height))))
            (ctrx (floor width 2))
            (ctry (floor height 2))
            (tt 0)
            (s 0)
            (lststyle 0)
            (lstpetal 0)
            (petstyle 0)
            (vectors 0)
            (r 0)
            (x1 0)
            (y1 0)
            (x2 0)
            (y2 0)
            (i 0)
            (gc (xlib:create-gcontext :drawable petal-window
                                      :foreground *black-pixel*
                                      :background *white-pixel*
                                      :line-width 0 :line-style :solid)))
        (loop
          (when (zerop veccnt)
            (setq tt 0 s 0 lststyle style lstpetal petal petal nupetal
                  style nustyle petstyle (rem (* petal style) d360)
                  vectors (complete style petal))
            (when continuous
              (setq nupetal  (+ nupetal petinc)
                    nustyle (+ nustyle styinc)))
            (when (or (/= lststyle style) (/= lstpetal petal))
              (xlib:clear-area petal-window)
              (xlib:display-force-output *display*)))
          (when (or (/= lststyle style) (/= lstpetal petal))
            (setq veccnt (1+ veccnt) i veccnt x1 x2 y1 y2
                  tt (rem (+ tt style) d360)
                  s (rem (+ s petstyle) d360)
                  r (pcos s))
            (setq x2 (+ ctrx (floor (high-16bits-* (pcos tt) r) scalfac))
                  y2 (+ ctry (floor (high-16bits-* (psin tt) r) scalfac)))
            (when (/= i 1)
              (xlib:draw-line petal-window gc x1 y1 x2 y2)
              (xlib:display-force-output *display*)))
          (when (> veccnt vectors)
            (setq veccnt 0)
            (setq how-many (1- how-many))
            (sleep 2)
            (when (zerop how-many) (return))))))))

(push (make-demo :name "Petal" :function #'petal) *demos*)

