(fiasco:define-test-package (#:xlib-test-displays :in xlib-test:xlib-all-tests)
  (:documentation "Tests the core protocol."))
(in-package #:xlib-test-displays)

;;; Manual notes:
;;;
;;; - xlib:display-error-handler documentation has broken reference "See
;;;   <undefined> [Errors], page <undefined>";

;;; This test will fail the day "FOO" extension is written.
(deftest display-protocol ()
  "Opens display, checks its attributes and closes it."
  (let ((display (xlib:open-default-display)))
    (is (null (xlib:query-extension display "FOO")))
    (is (typep (xlib:display-authorization-data display) 'vector))
    (is (typep (xlib:display-authorization-name display) 'string))
    (is (typep (xlib:display-bitmap-format display) 'xlib:bitmap-format))
    (is (typep (xlib:display-byte-order display) '(member :lsbfirst :msbfirst)))
    (is (typep (xlib:display-display display) 'integer))
    (is (typep (xlib:display-error-handler display) '(or function symbol)))
    (is (typep (xlib:display-image-lsb-first-p display) 'boolean))
    (multiple-value-bind (min-keycode max-keycode) (xlib:display-keycode-range display)
      (is (typep min-keycode 'xlib:card8))
      (is (typep max-keycode 'xlib:card8))
      (is (= min-keycode (xlib:display-min-keycode display)))
      (is (= max-keycode (xlib:display-max-keycode display))))
    (let ((max-request-size (xlib:display-max-request-length display)))
      (is (>= max-request-size 4096))
      (is (typep max-request-size 'xlib:card16)))
    (is (typep (xlib:display-motion-buffer-size display) 'xlib:card32))
    (is (typep (xlib:display-nscreens display) 'integer))
    (is (xlib:display-p display))
    (is (not (xlib:display-p :not-a-display)))
    (is (every #'xlib:pixmap-format-p (xlib:display-pixmap-formats display)))
    ;; display-plist
    (finishes (setf (getf (xlib:display-plist display) :foo) :bar))
    (is (eql :bar (getf (xlib:display-plist display) :foo)))
    (finishes (remf (xlib:display-plist display) :foo))
    (is (eql nil (getf (xlib:display-plist display) :foo)))
    (multiple-value-bind (major minor) (xlib:display-protocol-version display)
      (is (typep minor 'xlib:card16))
      (is (typep major 'xlib:card16))
      (is (= minor (xlib:display-protocol-minor-version display)))
      (is (= major (xlib:display-protocol-major-version display))))
    (is (typep (xlib:display-resource-id-base display) 'xlib:resource-id))
    (is (typep (xlib:display-resource-id-mask display) 'xlib:resource-id))
    (is (every #'xlib:screen-p (xlib:display-roots display)))
    (multiple-value-bind (name release) (xlib:display-vendor display)
      (is (typep name 'string))
      (is (typep release 'xlib:card32))
      (is (string= name (xlib:display-vendor-name display)))
      (is (= release (xlib:display-release-number display))))
    (is (typep (xlib:display-xid display) '(or function symbol)))
    ;; dummy test
    (let ((count 0))
      (finishes (setf (xlib:display-after-function display)
                      (lambda (display)
                        (declare (ignore display))
                        (incf count)))
                (xlib:with-display (display)
                  (xlib:query-extension display "FOO")
                  (xlib:display-finish-output display)
                  (xlib:query-extension display "FOO")
                  (xlib:display-force-output display)))
      (is (<= 2 count)))
    (is (null (xlib:close-display display)))
    ;; We can't query closed display.
    (signals xlib:closed-display (xlib:query-extension display "FOO"))))

(defmacro with-default-display (display &body body)
  `(let ((,display (xlib:open-default-display)))
     (unwind-protect
          (progn ,@body)
       (xlib:close-display ,display))))

(deftest screen-protocol ()
  "Gets the default screen of the default display and validates its attributes."
  (with-default-display display
    (let ((screen (xlib:display-default-screen display)))
      (is (member (xlib:screen-backing-stores screen) '(:always :never :when-mapped)))
      (is (typep (xlib:screen-black-pixel screen) 'xlib:pixel))
      (is (typep (xlib:screen-default-colormap screen) 'xlib:colormap))
      (let ((depths (xlib:screen-depths screen)))
        (loop for depth in depths do
             (is (consp depth))
             (is (< 0 (car depth)))
             (is (listp (cdr depth)))
             (loop for visual in (cdr depth) do
                  (is (typep visual 'xlib:visual-info)))))
      (is (typep (xlib:screen-event-mask-at-open screen) 'xlib:mask32))
      (is (typep (xlib:screen-height screen) 'xlib:card16))
      (is (typep (xlib:screen-height-in-millimeters screen) 'xlib:card16))
      (is (typep (xlib:screen-max-installed-maps screen) 'xlib:card16))
      (is (typep (xlib:screen-min-installed-maps screen) 'xlib:card16))

      ;; Test whether we can insert and retrieve a property.
      (is (xlib:screen-p screen))
      (is (typep (xlib:screen-plist screen) 'list))
      (finishes (setf (getf (xlib:screen-plist screen) 'foo) "hell is empty"))
      (is (string= "hell is empty" (getf (xlib:screen-plist screen) 'foo)))

      (is (typep (xlib:screen-root screen) '(or null xlib:window)))
      (is (typep (xlib:screen-root-depth screen) 'xlib:image-depth))
      (is (typep (xlib:screen-root-visual screen) 'xlib:card29))
      (is (typep (xlib:screen-save-unders-p screen) 'boolean))
      (is (typep (xlib:screen-white-pixel screen) 'xlib:pixel))
      (is (typep (xlib:screen-width screen) 'xlib:card16))
      (is (typep (xlib:screen-width-in-millimeters screen) 'xlib:card16)))))
