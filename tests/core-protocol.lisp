(fiasco:define-test-package (#:xlib-test-displays :in xlib-test:xlib-all-tests)
  (:documentation "Tests for `3. Displays' section of the manual."))
(in-package #:xlib-test-displays)

;;; Manual notes:
;;;
;;; - Manual claims xlib:display-authorization-data returns a string. Either
;;;   review manual or fix this;
;;;
;;; - xlib:display-error-handler documentation says, that it returns function,
;;;   yet by default it returns symbol (denoting a function) [minor];
;;;
;;; - xlib:display-error-handler documentation has broken reference "See
;;;   <undefined> [Errors], page <undefined>";
;;;
;;; - `xlib:display-vendor' returns 2 values, second is release. It is said,
;;;   that second value type card16, but it is card32. Also it is mentioned,
;;;   that function to probe second value is named `xlib:display-release-number'
;;;   (and this is implemented), but later we have documentation for
;;;   `xlib:display-version-number' – function which doesn't exist;
;;;
;;; - `xlib:display-xid' is documented to return a function, but default value
;;;   is a symbol (denoting a function) [minor];

;;; This test will fail the day "FOO" extension is written.
(deftest display-protocol ()
  "Opens display, checks its attributes and closes it."
  (let ((display (xlib:open-default-display)))
    (is (null (xlib:query-extension display "FOO")))
    (is (typep (xlib:display-authorization-data display) 'string))
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
