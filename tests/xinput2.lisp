;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XLIB-TEST-XINPUT2; -*-

(fiasco:define-test-package (#:xlib-test-xinput2 :in xlib-test:xlib-all-tests))
(in-package #:xlib-test-xinput2)

(defun fake-display (byte-order)
  (let ((display (xlib::make-display-internal)))
    (setf (xlib:display-byte-order display) byte-order
          (xlib::display-extension-alist display)
          '((:|XInputExtension| 131 0 0)))
    display))

(defun raw-key-data (little-endian-p)
  (make-array 18 :element-type '(unsigned-byte 8)
              :initial-contents
              (if little-endian-p
                  '(52 18 120 86 52 18 239 190 173 222 120 86 3 0 165 165 165 165)
                  '(18 52 18 52 86 120 222 173 190 239 86 120 0 3 165 165 165 165))))

(deftest raw-key-event-decoding ()
  (let ((display (fake-display :lsbfirst))
        (data (raw-key-data t)))
    (let ((event (xlib/xinput2:decode-raw-key-event
                  display 131 xlib/xinput2:+raw-key-press+ data)))
      (is (typep event 'xlib/xinput2:raw-key-event))
      (is (= #x1234 (xlib/xinput2:raw-key-event-deviceid event)))
      (is (= #x12345678 (xlib/xinput2:raw-key-event-time event)))
      (is (= #xdeadbeef (xlib/xinput2:raw-key-event-detail event)))
      (is (= #x5678 (xlib/xinput2:raw-key-event-sourceid event)))
      (is (= 3 (xlib/xinput2:raw-key-event-valuators-len event)))
      (is (= #xa5a5a5a5 (xlib/xinput2:raw-key-event-flags event)))
      (is (eq data (xlib/xinput2:raw-key-event-data event))))))

(deftest raw-key-event-big-endian-decoding ()
  (let ((display (fake-display :msbfirst))
        (data (raw-key-data nil)))
    (let ((event (xlib/xinput2:decode-raw-key-event
                  display 131 xlib/xinput2:+raw-key-release+ data)))
      (is (= #x1234 (xlib/xinput2:raw-key-event-deviceid event)))
      (is (= #x12345678 (xlib/xinput2:raw-key-event-time event)))
      (is (= #xdeadbeef (xlib/xinput2:raw-key-event-detail event)))
      (is (= #x5678 (xlib/xinput2:raw-key-event-sourceid event)))
      (is (= 3 (xlib/xinput2:raw-key-event-valuators-len event)))
      (is (= #xa5a5a5a5 (xlib/xinput2:raw-key-event-flags event))))))

(deftest raw-key-mask ()
  (is (= #x6000 (xlib/xinput2::%raw-key-mask))))

(deftest raw-key-event-rejection ()
  (let ((display (fake-display :lsbfirst)))
    (is (null (xlib/xinput2:decode-raw-key-event
               display 130 xlib/xinput2:+raw-key-press+ (raw-key-data t))))
    (is (null (xlib/xinput2:decode-raw-key-event
               display 131 12 (raw-key-data t))))
    (is (null (xlib/xinput2:decode-raw-key-event
               display 131 xlib/xinput2:+raw-key-release+
               (make-array 3 :element-type '(unsigned-byte 8)
                             :initial-contents '(0 1 2)))))))

(deftest live-xinput2-selection ()
  ;; Keep connection setup outside the protocol assertions.  A source
  ;; checkout may be tested without an X server; once a display is open,
  ;; request/reply and finish errors must fail the test.
  (let ((display (handler-case (xlib:open-default-display)
                   (error () (fiasco:skip)))))
    (unwind-protect
         (multiple-value-bind (opcode first-event first-error)
             (xlib:query-extension display "XInputExtension")
           (declare (ignore first-event first-error))
           (unless opcode
             (fiasco:skip))
           (multiple-value-bind (major minor)
               (xlib/xinput2:query-version display)
             (unless (or (> major 2)
                         (and (= major 2) (>= minor 1)))
               (fiasco:skip))
             (let ((root (xlib:screen-root
                          (xlib:display-default-screen display))))
               (xlib/xinput2:select-raw-key-events root)
               ;; Finish rather than merely force output: protocol errors
               ;; from malformed XI2 request encoding must surface here.
               (xlib:display-finish-output display)
               (xlib/xinput2:clear-raw-key-events root)
               (xlib:display-finish-output display))))
      (xlib:close-display display))))
