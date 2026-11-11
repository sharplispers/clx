(in-package #:xlib-test-generic-events)

(defun make-test-display ()
  (xlib::make-buffer 160 #'xlib::make-display-internal
                     :host "" :display 0
                     :output-stream nil :input-stream nil))

(defun make-generic-event (length)
  (let* ((size (+ xlib::+replysize+ (* length 4)))
         (event (xlib::make-reply-buffer
                 (if (<= size xlib::+replysize+)
                     xlib::+replysize+
                     (ash 1 (integer-length (1- size)))))))
    (xlib::with-buffer-input (event :sizes (8 16 32))
      (xlib::write-card8 0 35)
      (xlib::write-card8 1 #x83)
      (xlib::write-card16 2 #x1234)
      (xlib::write-card32 4 length)
      (xlib::write-card16 8 #x0201))
    (setf (xlib::reply-size event) 35
          (xlib::reply-data-size event) size)
    event))

(deftest generic-event-size-and-payload ()
  (let ((event (make-generic-event 1))
        (display (make-test-display)))
    (unwind-protect
         (progn
           (setf (aref (xlib::reply-ibuf8 event) 10) 7
                 (aref (xlib::reply-ibuf8 event) 11) 8
                 (aref (xlib::reply-ibuf8 event) 32) 9)
           (let (result data-result)
             (funcall (svref xlib::*event-handler-vector* 35)
                      display event
                      (lambda (&key extension evtype length sequence data
                                 &allow-other-keys)
                        (setf result (list extension evtype length sequence
                                            (coerce data 'list))
                              data-result data)))
             (is (equal result
                        (list 131 513 1 4660
                              (append '(7 8) (make-list 20 :initial-element 0)
                                      '(9 0 0 0)))))
             ;; DATA must not alias the pooled input buffer.
             (is (not (eq data-result (xlib::reply-ibuf8 event))))))
      (xlib::deallocate-event event))))

(deftest generic-event-variable-buffer-reuse ()
  (let ((event (make-generic-event 1)))
    (xlib::deallocate-event event)
    (let ((reused (xlib::allocate-reply-buffer 36)))
      (is (eq event reused))
      (is (= 64 (xlib::reply-size reused)))
      (xlib::deallocate-reply-buffer reused))))

(deftest generic-event-zero-length-uses-fixed-pool ()
  (let ((event (make-generic-event 0)))
    (is (= 32 (xlib::reply-data-size event)))
    (xlib::deallocate-event event)
    (let ((reused (xlib::allocate-event)))
      (is (eq event reused))
      (xlib::deallocate-event reused))))

(deftest generic-event-rejects-unrepresentable-pool-size ()
  (signals error (xlib::generic-event-size #xffffffff)))

(deftest generic-event-tail-read ()
  (let* ((tail #(9 10 11 12))
         (display (make-test-display))
         (event (make-generic-event 1)))
    (unwind-protect
         (progn
           (setf (xlib::buffer-input-function display)
                 (lambda (buffer vector start end timeout)
                   (declare (ignore buffer timeout))
                   (replace vector tail :start1 start :end1 end)
                   nil))
           (is (null (xlib::read-generic-event-input display event 36 nil)))
           (is (equal '(9 10 11 12)
                      (loop for index from 32 below 36
                            collect (aref (xlib::reply-ibuf8 event) index)))))
      (xlib::deallocate-event event))))

(deftest generic-event-keeps-following-packet-aligned ()
  (let* ((generic (make-generic-event 1))
         (wire (make-array 68 :element-type 'xlib:card8 :initial-element 0))
         (position 0)
         (display (make-test-display))
         (predicate-calls 0))
    (unwind-protect
         (progn
           (replace wire (xlib::reply-ibuf8 generic) :end1 32 :end2 32)
           (replace wire (xlib::reply-ibuf8 generic)
                    :start1 32 :start2 32 :end2 36)
           ;; A KeyPress packet immediately follows the GenericEvent tail.
           (setf (aref wire 36) 2)
           (xlib::deallocate-event generic)
           (setf (xlib::buffer-input-wait-function display)
                 (lambda (buffer timeout)
                   (declare (ignore buffer timeout))
                   nil)
                 (xlib::buffer-input-function display)
                 (lambda (buffer vector start end timeout)
                   (declare (ignore buffer timeout))
                   (let ((count (- end start)))
                     (replace vector wire :start1 start :end1 end
                                      :start2 position :end2 (+ position count))
                     (incf position count)
                     nil)))
           (is (null
                (xlib::read-input
                 display nil nil
                 (lambda (ignored)
                   (declare (ignore ignored))
                   (if (xlib::display-event-queue-head display)
                       (if (zerop predicate-calls)
                           (progn (incf predicate-calls) nil)
                           t)
                       nil))
                 display)))
           (let ((first (xlib::display-event-queue-head display)))
             (is (= 35 (xlib::event-code first)))
             (is (= 2 (xlib::event-code (xlib::reply-next first))))))
      (loop for event = (xlib::display-event-queue-head display)
            while event
            do (xlib::dequeue-event display event)
               (xlib::deallocate-event event)))))

(deftest generic-event-is-receive-only ()
  (signals error (xlib:queue-event nil :generic-event))
  (signals error (xlib:send-event :pointer-window :generic-event nil)))
