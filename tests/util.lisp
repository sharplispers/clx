(defpackage #:clx-test-utils
  (:use #:cl)
  (:export #:test-required-params
          #:with-default-display))

(in-package #:clx-test-utils)

;;; -----------------
;;; Custom assertions
;;; -----------------

(defmacro test-required-params (function-form &rest required-params)
  "Asserts that the function throws missing-parameter errors when any required parameter is missing."
  (labels ((make-pairs (lst)
	     (assert (evenp (length lst)))
	     (and lst
		  (cons (list (first lst) (second lst))
			(make-pairs (cddr lst)))))
	   (get-all-combinations-without-one-elem (keyword-list)
	     (labels ((get-combinations-rec (pre el post)
			(if post
			    (cons (concatenate 'list pre post)
				  (get-combinations-rec (cons el pre)
							(car post)
							(cdr post)))
			    (list pre))))
	       (get-combinations-rec nil (car keyword-list) (cdr keyword-list)))))
    `(progn
       ,@(loop for param-list in (mapcar #'alexandria:flatten
					 (get-all-combinations-without-one-elem
					  (make-pairs required-params)))
	    collecting
              `(fiasco:signals xlib:missing-parameter
                 (,@(alexandria:ensure-list function-form) ,@param-list))))))

;;; -------
;;; Helpers
;;; -------

(defmacro with-default-display (display &body body)
  `(let ((,display (xlib:open-default-display)))
     (unwind-protect
          (progn ,@body)
       (xlib:close-display ,display))))
