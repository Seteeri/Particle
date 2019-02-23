(in-package #:protoform.model)

;; Problem: Anims rely on frame times...
;; - must do synchronous
;; - if possible, move code to compute shader
;;   http://theorangeduck.com/page/avoiding-shader-conditionals

;; move below into task-manager

(defun execute-queue-tasks (queue)
  (let ((items-next ()))
    (loop
       :for item := (sb-concurrency:dequeue queue)
       :while item
       :do (destructuring-bind (id fn)
	       item
	     ;; Check ID if it's on the cancellation list
	     ;; otherwise execute - later check pause/play
	     (if (member id *cancel-qts*)
		 (progn
		   (remove id *cancel-qts*)
		   ;; should call cleanup fn also - create structure for item now
		   t)
		 (let ((item-next (funcall fn)))
		   (when (listp item-next)
		     (push item-next items-next))))))
    (dolist (item-next items-next)
      (sb-concurrency:enqueue item-next
			      queue))))

(defun execute-queue-tasks-deadline (queue deadline)
  (let ((items-next ())
	(time-elapsed 0.0))
    ;; POSS pull items until empty instead of processing one at a time
    (loop
       :for item := (sb-concurrency:dequeue queue)
       :while item
       :do (destructuring-bind (id fn)
	       item
	     (if (member id *cancel-qta*)
		 (progn
		   (remove id *cancel-qta*)
		   ;; should call cleanup fn also - create structure for item now
		   t)
		 (let* ((time (osicat:get-monotonic-time))
			(item-next (funcall fn))
			(time-final (osicat:get-monotonic-time))
			(time-delta (- time-final time)))
		   ;; (format t "time-elapsed: ~a | deadline: ~a | id: ~a~%" time-elapsed deadline id)
		   ;; Function can return item for next frame
		   (when (listp item-next)
		     (push item-next items-next))
		   (incf time-elapsed time-delta)
		   (update-timing-fn id time-delta)
		   (when (> time-elapsed deadline)
		     ;; (format t "(> ~a ~a)" time-elapsed deadline)
		     (return))))))
    (dolist (item-next items-next)
      (sb-concurrency:enqueue item-next
			      queue))))

(defun update-timing-fn (id time-delta)
  ;; avg or use kalman filter
  (setf (gethash id *ht-timing-fn*) time-delta))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defparameter *array* (make-array 1000000
;; 				  :element-type '(UNSIGNED-BYTE 8)
;; 				  :adjustable nil
;; 				  :fill-pointer nil
;; 				  :initial-contents (loop :for i :from 0 :below 1000000 :collect 0)))
;; (time (progn (qbase64:encode-bytes *array*) t))

;; (handler-case
;; 	(progn
;; 	  t
;;   (lparallel.ptree:ptree-redefinition-error (c)
;; 	t)
