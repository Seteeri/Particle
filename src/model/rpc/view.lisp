(in-package #:protoform.model)

(defun handle-view-sync (time-frame)

  (let* ((time (* (osicat:get-monotonic-time) 1000))
	 (time-alloted (* (/ 1 120) 1000))
	 (time-remain time-alloted)) ; 8 ms runtime
    
    ;; Frame time ratio:
    ;; 2x + 2y + 1z = 16

    (execute-queue-tasks *queue-tasks-sync*)

    (decf time-remain (- (* (osicat:get-monotonic-time) 1000) time))
    
    ;; async-deadline = alloted time - sync time
    ;; or min one...always executes at least one task
    (execute-queue-tasks-deadline *queue-tasks-async*
				  time-remain)

    t))


(defun execute-queue-tasks (queue)
  ;; Problem: Anims rely on frame times...
  ;; - must do synchronous
  ;; - if possible, move code to compute shader
  ;;   http://theorangeduck.com/page/avoiding-shader-conditionals

  ;; Try to run anims in parallel
  ;; - Use old method where callbacks enqueue a list and ptree is built/exec here

  (let ((items-next ()))
    (loop
       :for item := (sb-concurrency:dequeue queue)
       :while item
       :do (destructuring-bind (ptree id)
	       item
	     ;; Function can return item for next frame
	     (let ((ptree-next (call-ptree id ptree)))
	       (when (listp ptree-next)
		 (push ptree-next items-next)))))
    (dolist (item-ptree items-next)
      (sb-concurrency:enqueue item-ptree
			      queue))))

(defun execute-queue-tasks-deadline (queue deadline)
  (let ((items-next ())
	(time-elapsed 0.0))
    ;; POSS pull items until empty
    ;; Then call ptree
    (loop
       :for item := (sb-concurrency:dequeue queue)
       :while item
       :do (destructuring-bind (ptree id)
	       item
	     ;; Function can return item for next frame
	     (let* ((time (osicat:get-monotonic-time))
		    (ptree-next (call-ptree id ptree))
		    (time-final (osicat:get-monotonic-time))
		    (time-delta (- time-final time))
		    (time-delta-ms (* time-delta 1000)))
	       (when (listp ptree-next)
		 (push ptree-next items-next))
	       (incf time-elapsed time-delta-ms)
	       (update-timing-fn id time-delta-ms)
	       (when (> time-elapsed deadline)
		 (format t "(> ~a ~a)" time-elapsed deadline)
		 (return)))))
    (dolist (item-ptree items-next)
      (sb-concurrency:enqueue item-ptree
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
