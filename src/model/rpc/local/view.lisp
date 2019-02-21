(in-package #:protoform.model)

(defun handle-view-sync (time-frame)
  ;; async-deadline = alloted time - sync time
  ;; or min one...always executes at least one task  
  (let* ((time (osicat:get-monotonic-time))
	 (time-alloted (/ 8 1000))
	 (time-remain time-alloted)) ; 8 ms runtime
    
    (execute-queue-tasks *queue-tasks-sync*)

    (decf time-remain (- (osicat:get-monotonic-time) time))
    
    (execute-queue-tasks-deadline *queue-tasks-async*
				  time-remain)

    ;; Ensure view executes shm on next frame
    ;; view will execute all messages sent to it until message
    (send-serving nil)
    
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
       :do (destructuring-bind (id fn)
	       item	 
	     (let ((item-next (funcall fn)))
	       (when (listp item-next)
		 (push item-next items-next)))))
    (dolist (item-next items-next)
      (sb-concurrency:enqueue item-next
			      queue))))

(defun execute-queue-tasks-deadline (queue deadline)
  (let ((items-next ())
	(time-elapsed 0.0))
    ;; POSS pull items until empty
    ;; Then call ptree
    (loop
       :for item := (sb-concurrency:dequeue queue)
       :while item
       :do (destructuring-bind (id fn)
	       item
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
		 (return)))))
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
