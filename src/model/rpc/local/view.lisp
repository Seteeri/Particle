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
	     ;; (format t "time-elapsed: ~a | deadline: ~a | id: ~a~%" time-elapsed deadline id)
	     ;; Function can return item for next frame
	     (let* ((time (osicat:get-monotonic-time))
		    (ptree-next (call-ptree id ptree))
		    (time-final (osicat:get-monotonic-time))
		    (time-delta (- time-final time)))
	       (when (listp ptree-next)
		 (push ptree-next items-next))
	       (incf time-elapsed time-delta)
	       (update-timing-fn id time-delta)
	       (when (> time-elapsed deadline)
		 ;; (format t "(> ~a ~a)" time-elapsed deadline)
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
