(in-package #:protoform.model)

;; Problem: Anims rely on frame times...
;; - must do synchronous
;; - if possible, move code to compute shader
;;   http://theorangeduck.com/page/avoiding-shader-conditionals

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
