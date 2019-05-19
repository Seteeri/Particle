(in-package #:protoform.model)

;; Problem: Anims rely on frame times...
;; - must do synchronous
;; - if possible, move code to compute shader
;;   http://theorangeduck.com/page/avoiding-shader-conditionals

(defun handle-view-sync-prototype (time-frame)

  (when nil
    (let ((repeat-timer (make-timer (lambda ()
				      (format t "HERE!!!!!!!!!!!!!!!!!!!!!!~%")))))
      (schedule-timer repeat-timer
		      1)))
  
  t)

(defun handle-view-sync (time-frame)

  ;; Set timer - either here or view process
  ;; If tasks does not end before expiration, copy shm...
  ;; If tasks end before expiration, cancel timer and copy shm
  ;; Currently, view is at the mercy of model...
  (when nil
    (let ((repeat-timer (make-timer (lambda ()
				      (format t "HERE!!!!!!!!!!!!!!!!!!!!!!~%")))))
      (schedule-timer repeat-timer
		      1)))
  
  ;; async-deadline = alloted time - sync time
  ;; or min one...always executes at least one task  
  (let* ((time (osicat:get-monotonic-time))
	 (time-alloted (/ (* 0.8 (* (/ 1 60))) 1000)) ;(/ 8 1000))
	 (time-remain time-alloted)) ; 8 ms runtime
    
    (execute-queue-tasks *queue-tasks-sync*)

    (decf time-remain (- (osicat:get-monotonic-time) time))
    
    (execute-queue-tasks-deadline *queue-tasks-async*
				  time-remain)
    
    ;; Ensure view executes shm on next frame
    ;; view will execute all messages sent to it until message
    (send-serving nil)

    ;; GC after sending message to view process before recv next message
    ;; (sb-ext:gc)
    
    t))

(defun handle-view-sync-2 (time-frame)

  (sb-sys:without-gcing
  
      ;; execute ptree
      
      (send-serving nil)
    
    t)

  ;; gc
  ;; (sb-ext:gc)
  
  ;; build ptree
  
  t)
