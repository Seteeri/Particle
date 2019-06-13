(defun test-queue-kernel ()    
  (submit-task channel (lambda () (loop (sleep 0.0167))))
  (submit-task channel (lambda () (loop (sleep 0.0167))))
  (submit-task channel (lambda () (loop (sleep 0.0167))))
  
  ;; Represents frame consumer
  (submit-task channel
	       (lambda ()
		 (loop
		    (format t "pop: ~a~%" (pop-queue queue-anim)))))

  ;; Represents frame producer
  (submit-task channel
	       (lambda ()
		 (loop
		    (progn
		      (push-queue (format nil "Frame #~a~%" (get-internal-real-time))
				  queue-anim)
		      (sleep 0.0167)))))

  ;; Wait for finish - never will
  (receive-result channel))
