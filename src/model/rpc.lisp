(in-package #:protoform.model)

(defparameter *path-socket-view* "/tmp/protoform-view.socket")
(defparameter *time-last* 0)

(defun handle-view-sync (time-view)

  ;; TODO:
  ;; 1. Input event loop should not use sock
  ;;    -> Refactor callbacks to only do up to update shm function
  ;; 2. Anims push (self) task into next q
  
  (send-message *sock-view*
  		*buffer-sock-ptr*
  		"(pass)")
  (return-from handle-view-sync)
  
  ;; Brainstorm:
  ;; - Events rely on frame time
  ;;   - controller can trigger single-frame or multi-frame (per-frame) calculations
  ;;   - for multi-frame, task will readd itself to next q
  ;; - Controller places tasks in queue for frame-callback to execute
  ;;   - 2 types of tasks - sync, async
  ;;   - sync - put in queue
  ;;   - async - submit task

  ;; Each frame:
  ;; - Get functions/tasks from queue
  ;; - Submit tasks, receive results
  ;; - Submit tasks for updating shms, receive results
  ;; - Send return message to view
  
  (when nil
    (let ((time (osicat:get-monotonic-time)))
      (format t "Model: ~8$ ms~%" (* (- time *time-last*) 1000))
      (setf *time-last* time)))

  (loop
     :with counter := 0
     :for task := (sb-concurrency:dequeue *queue*)
     :while task
     :do (progn
	   ;; (format t "~S~%" task)
	   (incf counter)
	   (submit-task *chan-anim*
			task))
     :finally (if (> counter 0)
		(dotimes (i counter)
		  (receive-result *chan-anim*))
		(send-message *sock-view*
			      *buffer-sock-ptr*
			      (format nil "(pass)"))))

  ;; Swap queues
  ;; If controller writes to queue sometime between loop and swap
  ;; it will be delayed by one frame
  ;; setf atomic? -> https://sourceforge.net/p/sbcl/mailman/message/14171520/
  (when nil
    (if (eq *queue* *queue-front*)
	(setf *queue* *queue-back*)
	(setf *queue* *queue-front*))))

(defun init-conn-rpc-view ()
  (setf *sock-view* (init-sock-client *path-socket-view* :block))

  ;; Combine all of below into single call
  
  ;; Init buffers  
  (send-message *sock-view*
		*buffer-sock-ptr*
		(with-output-to-string (stream)
		  (format stream "(init-view-buffers (")
		  (loop
		     :for (name params) :on *params-shm* :by #'cddr
		     :do (format stream "~S " params))
		  (format stream "))")))

  ;; See get-sym-shm-from-string
  (loop
     :for (name params) :on *params-shm* :by #'cddr
     :for name2 := (string-downcase (symbol-name name))
     :do (memcpy-shm-to-cache name2
			      (symbol-value (find-symbol (str:concat "shm-" name2) :protoform.model))))
  
  ;; Enable draw flag for view loop
  (send-message *sock-view*
		*buffer-sock-ptr*
		(format nil "(set-draw t)")))

(defun serve-client ()
  (loop
     (let ((message (recv-message *sock-view*
				  *buffer-sock-ptr*)))
       (when message
	 ;; (fmt-model t "serve-client" "Message: ~S~%" message)
	 ;; (print (eval message))
	 ;; (force-output)

	 (if (listp (first message))
	     (dolist (n message)
	       (apply (symbol-function (find-symbol (string (first n)) :protoform.model))
		      (cdr n)))
	     (apply (symbol-function (find-symbol (string (first message)) :protoform.model))
		    (cdr message)))))))

;; TODO: Use macro

(defun memcpy-shm-to-cache (name
			    shm
			    &optional
			      (offset 0)
			      (size-cpy nil))
  (with-slots (ptr size)
      shm
    ;; (fmt-model t "main-model" "(memcpy-shm-to-cache ~S ~S ~S)~%" name name size)
    (send-message *sock-view*
    		  *buffer-sock-ptr*
		  (format nil "(memcpy-shm-to-cache ~S ~S ~S ~S)" name name offset size-cpy))))

;; (defun memcpy-shm-to-cache (name
;; 			    &optional
;; 			      (offset 0)
;; 			      (size-cpy nil))
;;   (with-slots (ptr size)
;;       (gethash name *handles-shm*)
;;     ;; (fmt-model t "main-model" "(memcpy-shm-to-cache ~S ~S ~S)~%" name name size)
;;     (send-message *sock-view*
;;     		  *buffer-sock-ptr*
;; 		  (format nil "(memcpy-shm-to-cache ~S ~S ~S ~S)" name name offset size-cpy))))

(defun memcpy-shm-to-cache* (names)
  ;; Default to full copy
  (send-message *sock-view*
    		*buffer-sock-ptr*
		(with-output-to-string (stream)
		  (format stream "(")
		  (dolist (name names)
		    (with-slots (ptr size)
			(gethash name *handles-shm*)
		      (format stream "(memcpy-shm-to-cache ~S ~S 0 nil) " name name)))
		  (format stream ")"))))

(defun set-cache-dirty (name value)
  (send-message *sock-view*
    		*buffer-sock-ptr*
		(format nil "(set-cache-flag-copy ~S ~S)" name value)))

(defun memcpy-shm-to-cache-flag* (caches)
  (send-message *sock-view*
    		*buffer-sock-ptr*
		(with-output-to-string (stream)
		  (format stream "(")
		  (dolist (cache caches)
		    (destructuring-bind (name-cache
					 offset-cache
					 size-cache)
			cache
		      (with-slots (ptr size)
			  ;; (gethash name-cache *handles-shm*)
			  (get-sym-shm-from-string name-cache)
			;; Pass offsets
			(format stream "(memcpy-shm-to-cache ~S ~S ~S ~S) " name-cache name-cache offset-cache size-cache)
			(format stream "(set-cache-flag-copy ~S 3) " name-cache))))
		  (format stream ")"))))

;; Memoize
(defun get-sym-shm-from-string (string)
  (symbol-value (find-symbol (str:concat "shm-" string) :protoform.model)))
