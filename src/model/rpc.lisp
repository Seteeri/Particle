(in-package #:protoform.model)

(defparameter *path-socket-view* "/tmp/protoform-view.socket")
(defparameter *time-last* 0)

;; (when nil
;;   (let ((time (osicat:get-monotonic-time)))
;;     (format t "Model: ~8$ ms~%" (* (- time *time-last*) 1000))
;;     (setf *time-last* time)))

(defun handle-view-sync (time-view)

  ;; Brainstorm:
  ;; 1. Input event loop should not use sock
  ;;    -> Refactor callbacks to only do up to update shm function
  ;; 2. Anims push (self) task into next q
  ;; 3. Use graph like init for consistency
  ;; 4. Refactor server message processing to accept self-evaluating symbols

  ;; Input -> Model -|-> View
  ;;
  ;; 1. Input loop
  ;;    - push sync tasks to queue
  ;;      - must execute in order/serial
  ;;        - if user was typing they'd get random text
  ;;      - animations go here
  ;; 2. Model loop
  ;;    - pull input tasks from queue -> funcall
  ;;      - purpose of this thread is to let input loop process events ASAP
  ;;      - funcall can submit tasks as needed to kernel or push sync tasks to queue
  ;;      - anims can only be done on frame
  ;;      - adding nodes can be done immediately
  ;;      - this cannot modify shm so any copying to shm must be done during view
  ;;        - but to write to shm is to read from lisp data structures which requires locking
  ;;        - poss when put in queue copy data/state?
  ;; 3. View loop (callback)
  ;;    - handles shm essentially -- can only modify shm in this "window"
  ;;    - pull tasks from queue -> submit tasks -> receive results
  ;;    - check shm flags, send-message

  ;; For input:
  ;; - Some events need to be handled in serial such as key presses
  ;;   - Users expect input to be handled in serial, at least within a domain ie mouse vs keyboard
  ;; - Each callback should execute a dep graph
  ;; - Shm is handled by model
  
  ;; (send-message *sock-view*
  ;; 		*buffer-sock-ptr*
  ;; 		"(pass)")
    
  (return-from handle-view-sync)
  
  ;; Check shm flags and send copy-shm messages
  
  ;; Send sync message
  
  ;; Swap queues
  ;; If controller writes to queue sometime between loop and swap
  ;; it will be delayed by one frame
  ;; setf atomic? -> https://sourceforge.net/p/sbcl/mailman/message/14171520/

  ;; (when nil
  ;;   (if (eq *queue-main* *queue-front*)
  ;; 	(setf *queue-main* *queue-back*)
  ;; 	(setf *queue-main* *queue-front*))))

  ;; (setf *queue-main* (if (eq *queue-main* *queue-front*)
  ;; 			 *queue-back*
  ;; 			 *queue-front*))

  t)

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

	 ;; Add option for self-evaluating symbol to avoid fn call
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
