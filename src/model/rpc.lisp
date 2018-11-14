(in-package #:protoform.model)

(defparameter *path-socket-view* "/tmp/protoform-view.socket")
(defparameter *time-last* 0)

;; (when nil
;;   (let ((time (osicat:get-monotonic-time)))
;;     (format t "Model: ~8$ ms~%" (* (- time *time-last*) 1000))
;;     (setf *time-last* time)))

(defun handle-view-sync (time-view)

  ;; Brainstorm:
  ;; 1. Refactor server message processing to accept self-evaluating symbols

  ;; Input -> Model -|-> View
  ;;
  ;; 1. Input loop
  ;;    - push sync tasks to queue
  ;;      - must execute in order/serial
  ;;        - if user was typing they'd get random text
  ;; 2. Model loop
  ;;    - pull input tasks from queue -> funcall
  ;;      - purpose of this thread is to let input loop process events ASAP
  ;;      - funcall can submit tasks+receive result, or push sync tasks to queue
  ;;        - anims can only be done on frame
  ;;          - Ex: camera anim and node anim can run simultaneously
  ;;        - adding nodes can be done immediately
  ;;      - shm can only be modified during view since poss view process will read during that time
  ;;        - for view, to write to shm is to read from lisp data which requires locking...
  ;;        - solution -> copy data/state when putting in queue - such as for a node being modified
  ;;          - changes propogate like a dag...
  ;;          - serialize data so view loop simply copies memory
  ;; 3. View loop (callback)
  ;;    - Execute frame tasks
  ;;    - Copy from lisp to shm
  ;;      - execute tasks copy shm
  ;;      - send shm message

  ;; For input:
  ;; - Some events need to be handled in serial such as key presses
  ;;   - Users expect input to be handled in serial, at least within a domain ie mouse vs keyboard
  ;; - Each callback could execute a dep graph

  ;; Time duration
  ;; If exceed 16.7 ms, break
  
  ;; Execute frame callbacks
  ;; - Can use submit task per callback/task
  ;; - Add flag whether to run sync or async
  ;;   - Can run async if no dependent on any other variables used by other threads
  ;; - Non-frame tasks would run in other thread...
  (loop
     :for task := (sb-concurrency:dequeue *queue-input*)
     :while task
     :do (destructuring-bind (channel
			      fn
			      seq-key)
	     task
	   (if channel
	       (submit-task channel
			    fn
			    seq-key)
	       (funcall fn seq-key))))

  ;; Call receive-results as needed
  
  ;; Execute shm copy tasks
  ;; - Can be done in parallel assuming no overlapping operations...
  (loop
     :for counter := 0
     :for task := (sb-concurrency:dequeue *queue-view*)
     :while task
     :do (destructuring-bind (channel
			      name-shm
			      data
			      offset)
	     task
	   (incf counter)
	   (submit-task *channel*
			#'copy-data-to-shm
			name-shm
			data
			offset))
     :finally (dotimes (i counter)
		(receive-result *channel*)))

  ;; Is it faster to copy large contiguous area or numerous small segments?
  ;; Send message to view to copy shm
  ;; Use highest offset from copy shm queue...
  (memcpy-shm-to-cache-flag* (list (list "nodes"
				       	 0
      				       	 (* +size-struct-instance+ (+ (digraph:count-vertices *digraph*)
				       				      (digraph:count-edges *digraph*))))
				   (list "projview"
				       	 0
      				       	 (* 4 16 2))))
      
  ;; Swap queues
  ;; If controller writes to queue sometime between loop and swap
  ;; it will be delayed by one frame
  ;; setf atomic? -> https://sourceforge.net/p/sbcl/mailman/message/14171520/

  (setf *queue-input* (if (eq *queue-input* *queue-front*)
  			  *queue-back*
  			  *queue-front*))

  t)

(defun copy-data-to-shm (shm data &optional (offset-ptr 0))
  (declare (type (array (unsigned-byte 8)) data))
  (with-slots (ptr size)
      shm
    (loop
       :for c :across data
       :for i :upfrom 0
       :do (setf (mem-aref ptr
    			   :uchar
    			   (+ offset-ptr i))
    		 c))))

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
