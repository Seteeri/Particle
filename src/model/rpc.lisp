(in-package #:protoform.model)

(defparameter *path-socket-view* "/tmp/protoform-view.socket")
(defparameter *time-last* 0)

;; (when nil
;;   (let ((time (osicat:get-monotonic-time)))
;;     (format t "Model: ~8$ ms~%" (* (- time *time-last*) 1000))
;;     (setf *time-last* time)))

(defun handle-view-sync (time-view)

  ;; TODO: Execute dependency graph for each callback/task
  ;; - share code with analyzer - use submit-receive-graph fn from main
  ;; - controller callbacks need to add to a graph
  
  ;; Execute frame callbacks
  ;; - Non-frame tasks would run in other thread - complicates things...
  ;; - optimistic loop - if > 16.7 ms: break (do on next frame)
  (loop
     :with counter := 0
     :for task := (sb-concurrency:dequeue *queue-input*)
     :while task
     :do (destructuring-bind (channel
			      fn
			      seq-key)
	     task
	   (if channel
	       (progn
		 ;; For now, assume same channel for all
		 ;; or add channel to list and receive-result for each channel
		 (submit-task channel
			      fn
			      seq-key)
		 (incf counter))
	       (funcall fn seq-key)))
     :finally (dotimes (i counter)
		(receive-result *channel*)))
  
  ;; Execute shm copy tasks
  ;; - Can be done in parallel assuming no overlapping operations...
  ;; - Can add detection code
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
  ;;
  ;; TODO:
  ;; digraph:count-vertices - checks hash-table-size
  ;; digraph:count-edges    - loops through vertices then edges
  ;; Reimplement cl-digaph with cl-tries
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
