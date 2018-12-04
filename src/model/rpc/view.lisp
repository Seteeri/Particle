(in-package #:protoform.model)

;; (when nil
;;   (let ((time (osicat:get-monotonic-time)))
;;     (format t "Model: ~8$ ms~%" (* (- time *time-last*) 1000))
;;     (setf *time-last* time)))

(defun handle-view-sync (time-view)

  ;; Controller creates ptree/queue
  ;; Pushes into queue for frame to consume here
  
  ;;;;;;;;;;;;;;;;
  ;; Execute ptree
  ;; - Pop ids from queue
  
  (loop
     :for ptree-queue := (sb-concurrency:dequeue *queue-frame*)
     :while ptree-queue
     :do (destructuring-bind (ptree queue)
	     ptree-queue
	   (loop
	      :for id-node := (sb-concurrency:dequeue queue)
	      :while id-node
	      :do (call-ptree id-node ptree))))
  
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
  ;; Reimplement cl-digaph with cl-tries?
  (memcpy-shm-to-cache-flag*
   (list (list "nodes"
	       0
      	       (* +size-struct-instance+ (+ (digraph:count-vertices *digraph*)
				       	    (digraph:count-edges *digraph*))))
	 (list "projview"
	       0
      	       (* 4 16 2)))))
