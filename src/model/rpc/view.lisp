(in-package #:protoform.model)

;; (when nil
;;   (let ((time (osicat:get-monotonic-time)))
;;     (format t "Model: ~8$ ms~%" (* (- time *time-last*) 1000))
;;     (setf *time-last* time)))

(defun handle-view-sync (time-view)
  
  ;; Execute ptrees
  (execute-tasks-main)
    
  ;; Execute shm copy tasks
  (execute-tasks-shm)

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

(defun execute-tasks-main ()
  ;; Controller creates ptree/queue
  ;; Pushes into queue for frame to consume here

  ;; How to run anims and input in parallel?

  ;; Anims can only run during frame call
  ;; so must integrate anim nodes during frame call
  ;; Queue anims

  ;; Controller callback will place ptree-fn args in
  ;; queue which will then be dequeued here

  ;; - if anims and ptree: add anim nodes to ptree
  ;;   else: create ptree and add
  ;; - Then pop ids from queue

  (let ((ptree-queue (sb-concurrency:dequeue *queue-frame*)))
    (if ptree-queue

	(destructuring-bind (ptree queue)
  	    ptree-queue
	  
	  ;; Add anim nodes first
	  (let ((ids ()))
	    (loop
	       :for anim := (sb-concurrency:dequeue *queue-anim*)
	       :while anim
	       :do (destructuring-bind (id args fn)
	  	       anim
	  	     (push id ids)
	  	     (ptree-fn id
			       args
			       fn
			       ptree)))
	    ;; Call nodes
  	    (loop
  	       :for id-node := (sb-concurrency:dequeue queue)
  	       :while id-node
  	       :do (call-ptree id-node
			       ptree))
	    (loop
  	       :for id-node :in ids
  	       :do (call-ptree id-node
			       ptree))))

	;; Create ptree
	(let ((ptree (make-ptree)))
	  
	  ;; Add all nodes, then call each fn
	  (let ((ids ()))
	    (loop
	       :for anim := (sb-concurrency:dequeue *queue-anim*)
	       :while anim
	       :do (destructuring-bind (id args fn)
		       anim
		     (push id ids)
		     (ptree-fn id
			       args
			       fn
			       ptree)))
	    (loop
  	       :for id-node :in ids
  	       :do (call-ptree id-node
			       ptree)))))))

(defun execute-tasks-shm ()
  ;; Can be done in parallel assuming no overlapping operations...
  ;; Can add detection code
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
		(receive-result *channel*))))
