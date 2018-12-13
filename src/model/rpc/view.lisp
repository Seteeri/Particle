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

  ;; Anims can only run during frame call
  ;; so must integrate anim nodes during frame call

  ;; Controller callbacks will place ids in queue
  ;; Anims will place ptree-fn args in another queue
  ;; - ids will be generated from it

  ;; To run multiple anims in parallel in a single ptree
  ;; need to link them to a final function
  ;; ....or anims will enqueue and create final function node here
  ;; since can't modify the fn

  ;; To run entire ptree, need a final function that
  ;; has all previous ids as args
  ;; - Use existing queue to build a list

  ;; Can optimize by caching some of the analysis internally
  ;; - look into lparallel code

  ;; Either integrate anims into existing input ptree
  ;; Or run each ptree in parallel - one for anims, one for input

  ;; For now, run each ptree separately - run input then anim
  (loop
     :for ptree-queue := (sb-concurrency:dequeue *queue-frame*)
     :while ptree-queue
     :do (destructuring-bind (ptree queue)
	     ptree-queue
	   (let ((ids ()))
	     (loop
  		:for id-node := (sb-concurrency:dequeue queue)
  		:while id-node
  		:do (push id-node ids))
	     (ptree-fn 'finish
  		       ids
  		       (lambda ())
  		       ptree)
	     (call-ptree 'finish ptree))))

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
      (ptree-fn 'finish
  		ids
  		(lambda ())
  		ptree))
    (call-ptree 'finish ptree))

  (when nil
    (let ((ptree-queue (sb-concurrency:dequeue *queue-frame*)))       
      (if ptree-queue

  	  (destructuring-bind (ptree queue)
  	      ptree-queue
	    
  	    ;; Add anim nodes, collect ids, add finish node, call finish node
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
  		 :for id-node := (sb-concurrency:dequeue queue)
  		 :while id-node
  		 :do (push id-node ids))
  	      (ptree-fn 'finish
  			ids
  			(lambda ())
  			ptree))
  	    (call-ptree 'finish ptree))

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
  	      (ptree-fn 'finish
  			ids
  			(lambda ())
  			ptree))
  	    (call-ptree 'finish ptree))))))

(defun execute-tasks-shm ()
  ;; Can be done in parallel assuming no overlapping operations...
  ;; Can add detection code
  (loop
     :for counter := 0
     :for task := (sb-concurrency:dequeue *queue-shm*)
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
