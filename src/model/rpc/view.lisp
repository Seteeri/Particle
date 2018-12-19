(in-package #:protoform.model)

;; (when nil
;;   (let ((time (osicat:get-monotonic-time)))
;;     (format t "Model: ~8$ ms~%" (* (- time *time-last*) 1000))
;;     (setf *time-last* time)))

(defun handle-view-sync (time-view)
  
  (execute-tasks-frame)
  (execute-tasks-anim)    
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

(defun execute-tasks-frame ()

  ;; Anims can only run during frame call
  ;; so must integrate anim nodes during frame call

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
	     (call-ptree 'finish ptree)))))

(defun execute-tasks-anim ()
  (let ((ptree (make-ptree)))
    ;; Add all nodes, then call each fn
    (let ((ids (make-hash-table :size 64)))
      (loop
	 :for item := (sb-concurrency:dequeue *queue-anim*)
	 :while item
	 :do (destructuring-bind (anim
				  id
				  args
				  fn)
  		 item
	       (handler-case
		   (progn
  		     (ptree-fn id
  			       args
  			       fn
  			       ptree)
		     (setf (gethash id ids) anim))
		 (lparallel.ptree:ptree-redefinition-error (c)
		   ;; Keep anim with latest start time or nil
		   ;; - Create callback for when animation pauses?
		   ;; - Create option to ignore instead of restart
		   (let ((anim-prev (gethash id ids)))
		     (unless (time-start anim-prev)
		       ;; Modify existing anim slots
		       (setf (value-start  anim-prev) (value-start  anim)
			     (time-start   anim-prev) (time-start   anim)
		             (time-end     anim-prev) (time-end     anim)
			     (time-elapsed anim-prev) (time-elapsed anim))))
		   (fmt-model t "execute-tasks-anim" "Restart anim for ~a~%" id)))))
      (ptree-fn 'finish
		(loop :for key :being :the hash-keys :of ids :collect key)
  		(lambda ())
  		ptree))
    (call-ptree 'finish ptree))

  ;; TODO:
  ;; - Implement atomic dirty flags for projview and nodes
  (update-mat-view)
  (update-mat-proj))

(defun execute-tasks-shm ()

  ;; Check flags in parallel?
  ;; If so, serialize and add data?
  
  ;; Will be refactored...
  (enqueue-mat-view)
  (enqueue-mat-proj)
  (enqueue-node-pointer)
  
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
