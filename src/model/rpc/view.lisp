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
    (let ((ids ()))
      (loop
	 :for anim := (sb-concurrency:dequeue *queue-anim*)
	 :while anim
	 :do (destructuring-bind (id args fn)
  		 anim
	       (handler-case
		   (progn
  		     (ptree-fn id
  			       args
  			       fn
  			       ptree)
		     (push id ids))
		 (lparallel.ptree:ptree-redefinition-error (c)
		   ;; Modify existing anim instance to restart?
		   ;; - Store object in anim, retrieve from destructuring-bind (enqueue list)
		   ;;   - Instead of list of ids; use hashtable?
		   ;; - Assumes only one instance of anim per object/node -> store in object
		   (fmt-model t "execute-tasks-anim" "Anim running already for ~a~%" id)))))
      (ptree-fn 'finish
  		ids
  		(lambda ())
  		ptree))
    (call-ptree 'finish ptree))

  ;; TODO:
  ;; - Implement dirty flags for projview and nodes
  (update-mat-view)
  (update-mat-proj))

(defun execute-tasks-shm ()

  ;; Will be refactored...
  (enqueue-mat-view)
  (enqueue-mat-proj)
  
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
