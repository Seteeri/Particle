(in-package #:protoform.model)

(defun print-monotonic-time ()
  (let ((time (osicat:get-monotonic-time)))
    (format t "Model: ~8$ ms~%" (* time 1000))))

(defun print-hash-table (ht)
  (maphash (lambda (key value)
	     (fmt-model t "" "~S : ~S~%" key value))
	   ht))

(defun get-extents (shm extents)
  (let ((extents-nodes (gethash *shm-nodes* extents)))
    (when extents-nodes
      (values (first extents-nodes)
	      (second extents-nodes)))))

(defun handle-view-sync (time-view)

  (execute-tasks-frame)
  
  ;; TODO
  ;; - refactor to function
  ;; All mem
  ;; (* +size-struct-instance+ (+ (car *vertices-digraph*)
  ;; 				 (car *edges-digraph*))))
  (let ((extents (execute-tasks-shm)))
    (when (> (hash-table-count extents) 0)
      (multiple-value-bind (min-nodes max-nodes)
	  (get-extents *shm-nodes* extents)
	(if min-nodes
	    (memcpy-shm-to-cache-flag*
	     `(("nodes"    ,min-nodes ,max-nodes)
	       ("projview" 0          ,(* 4 16 2))))
	    (memcpy-shm-to-cache-flag*
	     `(("projview" 0          ,(* 4 16 2)))))))))

(defun execute-tasks-frame ()
  ;; Build ptree - serial
  ;; Execute ptree - parallel

  ;; Loop until queue empty
  (loop
     :with q := *queue-anim*
     :do (let ((ptree (make-ptree))
	       (ids (make-hash-table :size 32 :test 'equal))
	       (queue-retry (sb-concurrency:make-queue)))
	   
	   (loop
	      :for item := (sb-concurrency:dequeue q)
	      :while item
	      :do (when-let ((item-retry (parse-task-frame item ptree ids)))
			    (sb-concurrency:enqueue item-retry queue-retry)))
	   
	   (unless (zerop (hash-table-size ids))
	     (ptree-fn 'finish
		       ;; the only ids need are the terminal ones
		       ;; -> prefix id with finish so we know which ids to track
		       (loop :for key :being :the hash-keys :of ids :collect key)
  		       (lambda ())
  		       ptree)
	     (call-ptree 'finish ptree))

	   (if (sb-concurrency:queue-empty-p queue-retry)
	       (return)
	       (progn
		 (setf q queue-retry)
		 (format t "RETRY~%"))))))

(defun parse-task-frame (item ptree ids)
  (destructuring-bind (anim id args fn)
      item
    (handler-case
	(progn
  	  (ptree-fn id
  		    args
  		    fn
  		    ptree)
	  ;; Does below need to be in handler-case?
	  (setf (gethash id ids) anim))
      (lparallel.ptree:ptree-redefinition-error (c)

	(when (not anim)
	  ;; (warn (format nil "parse-task-frame -> retry enqueue task: ~S" item))
	  (return-from parse-task-frame item))
	
	;; Keep anim with latest start time or nil
	;; - Create callback for when animation pauses?
	;; - Create option to ignore instead of restart
	(when anim
	  (let ((anim-prev (gethash id ids)))
	    (unless (time-start anim-prev)
	      ;; Modify existing anim slots
	      (copy-anim anim-prev anim)))
	  
	  (fmt-model t "execute-tasks-anim" "Restart anim for ~a~%" id)))))
  nil)

(defun execute-tasks-shm ()
  
  ;; (update-mat-view)
  ;; (update-mat-proj)
  ;; (enqueue-mat-view)
  ;; (enqueue-mat-proj)
  ;; (enqueue-node-pointer)

  ;; REFACTOR:
  ;; Use ptree instead?
  
  (let ((ids (make-hash-table :size 16 :test 'equal))
	(extents (make-hash-table :size 16 :test 'equal)))
    (loop
       :for task := (sb-concurrency:dequeue *queue-shm*)
       :while task
       :do (destructuring-bind (channel
				shm
				fn-data
				offset)
	       task
	     ;; Ignore if already in task list
	     (let ((hash (list shm offset)))
	       (unless (gethash hash ids)
		 (submit-task *channel*
			      #'copy-data-to-shm
			      shm
			      offset
			      fn-data)
		 (setf (gethash hash ids)
		       t))))
       :finally (dotimes (i (hash-table-count ids))
		  (destructuring-bind (shm offset len-data)
		      (receive-result *channel*)
		    ;; Check if range exceeds current
		    ;;
		    ;; Cannot do in loop since
		    ;; data is not serialized until task is called
		    ;; so it can be done in parallel

		    (if (gethash shm extents)
			(progn
			  (setf (first (gethash shm extents))
				(min (first (gethash shm extents))
				     offset))
			  (setf (second (gethash shm extents))
				(max (second (gethash shm extents))
				     (+ offset len-data))))
			(progn
			  (setf (gethash shm extents)
				(list offset (+ offset len-data))))))))
    extents))

;; Maybe move to memcpy
(defun copy-data-to-shm (shm offset-ptr fn-data)
  (list shm
	offset-ptr
	(copy-data-to-shm-2 shm
			    offset-ptr
			    (funcall fn-data))))

(defun copy-data-to-shm-2 (shm offset-ptr data)
  (declare (type (array (unsigned-byte 8)) data))
  (with-slots (ptr size)
      shm
    (loop
       :for c :across data
       :for i :upfrom 0
       :do (setf (mem-aref ptr
    			   :uchar
    			   (+ offset-ptr i))
    		 c)))
  (length data))
