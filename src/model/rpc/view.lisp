(in-package #:protoform.model)

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
  ;; (* +size-struct-instance+ (+ (car *vertices-main*)
  ;; 				 (car *edges-main*))))
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
  (let ((items-next ()))
    (loop
       :for item := (sb-concurrency:dequeue *queue-anim*)
       :while item
       :do (destructuring-bind (ptree id)
	       item
	     ;; Function can return item for next frame
	     (let ((ptree-next (call-ptree id ptree)))
	       (when (listp ptree-next)
		 (push ptree-next items-next)))))
    (dolist (item-ptree items-next)
      (sb-concurrency:enqueue item-ptree	    
			      *queue-anim*))))

;; (handler-case
;; 	(progn
;; 	  t
;;   (lparallel.ptree:ptree-redefinition-error (c)
;; 	t)

(defun execute-tasks-shm ()
  
  ;; (update-mat-view)
  ;; (update-mat-proj)
  ;; (enqueue-mat-view)
  ;; (enqueue-mat-proj)
  ;; (enqueue-node-ptr)

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
