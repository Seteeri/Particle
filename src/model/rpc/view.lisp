(in-package #:protoform.model)

;; (when nil
;;   (let ((time (osicat:get-monotonic-time)))
;;     (format t "Model: ~8$ ms~%" (* (- time *time-last*) 1000))
;;     (setf *time-last* time)))

(defun handle-view-sync (time-view)

  (execute-tasks-frame)
  (execute-tasks-shm)

  ;; TODO:
  ;; Instead of copying everything, find max index during execute-tasks-shm
  (memcpy-shm-to-cache-flag*
   `(("nodes"
      0
      ,(* +size-struct-instance+ (+ (car *vertices-digraph*)
  				    (car *edges-digraph*))))
     ("projview"
      0
      ,(* 4 16 2)))))
  
(defun execute-tasks-frame ()
  ;; Build ptree - serial
  ;; Execute ptree - parallel
  (let ((ptree (make-ptree))
	(ids (make-hash-table :size 32 :test 'equal)))
    (loop
       :for item := (sb-concurrency:dequeue *queue-anim*)
       :while item
       :do (parse-task item ptree ids))
    (ptree-fn 'finish
	      (loop :for key :being :the hash-keys :of ids :collect key)
  	      (lambda ())
  	      ptree)
    (call-ptree 'finish ptree)))

(defun parse-task (item ptree ids)
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
	;; Keep anim with latest start time or nil
	;; - Create callback for when animation pauses?
	;; - Create option to ignore instead of restart
	(when anim
	  (let ((anim-prev (gethash id ids)))
	    (unless (time-start anim-prev)
	      ;; Modify existing anim slots
	      (copy-anim anim-prev anim)))
	  (fmt-model t "execute-tasks-anim" "Restart anim for ~a~%" id))))))

(defun execute-tasks-shm ()

  ;; Check flags in parallel or have cb queue flags (prefer queue)
  ;; - With flags, need to reset after
  ;; If so, serialize data and memcpy it

  ;; Issue is if different callbacks work on the same node
  ;; then both changes won't be captured
  
  ;; (update-mat-view)
  ;; (update-mat-proj)
  ;; (enqueue-mat-view)
  ;; (enqueue-mat-proj)
  ;; (enqueue-node-pointer)

  ;; For below, ignore if same node...
  (let ((ids (make-hash-table :size 64 :test 'equal)))
    (loop
       :for task := (sb-concurrency:dequeue *queue-shm*)
       :while task
       :do (destructuring-bind (channel
				name-shm
				fn-data
				offset)
	       task
	     (when (not (gethash (list name-shm offset) ids))
	       (submit-task *channel*
			    #'copy-data-to-shm
			    name-shm
			    fn-data
			    offset)
	       (setf (gethash (list name-shm offset) ids)
		     t)))
       :finally (dotimes (i (hash-table-count ids))
		  (receive-result *channel*)))))
