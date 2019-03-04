(in-package #:protoform.model)

;; Problem: Anims rely on frame times...
;; - must do synchronous
;; - if possible, move code to compute shader
;;   http://theorangeduck.com/page/avoiding-shader-conditionals

;; option: queue
(defun enqueue-task-async (task)
  (setf (gethash (id task) *tasks-active*) task)
  (sb-concurrency:enqueue task
			  *queue-tasks-async*))

(defun enqueue-task-sync (task)
  (setf (gethash (id task) *tasks-active*) task)
  (sb-concurrency:enqueue task
			  *queue-tasks-sync*))

(defun enqueue-task-queue (queue task)
  (setf (gethash (id task) *tasks-active*) task)
  (sb-concurrency:enqueue task
			  queue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun execute-mb-tasks (mb)
  (execute-loop-task mb
		     #'sb-concurrency:receive-message
		     #'sb-concurrency:send-message))

(defun execute-queue-tasks (queue)
  (execute-loop-task queue
		     #'sb-concurrency:dequeue
		     #'enqueue-task-queue))

(defun execute-loop-task (tgt
			  fn-for
			  fn-post)
  ;; Alternative: Pull tasks into list until empty
  ;; - fn-play can then enqueue immediately instead of post-loop
  (let ((tasks-next ()))
    (loop
       :for task := (funcall fn-for tgt)
       :while task
       :do (let* ((stat (stat task))
		  (fn (cond ((eq stat 'play)
			     (fn-play task))
			    ((eq stat 'stop)
			     (fn-stop task))
			    ((eq stat 'pause)
			     (fn-pause task))
			    ((eq stat 'resume)
			     (fn-resume task))))
		  (task-next (when fn
			       (funcall fn task))))
	     (when (eq (type-of task-next) 'task)
	       (push task-next tasks-next))))
    (dolist (task-next tasks-next)
      (funcall fn-post tgt task-next))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun execute-queue-tasks-deadline (queue deadline)
  (let ((tasks-next ())
	(time-elapsed 0.0))
    ;; POSS pull items until empty instead of processing one at a time
    (loop
       :for task := (sb-concurrency:dequeue queue)
       :while task
       :do (let* ((stat (stat task))
		  (fn (cond ((eq stat 'play)
			     (fn-play task))
			    ((eq stat 'stop)
			     (fn-stop task))
			    ((eq stat 'pause)
			     (fn-pause task))
			    ((eq stat 'resume)
			     (fn-resume task))))
		  (time (osicat:get-monotonic-time))
		  (task-next (when fn
			       (funcall fn task)))
		  (time-final (osicat:get-monotonic-time))
		  (time-delta (- time-final time)))
	     ;; (format t "time-elapsed: ~a | deadline: ~a | id: ~a~%" time-elapsed deadline id)
	     ;; Function can return item for next frame
	     (when (eq (type-of task-next) 'task)
	       (push task-next tasks-next))
	     (incf time-elapsed time-delta)
	     (update-timing-fn (id task) time-delta)
	     (when (> time-elapsed deadline)
	       ;; (format t "(> ~a ~a)" time-elapsed deadline)
	       (return))))
    (dolist (task-next tasks-next)
      ;; (format t "[dolist...] ~a ~a~%" (id task-next) task-next)
      (enqueue-task-async task-next))))

(defun update-timing-fn (id time-delta)
  ;; avg or use kalman filter
  (setf (gethash id *ht-timing-fn*) time-delta))
