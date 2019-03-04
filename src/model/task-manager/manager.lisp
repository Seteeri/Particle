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

(defun execute-queue-tasks (queue)
  ;; Alternative: Pull tasks into list until empty
  ;; - fn-play can then enqueue immediately instead of post-loop
  (let ((tasks-next ()))
    (loop
       :for task := (sb-concurrency:dequeue queue)
       :while task
       :do (with-slots (id stat fn-play fn-stop)
	       task
	     ;; Check ID if it's on the cancellation list
	     ;; otherwise execute - later check pause/play

	     ;; use cond and funcall play stop etc
	     (when (eq stat 'play)
	       (let ((task-next (funcall fn-play task)))
		 (when (eq (type-of task-next) 'task)
		   (push task-next tasks-next))))))
    (dolist (task-next tasks-next)
      (enqueue-task-sync task-next))))

(defun execute-queue-tasks-deadline (queue deadline)
  (let ((tasks-next ())
	(time-elapsed 0.0))
    ;; POSS pull items until empty instead of processing one at a time
    (loop
       :for task := (sb-concurrency:dequeue queue)
       :while task
       :do (with-slots (id stat fn-play fn-stop)
	       task
	     ;; (format t "[exec...] ~a ~a = ~a~%" id task stat)
	     (when (eq stat 'play)	     
	       (let* ((time (osicat:get-monotonic-time))
		      (task-next (funcall fn-play task))
		      (time-final (osicat:get-monotonic-time))
		      (time-delta (- time-final time)))
		 ;; (format t "time-elapsed: ~a | deadline: ~a | id: ~a~%" time-elapsed deadline id)
		 ;; Function can return item for next frame
		 (when (eq (type-of task-next) 'task)
		   (push task-next tasks-next))
		 (incf time-elapsed time-delta)
		 (update-timing-fn id time-delta)
		 (when (> time-elapsed deadline)
		   ;; (format t "(> ~a ~a)" time-elapsed deadline)
		   (return))))))
    (dolist (task-next tasks-next)
      ;; (format t "[dolist...] ~a ~a~%" (id task-next) task-next)
      (enqueue-task-async task-next))))

(defun update-timing-fn (id time-delta)
  ;; avg or use kalman filter
  (setf (gethash id *ht-timing-fn*) time-delta))

(defun execute-mb-tasks (mb)
  (let ((tasks-next ()))
    (loop
       :for task := (sb-concurrency:receive-message mb)
       :while task
       :do (with-slots (id stat fn-play fn-stop)
	       task
	     ;; Check ID if it's on the cancellation list
	     ;; otherwise execute - later check pause/play
	     (when (eq stat 'play)
	       (let ((task-next (funcall fn-play task)))
		 (when (eq (type-of task-next) 'task)
		   (push task-next tasks-next))))))
    (dolist (task-next tasks-next)
      (sb-concurrency:send-message mb fn-next))))
