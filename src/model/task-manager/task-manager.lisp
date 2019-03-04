(in-package #:protoform.model)

;; Problem: Anims rely on frame times...
;; - must do synchronous
;; - if possible, move code to compute shader
;;   http://theorangeduck.com/page/avoiding-shader-conditionals

(defclass task ()
  ;; add frame type - async/async
  ((id        :accessor id        :initarg :id        :initform nil)
   (stat      :accessor stat      :initarg :stat      :initform 'play)
   (fn-play   :accessor fn-play   :initarg :fn-play   :initform nil)
   (fn-stop   :accessor fn-stop   :initarg :fn-stop   :initform nil)
   (fn-pause  :accessor fn-pause  :initarg :fn-pause  :initform nil)
   (fn-resume :accessor fn-resume :initarg :fn-resume :initform nil)))

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

;;;;;;;;;;;;;;;;;;;;;;;;

(defun pause-task-mb (id)
  (sb-concurrency:send-message
   *mb-model*
   (make-task-stop id))

  (sb-concurrency:send-message
   *mb-model*
   (make-task-inactivate id)))

(defun resume-task-mb (id)
  (sb-concurrency:send-message
   *mb-model*
   (make-task-activate id)))

(defun stop-task-mb (id)
  ;; handle pausing in stop state
  (sb-concurrency:send-message
   *mb-model*
   (make-task-stop-remove id)))

;;;;;;;;;;;;;;;;;;;;;

(defun pause-task (id)
  (enqueue-task-async
   (make-task-stop id))
  
  (enqueue-task-async
   (make-task-inactivate id)))

(defun resume-task (id)
  (enqueue-task-async
   (make-task-activate id)))

(defun stop-task (id)  
  (enqueue-task-async
   (make-task-stop-remove id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-task-activate (id)
  (make-instance 'task
		 :id 'resume-task
		 :fn-play (lambda (task)
			    ;; move from hash table active -> inactive (handle undo tree etc)
			    (multiple-value-bind (task bool)
				(gethash id *tasks-inactive*)
			      (when bool			     
				(setf (gethash id *tasks-active*) task
				      (stat task) 'play)
				(enqueue-task-async task)))
			    (remhash id *tasks-inactive*))))

(defun make-task-inactivate (id)
  (make-instance 'task
		 :id 'pause-task
		 :fn-play (lambda (task)
			    ;; stop task already placed which will remove it
			    ;; by time this is executed
			    ;; move from hash table active -> inactive (handle undo tree etc)
			    (multiple-value-bind (task bool)
				(gethash id *tasks-active*)
			      (when bool
				(setf (gethash id *tasks-inactive*) task
				      (stat task) 'pause))
			      (remhash id *tasks-active*)))))

(defun make-task-stop-remove (id)
  (make-instance 'task
		 :id 'stop-task
		 :fn-play (lambda (task)			     
			    ;; change stat of existing task
			    ;; del from hash table active (or move to undo tree etc)			     
			    (multiple-value-bind (task bool)
				(gethash id *tasks-active*)
			      (when bool
				(setf (stat task) 'stop))
			      (remhash id *tasks-active*)))))

(defun make-task-stop (id)
  (make-instance 'task
		 :id 'pause-task
		 :fn-play (lambda (task)			     
			    ;; stop existing task to prevent execution (and enqueueing)
			    (multiple-value-bind (task bool)
				(gethash id *tasks-active*)
			      (when bool
				(setf (stat task) 'stop))))))
  

