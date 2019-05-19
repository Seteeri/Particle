(in-package #:protoform.model)

(defclass id ()
  ((name :accessor name :initarg :name)
   (time-start :accessor time-start :initarg :time-start)))

(defclass task ()
  ;; add frame type - async/async - why?
  ((id        :accessor id        :initarg :id        :initform nil)
   (stat      :accessor stat      :initarg :stat      :initform 'play)
   (fn-play   :accessor fn-play   :initarg :fn-play   :initform nil)
   (fn-stop   :accessor fn-stop   :initarg :fn-stop   :initform nil)
   (fn-pause  :accessor fn-pause  :initarg :fn-pause  :initform nil)
   (fn-resume :accessor fn-resume :initarg :fn-resume :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;

(defun pause-task-mb (id)
  (sb-concurrency:send-message
   *mb-async*
   (make-task-stop id))

  (sb-concurrency:send-message
   *mb-async*
   (make-task-inactivate id)))

(defun resume-task-mb (id)
  (sb-concurrency:send-message
   *mb-async*
   (make-task-activate id)))

(defun stop-task-mb (id)
  ;; handle pausing in stop state
  (sb-concurrency:send-message
   *mb-async*
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
