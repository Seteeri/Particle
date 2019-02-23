(in-package #:protoform.model)

;; Problem: Anims rely on frame times...
;; - must do synchronous
;; - if possible, move code to compute shader
;;   http://theorangeduck.com/page/avoiding-shader-conditionals

(defclass task ()
  ((id       :accessor id       :initarg :id       :initform nil)
   (fn-play  :accessor fn-play  :initarg :fn-play  :initform nil)
   (fn-stop  :accessor fn-stop  :initarg :fn-stop  :initform nil)
   (fn-pause :accessor fn-pause :initarg :fn-pause :initform nil)))

;; option: queue
(defun enqueue-task-async (task)
  (sb-concurrency:enqueue task
			  *queue-tasks-async*))

(defun enqueue-task-sync (task)
  (sb-concurrency:enqueue task
			  *queue-tasks-sync*))

(defun execute-queue-tasks (queue)
  (let ((tasks-next ()))
    (loop
       :for task := (sb-concurrency:dequeue queue)
       :while task
       :do (with-slots (id fn-play fn-stop)
	       task
	     ;; Check ID if it's on the cancellation list
	     ;; otherwise execute - later check pause/play
	     (if (member id *cancel-qts*)
		 (progn
		   (remove id *cancel-qts*)
		   (when fn-stop
		     (funcall fn-stop task)))
		 (let ((task-next (funcall fn-play)))
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
       :do (with-slots (id fn-play fn-stop)
	       task
	     (if (member id *cancel-qta*)
		 (progn
		   (remove id *cancel-qta*)
		   (when fn-stop
		     (funcall fn-stop task)))
		 (let* ((time (osicat:get-monotonic-time))
			(task-next (funcall fn-play))
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
      (enqueue-task-async task-next))))

(defun update-timing-fn (id time-delta)
  ;; avg or use kalman filter
  (setf (gethash id *ht-timing-fn*) time-delta))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defparameter *array* (make-array 1000000
;; 				  :element-type '(UNSIGNED-BYTE 8)
;; 				  :adjustable nil
;; 				  :fill-pointer nil
;; 				  :initial-contents (loop :for i :from 0 :below 1000000 :collect 0)))
;; (time (progn (qbase64:encode-bytes *array*) t))

;; (handler-case
;; 	(progn
;; 	  t
;;   (lparallel.ptree:ptree-redefinition-error (c)
;; 	t)
