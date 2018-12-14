(in-package :protoform.model)

;; Abstract function to anim later
(defun translate-camera (seq-event
			 ptree
			 queue
			 fn-new
			 fn-update
			 start
			 delta
			 id)
  (fmt-model t "translate-camera" "~a -> ~a~%"
	     start
	     (+ start
		delta))
  
  (let ((anim (make-instance 'animation
			     :fn-easing #'easing:in-cubic
			     :fn-new fn-new
			     :fn-update fn-update
			     :fn-enqueue #'run-anim
			     :value-start start
			     :value-delta delta)))

    ;; Deps = obj/slot
    (ptree-fn id
	      '()
	      (lambda ()
		(funcall #'run-anim-view
			 seq-key
			 anim))
	      ptree))
  
  (sb-concurrency:enqueue id
			  queue))

(defun translate-camera-left (seq-event ptree queue)
  (with-slots (pos
	       displace)
      *projview*
    (translate-camera seq-event
		      ptree
		      queue
		      (lambda (value-new)
			(setf (vx3 pos) value-new))
		      (lambda ()
			(update-mat-view)
			(enqueue-mat-view))	
		      (vx3 pos)
		      (- (vx3 displace))
		      'run-anim-view)))

(defun translate-camera-right (seq-event ptree queue)
  (with-slots (pos
	       displace)
      *projview*
    (translate-camera seq-event
		      ptree
		      queue
		      (lambda (value-new)
			(setf (vx3 pos) value-new))
		      (lambda ()
			(update-mat-view)
			(enqueue-mat-view))		      
		      (vx3 pos)
		      (vx3 displace)
		      'run-anim-view)))

(defun translate-camera-up (seq-event ptree queue)
  (with-slots (pos
	       displace)
      *projview*
    (translate-camera seq-event
		      ptree
		      queue
		      (lambda (value-new)
			(setf (vy3 pos) value-new))
		      (lambda ()
			(update-mat-view)
			(enqueue-mat-view))		      
		      (vy3 pos)
		      (vy3 displace)
		      'run-anim-view)))

(defun translate-camera-down (seq-event ptree queue)
  (with-slots (pos
	       displace)
      *projview*
    (translate-camera seq-event
		      ptree
		      queue
		      (lambda (value-new)
			(setf (vy3 pos) value-new))
		      (lambda ()
			(update-mat-view)
			(enqueue-mat-view))			      
		      (vy3 pos)
		      (- (vy3 displace))
		      'run-anim-view)))
