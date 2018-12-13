(in-package :protoform.model)

(defun scale-ortho (seq-event ptree queue id delta)
  (with-slots (scale-ortho)
      *projview*
    
    (fmt-model t "scale-ortho" "~a -> ~a~%"
	       scale-ortho
	       (+ scale-ortho
		  delta))
    
    (let ((anim (make-instance 'animation
			       :object *projview*
			       :slot 'scale-ortho
			       :fn #'easing:in-cubic
			       :value-start scale-ortho
			       :value-delta delta
			       ;; :time-duration 4.0 ; secs
			       :time-elapsed 0.0)))

      ;; Deps = obj/slot
      (ptree-fn id
		'()
		(lambda ()
		  (funcall #'run-anim
			   seq-key
			   anim))
		ptree))
    
    (sb-concurrency:enqueue id
			    queue)))

(defun scale-ortho-down (seq-event ptree queue) ; zoom in
  (scale-ortho seq-event
	       ptree
	       queue
	       'run-anim-scale-ortho-down
	       (- (vz3 (displace *projview*)))))

(defun scale-ortho-up (seq-event ptree queue) ; zoom out
  (scale-ortho seq-event
	       ptree
	       queue
	       'run-anim-scale-ortho-up
	       (vz3 (displace *projview*))))

(defun move-camera-left (seq-event)
  (with-slots (pos
	       displace)
      *projview*
    (fmt-model t "move-camera-left" "~a -> ~a~%"
	       (vx3 pos)
	       (decf (vx3 pos)
		     (vx3 displace))))
  (update-mat-view)
  (enqueue-mat-view))

(defun move-camera-right (seq-event)
  (with-slots (pos
	       displace)
      *projview*
    (fmt-model t "move-camera-right" "~a -> ~a~%"
	       (vx3 pos)
	       (incf (vx3 pos)
		     (vx3 displace))))
  (update-mat-view)
  (enqueue-mat-view))

(defun move-camera-up (seq-event)
  (with-slots (pos
	       displace)
      *projview*
    (fmt-model t "move-camera-up" "~a -> ~a~%"
	       (vy3 pos)
	       (incf (vy3 pos)
		     (vy3 displace))))
  (update-mat-view)
  (enqueue-mat-view))

(defun move-camera-down (seq-event)
  (with-slots (pos
	       displace)
      *projview*
    (fmt-model t "move-camera-down" "~a -> ~a~%"
	       (vy3 pos)
	       (decf (vy3 pos)
		     (vy3 displace))))
  (update-mat-view)
  (enqueue-mat-view))
