(in-package :protoform.model)

(defun scale-ortho (seq-event ptree queue id delta fn-new)
  (with-slots (scale-ortho)
      *projview*
    
    (fmt-model t "scale-ortho" "~a -> ~a~%"
	       scale-ortho
	       (+ scale-ortho
		  delta))
    
    (let ((anim (make-instance 'animation
			       :object *projview*
			       :slot 'scale-ortho
			       :fn-easing #'easing:in-cubic
			       :fn-new fn-new
			       :value-start scale-ortho
			       :value-delta delta
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
	       (- (vz3 (displace *projview*)))
	       (lambda (value-new)
		 (setf (scale-ortho *projview*) value-new))))

(defun scale-ortho-up (seq-event ptree queue) ; zoom out
  (scale-ortho seq-event
	       ptree
	       queue
	       'run-anim-scale-ortho-up
	       (vz3 (displace *projview*))
	       (lambda (value-new)
		 (setf (scale-ortho *projview*) value-new))))
