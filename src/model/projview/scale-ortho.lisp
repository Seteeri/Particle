(in-package :protoform.model)

;; Match translate fn?
(defun scale-ortho (seq-event
		    ptree
		    queue
		    fn-new
		    delta
		    id)
  (with-slots (scale-ortho)
      *projview*
    
    (fmt-model t "scale-ortho" "~a -> ~a~%"
	       scale-ortho
	       (+ scale-ortho
		  delta))
    
    (let ((anim (make-instance 'animation
			       :id id
			       :fn-easing #'easing:linear
			       :fn-new fn-new
			       :value-start scale-ortho
			       :value-delta delta)))
      ;; in animation.lisp
      (enqueue-anim anim
		    id
		    (lambda ()
		      (funcall #'run-anim
			       seq-event
			       anim))))))

(defun scale-ortho-down-cb (seq-event ptree queue) ; zoom in
  (scale-ortho seq-event
	       ptree
	       queue
	       (lambda (value-new)
		 (setf (scale-ortho *projview*) value-new)
		 (enqueue-mat-proj))
	       (- (vz3 (displace *projview*)))
	       'scale-ortho))

(defun scale-ortho-up-cb (seq-event ptree queue) ; zoom out
  (scale-ortho seq-event
	       ptree
	       queue
	       (lambda (value-new)
		 (setf (scale-ortho *projview*) value-new)
		 (enqueue-mat-proj))
	       (vz3 (displace *projview*))
	       'scale-ortho))
