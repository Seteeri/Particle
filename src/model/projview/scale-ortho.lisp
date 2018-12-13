(in-package :protoform.model)

;; Match translate fn?
(defun scale-ortho (seq-event ptree queue fn-new delta id)
  (with-slots (scale-ortho)
      *projview*
    
    (fmt-model t "scale-ortho" "~a -> ~a~%"
	       scale-ortho
	       (+ scale-ortho
		  delta))
    
    (let ((anim (make-instance 'animation
			       :fn-easing #'easing:in-cubic
			       :fn-new fn-new
			       :value-start scale-ortho
			       :value-delta delta)))

      ;; Deps = obj/slot
      (ptree-fn id
		'()
		(lambda ()
		  (funcall #'run-anim-proj
			   seq-key
			   anim))
		ptree))
    
    (sb-concurrency:enqueue id
			    queue)))

(defun scale-ortho-down (seq-event ptree queue) ; zoom in
  (scale-ortho seq-event
	       ptree
	       queue
	       (lambda (value-new)
		 (setf (scale-ortho *projview*) value-new))
	       (- (vz3 (displace *projview*)))
	       'run-anim-proj))

(defun scale-ortho-up (seq-event ptree queue) ; zoom out
  (scale-ortho seq-event
	       ptree
	       queue
	       (lambda (value-new)
		 (setf (scale-ortho *projview*) value-new))
	       (vz3 (displace *projview*))
	       'run-anim-proj))
