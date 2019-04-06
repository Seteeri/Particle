(in-package :protoform.model)

;; Abstract function to anim later
(defun animate-translate-camera (seq-event
				 fn-new
				 start
				 delta
				 id)
  
  (fmt-model t "animate-translate-camera" "~a -> ~a~%"
	     start
	     (+ start
		delta))

  ;; Make sure existing anim not running
  (when (or (gethash id *tasks-active*)
	    (gethash id *tasks-inactive*))
    (return-from animate-translate-camera))
  
  (let ((anim (make-instance 'animation
			     :id id
			     :fn-easing #'easing:in-cubic
			     :fn-new fn-new
			     :value-start start
			     :value-delta delta)))
    ;; Below will throw error if the func exists (anim already playing)
    ;; Solutions:
    ;; 1. Replace existing animation
    ;;    - Get anim instance and set state to :cancel
    ;;    - Need to store anims somewhere...
    ;;      - obj: tcl=anim, tcr=anim, tcu=anim, tcd=anim
    ;;    - or modify anim object...
    ;; 2. On error, ignore
    ;;    - Anim will only play if users activates it
    ;;      when there is no existing anim playing

    (enqueue-task-sync (make-instance 'task
				      :id id
				      :fn-play (lambda (task)
						 (funcall #'run-anim
							  seq-event
							  anim))))))

;; Match translate fn?
(defun animate-scale-ortho (seq-event
			    fn-new
			    delta
			    id)

  ;; Make sure existing anim not running
  (when (or (gethash id *tasks-active*)
	    (gethash id *tasks-inactive*))
    (return-from animate-scale-ortho))

  (with-slots (scale-ortho)
      *projview*
    
    (fmt-model t "animate-scale-ortho" "~a -> ~a~%"
	       scale-ortho
	       (+ scale-ortho
		  delta))
    
    (let ((anim (make-instance 'animation
			       :id id
			       :fn-easing #'easing:linear
			       :fn-new fn-new
			       :value-start scale-ortho
			       :value-delta delta)))
    (enqueue-task-sync (make-instance 'task
				      :id id
				      :fn-play (lambda (task)
						 (funcall #'run-anim
							  seq-event
							  anim)))))))
