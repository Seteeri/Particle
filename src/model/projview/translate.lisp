(in-package :protoform.model)

;; Abstract function to anim later
(defun translate-camera (seq-event
			 ptree
			 queue
			 fn-new
			 start
			 delta
			 id)
  (fmt-model t "translate-camera" "~a -> ~a~%"
	     start
	     (+ start
		delta))
  
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
    
    ;; Deps = obj/slot
    (ptree-fn id
	      '()
	      (lambda ()
		(funcall #'run-anim
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
		      (vx3 pos)
		      (- (vx3 displace))
		      'translate-camera-left)))

(defun translate-camera-right (seq-event ptree queue)
  (with-slots (pos
	       displace)
      *projview*
    (translate-camera seq-event
		      ptree
		      queue
		      (lambda (value-new)
			(setf (vx3 pos) value-new))
		      (vx3 pos)
		      (vx3 displace)
		      'translate-camera-right)))

(defun translate-camera-up (seq-event ptree queue)
  (with-slots (pos
	       displace)
      *projview*
    (translate-camera seq-event
		      ptree
		      queue
		      (lambda (value-new)
			(setf (vy3 pos) value-new))   
		      (vy3 pos)
		      (vy3 displace)
		      'translate-camera-up)))

(defun translate-camera-down (seq-event ptree queue)
  (with-slots (pos
	       displace)
      *projview*
    (translate-camera seq-event
		      ptree
		      queue
		      (lambda (value-new)
			(setf (vy3 pos) value-new))   
		      (vy3 pos)
		      (- (vy3 displace))
		      'translate-camera-down)))
