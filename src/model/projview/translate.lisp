(in-package :protoform.model)

;; Abstract function to anim later
(defun translate-camera (seq-event
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
    
    ;; in animation.lisp
    (enqueue-anim anim
		  id
		  (lambda ()
		    (funcall #'run-anim
			     seq-event
			     anim)))))

(defun translate-camera-left-cb (seq-event)
  (with-slots (pos
	       displace)
      *projview*
    (translate-camera seq-event
		      (lambda (value-new)
			(setf (vx3 pos) value-new)
			(enqueue-mat-view))
		      (vx3 pos)
		      (- (vx3 displace))
		      'translate-camera-x)))

(defun translate-camera-right-cb (seq-event)
  (with-slots (pos
	       displace)
      *projview*
    (translate-camera seq-event
		      (lambda (value-new)
			(setf (vx3 pos) value-new)
			(enqueue-mat-view))
		      (vx3 pos)
		      (vx3 displace)
		      'translate-camera-x)))

(defun translate-camera-up-cb (seq-event)
  (with-slots (pos
	       displace)
      *projview*
    (translate-camera seq-event
		      (lambda (value-new)
			(setf (vy3 pos) value-new)
			(enqueue-mat-view))
		      (vy3 pos)
		      (vy3 displace)
		      'translate-camera-y)))

(defun translate-camera-down-cb (seq-event)
  (with-slots (pos
	       displace)
      *projview*
    (translate-camera seq-event
		      (lambda (value-new)
			(setf (vy3 pos) value-new)
			(enqueue-mat-view))
		      (vy3 pos)
		      (- (vy3 displace))
		      'translate-camera-y)))
