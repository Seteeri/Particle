(in-package :protoform.model)

(defun translate-camera (seq-event ptree queue id delta)
  (with-slots (pos)
      *projview*
    
    (fmt-model t "pos" "~a -> ~a~%"
	       pos
	       (+ pos
		  delta))
    
    (let ((anim (make-instance 'animation
			       :object *projview*
			       :slot 'pos
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

;; (defun translate-camera-left (seq-event ptree queue)
;;   (translate-camera-left seq-event
;; 			 ptree
;; 			 queue
;; 			 'run-anim-translate-camera-left
;; 			 (- (vx3 (displace *projview*)))))

(defun translate-camera-left (seq-event)
  (with-slots (pos
	       displace)
      *projview*
    (fmt-model t "move-camera-left" "~a -> ~a~%"
	       (vx3 pos)
	       (decf (vx3 pos)
		     (vx3 displace))))
  (update-mat-view)
  (enqueue-mat-view))

(defun translate-camera-left (seq-event)
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
