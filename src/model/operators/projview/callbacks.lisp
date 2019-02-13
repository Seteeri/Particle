(in-package :protoform.model)

;; See notes in dispatch

(defun scale-ortho-down-cb (seq-event)
  ;; zoom in
  (animate-scale-ortho seq-event
		       (lambda (value-new)
			 (setf (scale-ortho *projview*) value-new)
			 (send-mat-proj))
		       (- (vz3 (displace *projview*)))
		       'scale-ortho))

(defun scale-ortho-up-cb (seq-event)
  ;; zoom out
  (animate-scale-ortho seq-event
		       (lambda (value-new)
			 (setf (scale-ortho *projview*) value-new)
			 (send-mat-proj))
		       (vz3 (displace *projview*))
		       'scale-ortho))

(defun translate-camera-left-cb (seq-event)
  (with-slots (pos
	       displace)
      *projview*
    (animate-translate-camera seq-event
			      (lambda (value-new)
				(setf (vx3 pos) value-new)
				(send-mat-view))
			      (vx3 pos)
			      (- (vx3 displace))
			      'translate-camera-x)))

(defun translate-camera-right-cb (seq-event)
  (with-slots (pos
	       displace)
      *projview*
    (animate-translate-camera seq-event
			      (lambda (value-new)
				(setf (vx3 pos) value-new)
				(send-mat-view))
			      (vx3 pos)
			      (vx3 displace)
			      'translate-camera-x)))

(defun translate-camera-up-cb (seq-event)
  (with-slots (pos
	       displace)
      *projview*
    (animate-translate-camera seq-event
			      (lambda (value-new)
				(setf (vy3 pos) value-new)
				(send-mat-view))
			      (vy3 pos)
			      (vy3 displace)
			      'translate-camera-y)))

(defun translate-camera-down-cb (seq-event)
  (with-slots (pos
	       displace)
      *projview*
    (animate-translate-camera seq-event
			      (lambda (value-new)
				(setf (vy3 pos) value-new)
				(send-mat-view))
			      (vy3 pos)
			      (- (vy3 displace))
			      'translate-camera-y)))
