(in-package :protoform.model)

(defclass projview ()
  ((width :accessor width :initarg :width :initform nil)
   (height :accessor height :initarg :height :initform nil)
   (mat-proj :accessor mat-proj :initarg :mat-proj :initform nil)
   (type-proj :accessor type-proj :initarg :type-proj :initform nil)
   (scale-ortho :accessor scale-ortho :initarg :scale-ortho :initform 20.25) ; bigger number = smaller view
   (near-ortho :accessor near-ortho :initarg :near-ortho :initform 1)
   (ortho-far :accessor ortho-far :initarg :ortho-far :initform 512)
   (mat-view :accessor mat-view :initarg :mat-view :initform nil)
   (pos :accessor pos :initarg :pos :initform (vec3 -3 8 10))
   (rot :accessor rot :initarg :rot :initform (vec3 0 0 0))
   (displace :accessor displace :initarg :displace :initform (vec3 1.0 1.0 0.75))))

(defun update-mat-proj ()
  (with-slots (width
	       height
	       type-proj
	       mat-proj
	       scale-ortho
	       near-ortho
	       ortho-far)
      *projview*
    (setf mat-proj (if (eq type-proj :perspective)
		       (make-perspective-vector width height)
		       (make-orthographic-vector width height
						 scale-ortho
						 near-ortho
						 ortho-far)))))

(defun update-mat-view ()
  (with-slots (mat-view
	       pos
	       rot)
      *projview*
    (setf mat-view (minv (m* (mtranslation pos)
			     (mrotation +vz+ (vz3 rot))
			     (mrotation +vy+ (vy3 rot))
			     (mrotation +vx+ (vx3 rot))
			     (mscaling (vec3 1 1 1)))))))

(defun copy-mat-proj-to-shm ()
  (set-matrix (ptr *shm-projview*)
	      (mat-proj *projview*)
	      0))

(defun copy-mat-view-to-shm ()
  (set-matrix (ptr *shm-projview*)
	      (mat-view *projview*)
	      16))

(defun enqueue-mat (mat offset)
  (let ((arr (marr (mtranspose mat))))
    (sb-concurrency:enqueue
     (list *channel*
	   *shm-projview*
	   (pack:pack "<16f"
		      (aref arr 0)  (aref arr 1)  (aref arr 2)  (aref arr 3)
		      (aref arr 4)  (aref arr 5)  (aref arr 6)  (aref arr 7)
		      (aref arr 8)  (aref arr 9)  (aref arr 10) (aref arr 11)
		      (aref arr 12) (aref arr 13) (aref arr 14) (aref arr 15))
	   offset)
     *queue-view*)))

(defun enqueue-mat-proj ()
  (enqueue-mat (mat-proj *projview*)
	       0))

(defun enqueue-mat-view ()
  (enqueue-mat (mat-view *projview*)
	       (* 16 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;

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
