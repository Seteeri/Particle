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
  (with-slots (ptr size)
      *shm-projview*
    (set-matrix ptr
		(mat-proj *projview*)
		0)))

(defun copy-mat-view-to-shm ()
  (with-slots (ptr size)
      *shm-projview*
    (set-matrix ptr
		(mat-view *projview*)
		16)))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun enqueue-mat-proj ()
  (let ((arr-proj (marr (mtranspose (mat-proj *projview*)))))
    (sb-concurrency:enqueue
     (list *channel*
	   *shm-projview*
	   (pack:pack "<16f"
		      (aref arr-proj 0)  (aref arr-proj 1)  (aref arr-proj 2)  (aref arr-proj 3)
		      (aref arr-proj 4)  (aref arr-proj 5)  (aref arr-proj 6)  (aref arr-proj 7)
		      (aref arr-proj 8)  (aref arr-proj 9)  (aref arr-proj 10) (aref arr-proj 11)
		      (aref arr-proj 12) (aref arr-proj 13) (aref arr-proj 14) (aref arr-proj 15))
	   0)
     *queue-view*)))

(defun enqueue-mat-view ()
  (let ((arr-view (marr (mtranspose (mat-view *projview*)))))
    (sb-concurrency:enqueue
     (list *channel*
	   *shm-projview*
	   (pack:pack "<16f"
		      (aref arr-view 0)  (aref arr-view 1)  (aref arr-view 2)  (aref arr-view 3)
		      (aref arr-view 4)  (aref arr-view 5)  (aref arr-view 6)  (aref arr-view 7)
		      (aref arr-view 8)  (aref arr-view 9)  (aref arr-view 10) (aref arr-view 11)
		      (aref arr-view 12) (aref arr-view 13) (aref arr-view 14) (aref arr-view 15))
	   (* 16 4))
     *queue-view*)))

(defun scale-ortho-in (seq-event) ; in
  (with-slots (scale-ortho
	       displace)
      *projview*
    (fmt-model t "scale-ortho-in" "~a -> ~a~%"
	       scale-ortho
	       (decf scale-ortho
		     (vz3 displace))))
  (update-mat-proj)
  (enqueue-mat-proj))

(defun scale-ortho-out (seq-event) ; out
  (with-slots (scale-ortho
	       displace)
      *projview*
    (fmt-model t "scale-ortho-out" "~a -> ~a~%"
	       scale-ortho
	       (incf scale-ortho
		     (vz3 displace))))
  (update-mat-proj)
  (enqueue-mat-proj))


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
