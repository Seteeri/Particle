(in-package :protoform.model)

(defclass projview ()
  ((width :accessor width :initarg :width :initform nil)
   (height :accessor height :initarg :height :initform nil)
   (mat-proj :accessor mat-proj :initarg :mat-proj :initform nil)
   (type-proj :accessor type-proj :initarg :type-proj :initform nil)
   (scale-ortho :accessor scale-ortho :initarg :scale-ortho :initform 24.0) ; bigger number = smaller view
   (near-ortho :accessor near-ortho :initarg :near-ortho :initform 1)
   (ortho-far :accessor ortho-far :initarg :ortho-far :initform 512)
   (mat-view :accessor mat-view :initarg :mat-view :initform nil)
   (pos :accessor pos :initarg :pos :initform (vec3 0 0 10))
   (rot :accessor rot :initarg :rot :initform (vec3 0 0 0))
   (displace :accessor displace :initarg :displace :initform (vec3 1.0 1.0 0.6))))

(defun update-mat-proj ()
  (with-slots (width
	       height
	       type-proj
	       mat-proj
	       scale-ortho
	       near-ortho
	       ortho-far)
      (projview *model*)
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
      (projview *model*)
    (setf mat-view (minv (m* (mtranslation pos)
			     (mrotation +vz+ (vz3 rot))
			     (mrotation +vy+ (vy3 rot))
			     (mrotation +vx+ (vx3 rot))
			     (mscaling (vec3 1 1 1)))))))

(defun copy-mat-proj-to-shm ()
  (with-slots (projview handles-shm)
      *model*
    (with-slots (ptr size)
	(gethash "projview" handles-shm)
      (set-matrix ptr
		  (mat-proj projview)
		  0))))

(defun copy-mat-view-to-shm ()
  (with-slots (projview handles-shm)
      *model*
    (with-slots (ptr size)
	(gethash "projview" handles-shm)
      (set-matrix ptr
		  (mat-view projview)
		  16))))

(defun copy-projview-to-shm (&optional (memcpy-shm-to-cache t))
  
  (update-mat-proj)
  (update-mat-view)
  ;; (write-matrix (mat-view (projview model)) t)

  (copy-mat-proj-to-shm)
  (copy-mat-view-to-shm)
  
  ;; Flag dirty do at end of loop
  (when memcpy-shm-to-cache
    (memcpy-shm-to-cache "projview")))

(defun update-scale-ortho-in (seq-event) ; in
  (with-slots (projview)
      *model*
    (with-slots (pos
		 scale-ortho)
	projview
      (decf (scale-ortho projview)
	    (vz3 (displace projview)))
      (copy-projview-to-shm))))

(defun update-scale-ortho-out (seq-event) ; out
  (with-slots (projview)
      *model*
    (with-slots (pos
		 scale-ortho)
	projview
      (incf (scale-ortho projview)
	    (vz3 (displace projview)))
      (copy-projview-to-shm))))

(defun move-camera-left (seq-event)
  (with-slots (projview)
      *model*
    (with-slots (pos
		 displace)
	projview
      (decf (vx3 pos)
	    (vx3 displace)))
    (copy-projview-to-shm)))

(defun move-camera-right (seq-event)
  (with-slots (projview)
      *model*
    (with-slots (pos
		 displace)
	projview
      (incf (vx3 pos)
	    (vx3 displace)))
    (copy-projview-to-shm)))

(defun move-camera-up (seq-event)
  (with-slots (projview)
      *model*
    (with-slots (pos
		 displace)
	projview
      (incf (vy3 pos)
	    (vy3 displace)))
    (copy-projview-to-shm)))

(defun move-camera-down (seq-event)
  (with-slots (projview)
      *model*    
    (with-slots (pos
		 displace)
	projview
      (decf (vy3 pos)
	    (vy3 displace)))
    (copy-projview-to-shm)))


(defun update-clip-planes (msdf)
  (multiple-value-bind (near far bottom top left right)
      (extract-clip-planes (m* (mat-proj msdf)
			       (mat-view msdf)))
    (cffi-sys:with-pointer-to-vector-data (ptr (make-array 4
							   :element-type 'single-float
							   :initial-contents (list (vx4 near)
										   (vy4 near)
										   (vz4 near)
										   (vw4 near))))
      (%gl:uniform-4fv (gl:get-uniform-location (program-compute msdf) "clip_near")
		       1
		       ptr))
    (cffi-sys:with-pointer-to-vector-data (ptr (make-array 4
							   :element-type 'single-float
							   :initial-contents (list (vx4 far)
										   (vy4 far)
										   (vz4 far)
										   (vw4 far))))
      (%gl:uniform-4fv (gl:get-uniform-location (program-compute msdf) "clip_far")
		       1
		       ptr))
    (cffi-sys:with-pointer-to-vector-data (ptr (make-array 4
							   :element-type 'single-float
							   :initial-contents (list (vx4 bottom)
										   (vy4 bottom)
										   (vz4 bottom)
										   (vw4 bottom))))
      (%gl:uniform-4fv (gl:get-uniform-location (program-compute msdf) "clip_bottom")
		       1
		       ptr))
    (cffi-sys:with-pointer-to-vector-data (ptr (make-array 4
							   :element-type 'single-float
							   :initial-contents (list (vx4 top)
										   (vy4 top)
										   (vz4 top)
										   (vw4 top))))
      (%gl:uniform-4fv (gl:get-uniform-location (program-compute msdf) "clip_top")
		       1
		       ptr))
    (cffi-sys:with-pointer-to-vector-data (ptr (make-array 4
							   :element-type 'single-float
							   :initial-contents (list (vx4 left)
										   (vy4 left)
										   (vz4 left)
										   (vw4 left))))
      (%gl:uniform-4fv (gl:get-uniform-location (program-compute msdf) "clip_left")
		       1
		       ptr))
    (cffi-sys:with-pointer-to-vector-data (ptr (make-array 4
							   :element-type 'single-float
							   :initial-contents (list (vx4 right)
										   (vy4 right)
										   (vz4 right)
										   (vw4 right))))
      (%gl:uniform-4fv (gl:get-uniform-location (program-compute msdf) "clip_right")
		       1
		       ptr))))
