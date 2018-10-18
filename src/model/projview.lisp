(in-package :protoform.model)

(defclass projview ()
  ((width :accessor width :initarg :width :initform nil)
   (height :accessor height :initarg :height :initform nil)
   (projection-matrix :accessor projection-matrix :initarg :projection-matrix :initform nil)
   (projection-type :accessor projection-type :initarg :projection-type :initform nil)
   (ortho-scale :accessor ortho-scale :initarg :ortho-scale :initform 24.0) ; bigger number = smaller view
   (ortho-near :accessor ortho-near :initarg :ortho-near :initform 1)
   (ortho-far :accessor ortho-far :initarg :ortho-far :initform 512)
   (view-matrix :accessor view-matrix :initarg :view-matrix :initform nil)
   (camera-position :accessor camera-position :initarg :camera-position :initform (vec3 0 0 10))
   (camera-rotation :accessor camera-rotation :initarg :camera-rotation :initform (vec3 0 0 0))
   (camera-displacement :accessor camera-displacement :initarg :camera-displacement :initform (vec3 1.0 1.0 0.6))))

(defun copy-projection-matrix-to-shm ()
  (with-slots (projview handles-shm)
      *model*
    (with-slots (ptr size)
	(gethash "projview" handles-shm)
      (set-matrix ptr
		  (projection-matrix projview)
		  0))))

(defun copy-view-matrix-to-shm ()
  (with-slots (projview handles-shm)
      *model*
    (with-slots (ptr size)
	(gethash "projview" handles-shm)
      (set-matrix ptr
		  (view-matrix projview)
		  16))))

;; rename to init-*
(defun update-projection-matrix ()
  (with-slots (width
	       height
	       projection-type
	       projection-matrix
	       ortho-scale
	       ortho-near
	       ortho-far)
      (projview *model*)
    (setf projection-matrix (if (eq projection-type 'perspective)
				(make-perspective-vector width height)
				(make-orthographic-vector width height
							  ortho-scale
							  ortho-near
							  ortho-far)))))

(defun update-view-matrix ()
  (with-slots (view-matrix
	       camera-position
	       camera-rotation)
      (projview *model*)
  (setf view-matrix (minv (m* (mtranslation camera-position)
			      (mrotation +vz+ (vz3 camera-rotation))
			      (mrotation +vy+ (vy3 camera-rotation))
			      (mrotation +vx+ (vx3 camera-rotation))
			      (mscaling (vec3 1 1 1)))))))

(defun copy-projview-to-shm (&optional (memcpy-shm-to-cache t))
    
  (update-projection-matrix)
  (update-view-matrix)
  ;; (write-matrix (view-matrix (projview model)) t)

  (copy-projection-matrix-to-shm)
  (copy-view-matrix-to-shm)

  ;; Flag dirty do at end of loop
  (when memcpy-shm-to-cache
    (memcpy-shm-to-cache "projview")))

(defun update-scale-ortho-in (seq-event) ; in
  (with-slots (projview)
      *model*
    (with-slots (camera-position
		 ortho-scale)
	projview
      (decf (ortho-scale projview)
	    (vz3 (camera-displacement projview)))
      (copy-projview-to-shm))))

(defun update-scale-ortho-out (seq-event) ; out
  (with-slots (projview)
      *model*
    (with-slots (camera-position
		 ortho-scale)
	projview
      (incf (ortho-scale projview)
	    (vz3 (camera-displacement projview)))
      (copy-projview-to-shm))))

(defun move-camera-left (seq-event)
  (with-slots (projview)
      *model*
    (with-slots (camera-position
		 camera-displacement)
	projview
      (decf (vx3 camera-position)
	    (vx3 camera-displacement)))
      (copy-projview-to-shm)))

(defun move-camera-right (seq-event)
  (with-slots (projview)
      *model*
    (with-slots (camera-position
		 camera-displacement)
	projview
      (incf (vx3 camera-position)
	    (vx3 camera-displacement)))
      (copy-projview-to-shm)))

(defun move-camera-up (seq-event)
  (with-slots (projview)
      *model*
    (with-slots (camera-position
		 camera-displacement)
	projview
      (incf (vy3 camera-position)
	    (vy3 camera-displacement)))
      (copy-projview-to-shm)))

(defun move-camera-down (seq-event)
  (with-slots (projview)
      *model*    
    (with-slots (camera-position
		 camera-displacement)
	projview
      (decf (vy3 camera-position)
	    (vy3 camera-displacement)))
      (copy-projview-to-shm)))


(defun update-clip-planes (msdf)
  (multiple-value-bind (near far bottom top left right)
      (extract-clip-planes (m* (projection-matrix msdf)
			       (view-matrix msdf)))
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
