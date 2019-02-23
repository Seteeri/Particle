(in-package :protoform.model)

(defclass projview ()
  ((width       :accessor width       :initarg :width       :initform nil)
   (height      :accessor height      :initarg :height      :initform nil)
   (mat-proj    :accessor mat-proj    :initarg :mat-proj    :initform nil)
   (type-proj   :accessor type-proj   :initarg :type-proj   :initform nil)
   (scale-ortho :accessor scale-ortho :initarg :scale-ortho :initform 48.0)
   ;; bigger number = smaller view
   (near-ortho  :accessor near-ortho :initarg :near-ortho   :initform 1)
   (ortho-far   :accessor ortho-far  :initarg :ortho-far    :initform 512)
   (mat-view    :accessor mat-view   :initarg :mat-view     :initform nil)
   (pos         :accessor pos        :initarg :pos          :initform (vec3 11 -8 10))
   (rot         :accessor rot        :initarg :rot          :initform (vec3 0 0 0))
   (displace    :accessor displace   :initarg :displace     :initform (vec3 4.0 4.0 4.0))))

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
