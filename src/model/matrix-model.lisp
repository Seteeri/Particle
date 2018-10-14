(in-package :protoform.model)

(defclass model-matrix ()
  ((matrix :accessor matrix :initarg :matrix :initform (mat4 1))
   (translation :accessor translation :initarg :translation :initform (vec3 0.0 0.0 0.0))
   (rotation :accessor rotation :initarg :rotation :initform (vec3 0.0 0.0 0.0))
   (scale :accessor scale :initarg :scale :initform (vec3 1.0 1.0 1.0))))

(defmethod initialize-instance :after ((mm model-matrix) &key)
  (update-transform mm))

(defun set-matrix (ptr-dest matrix-src offset)
  (let ((matrix-arr (marr (mtranspose matrix-src))))
    (dotimes (i 16)
      (setf (mem-aref ptr-dest :float (+ offset i))
	    (aref matrix-arr i)))))

;; move to model matrix file?
(defun update-transform (model-matrix)
  (with-slots (matrix
	       translation
	       rotation
	       scale)
      model-matrix
    (setf matrix (mtranspose (m* (mtranslation translation)
    				 (mrotation +vz+ (vz3 rotation))
    				 (mrotation +vy+ (vy3 rotation))
    				 (mrotation +vx+ (vx3 rotation))
    				 (mscaling scale))))))
