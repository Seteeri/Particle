(in-package :protoform.model)

(defclass node ()
  ((chr :accessor chr :initarg :chr :initform nil)
   (model-matrix :accessor model-matrix :initarg :model-matrix :initform (make-instance 'model-matrix))
   (rgba :accessor rgba :initarg :rgba :initform (make-array (* 4 4) ; or use vec4
							     :adjustable nil
							     :fill-pointer nil
							     :element-type 'single-float
							     :initial-contents (list  (coerce (/ 131 255) 'single-float)
							     			      (coerce (/ 148 255) 'single-float)
							     			      (coerce (/ 155 255) 'single-float)
							     			      (coerce (/ 255 255) 'single-float)

							     			      (coerce (/ 131 255) 'single-float)
							     			      (coerce (/ 148 255) 'single-float)
							     			      (coerce (/ 155 255) 'single-float)
							     			      (coerce (/ 255 255) 'single-float)

							     			      (coerce (/ 131 255) 'single-float)
							     			      (coerce (/ 148 255) 'single-float)
							     			      (coerce (/ 155 255) 'single-float)
							     			      (coerce (/ 255 255) 'single-float)

							     			      (coerce (/ 131 255) 'single-float)
							     			      (coerce (/ 148 255) 'single-float)
							     			      (coerce (/ 155 255) 'single-float)
							     			      (coerce (/ 255 255) 'single-float))))
   ;; top right, bottom right, bottom left, top left
   (uv
    :accessor uv
    :initform (make-array 16
			  :adjustable nil
			  :fill-pointer nil
			  :element-type 'single-float
			  :initial-contents (list 1.0 1.0  0.0 0.0
			  			  1.0 0.0  0.0 0.0
			  			  0.0 0.0  0.0 0.0
			  			  0.0 1.0  0.0 0.0)))
   (flags :accessor flags :initarg :flags :initform 1)
   (coline :accessor coline :initarg :coline :initform (vec2 0.0 0.0))))

;; uvw retrived from metrics

(defun init-node (cursor
		  scale-glyph
		  ch)
  (let* ((node (make-instance 'node :chr ch))
	 (model-matrix (model-matrix node)))
    (update-transform-node cursor
			   scale-glyph
			   node)
    node))


(defun update-transform-node (cursor
			      scale-glyph
			      node)
  ;; Setup model matrix
  (with-slots (matrix
	       translation
	       rotation
	       scale)
      (model-matrix node)
    
    (setf (vx3 translation) (vx3 cursor))
    (setf (vy3 translation) (vy3 cursor))
    (setf (vz3 translation) (vz3 cursor))
    
    (setf (vx3 scale) (* 4.0 (/ 58 113)))
    (setf (vy3 scale) 4.0) ; 58/113
    (setf (vz3 scale) 4.0)

    ;; (setf rotation (vec3 0.0 0.0 0.0))
    
    (setf matrix (mtranspose (m* (mtranslation translation)
				 (mrotation +vz+ (vz3 rotation))
				 (mrotation +vy+ (vy3 rotation))
				 (mrotation +vx+ (vx3 rotation))
				 (mscaling scale))))))
