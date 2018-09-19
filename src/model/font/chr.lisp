(in-package :protoform.model)

(defclass chr ()
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
   (flags :accessor flags :initarg :flags :initform 1)
   (coline :accessor coline :initarg :coline :initform (vec2 0.0 0.0))))

;; uvw retrived from metrics

(defun init-chr (cursor
		 metrics-glyph
		 scale-glyph
		 dpi-glyph
		 ch)
  (let* ((chr (make-instance 'chr :chr ch))
	 (model-matrix (model-matrix chr)))
    (update-transform-chr cursor
			  metrics-glyph
			  scale-glyph
			  dpi-glyph
			  chr)
    chr))


(defun update-transform-chr (cursor
			     metrics-glyph
			     scale-glyph
			     dpi-glyph
			     chr)
  
  ;; Setup model matrix
  (with-slots (matrix
	       translation
	       rotation
	       scale)
      (model-matrix chr)
    
    (with-slots (bounds
		 advance
		 ;; scale
		 translate
		 range
		 ratio-aspect
		 scale-uv)
	metrics-glyph
      
      (setf (vx3 translation) (+ (vx3 cursor) (* (aref bounds 0) scale-glyph)))
      (setf (vy3 translation) (+ (vy3 cursor) (* (aref bounds 1) scale-glyph)))
      (setf (vz3 translation) (vz3 cursor))

      ;; * adv scale-glyph = constant
      ;; * dpi-glyph 1 = constant
      (incf (vx3 cursor) (* advance scale-glyph))
      ;; (incf (vz3 cursor) (* dpi-glyph 1)) ; dpi is arbitrary
      
      (setf (vx3 scale) (vx2 scale-uv))
      (setf (vy3 scale) (vy2 scale-uv))
      (setf (vz3 scale)	1.0)

      ;; (setf rotation (vec3 0.0 0.0 0.0))
      
      (setf matrix (mtranspose (m* (mtranslation translation)
				   (mrotation +vz+ (vz3 rotation))
				   (mrotation +vy+ (vy3 rotation))
				   (mrotation +vx+ (vx3 rotation))
				   (mscaling scale)))))))
