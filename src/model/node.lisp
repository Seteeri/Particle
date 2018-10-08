(in-package :protoform.model)

(defparameter *uv-default-node* (list 1.0 1.0  0.0 0.0
			  	      1.0 0.0  0.0 0.0
			  	      0.0 0.0  0.0 0.0
			  	      0.0 1.0  0.0 0.0))

(defparameter *color-default-node* (list  (coerce (/ 131 255) 'single-float)
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
					  (coerce (/ 255 255) 'single-float)))

(defclass node ()
  ((data :accessor data :initarg :data :initform nil) ; formerly chr
   (index :accessor index :initarg :index :initform nil)
   (origin :accessor origin :initarg :origin :initform (vec3 0 0 0))
   (model-matrix :accessor model-matrix :initarg :model-matrix :initform (make-instance 'model-matrix))
   (rgba :accessor rgba :initarg :rgba :initform (make-array (* 4 4) ; or use vec4
							     :adjustable nil
							     :fill-pointer nil
							     :element-type 'single-float
							     :initial-contents *color-default-node*))
   
   (offset-texel-texture :accessor offset-texel-texture :initarg :offset-texel-texture :initform 0)
   (dims-texture :accessor dims-texture :initarg :dims-texture :initform (vec2 0 0))
   (uv :accessor uv :initform (make-array 16
					  :adjustable nil
					  :fill-pointer nil
					  :element-type 'single-float
					  :initial-contents *uv-default-node*))
   (flags :accessor flags :initarg :flags :initform 1)))

(defun init-node (cursor
		  scale
		  ix
		  data)
  (let ((node (make-instance 'node
			     :data data
			     :index ix
			     :model-matrix (make-instance 'model-matrix
							  :scale scale
							  :translation cursor))))
    
    (update-node-texture node
			 data)
    
    ;; Update transform
    (update-transform (model-matrix node))
    
    node))

(defun update-node-texture (node
			    data)

  ;; Separate this function maybe
  (multiple-value-bind (offset-texel-texture
			dims-texture
			data-size)
      (convert-pm-to-texture
       (format nil "<span foreground=\"#FFCC00\" font=\"Inconsolata-g 59\" strikethrough=\"false\">~A</span>" data))
    
    (setf (offset-texel-texture node) offset-texel-texture)
    (setf (dims-texture node) dims-texture)

    (fmt-model t "update-node-texture"
	       (with-output-to-string (stream)
		 (write-char #\Newline stream)
		 (format stream "  Name: ~S~%" data)
		 (format stream "  Dims: ~S~%" dims-texture)
		 (format stream "  Start Texels Offset: ~S~%" offset-texel-texture)
		 (format stream "  Data Size: ~S~%" data-size)
		 (format stream "  Current Offset: ~S~%" (offset-bytes-textures *model*)))))

  ;; Update scale to match texture
  (setf (vx3 (scale (model-matrix node))) (* (/ 1 96) (vx2 (dims-texture node))))
  (setf (vy3 (scale (model-matrix node))) (* (/ 1 96) (vy2 (dims-texture node)))))

(defun copy-node-to-shm (node &optional (offset-ptr 0))
    
  (with-slots (ptr size)
      (gethash "instance" (handles-shm *model*))
    
    (with-slots (data
		 model-matrix
		 rgba
		 offset-texel-texture
		 dims-texture
		 uv
		 flags)
	node
      
      (loop
	 :for c :across (marr (matrix model-matrix))
	 :for c-i :upfrom 0
	 :do (setf (mem-aref ptr :float (+ offset-ptr c-i))
		   c))
      (incf offset-ptr 16)
      
      (loop
	 :for c :across rgba
	 :for c-i :upfrom 0
	 :do (setf (mem-aref ptr :float (+ offset-ptr c-i))
		   c))
      (incf offset-ptr 16)

      (loop
	 :for c :across uv
	 :for c-i :upfrom 0
	 :do (setf (mem-aref ptr :float (+ offset-ptr c-i))
		   c))
      ;; (incf offset-ptr 8) ; double check this
      (incf offset-ptr 16)
      
      ;; Glyph, Flags, pad, pad
      ;; (setf (mem-aref ptr :int (+ offset-ptr 0)) (- (char-code data) 32))
      ;; http://www.lispworks.com/documentation/lcl50/aug/aug-90.html#HEADING90-0
      (setf (mem-aref ptr :int (+ offset-ptr 0)) offset-texel-texture) ; tex offset
      (setf (mem-aref ptr :int (+ offset-ptr 1)) (truncate (vx2 dims-texture))) ; tex dim x
      (setf (mem-aref ptr :int (+ offset-ptr 2)) (truncate (vy2 dims-texture))) ; tex dim y
      (setf (mem-aref ptr :int (+ offset-ptr 3)) flags) ; draw
      (incf offset-ptr 4)))

  (fmt-model t "copy-node-to-shm" "offset: ~S, bytes: ~S~%" offset-ptr (* offset-ptr 4)))
