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
   (origin :accessor origin :initarg :origin :initform (vec3 0 0 0))
   (model-matrix :accessor model-matrix :initarg :model-matrix :initform (make-instance 'model-matrix))
   (rgba :accessor rgba :initarg :rgba :initform (make-array (* 4 4) ; or use vec4
							     :adjustable nil
							     :fill-pointer nil
							     :element-type 'single-float
							     :initial-contents *color-default-node*))
   
   (offset-texture :accessor offset-texture :initarg :offset-texture :initform nil)
   (dimensions-texture :accessor dimensions-texture :initarg :dimensions-texture :initform nil)
   (uv :accessor uv :initform (make-array 16
					  :adjustable nil
					  :fill-pointer nil
					  :element-type 'single-float
					  :initial-contents *uv-default-node*))
   (flags :accessor flags :initarg :flags :initform 1)))

(defun init-node (cursor
		  scale
		  data)
  (let* ((node (make-instance 'node
			      :data data))
	 (model-matrix (model-matrix node)))

    ;; make scale a vector
    (setf (vx3 (scale model-matrix)) scale)
    (setf (vy3 (scale model-matrix)) scale)
    (setf (vz3 (scale model-matrix)) scale)
    
    (update-transform cursor
		      model-matrix)
    
    node))

;; move to model matrix file?
(defun update-transform (cursor
			 model-matrix)
  ;; Setup model matrix
  (with-slots (matrix
	       translation
	       rotation
	       scale)
      model-matrix
    
    (setf (vx3 translation) (vx3 cursor))
    (setf (vy3 translation) (vy3 cursor))
    (setf (vz3 translation) (vz3 cursor))
    
    (setf (vx3 scale) (* 1.0 (/ 10 20))) ; ratio
    (setf (vy3 scale) 1.0)
    (setf (vz3 scale) 1.0)

    ;; (setf rotation (vec3 0.0 0.0 0.0))
    
    (setf matrix (mtranspose (m* (mtranslation translation)
				 (mrotation +vz+ (vz3 rotation))
				 (mrotation +vy+ (vy3 rotation))
				 (mrotation +vx+ (vx3 rotation))
				 (mscaling scale))))))

(defun copy-node-to-shm (node)
  
  (let* ((offset-ptr 0)) ; index
    
    (with-slots (ptr size)
	(gethash "instance" (handles-shm *model*))
      
      (with-slots (data
		   model-matrix
		   rgba
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
	(setf (mem-aref ptr :int (+ offset-ptr 0)) 0) ; tex offset
	(setf (mem-aref ptr :int (+ offset-ptr 1)) 10) ; tex dim x
	(setf (mem-aref ptr :int (+ offset-ptr 2)) 20) ; tex dim y
	(setf (mem-aref ptr :int (+ offset-ptr 3)) flags) ; draw
	(incf offset-ptr 4)

	t))))
