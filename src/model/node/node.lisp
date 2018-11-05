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

(defparameter +linegap+ (+ 96 (* 8 5.8239365)))

(defclass node ()
  ((data :accessor data :initarg :data :initform nil)
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

(defun copy-node-to-shm (node &optional (offset-ptr 0))
  
  (with-slots (ptr size)
      *shm-nodes*
    
    (with-slots (data
		 model-matrix
		 rgba
		 offset-texel-texture
		 dims-texture
		 uv
		 flags)
	node

      ;; Unroll below
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
      (incf offset-ptr 16)
      
      ;; Glyph, Flags, pad, pad
      ;; (setf (mem-aref ptr :int (+ offset-ptr 0)) (- (char-code data) 32))
      ;; http://www.lispworks.com/documentation/lcl50/aug/aug-90.html#HEADING90-0
      (setf (mem-aref ptr :int (+ offset-ptr 0)) offset-texel-texture ; tex offset
	    (mem-aref ptr :int (+ offset-ptr 1)) (aref dims-texture 0) ; tex dim x
	    (mem-aref ptr :int (+ offset-ptr 2)) (aref dims-texture 1) ; tex dim y
	    (mem-aref ptr :int (+ offset-ptr 3)) flags) ; draw
      (incf offset-ptr 4)))

  ;; (fmt-model t "copy-node-to-shm" "offset: ~S, bytes: ~S~%" offset-ptr (* offset-ptr 4))

  t)

;; TODO: Refactor to pass offsets, range, etc.
(defun copy-nodes-to-shm ()
  (digraph:mapc-vertices (lambda (node)
			   (copy-node-to-shm node
					     (* (index node)
						(/ +size-struct-instance+ 4))))
			 *digraph*))

(defun zero-node-to-shm (&optional (offset-ptr 0))
  (with-slots (ptr size)
      *shm-nodes*
    (dotimes (i (/ +size-struct-instance+ 4))
      (setf (mem-aref ptr :int (+ offset-ptr i)) 0))
    ;; (fmt-model t "zero-node-to-shm" "offset: ~S, bytes: ~S~%" offset-ptr (* offset-ptr 4))
    t))

(defun init-node-msdf (cursor
		       scale-glyph
		       ix
		       data
		       &optional
			 (color nil))

  (let ((node (make-instance 'node
			     :data data
			     :index ix
			     :model-matrix (make-instance 'model-matrix
							  :scale (vec3 scale-glyph scale-glyph scale-glyph)
							  :translation (vcopy3 cursor))))
	(metrics-glyph (gethash (char-code data) *metrics*)))
    
    ;; Set color
    (when color
      (let ((rgba (rgba node)))
	(dotimes (i 16)
	  (setf (aref rgba i) (nth i color)))))

    ;; ascii - 1
    (setf (offset-texel-texture node) (* (- (char-code data) 1) 96 96)
          (dims-texture node)         (make-array 2
						  :adjustable nil
						  :initial-contents '(96 96)))
    
    ;; Set UVs
    (with-slots (advance
		 translate
		 bounds
		 scale
		 dims-glyph)
	metrics-glyph

      (let ((translation-mm (translation (model-matrix node))))
	;; (setf (vx3 translation-mm) (- (vx3 cursor) (* (vx2 translate) scale scale-glyph)))
	(setf (vy3 translation-mm) (- (vy3 cursor) (* (vy2 translate) scale scale-glyph)))
	(setf (vz3 translation-mm) (vz3 cursor)))

      ;; (fmt-model t "init-node-msdf" "advance: ~a~%" (* advance scale-glyph))

      (let ((scale-mm (scale (model-matrix node))))
	(setf (vx3 scale-mm) (* (vx2 dims-glyph) scale-glyph))
	(setf (vy3 scale-mm) (* (vy2 dims-glyph) scale-glyph))))
    
    ;; Update transform
    (update-transform (model-matrix node))
    
    node))
