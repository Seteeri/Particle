(in-package :protoform.model)

(defconstant +size-struct-instance+ 208)

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
	(setf (vy3 translation-mm) (- (vy3 cursor) (* (vy2 translate) scale scale-glyph))
	      (vz3 translation-mm) (vz3 cursor)))

      ;; (fmt-model t "init-node-msdf" "advance: ~a~%" (* advance scale-glyph))

      (let ((scale-mm (scale (model-matrix node))))
	(setf (vx3 scale-mm) (* (vx2 dims-glyph) scale-glyph)
	      (vy3 scale-mm) (* (vy2 dims-glyph) scale-glyph))))
    
    ;; Update transform
    (update-transform (model-matrix node))
    
    node))

(defun serialize-node (node)
  ;; (let ((data (make-array +size-struct-instance+
  ;; 			  :fill-pointer 0
  ;;                      :adjustable nil
  ;; 			  :element-type '(unsigned-byte 8))))
  (with-slots (data
	       model-matrix
	       rgba
	       offset-texel-texture
	       dims-texture
	       uv
	       flags)
      node
    (let ((marr (marr (matrix model-matrix))))
      (pack:pack "<48f4i"
		 (aref marr 0)  (aref marr 1)  (aref marr 2)  (aref marr 3)
		 (aref marr 4)  (aref marr 5)  (aref marr 6)  (aref marr 7)
		 (aref marr 8)  (aref marr 9)  (aref marr 10) (aref marr 11)
		 (aref marr 12) (aref marr 13) (aref marr 14) (aref marr 15)
		 (aref rgba 0)  (aref rgba 1)  (aref rgba 2)  (aref rgba 3)
		 (aref rgba 4)  (aref rgba 5)  (aref rgba 6)  (aref rgba 7)
		 (aref rgba 8)  (aref rgba 9)  (aref rgba 10) (aref rgba 11)
		 (aref rgba 12) (aref rgba 13) (aref rgba 14) (aref rgba 15)
		 (aref uv 0)    (aref uv 1)    (aref uv 2)    (aref uv 3)
		 (aref uv 4)    (aref uv 5)    (aref uv 6)    (aref uv 7)
		 (aref uv 8)    (aref uv 9)    (aref uv 10)   (aref uv 11)
		 (aref uv 12)   (aref uv 13)   (aref uv 14)   (aref uv 15)
		 offset-texel-texture
		 (aref dims-texture 0)
		 (aref dims-texture 1)
		 flags))))

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

      (let ((marr (marr (matrix model-matrix))))
	(setf (mem-aref ptr :float offset-ptr)        (aref marr 0)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 1)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 2)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 3)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 4)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 5)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 6)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 7)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 8)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 9)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 10)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 11)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 12)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 13)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 14)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 15)))

      (setf (mem-aref ptr :float (incf offset-ptr)) (aref rgba 0)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 1)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 2)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 3)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 4)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 5)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 6)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 7)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 8)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 9)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 10)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 11)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 12)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 13)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 14)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 15)
	    
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 0)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 1)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 2)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 3)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 4)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 5)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 6)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 7)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 8)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 9)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 10)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 11)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 12)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 13)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 14)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 15)
      
	    ;; http://www.lispworks.com/documentation/lcl50/aug/aug-90.html#HEADING90-0
            (mem-aref ptr :int (incf offset-ptr)) offset-texel-texture  ; tex offset
	    (mem-aref ptr :int (incf offset-ptr)) (aref dims-texture 0) ; tex dim x
	    (mem-aref ptr :int (incf offset-ptr)) (aref dims-texture 1) ; tex dim y
	    (mem-aref ptr :int (incf offset-ptr)) flags))))

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
      (setf (mem-aref ptr :int (+ offset-ptr i)) 0))))
