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

(defun copy-node-to-shm (node &optional (offset-ptr 0))
    
  (with-slots (ptr size)
      (gethash "nodes" (handles-shm *model*))
    
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
      (incf offset-ptr 16)
      
      ;; Glyph, Flags, pad, pad
      ;; (setf (mem-aref ptr :int (+ offset-ptr 0)) (- (char-code data) 32))
      ;; http://www.lispworks.com/documentation/lcl50/aug/aug-90.html#HEADING90-0
      (setf (mem-aref ptr :int (+ offset-ptr 0)) offset-texel-texture) ; tex offset
      (setf (mem-aref ptr :int (+ offset-ptr 1)) (truncate (vx2 dims-texture))) ; tex dim x
      (setf (mem-aref ptr :int (+ offset-ptr 2)) (truncate (vy2 dims-texture))) ; tex dim y
      (setf (mem-aref ptr :int (+ offset-ptr 3)) flags) ; draw
      (incf offset-ptr 4)))

  ;; (fmt-model t "copy-node-to-shm" "offset: ~S, bytes: ~S~%" offset-ptr (* offset-ptr 4))

  t)

;; TODO: Refactor to pass offsets, range, etc.
(defun copy-nodes-to-shm ()
  (digraph:mapc-vertices (lambda (node)
			   (copy-node-to-shm node
					     (* (index node)
						(/ +size-struct-instance+ 4))))
			 (digraph *model*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	(metrics-glyph (gethash (char-code data) (metrics *model*))))

    ;; Set color
    (when color
      (let ((rgba (rgba node)))
	(dotimes (i 16)
	  (setf (aref rgba i) (nth i color)))))

    ;; ascii - 32
    (setf (offset-texel-texture node) (* (- (char-code data) 32) 96 96))
    (setf (dims-texture node) (vec2 96 96))
    
    ;; Set UVs
    (with-slots (bounds
		 translate
		 advance
		 scale
		 scale-uv)
	metrics-glyph

      (let ((translation-mm (translation (model-matrix node))))
	;; (setf (vx3 translation-mm) (- (vx3 cursor) (* (vx2 translate) scale scale-glyph)))
	(setf (vy3 translation-mm) (- (vy3 cursor) (* (vy2 translate) scale scale-glyph)))
	(setf (vz3 translation-mm) (vz3 cursor)))

      ;; (fmt-model t "init-node-msdf" "advance: ~a~%" (* advance scale-glyph))

      (let ((scale-mm (scale (model-matrix node))))
	(setf (vx3 scale-mm) (* (vx2 scale-uv) scale-glyph))
	(setf (vy3 scale-mm) (* (vy2 scale-uv) scale-glyph))))
    
    ;; Update transform
    (update-transform (model-matrix node))
    
    node))

(defun add-node-msdf (seq-key)
  (with-slots (digraph
	       node-pointer
	       scale-node
	       cursor
	       metrics)
      *model*
    (let* ((metrics-space (gethash 32 metrics))
	   (spacing (* (advance metrics-space) (scale metrics-space) scale-node))
	   (cursor-new (vec3 (+ (vx3 cursor) (* 96 scale-node))
			     (vy3 cursor)
			     (vz3 cursor)))
	   (node (init-node-msdf cursor-new
				 scale-node
				 (digraph:count-vertices digraph)
				 (code-char (first seq-key)))))

      ;; Advance - origin to origin
      ;; 1. Find glyph A origin
      ;;    1. Model trans + glyph trans
      ;; 2. Set glyph B origin
      ;;    1. origin A + advance - glyph trans

      (when nil
	(let* ((prev-metrics (gethash (char-code (data node-pointer)) (metrics *model*)))
	       (prev-bl (v+ (translation (model-matrix node-pointer))
			    (vec3 (vx2 (translate prev-metrics))
				  (vy2 (translate prev-metrics))
				  0.0)))
	       (cur-metrics (gethash (char-code (data node-pointer)) (metrics *model*)))
	       (cur-bl (v+ (translation (model-matrix node-pointer))
			   (vec3 (vx2 (translate cur-metrics))
				 (vy2 (translate cur-metrics))
				 0.0))))
	  (setf (vx3 (translation (model-matrix node)))
		(- (+ (vx3 prev-bl)
		      spacing)
		   (vx2 (translate cur-metrics))))))
      
      ;; node-pointer or use digraph:root
      
      (update-transform (model-matrix node))
      
      (digraph:insert-vertex digraph node)

      (digraph:insert-edge digraph node-pointer node)

      (setf node-pointer node)

      ;; Keep X, Y will be adjusted
      (setf cursor (vec3 (vx3 (translation (model-matrix node)))
			 (vy3 cursor)
			 (vz3 cursor)))

      ;; (fmt-model t "init-node-msdf" "cursor: ~a~%" cursor)
      
      (copy-node-to-shm node
			(* (index node)
			   (/ +size-struct-instance+ 4)))

      (memcpy-shm-to-cache-flag* (list (list "nodes"
				       	     0
      				       	     (* +size-struct-instance+ (+ (digraph:count-vertices digraph)
				       					  (digraph:count-edges digraph)))))))))

(defun backspace-node-msdf (seq-key)
  (with-slots (digraph
	       node-pointer
	       scale-node
	       cursor
	       metrics)
      *model*

    ;; Zero node
    (zero-node-to-shm (* (index node-pointer)
			 (/ +size-struct-instance+ 4)))
    (memcpy-shm-to-cache-flag* (list (list "nodes"
				       	   0
      				       	   (* +size-struct-instance+ (+ (digraph:count-vertices digraph)
				       					(digraph:count-edges digraph))))))

    (setf cursor (vec3 (- (vx3 cursor) (* 96 scale-node))
		       (vy3 cursor)
		       (vz3 cursor)))
    ;; (fmt-model t "init-node-msdf" "cursor: ~a~%" cursor)
    
    ;; Get predecessor of pointer
    (let ((pred (first (digraph:predecessors digraph node-pointer))))
    
      (digraph:remove-edge digraph node-pointer pred)
      (digraph:remove-vertex digraph node-pointer)

      (setf node-pointer pred))))


(defun zero-node-to-shm (&optional (offset-ptr 0))
    
  (with-slots (ptr size)
      (gethash "nodes" (handles-shm *model*))

    (dotimes (i (/ +size-struct-instance+ 4))
      (setf (mem-aref ptr :int (+ offset-ptr i)) 0))
    
    ;; (fmt-model t "zero-node-to-shm" "offset: ~S, bytes: ~S~%" offset-ptr (* offset-ptr 4))

    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun move-node-x (node displacement)
  (with-slots (digraph
	       scale-node)
      *model*

    (with-slots (model-matrix
		 index)
	node
      (incf (vx3 (translation model-matrix)) displacement)
      (update-transform model-matrix)
      (copy-node-to-shm node
			(* index
			   (/ +size-struct-instance+ 4)))
      (memcpy-shm-to-cache-flag* (list (list "nodes"
				       	     0
      				       	     (* +size-struct-instance+ (+ (digraph:count-vertices digraph)
				       					  (digraph:count-edges digraph)))))))))

(defun move-node-y (node displacement)
  (with-slots (digraph
	       scale-node)
      *model*

    (with-slots (model-matrix
		 index)
	node
      (incf (vy3 (translation model-matrix)) displacement)
      (update-transform model-matrix)
      (copy-node-to-shm node
			(* index
			   (/ +size-struct-instance+ 4)))
      (memcpy-shm-to-cache-flag* (list (list "nodes"
				       	     0
      				       	     (* +size-struct-instance+ (+ (digraph:count-vertices digraph)
				       					  (digraph:count-edges digraph)))))))))

(defun move-pointer-left (seq-key)
  (with-slots (node-pointer
	       scale-node)
      *model*
    (move-node-x node-pointer
		 (- (* 96 scale-node)))))

(defun move-pointer-up (seq-key)
  (with-slots (node-pointer
	       scale-node)
      *model*
    (move-node-y node-pointer
		 (* 96 scale-node))))

(defun move-pointer-right (seq-key)
  (with-slots (node-pointer
	       scale-node)
      *model*
    (move-node-x node-pointer
		 (* 96 scale-node))))

(defun move-pointer-down (seq-key)
  (with-slots (node-pointer
	       scale-node)
      *model*
    (move-node-y node-pointer
		 (- (* 96 scale-node)))))
