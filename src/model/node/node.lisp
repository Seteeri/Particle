(in-package :protoform.model)

(defconstant +size-struct-instance+ 208)
(defconstant +scale-msdf+ 5.8239365)
(defconstant +spaces-tab+ 1)

(defparameter *dpi-glyph* (/ 1 90))
(defparameter *scale-node* 0.008)
(defparameter +linegap+ (* (* 9.375 2)    ;; use advance....
			   +scale-msdf+))  ;; multiplied by scale-node later (per node)

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

(defparameter *data-zero-node* (make-array +size-struct-instance+
					   :adjustable nil
					   :fill-pointer nil
					   :element-type '(unsigned-byte 8)
					   :initial-element (coerce 0 '(unsigned-byte 8))))
;; :data (make-array size
;; 		  :element-type '(unsigned-byte 8)
;; 		  :initial-element (coerce 0 '(unsigned-byte 8)))

(defclass node ()
  ((char-glyph :accessor char-glyph :initarg :char-glyph :initform nil)
   (data-obj :accessor data-obj :initarg :data-obj :initform nil)
   
   (offset-texel-texture :accessor offset-texel-texture :initarg :offset-texel-texture :initform 0)
   (dims-texture :accessor dims-texture :initarg :dims-texture :initform (make-array 2
										     :adjustable nil
										     :fill-pointer nil
										     :element-type 'integer
										     :initial-contents '(0 0)))
   (uv :accessor uv :initform (make-array 16
					  :adjustable nil
					  :fill-pointer nil
					  :element-type 'single-float
					  :initial-contents *uv-default-node*))
   
   (index :accessor index :initarg :index :initform nil)
   (origin :accessor origin :initarg :origin :initform (vec3 0 0 0))
   (model-matrix :accessor model-matrix :initarg :model-matrix :initform (make-instance 'model-matrix))
   (rgba :accessor rgba :initarg :rgba :initform (make-array (* 4 4) ; or use vec4
							     :adjustable nil
							     :fill-pointer nil
							     :element-type 'single-float
							     :initial-contents *color-default-node*))
   (flags :accessor flags :initarg :flags :initform 1)))

(defun insert-vertex (vert &optional
			     (digraph *digraph-main*)
			     (verts   *vertices-main*))
  (digraph:insert-vertex digraph vert)
  (sb-ext:atomic-incf (car verts)))

(defun remove-vertex (vert &optional
			     (digraph *digraph-main*)
			     (verts   *vertices-main*))
  (digraph:remove-vertex digraph vert)
  (sb-ext:atomic-decf (car verts)))

(defun insert-edge (vert-a
		    vert-b
		    &optional
		      (digraph *digraph-main*)
		      (edges   *edges-main*))
  (digraph:insert-edge digraph vert-a vert-b)
  (sb-ext:atomic-incf (car edges)))

(defun remove-edge (vert-a
		    vert-b
		    &optional
		      (digraph *digraph-main*)
		      (edges   *edges-main*))
  (digraph:remove-edge digraph vert-a vert-b)
  (sb-ext:atomic-decf (car edges)))

(defun node-rect (node)
  "make a bounding box function."
  (with-slots (model-matrix)
      node

    (let ((pos (translation model-matrix))
	  (scale (scale model-matrix)))
      
      ;; Use scale to create bounds
      ;; Origin/pos is from bottom-left
      ;; Note, scale is already adjusted by glyph scale

      ;; lx,ly = left-bottom
      ;; hx,hy = top right
      
      (rectangles:make-rectangle
       :lows (list (vx3 pos)
		   (vy3 pos))
       :highs (list (+ (vx3 pos) (vx3 scale))
		    (+ (vy3 pos) (vy3 scale)))))))

(defun init-node-msdf (cursor
		       scale-glyph
		       ix
		       &optional
			 (char-glyph nil)
			 (update-transform t))

  (let ((node (make-instance 'node
			     :char-glyph char-glyph
			     :index ix
			     :model-matrix (make-instance 'model-matrix
							  :scale (vec3 scale-glyph
								       scale-glyph
								       scale-glyph)
							  :translation (vcopy3 cursor)))))
    
    ;; Set color

    (when char-glyph
      (update-glyph-node node char-glyph cursor))
    
    ;; Update transform - make optional
    (when update-transform
      (update-transform node))
    
    node))

(defun update-transform-node (node)
  (update-transform (model-matrix node)))

(defun update-translation-node (node translation-new)
  (with-slots (translation)
      (model-matrix node)
    (setf translation translation-new)))

(defun update-rgba-node (node rgba-new)
  (with-slots (rgba)
      node
    (setf (aref rgba 0)  (aref rgba-new 0)
	  (aref rgba 1)  (aref rgba-new 1)
	  (aref rgba 2)  (aref rgba-new 2)
	  (aref rgba 3)  (aref rgba-new 3)
	  (aref rgba 4)  (aref rgba-new 4)
	  (aref rgba 5)  (aref rgba-new 5)
	  (aref rgba 6)  (aref rgba-new 6)
	  (aref rgba 7)  (aref rgba-new 7)
	  (aref rgba 8)  (aref rgba-new 8)
	  (aref rgba 9)  (aref rgba-new 9)
	  (aref rgba 10) (aref rgba-new 10)
	  (aref rgba 11) (aref rgba-new 11)
	  (aref rgba 12) (aref rgba-new 12)
	  (aref rgba 13) (aref rgba-new 13)
	  (aref rgba 14) (aref rgba-new 14)
	  (aref rgba 15) (aref rgba-new 15))))

(defun update-glyph-node (node char-glyph-new)
  ;; ascii - 1
  (with-slots (offset-texel-texture dims-texture model-matrix char-glyph)
      node

    (setf char-glyph char-glyph-new)
    
    (setf offset-texel-texture (* (- (char-code char-glyph) 1) 96 96)
          dims-texture         (make-array 2
					   :adjustable nil
					   :initial-contents '(96 96)))
    
    (with-slots (bounds-origin
		 dims-glyph
		 uv)
	(gethash (char-code char-glyph) *metrics*)

      ;; All textures have been cropped through UV
      ;; so glyphs touch edges of node - allows proper spacing
      ;; so nodes don't physically overlap
      ;; This means the y position needs to be adjusted
      ;; by the rel-to-baseline/translate bottom bounds
      ;;
      ;; The x position is adjusted also...
      ;;
      ;; Fonts are monospaced so the advance is the same for every font
      ;; Advance is from one origin to the next origin
      ;; The left bottom corner of the first char/node is considered the baseline
      ;; - Question is whether the glyph should touch that or adjust from there
      ;;   - Then first char x pos would need not be shifted
      (let ((translation (translation model-matrix))
	    (scale (scale model-matrix)))

	;; Translation should be the baseline already
	(setf (vx3 translation) (+ (vx3 translation)
				   (* (aref bounds-origin 0)
				      (vx3 scale)))
	      (vy3 translation) (+ (vy3 translation)
				   (* (aref bounds-origin 1)
				      (vy3 scale))))

	;; Aspect ratio of node must match aspect ratio of UV
	;; otherwise texture will appear distorted
	;; assume scale already set on init-node
	(setf (vx3 scale) (* (vx2 dims-glyph)
			     (vx3 scale))
      	      (vy3 scale) (* (vy2 dims-glyph)
			     (vy3 scale))))
      
      (setf (uv node) uv))))

(defun serialize-node (node)
  (with-slots (model-matrix
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

;; idk where to put this, but for now...
(defun randomize-color-node (vert)
  ;; random color
  (setf (aref (rgba vert) 0)  (coerce (random 1.0) 'single-float)
	(aref (rgba vert) 1)  (coerce (random 1.0) 'single-float)
	(aref (rgba vert) 2)  (coerce (random 1.0) 'single-float)
	;; (aref (rgba vert) 3)  (coerce (/ 255 255)  'single-float)
	
	(aref (rgba vert) 4)  (coerce (random 1.0) 'single-float)
	(aref (rgba vert) 5)  (coerce (random 1.0) 'single-float)
	(aref (rgba vert) 6)  (coerce (random 1.0) 'single-float)
	;; (aref (rgba vert) 7)  (coerce (/ 255 255)  'single-float)
	
	(aref (rgba vert) 8)  (coerce (random 1.0) 'single-float)
	(aref (rgba vert) 9)  (coerce (random 1.0) 'single-float)
	(aref (rgba vert) 10) (coerce (random 1.0) 'single-float)
	;; (aref (rgba vert) 11) (coerce (/ 255 255)  'single-float)
	
	(aref (rgba vert) 12) (coerce (random 1.0) 'single-float)
	(aref (rgba vert) 13) (coerce (random 1.0) 'single-float)
	(aref (rgba vert) 14) (coerce (random 1.0) 'single-float)
	;; (aref (rgba vert) 15) (coerce (/ 255 255)  'single-float)
	)
  (send-node vert))
