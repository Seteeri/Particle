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
  ((data :accessor data :initarg :data :initform nil)
   
   (offset-texel-texture :accessor offset-texel-texture :initarg :offset-texel-texture :initform 0)
   (dims-texture :accessor dims-texture :initarg :dims-texture :initform (vec2 0 0))
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
  (enqueue-node vert))

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
							  :scale (vec3 scale-glyph
								       scale-glyph
								       scale-glyph)
							  :translation (vcopy3 cursor))))
	(metrics-glyph (gethash (char-code data) *metrics*)))

    ;; Remember to dec on removal
    (sb-ext:atomic-incf (car *vertices-digraph*))
    
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
    
    (with-slots (bounds-origin
		 dims-glyph
		 uv)
	metrics-glyph

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
      (let ((translation-mm (translation (model-matrix node))))
	(setf (vx3 translation-mm) (+ (vx3 cursor) (* (aref bounds-origin 0) scale-glyph))
	      (vy3 translation-mm) (+ (vy3 cursor) (* (aref bounds-origin 1) scale-glyph))
	      (vz3 translation-mm) (vz3 cursor)))

      ;; Aspect ratio of node must match aspect ratio of UV
      ;; otherwise texture will appear distorted
      (let ((scale-mm (scale (model-matrix node))))
      	(setf (vx3 scale-mm) (* (vx2 dims-glyph) scale-glyph)
      	      (vy3 scale-mm) (* (vy2 dims-glyph) scale-glyph)))

      (setf (uv node) uv))
    
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

(defun insert-vertex (vert)
  (digraph:insert-vertex *digraph* vert)
  (sb-ext:atomic-incf (car *vertices-digraph*)))

(defun remove-vertex (vert)
  (digraph:remove-vertex *digraph* vert)
  (sb-ext:atomic-decf (car *vertices-digraph*)))

(defun insert-edge (vert-a vert-b)
  (digraph:insert-edge *digraph* vert-a vert-b)
  (sb-ext:atomic-incf (car *edges-digraph*)))

(defun remove-edge (vert-a vert-b)
  (digraph:remove-edge *digraph* vert-a vert-b)
  (sb-ext:atomic-decf (car *edges-digraph*)))

;; Refactor 3x below to macros

(defun get-node-in (*)
  ;; TODO:
  ;; - Add arg to get first or all
  (when-let ((node-* (first (digraph:predecessors *digraph* *))))
	    (remove-edge node-* *)
	    node-*))

(defun get-node-out (*)
  (when-let ((*-node (first (digraph:successors *digraph* *))))
	    (remove-edge * *-node)
	    *-node))

(defun get-node-bi (*)
  (values (get-node-in *)
	  (get-node-out *)))

(defun get-node-neighs (* dir)
  (cond ((eq dir :in)  (get-node-in *))
	((eq dir :out) (get-node-out *))
	(t t)))

;; Rewrite this function
(defun insert-node (node
		    &key
		      (node-ptr *node-pointer*)
		      (dir-ptr :out) ; will unlink node in this pos
		      (dir-ref :in)) ;; will link node in this pos

  ;; REFACTOR:
  ;; - args = node-tgt node-ptr
  ;;          ptr can be pointer or specific node
  ;;          if not passed, use pointer
  
  ;; Linking Process:
  ;;
  ;; (ins here)
  ;;    |
  ;; [a]<-[*] (start)
  ;; [b]
  ;;
  ;; #1
  ;; [a] [*]
  ;; [b] 
  ;;
  ;; #2
  ;; [a]->[b] [*]
  ;;
  ;; #3
  ;; [a]->[b]<-[*]

  ;; Ordered this way in case there is no node-*
  
  (let ((node-* (get-node-neighs node-ptr dir-ptr)))
    (when node-*    
      (cond ((eq dir-ref :in)
  	     ;; 1. Remove edge between node-*   and ptr
  	     (if (eq dir-ptr :out)
  	     	 (remove-edge node-ptr node-*)
  	     	 (remove-edge node-* node-ptr))
	     ;; 2. Insert edge between node-*   and node-new
  	     (insert-edge node-* node))
	     
  	    ((eq dir-ref :out)
  	     ;; 1. Remove edge between node-*   and ptr
  	     (if (eq dir-ptr :out)
  		 (remove-edge node-ptr node-*)
  		 (remove-edge node-* node-ptr))
  	     ;; Flip above
  	     ;; 2. Insert edge between node-*   and node-new
  	     (insert-edge node node-*))
	    
  	    (t
  	     t))))
  
  ;; 3. Insert edge between node new and ptr
  (if (eq dir-ptr :out)
      (insert-edge node-ptr node)
      (insert-edge node node-ptr)))

;; move back to ops?
(defun delete-node (&key
		      (node-ptr *node-pointer*)
		      (dir-ptr :out))

  ;; Linking Process:
  ;;
  ;; From:
  ;; [a]->[b]<-[*] (start)
  ;; To:
  ;; [a]<-[*] (start)
  ;;
  ;; #1 - Break b edges
  ;; [a] [*]
  ;; [b] 
  ;;
  ;; #2 - Link pointer to a
  ;; [a]<-[*]
  ;;
  ;; #3 - Remove [b]

(let ((node-* (get-node-neighs node-ptr dir-ptr))
      (node-** nil))
    
    (when node-*

      ;; Note:
      ;; preds - nodes that point to it (pointer)
      ;; succs - nodes that it points to (prev chars)
      
      ;; Remove node-* preds
      ;; - char node(s), pointer(s)
      ;; - Assume 1 char for now...
      ;; - Multi ptrs possible which we ignore
      ;; Get node-** first before removing etc
      (let ((nbhrs (digraph:predecessors *digraph* node-*)))
	(dolist (node-i nbhrs)
	  (remove-edge node-i node-*)
	  (unless (eq node-i node-ptr)
	    (setf node-** node-i))))

      ;; Remove node-* succs
      ;; - char node(s)
      ;; - should not be pointers
      (let ((nbhrs (digraph:successors *digraph* node-*)))
	(dolist (node-i nbhrs)
	  (remove-edge node-* node-i)))

      (when node-**
	(insert-edge node-ptr node-**))
      
      (remove-vertex node-*)

      ;; TODO:
      ;; * Refactor move-node-right-of... to take :before/after arg
      
      ;; If not node-**, that means first char just deleted
      ;; so use its position instead
      (if node-**
	  ;; Update pointer to right of node-** (instead of node-* pos)
	  (if (char-equal (data node-**) #\Newline)
	      (move-node-to-node node-ptr node-*)
	      (move-node-x-of-node node-ptr node-** :+))
	  (move-node-to-node node-ptr node-*)))

    ;; Return deleted node
    ;; Caller should not store this so it can be GC'd
    node-*))

;; Secondary/Aux operators
;; Need to implement hyperweb first to identify nodes

(defun move-node-x-of-node (node-a node-b pos
			    &key
			      (offset (vec3 0 0 0))
			      (ignore-y nil))
  ;; Move a rel to b
  ;;
  ;; REFACTOR
  ;; - Add option for update
  (let ((pos-a (translation (model-matrix node-a)))
	(pos-b (translation (model-matrix node-b)))
	(bounds-origin (bounds-origin (gethash (char-code (data node-b)) *metrics*))))
    (setf (translation (model-matrix node-a))
	  (vec3 (+ (vx3 pos-b) (vx3 offset) ; get x origin of b 
		   (- (* (aref bounds-origin 0) *scale-node*)) ; l b r t
		   (cond ((eq pos :+)
			  (* 9.375 +scale-msdf+ *scale-node*))
			 ((eq pos :-)
			  (- (* 9.375 +scale-msdf+ *scale-node*)))
			 (t
			  t)))
		;; To get baseline of B, inverse bounds (subtract) since its rel to origin
		(+ (vy3 pos-b) (vy3 offset)
		   (if ignore-y
		       0.0
		       (- (* (aref bounds-origin 1) *scale-node*))))
		(+ (vz3 pos-b) (vz3 offset)))))
  (update-transform (model-matrix node-a)))

(defun move-node-to-node (node-a node-b &optional (offset (vec3 0 0 0)))
  ;; Move node-a to node-b
  ;; REFACTOR
  ;; - Copy/replace or modify
  ;; - Add option for update
  ;; - Add option for offset
  (setf (translation (model-matrix node-a))
	(v+ (translation (model-matrix node-b))
	    offset))
  (update-transform (model-matrix node-a)))

(defun build-string-from-nodes ()
  ;; Pass starting node else use node-pointer
  ;; To eval, build string from predecessors until newline

  (fmt-model t "build-string-from-nodes" "Pointer: ~a~%" *node-pointer*)
  
  (let ((chrs nil))
    (loop
       ;; currently assuming linear
       :for pred := (digraph:successors *digraph* *node-pointer*)
       :then (digraph:predecessors *digraph* pred)
       :while pred
       :do (loop
	      ;; Leave on first non-ptr node
	      :for node :in pred
	      :do (unless (eq node *node-pointer*)
		    (when (char-equal (data node) #\Newline)
		      (return))
		    (push (data node) chrs)
		    (setf pred node)
		    (return))))
    
    (with-output-to-string (stream)
      (dolist (c chrs)
	(write-char c stream)))))

(defun remove-all-nodes ()
  ;; Exclude pointer
  (digraph:mapc-vertices (lambda (v)
			   (unless (eq v *node-pointer*)
			     (enqueue-node-zero (index v))
			     (remove-vertex v)))
			 *digraph*)
  (digraph:mapc-edges (lambda (e)
			(remove-edge e))
		      *digraph*))

;; Find end, specify successor or predecessor direction
(defun find-node-line-start (node-end dir)
  (let ((node-start node-end)
	(fn (cond ((eq dir :in)
		   #'digraph:predecessors)
		  ((eq dir :out)
		   #'digraph:successors)
		  (t
		   t))))

    ;; Could detect first char to see which direction text is in...
    ;; - Might introduce bugs -> print warning?
    
    ;; Create fn - loop until specified character or pass lambda as predicate
    ;; Start with newline char instead of pointer or it will terminate immediately
    (loop
       :for pred := (funcall fn *digraph* node-end)
       :then (funcall fn *digraph* pred)
       :while pred
       :do (loop
	      :for node :in pred
	      :do (unless (equal node *node-pointer*) ; skip pointer
		    (when (char-equal (data node) #\Newline)
		      ;; leave until newline (or end)
		      (return))
		    ;; (format t "~S : ~S~%" node (data node))
		    ;; Else set node to get preds and goto next iteration
		    (setf pred node
			  node-start node)
		    (return))))
    node-start))

(defun find-node-end (node-default dir-1 dir-2)
  (let ((node-start node-default)
	(fn-1 (cond ((eq dir-1 :in)
		     #'digraph:predecessors)
		    ((eq dir-1 :out)
		     #'digraph:successors)
		    (t
		     t)))
	(fn-2 (cond ((eq dir-2 :in)
		     #'digraph:predecessors)
		    ((eq dir-2 :out)
		     #'digraph:successors)
		    (t
		     t))))
    ;; Could detect first char to see which direction text is in...
    ;; - Might introduce bugs -> print warning?
    
    ;; Create fn - loop until specified character or pass lambda as predicate
    ;; Start with newline char instead of pointer or it will terminate immediately
    (loop
       :for pred := (funcall fn-1 *digraph* node-default)
       :then (funcall fn-2 *digraph* pred)
       :while pred
       :do (loop
	      :for node :in pred
	      :do (unless (equal node *node-pointer*) ; skip pointer option
		    ;; Else set node to get preds and goto next iteration
		    (setf pred node
			  node-start node)
		    (return))))
    node-start))

;; Need do-node macro
(defmacro do-node ((var node-default dir-1 dir-2) &body body)
  `(let ((fn-1 (cond ((eq ,dir-1 :in)
		    #'digraph:predecessors)
		     ((eq ,dir-1 :out)
		      #'digraph:successors)
		     (t
		      t)))
	 (fn-2 (cond ((eq ,dir-2 :in)
		      #'digraph:predecessors)
		     ((eq ,dir-2 :out)
		      #'digraph:successors)
		     (t
		      t))))
     ;; Could detect first char to see which direction text is in...
     ;; - Might introduce bugs -> print warning?
     
     ;; Create fn - loop until specified character or pass lambda as predicate
     ;; Start with newline char instead of pointer or it will terminate immediately
     (loop
	:for pred := (funcall fn-1 *digraph* ,node-default)
	:then (funcall fn-2 *digraph* pred)
	:while pred
	:do (loop
	       :for node-pred :in pred
	       :do (unless (equal node-pred *node-pointer*) ; skip pointer
		     (let ((,var node-pred))
		       ,@body)
		     (setf pred node-pred))))))

(defun nth-node (node-end nth)
  t)
