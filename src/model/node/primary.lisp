(in-package :protoform.model)

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
	(setf (vx3 translation-mm) (+ (vx3 cursor)
				      (* (aref bounds-origin 0) scale-glyph))
	      (vy3 translation-mm) (+ (vy3 cursor)
				      (* (aref bounds-origin 1) scale-glyph))
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

;;;;;;;;;;;
;; linking

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

(defun get-nodes-in (* &optional (ptr-ignore t))
  ;; Filter pointer?
  (digraph:predecessors *digraph* *))

(defun get-nodes-out (*)
  ;; POSS: Add ignore-ptr
  (digraph:successors *digraph* *))

(defun get-node-in (* &optional (ptr-ignore t))
  (if ptr-ignore
      (dolist (node-i (digraph:predecessors *digraph* *))
      	(unless (eq node-i *node-pointer*)
      	  (return-from get-node-in node-i)))
      (first (digraph:predecessors *digraph* *))))

(defun get-node-out (*)
  ;; POSS: Add ignore-ptr
  (first (digraph:successors *digraph* *)))

(defun get-node-bi (*)
  (values (get-node-in *)
	  (get-node-out *)))

(defun get-node-dir (* dir)
  (cond ((eq dir :in)  (get-node-in *))
	((eq dir :out) (get-node-out *))
	((eq dir :bi)  (get-node-bi *))))

(defun link-node (node-src node-dest dir)
  ;; TEMP: add assertion to prevent cycles, aka node link to itself
  ;; (when (not (eq node-src node-dest))
  ;;   (draw-graph)
  ;;   (assert (not (eq node-src node-dest))))
  (assert (not (eq node-src node-dest)))
  (cond ((eq dir :in)
	 (insert-edge node-src node-dest))
	((eq dir :out)
	 (insert-edge node-dest node-src))))

(defun unlink-node (node-src node-dest dir)
  (cond ((eq dir :in)
	 (remove-edge node-src node-dest))
	((eq dir :out)
	 (remove-edge node-dest node-src))))

(defun unlink-node-first (* &optional (dir :out)) ; does first
  (cond ((eq dir :in)
	 (when-let ((node-* (get-node-in *)))
		   (remove-edge node-* *)
		   node-*))
	((eq dir :out)
	 (when-let ((*-node (get-node-out *)))
		   (remove-edge * *-node)
		   *-node))
	((eq dir :bi)
	 (multiple-value-bind (node-* *-node)
	     (get-node-bi *)
	   (when node-*
	     (remove-edge node-* *))
	   (when *-node
	     (remove-edge * *-node))
	   (values node-* *-node)))))

(defun get-node-type (node-ref) ; rename to get-node-type-rel?
  (let ((node-ref-in  (get-node-in node-ref))
	(node-ref-out (get-node-out node-ref)))
    (values (cond ((and node-ref-in
			node-ref-out)
		   :intra)
		  ((and node-ref-in
			(not node-ref-out))
		   :end)
		  ((and (not node-ref-in)
			node-ref-out)
		   :start)
		  ((and (not node-ref-in)
			(not node-ref-out))
		   :iso))
	    node-ref-in
	    node-ref-out)))

;;;;;;;;;;;;;
;; relational

;; rename to join
(defun insert-node (node-src
		    node-dest
		    dir-src)
  
  ;; Procedure
  ;;
  ;; Scenario #1 - Insert b out of * (into a):
  ;; a <------ * | GIVEN B
  ;; a <- b <- * | GOAL
  ;;
  ;; - Plugin 

  ;; Scenario #2 - Insert b into *:
  ;; a <- * <------ c | GIVEN B
  ;; a <- * <- b <- c | GOAL
  
  ;; nodes in opposite direction remain attached

  ;; only manages linkage
  
  (loop
     :for node :in (cond ((eq dir-src :in)
			  (get-nodes-in node-dest))
			 ((eq dir-src :out)
			  (get-nodes-out node-dest)))
     :do (progn
	   (unlink-node node     node-dest dir-src)
	   ;; (format t "~S ~S: ~S~%" (data node-dest) dir-src (data node))
	   ;; (format t "~S - ~S : ~S~%" (data node-src) (data node) dir-src)
	   (link-node   node-src node      dir-src)))
  
  ;; link src to dest
  (link-node node-src
	     node-dest
	     dir-src))

(defun pop-node (&key
		   (node-ptr *node-pointer*)
		   (dir-ptr :out))

  ;; SHOULD only manage linkage, not moving
  
  ;; Cases:
  ;;   
  ;; - intra node:  y in, y out
  ;;   A -> B -> C
  ;;        *
  ;;   A ->   -> C
  ;;        *
  ;;   - move C next to A
  ;;   - point to C
  ;;
  ;;
  ;; - end node:    y in, n out
  ;;   A -> B -> C
  ;;             *
  ;;   A -> B -> 
  ;;        *
  ;;   - point to B
  ;;
  ;;
  ;; - start node:  n in, y out
  ;;   A -> B -> C
  ;;   *
  ;;     -> B -> C
  ;;        *
  ;;   - point to B
  ;;
  ;;
  ;; - single node: n in, n out
  ;;     B 
  ;;     *
  ;;   - do nothing

  (when-let ((node-ref (get-node-ptr-out)))
	    (unlink-node-ptr :out) ; unlink ref only

	    (multiple-value-bind (type-node-ref
				  node-ref-in
				  node-ref-out)
		(get-node-type node-ref)

	      (cond ((eq type-node-ref :intra)
		     ;; Insert C out of A
		     (insert-node node-ref-out node-ref-in :out)
		     ;; Link pointer to next node
		     (link-node-ptr node-ref-out))
		    
		    ((eq type-node-ref :end)
		     (link-node-ptr node-ref-in))

		    ((eq type-node-ref :start)
		     (link-node-ptr node-ref-out))

		    ((eq type-node-ref :iso)
		     t))
	      
	      ;; Return deleted node, and surrounding nodes (for shm update)
	      (values node-ref
		      type-node-ref
		      node-ref-in
		      node-ref-out))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-node (data &optional (move-pointer t)) ; rename add-node-to-ptr
  (let* ((baseline (get-origin-from-node-pos *node-pointer*))
	 (node (init-node-msdf baseline
			       *scale-node*
			       (pop *stack-i-nodes*)
			       data)))
    
    (insert-vertex node)
    (spatial-trees:insert node *r-tree*)
    
    ;; Factor this out...
    (when-let* ((node-ptr-out (get-node-ptr-out))
		(node-type (get-node-type node-ptr-out)))

	       ;; Push new node above for now
	       (when (or (eq node-type :intra)
			 (eq node-type :start))
		 (let* ((bounds-origin (bounds-origin (gethash (char-code (data node)) *metrics*))))
		   (translate-node-to-node node
					   node
					   :offset (vec3 0.0
    							 (* +linegap+ *scale-node*)
    							 0.0)))))
    ;; Attach node to ptr
    (insert-node node *node-pointer* :out)
    
    ;; Advance pointer
    (when move-pointer
      (advance-node-of-node *node-pointer*
    			    node
    			    1.0))
        
    node))

(defun delete-node (&key
		      (node-ptr *node-pointer*)
		      (dir-ptr :out))

  (multiple-value-bind (node-ref
			type-node-ref
			node-ref-in
			node-ref-out)
      (pop-node :node-ptr node-ptr
		:dir-ptr dir-ptr)
    (when node-ref
      (translate-node-to-node *node-pointer*
  		       	      node-ref)
      (when (eq type-node-ref :intra)
	;; Move C next to A
  	(advance-node-of-node node-ref-out
  			      node-ref-in
  			      1.0))
      (push (index node-ref) *stack-i-nodes*)
      (remove-vertex node-ref)
      (spatial-trees:delete node-ref *r-tree*))
    (values node-ref
	    node-ref-in
	    node-ref-out)))
