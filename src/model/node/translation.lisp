(in-package :protoform.model)

(defun get-origin-from-node-pos (node)
  ;; Get origin from node position
  ;; l b r t
  ;;
  ;; Get scale from model-matrix
  (v+ (translation (model-matrix node))
      (let ((bounds-origin (bounds-origin (gethash (char-code (char-glyph node)) *metrics*))))
	(vec3 (- (* (aref bounds-origin 0) *scale-node*))
	      (- (* (aref bounds-origin 1) *scale-node*))
	      0.0))))

(defun advance-node-of-node (node-a
			     node-b
			     n-adv
			     &key
			       (offset (vec3 0 0 0)))
  (let* ((origin-b (get-origin-from-node-pos node-b))
	 (bounds-a (bounds-origin (gethash (char-code (char-glyph node-a)) *metrics*)))
	 (new-pos (v+ origin-b
		      offset
		      (vec3 (+ (* 9.375 +scale-msdf+ *scale-node* n-adv)
			       (* (aref bounds-a 0) *scale-node*))
			    (* (aref bounds-a 1) *scale-node*)
			    0.0))))
    (setf (translation (model-matrix node-a)) new-pos)
    (update-transform (model-matrix node-a))
    (spatial-trees:delete node-a *r-tree*)
    (spatial-trees:insert node-a *r-tree*)
    new-pos))

(defun translate-node-to-node (node-a
			       node-b
			       &key
				 (offset (vec3 0 0 0)))
  ;; Move node-a to node-b position; don't change links
  ;; Adjust for baseline
  (let* ((origin-b (get-origin-from-node-pos node-b))
	 (bounds-a (bounds-origin (gethash (char-code (char-glyph node-a)) *metrics*)))
	 (new-pos (v+ origin-b
		      offset
		      (vec3 (* (aref bounds-a 0) *scale-node*)
			    (* (aref bounds-a 1) *scale-node*)
			    0.0))))
    (setf (translation (model-matrix node-a)) new-pos)
    (update-transform (model-matrix node-a))
    (spatial-trees:delete node-a *r-tree*)
    (spatial-trees:insert node-a *r-tree*)
    new-pos))

(defun translate-node-rel (node
			   dx
			   dy)

  (when (not node)
    (setf node *node-ptr-main*))

  (with-slots (translation)
      (model-matrix node)
  
    ;; Don't adjust for baseline
    (let* ((new-pos (nv+ translation
			 (vec3 (* dx *scale-node*)
			       (* dy *scale-node* -1)
			       0))))
      (update-transform (model-matrix node))
      (spatial-trees:delete node *r-tree*)
      (spatial-trees:insert node *r-tree*)
      new-pos))

  ;; TEMP
  (send-node *node-ptr-main*))

(defun update-transform-line (node-start offset)
  ;; If assumed, glyphs are already aligned,
  ;; can use offset in line to update nodes simultaneously
  (let ((pos-start (translate (model-matrix node-start)))
	(node-prev nil))
    (do-node (node node-start :out :out)
      (setf (translation (model-matrix node))
	    (v+ (translation (model-matrix node))))
      (update-transform (model-matrix node))
      (send-node node))))
