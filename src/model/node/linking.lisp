(in-package :protoform.model)

(defun insert-vertex (vert &optional
			     (digraph *digraph*)
			     (verts   *vertices-digraph*))
  (digraph:insert-vertex digraph vert)
  (sb-ext:atomic-incf (car verts)))

(defun remove-vertex (vert &optional
			     (digraph *digraph*)
			     (verts   *vertices-digraph*))
  (digraph:remove-vertex digraph vert)
  (sb-ext:atomic-decf (car verts)))

(defun insert-edge (vert-a
		    vert-b
		    &optional
		      (digraph *digraph*)
		      (edges   *edges-digraph*))
  (digraph:insert-edge digraph vert-a vert-b)
  (sb-ext:atomic-incf (car edges)))

(defun remove-edge (vert-a
		    vert-b
		    &optional
		      (digraph *digraph*)
		      (edges   *edges-digraph*))
  (digraph:remove-edge digraph vert-a vert-b)
  (sb-ext:atomic-decf (car edges)))

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
