(in-package :protoform.model)

(defun get-nodes-in (*
		     &optional
		       (digraph *digraph*))
  ;; Filter pointer?
  (digraph:predecessors digraph *))

(defun get-nodes-out (*
		      &optional
			(digraph *digraph*))
  ;; POSS: Add ignore-ptr
  (digraph:successors digraph *))

(defun get-node-in (*
		    &optional
		      (digraph *digraph*))
  ;; Pass ptr
  (if t ; ptr-ignore
      (dolist (node-i (digraph:predecessors digraph *))
      	(unless (eq node-i *node-pointer*)
      	  (return-from get-node-in node-i)))
      (first (digraph:predecessors digraph *))))

(defun get-node-out (*
		    &optional
		      (digraph *digraph*))		     
  ;; POSS: Add ignore-ptr
  (first (digraph:successors digraph *)))

(defun get-node-bi (*
		    &optional
		      (digraph *digraph*))		    
  (values (get-node-in * digraph)
	  (get-node-out * digraph)))

(defun get-node-dir (*
		     dir
		     &optional
		       (digraph *digraph*))
  (cond ((eq dir :in)  (get-node-in * digraph))
	((eq dir :out) (get-node-out * digraph))
	((eq dir :bi)  (get-node-bi * digraph))))

(defun get-node-type (node-ref
		      &optional
			(graph *digraph*))
  ;; rename to get-node-type-rel?
  ;; Pass digraph
  (let ((node-ref-in  (get-node-in node-ref graph))
	(node-ref-out (get-node-out node-ref graph)))
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

