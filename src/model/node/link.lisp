(in-package :protoform.model)

(defun link-node (node-src
		  node-dest
		  dir
		  &optional
		    (graph *digraph*)
		    (edges *edges-digraph*))
  ;; TEMP: add assertion to prevent cycles, aka node link to itself
  ;; (when (not (eq node-src node-dest))
  ;;   (draw-graph)
  ;;   (assert (not (eq node-src node-dest))))
  (assert (not (eq node-src node-dest)))
  (cond ((eq dir :in)
	 (insert-edge node-src node-dest graph edges))
	((eq dir :out)
	 (insert-edge node-dest node-src graph edges))))

(defun unlink-node (node-src
		    node-dest
		    dir
		    &optional
		      (graph *digraph*)
		      (edges *edges-digraph*))
  (cond ((eq dir :in)
	 (remove-edge node-src node-dest graph edges))
	((eq dir :out)
	 (remove-edge node-dest node-src graph edges))))

(defun unlink-node-first (*
			  dir
			  &optional
			    (graph *digraph*)
			    (edges *edges-digraph))
  (cond ((eq dir :in)
	 (when-let ((node-* (get-node-in *)))
		   (remove-edge node-* * digraph edges)
		   node-*))
	((eq dir :out)
	 (when-let ((*-node (get-node-out *)))
		   (remove-edge * *-node digraph edges)
		   *-node))
	((eq dir :bi)
	 (multiple-value-bind (node-* *-node)
	     (get-node-bi *)
	   (when node-*
	     (remove-edge node-* * digraph edges))
	   (when *-node
	     (remove-edge * *-node digraph edges))
	   (values node-* *-node)))))
