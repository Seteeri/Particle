(in-package :protoform.model)

;;;;;;;;;;;;;
;; relational

(defun print-node-dirs (&key
			  (node-ptr *node-pointer*)
			  (dir-ptr :out))
  ;; Print node
  (when-let ((node-ref (get-node-ptr-out)))	    
	    (let ((nodes-ref-in (loop :for n :in (get-nodes-in node-ref) :collect (data n)))
		  (nodes-ref-out (loop :for n :in (get-nodes-out node-ref) :collect (data n))))
	      (format t "ptr: ~a~%" *node-pointer*)
	      (format t "ptr-ref (ptr out): ~a~%" node-ref)
	      (format t "in: ~a~%" nodes-ref-in)
	      (format t "out: ~a~%" nodes-ref-out))))

;; rename to join
(defun insert-node (node-src
		    node-dest
		    dir-src
		    &optional
		      (graph *digraph*)
		      (edges *edges-digraph*))
  
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
			  (get-nodes-in node-dest graph))
			 ((eq dir-src :out)
			  (get-nodes-out node-dest graph)))
     :do (progn
	   (unlink-node node
			node-dest
			dir-src
			graph
			edges)
	   (link-node node-src
		      node
		      dir-src
		      graph
		      edges)))
  
  ;; link src to dest
  (link-node node-src
	     node-dest
	     dir-src
	     graph
	     edges))

(defun pop-node (&key
		   (node-ptr *node-pointer*)
		   (dir-ptr :out))
  
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

;; Move to operators?

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

(defun copy-node-to-node (node-src)
  (let* ((baseline (get-origin-from-node-pos node-src))
	 (node (init-node-msdf baseline
			       *scale-node* ; get from node-src
			       (digraph:count-vertices *digraph*)
			       (data node-src))))
    (insert-vertex node)
    (spatial-trees:insert node *r-tree*)
    
    node))

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

(defun replace-node (node-src
		     node-dest
		     dir-src)
  
  ;; Procedure
  ;;
  ;; Insert d:
  ;;
  ;; a <- b <- * <- a | GIVEN
  ;;
  ;; 1. Unlink src
  ;; 1. Get all the ins of dest
  ;;    2. Unlink old, link new
  ;; 3. Get all the outs of dest
  ;;    4. Unlink old, link new

  ;; unlink old
  
  (let ((nodes-in (get-nodes-in node-dest))
	(nodes-out (get-nodes-out node-dest)))

    ;; Unlink dest
    (loop
       :for node :in nodes-in
       :do (remove-edge node node-dest))
    (loop
       :for node :in nodes-out
       :do (remove-edge node-dest node))

    ;; Link src
    (loop
       :for node :in nodes-in
       :do (insert-edge node node-src))
    (loop
       :for node :in nodes-out
       :do (insert-edge node-src node)))

  ;; Could easily do swap function
  t)
