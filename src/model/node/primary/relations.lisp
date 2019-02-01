(in-package :protoform.model)

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
