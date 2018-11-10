(in-package :protoform.model)

;; Separate callbacks into callbacks-node

(defun add-node-msdf (seq-key)
  ;; Add node to pointer position
  ;; Move pointer right
  ;; Maybe have pointer appear below/above so edge will show

  (fmt-model t "add-node-msdf" "~a~%" seq-key)
  
  ;; Advance - origin to origin
  ;; 1. Find glyph A origin
  ;;    1. Model trans + glyph trans
  ;; 2. Set glyph B origin
  ;;    1. origin A + advance - glyph trans  
    (let* ((metrics-space (gethash 32 *metrics*))
	   (spacing (* (advance metrics-space) (scale metrics-space) *scale-node*))
	   (cursor (translation (model-matrix *node-pointer*)))
	   (key-first (first (second seq-key)))
	   (data (if (= key-first +xk-return+)
		     #\Newline
		     (code-char key-first)))
	   (node (init-node-msdf (vcopy3 cursor)
				 *scale-node*
				 (digraph:count-vertices *digraph*)
				 data)))
      
      (update-transform (model-matrix node))
      
      ;; Make new node a child of pointer node
      ;; and child of previous
      ;; Old:
      ;; [a]-[*]
      ;; graph
      ;; [*]
      ;;  |
      ;; [a]
      ;;
      ;; New:
      ;; [a]-[b]-[*]
      ;; graph
      ;; [*]--+
      ;;      |
      ;; [a]-[b]

      (digraph:insert-vertex *digraph* node)
      
      (when (first (digraph:successors *digraph* *node-pointer*))
	;; Insert edge a-b
	(digraph:insert-edge *digraph*
			     (first (digraph:successors *digraph* *node-pointer*))
			     node)
	;; Remove edge edge *-a
	(digraph:remove-edge *digraph*
			     *node-pointer*
			     (first (digraph:successors *digraph* *node-pointer*))))
      ;; Insert edge *-b
      (digraph:insert-edge *digraph*
			   *node-pointer*
			   node)
      ;; Move pointer node to right
      (move-node-x *node-pointer*
		   (* 96 *scale-node*)
		   :relative
		   t
		   nil)

      ;; (fmt-model t "init-node-msdf" "cursor: ~a~%" cursor)

      (when nil
	;; Copy only this node
	(copy-node-to-shm node
			  (* (index node)
			     (/ +size-struct-instance+ 4)))
	;; Copy all nodes
	(memcpy-shm-to-cache-flag* (list (list "nodes"
				       	       0
      				       	       (* +size-struct-instance+ (+ (digraph:count-vertices *digraph*)
				       					    (digraph:count-edges *digraph*)))))))

      node))

(defun backspace-node-msdf (seq-key)
  
    (let ((node-tgt (first (digraph:successors *digraph* *node-pointer*))))
      (when node-tgt

	;; Remove node data
	(zero-node-to-shm (* (index node-tgt)
			     (/ +size-struct-instance+ 4)))
	
	;; Remove node from graph
	;; 1. Insert edge: ptr-pred
	;; 2. Remove edges: ptr-node, pred-node
	(let ((preds (digraph:predecessors *digraph* node-tgt)))
	  ;; Find non-ptr edge and create edge from ptr to pred
	  (dolist (pred preds)
	    (if (eq pred *node-pointer*)
		t
		(progn
		  (digraph:insert-edge *digraph*
				       *node-pointer*
				       pred)
		  ;; Move pointer node to right of pred and up a line
		  (move-node-x *node-pointer*
			       (+ (vx3 (translation (model-matrix pred)))
				  (* 96 *scale-node*))
			       :absolute
			       t ; do on move-node-y
			       nil)
		  ;; Only move if end of line - REFACTOR ENTER
		  (when nil
		    (move-node-y *node-pointer*
				 (* +linegap+ scale-node) ; add more spacing due to bl adjustments
				 :relative
				 t
				 nil)))))
	  ;; Now can remove edges
	  (dolist (pred preds)
	    (digraph:remove-edge *digraph*
				 pred
				 node-tgt)))
	;; Remove vertex
	(digraph:remove-vertex *digraph*
			       node-tgt)

	(when nil
	  (memcpy-shm-to-cache-flag* (list (list "nodes"
				       		 0
      				       		 (* +size-struct-instance+ (+ (digraph:count-vertices *digraph*)
				       					      (digraph:count-edges *digraph*))))))))))

(defun return-node-msdf (seq-key)
  ;; Add node
  ;; Move pointer back
  (let ((node (add-node-msdf seq-key)))
    (move-node-x *node-pointer*
		 -11.5199995
		 :absolute)
    (move-node-y *node-pointer*
		 (- (* +linegap+ *scale-node*))
		 :relative) ; add more spacing due to bl adjustments    
    (fmt-model t "move-pointer-*" "~a~%" (translation (model-matrix *node-pointer*)))))
