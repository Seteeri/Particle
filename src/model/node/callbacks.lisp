(in-package :protoform.model)

(defun add-node-msdf (seq-key ptree queue)
  ;; Add node to pointer position
  ;; Move pointer right
  ;; Maybe have pointer appear below/above so edge will show

  (fmt-model t "add-node-msdf" "~a~%" seq-key)

  (ptree-fn 'add-node-msdf
	    '()
	    (lambda ()
	      (funcall #'add-node-msdf-2 seq-key))
	    ptree)

  (sb-concurrency:enqueue 'add-node-msdf queue))

(defun backspace-node-msdf (seq-key ptree queue)

  (fmt-model t "backspace-node-msdf" "~a~%" seq-key)

  (ptree-fn 'backspace-node-msdf
	    '()
	    (lambda ()
	      (funcall #'backspace-node-msdf-2 seq-key))
	    ptree)

  (sb-concurrency:enqueue 'backspace-node-msdf queue))

(defun return-node-msdf (seq-key ptree queue)
  
  (fmt-model t "return-node-msdf" "~a~%" seq-key)

  (ptree-fn 'return-node-msdf
	    '()
	    (lambda ()
	      (funcall #'return-node-msdf-2 seq-key))
	    ptree)

  (sb-concurrency:enqueue 'return-node-msdf queue))

(defun add-node-msdf-2 (seq-key)
  
  ;; Split into
  ;; node new
  ;; digraph
  ;; pointer
  
  ;; Advance - origin to origin
  ;; 1. Find glyph A origin
  ;;    1. Model trans + glyph trans
  ;; 2. Set glyph B origin
  ;;    1. origin A + advance - glyph trans  
  (let* ((metrics-space (gethash 32 *metrics*))
	 (spacing (* (advance metrics-space) (scale metrics-space) *scale-node*))
	 
	 (cursor (translation (model-matrix *node-pointer*)))

	 (key-first (second (reverse (second seq-key))))
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
    (displace-node-x *node-pointer*
		     (* 96 *scale-node*)
		     :rel)

    ;; (fmt-model t "init-node-msdf" "cursor: ~a~%" cursor)

    (enqueue-node node)

    node))
  
(defun backspace-node-msdf-2 (seq-key)
  
  (let ((node-tgt (first (digraph:successors *digraph* *node-pointer*))))
    (when node-tgt
      
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
		(displace-node-x *node-pointer*
				 (+ (vx3 (translation (model-matrix pred)))
				    (* 96 *scale-node*))
				 :abs)
		;; Only move if end of line - REFACTOR ENTER
		(when nil
		  (displace-node-y *node-pointer*
			       (* +linegap+ scale-node) ; add more spacing due to bl adjustments
			       :rel)))))
	;; Now can remove edges
	(dolist (pred preds)
	  (digraph:remove-edge *digraph*
			       pred
			       node-tgt)))
      ;; Remove vertex
      (digraph:remove-vertex *digraph*
			     node-tgt)

      (enqueue-node-pointer)
      (enqueue-node-zero (index node-tgt)))))

(defun return-node-msdf (seq-key)
  ;; Move pointer
  ;; Add node

  ;; Do first since add-node will do pointer also - refactor that...
  (displace-node-x *node-pointer*
		   -11.5199995
		   :abs)
  (displace-node-y *node-pointer*
		   (- (* +linegap+ *scale-node*))
		   :rel) ; add more spacing due to bl adjustments
  
  (add-node-msdf-2 seq-key)

  ;; (fmt-model t "move-pointer-*" "~a~%" (translation (model-matrix *node-pointer*)))
  
  t)
