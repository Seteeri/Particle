(in-package :protoform.model)

(defparameter *data-zero-node* (make-array +size-struct-instance+
					   :adjustable nil
					   :fill-pointer nil
					   :element-type '(unsigned-byte 8)
					   :initial-element (coerce 0 '(unsigned-byte 8))))
;; :data (make-array size
;; 		  :element-type '(unsigned-byte 8)
;; 		  :initial-element (coerce 0 '(unsigned-byte 8)))

(defun enqueue-node (node &optional (pointer t))
  (when pointer
    (enqueue-node-pointer))
  (sb-concurrency:enqueue (list *channel*
				*shm-nodes*
				(serialize-node node)
				(* (index node)
				   +size-struct-instance+))
			  *queue-view*))

(defun enqueue-node-pointer ()
  (sb-concurrency:enqueue (list *channel*
				*shm-nodes*
				(serialize-node *node-pointer*)
				(* (index *node-pointer*)
				   +size-struct-instance+))
			  *queue-view*))  

(defun add-node-msdf (seq-key)
  ;; Add node to pointer position
  ;; Move pointer right
  ;; Maybe have pointer appear below/above so edge will show

  (fmt-model t "add-node-msdf" "~a~%" seq-key)

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
		     :relative
		     t
		     nil)

    ;; (fmt-model t "init-node-msdf" "cursor: ~a~%" cursor)

    (enqueue-node node)

    node))

(defun backspace-node-msdf (seq-key)
  
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
				 :absolute
				 t ; do on move-node-y
				 nil)
		;; Only move if end of line - REFACTOR ENTER
		(when nil
		  (displace-node-y *node-pointer*
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

      (enqueue-node node))))

(defun return-node-msdf (seq-key)
  ;; Move pointer
  ;; Add node

  ;; Do first since add-node will do pointer also - refactor that...
  (displace-node-x *node-pointer*
		   -11.5199995
		   :absolute)
  (displace-node-y *node-pointer*
		   (- (* +linegap+ *scale-node*))
		   :relative) ; add more spacing due to bl adjustments
  
  (add-node-msdf seq-key)

  ;; (fmt-model t "move-pointer-*" "~a~%" (translation (model-matrix *node-pointer*)))
  
  t)
