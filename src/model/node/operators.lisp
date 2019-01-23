(in-package :protoform.model)

;; shm functions

(defun enqueue-node (node &optional (pointer t))
  (when pointer
    (enqueue-node-pointer))
  (sb-concurrency:enqueue (list *channel*
				*shm-nodes*
				(lambda ()
				  (update-transform (model-matrix node))
				  (serialize-node node))
				(* (index node)
				   +size-struct-instance+))
			  *queue-shm*))

(defun enqueue-node-pointer ()
  (sb-concurrency:enqueue (list *channel*
				*shm-nodes*
				(lambda ()
				  (update-transform (model-matrix *node-pointer*))
				  (serialize-node *node-pointer*))
				(* (index *node-pointer*)
				   +size-struct-instance+))
			  *queue-shm*))  

(defun enqueue-node-zero (index)
  (sb-concurrency:enqueue (list *channel*
				*shm-nodes*
				(lambda ()
				  *data-zero-node*)
				(* index
				   +size-struct-instance+))
			  *queue-shm*))

;; core functions

(defun add-node (code &optional (move-pointer-right t))
  
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
	 (spacing (* (advance metrics-space)
		     (scale metrics-space)
		     *scale-node*))
	 
	 (cursor (translation (model-matrix *node-pointer*)))
	 
	 (node (init-node-msdf cursor
			       *scale-node*
			       (digraph:count-vertices *digraph*)
			       (code-char code))))

    ;; Why repeat when init-node does this?
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
			   (first (digraph:successors *digraph*
						      *node-pointer*))
			   node)
      ;; Remove edge edge *-a
      (digraph:remove-edge *digraph*
			   *node-pointer*
			   (first (digraph:successors *digraph*
						      *node-pointer*))))
    ;; Insert edge *-b
    (digraph:insert-edge *digraph*
			 *node-pointer*
			 node)
    
    ;; Move pointer node to right - make this an optional arg
    (when move-pointer-right
      (translate-node-x *node-pointer*
			(* 9.375 +scale-msdf+ *scale-node*)
			:rel))

    ;; (fmt-model t "init-node-msdf" "cursor: ~a~%" cursor)

    (sb-ext:atomic-incf (car *vertices-digraph*))
    (sb-ext:atomic-incf (car *edges-digraph*))
    
    (enqueue-node node)

    node))

(defun backspace-node ()

  ;; Move pointer to node
  
  (let ((node-tgt (first (digraph:successors *digraph*
					     *node-pointer*))))
    (when node-tgt
      
      ;; Remove node from graph
      ;; 1. Insert edge: ptr-pred
      ;; 2. Remove edges: ptr-node, pred-node
      (let ((preds (digraph:predecessors *digraph*
					 node-tgt)))
	;; Find non-ptr edge and create edge from ptr to pred
	(dolist (pred preds)
	  (if (eq pred *node-pointer*)
	      t
	      (progn
		(digraph:insert-edge *digraph*
				     *node-pointer*
				     pred)

		(translate-node-x *node-pointer*
				  (+ (vx3 (translation (model-matrix pred)))
				     (* 96 *scale-node*))
				  :abs)

		(when nil
		  (translate-node-y *node-pointer*
				    (vy3 (translation (model-matrix pred)))
				    :abs))
		(when nil
		  (translate-node-y *node-pointer*
				    (* +linegap+ scale-node) 
				    :rel)))))
	;; Now can remove edges
	(dolist (pred preds)
	  (digraph:remove-edge *digraph*
			       pred
			       node-tgt)))
      ;; Remove vertex
      (digraph:remove-vertex *digraph*
			     node-tgt)

      (sb-ext:atomic-decf (car *vertices-digraph*))
      (sb-ext:atomic-decf (car *edges-digraph*))
      
      (enqueue-node-pointer)
      (enqueue-node-zero (index node-tgt)))))

(defun insert-node-newline ()
  ;; 1. Move pointer right
  ;; 2. Add node
  ;; 3. Move pointer down
  ;; 3. Update newline property: pos-start-line
  ;;    1. Find beginning of line

  ;; Do first since add-node will do pointer also - refactor that...
  (when nil
    (translate-node-x *node-pointer*
		      -11.5199995 ; need to track newline chars
		      :abs
		      nil)
    (translate-node-y *node-pointer*
		      (- (* +linegap+ *scale-node*))
		      :rel
		      nil) ; add more spacing due to bl adjustments
    (update-transform (model-matrix *node-pointer*))
    (enqueue-node-pointer))

  ;; Seq-key is +xk-return+ = 65293
  ;; Pass newline char however
  (add-node (char-code #\Newline)))

;; Alternative is to animate it
(defun translate-node-x (node
			 displacement
			 type-displace
			 &optional (update t))
  (with-slots (model-matrix)
      node
    (cond ((eq type-displace :abs)
	   (setf (vx3 (translation model-matrix)) displacement))
	  ((eq type-displace :rel)
	   (incf (vx3 (translation model-matrix)) displacement))
	  (t
	   (error "Unknown type-displace")))
    (when update
      (update-transform model-matrix))))

(defun translate-node-y (node
			 displacement
			 type-displace
			 &optional (update t))
  (with-slots (model-matrix)
      node
    (cond ((eq type-displace :abs)
	   (setf (vy3 (translation model-matrix)) displacement))
	  ((eq type-displace :rel)
	   (incf (vy3 (translation model-matrix)) displacement))
	  (t
	   (error "Unknown type-displace")))
    (when update
      (update-transform model-matrix))))

;; Refactor and move to callbacks later
(defun eval-node (&optional (create-node-output t))
  ;; Two execution contexts:
  ;; 1. Inside frame
  ;; 2. Outside frame

  ;; Outside frame runs into concurrency/locking issues
  ;; POSS: Detect before running and warn user?
  ;; - See if ID exists in graph

  ;; https://lispcookbook.github.io/cl-cookbook/os.html#running-external-programs
  ;; https://www.reddit.com/r/lisp/comments/8kpbcz/shcl_an_unholy_union_of_posix_shell_and_common/

  ;; TODO:
  ;; 1. Add error handling...
  ;; 2. Parallelize add-node  
  ;; 3. Create another function which ignores output

  (let* ((str (build-string-from-nodes))
	 (output-eval (eval (read-from-string str)))
	 (output-str  (format nil "~S" output-eval)))

    (fmt-model t "eval-node" "Input Str (to eval): ~S~%" str)
    (fmt-model t "eval-node" "Output Str (from eval): ~S~%" output-str)
    (fmt-model t "eval-node" "ID (monotonic time): ~S~%" (osicat:get-monotonic-time))

    (when create-node-output
      
      ;; (insert-node-newline `(t (,+xk-return+ t) t))
      
      ;; Keep track of output objects -> use gensym

      ;; Move pointer down - use anim? -> translate-pointer instead
      (translate-node-x *node-pointer*
		       -11.5199995 ; need to track newline chars
		       :abs
		       nil)
      (translate-node-y *node-pointer*
		       (- (* +linegap+ *scale-node*))
		       :rel
		       nil) ; add more spacing due to bl adjustments
      (update-transform (model-matrix *node-pointer*))
      (enqueue-node-pointer)

      (loop
      	 :for char :across output-str
      	 :do (let ((node (add-node (char-code char))))
	       ;; initial data used for glyph
	       (setf (data node) output-eval)
	       t)))))

(defun remove-all-nodes ()
  ;; Exclude pointer
  (digraph:mapc-vertices (lambda (v)
			   (unless (eq v *node-pointer*)
			     (enqueue-node-zero (index v))
			     (digraph:remove-vertex *digraph* v)))
			 *digraph*)
  (digraph:mapc-edges (lambda (e)
			(digraph:remove-edge *digraph* e))
		      *digraph*))

;; Secondary operators
;; Need to implement hyperweb first to identify nodes

(defun link-node (node-a node-b)
  (digraph:insert-edge *digraph*
		       node-a
		       node-b))

(defun unlink-node (node-a node-b)
  (digraph:remove-edge *digraph*
		       node-a
		       node-b))  

(defun swap-nodes (node-src node-dest)
  ;; Get preds of src
  ;; Remove edges
  ;; Get preds of dest
  ;; Remove edges
  ;; Insert edges between src pres and dest
  ;; Swap positions
  t)

(defun swap-id-with-node (node-src node-dest)
  ;; node-src should be chars or string object
  ;; node-dest should be dest
  ;;
  ;; provide inverse operation to produce id from node
  t)

(defun build-string-from-nodes ()
  ;; Pass starting node else use node-pointer
  ;; To eval, build string from predecessors until newline

  (fmt-model t "build-string-from-nodes" "Pointer: ~a~%" *node-pointer*)
  
  (let ((chrs nil))
    (loop
       ;; currently assuming linear
       :for pred
	 := (digraph:successors *digraph* *node-pointer*)
       :then (digraph:predecessors *digraph* pred)
       :while pred
       :do (loop
	      ;; Leave on first non-ptr node
	      :for n :in pred
	      :do (when (not (eq n *node-pointer*))
		    (when (char-equal (data n) #\Newline)
		      (return))
		    (push (data n) chrs)
		    (setf pred n)
		    (return))))
    
    (with-output-to-string (stream)
      (dolist (c chrs)
	(write-char c stream)))))
