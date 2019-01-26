(in-package :protoform.model)

;; core functions - triggered by callbacks or meant to be called by user

(defun add-node (code &optional (move-pointer t))
  
  ;; Split into
  ;; node new
  ;; digraph
  ;; pointer
  
  (let* ((baseline (get-origin-from-node-pos *node-pointer*))
	 (node (init-node-msdf baseline
			       *scale-node*
			       (digraph:count-vertices *digraph*)
			       (code-char code))))

    (insert-vertex node)

    (insert-node node *node-pointer* :out)
    
    (when move-pointer
      (advance-node-of-node *node-pointer*
			    node
			    1.0))
    
    (enqueue-node node)

    node))

(defun backspace-node ()
  (let ((node-del (delete-node)))
    (when node-del
      ;; Update shm
      (enqueue-node-pointer)
      (enqueue-node-zero (index node-del)))))

(defun insert-node-tab ()
  (dotimes (n (1- +spaces-tab+))
    (add-node (char-code #\Tab) nil))
  ;; On last add-node move pointer
  (add-node (char-code #\Tab)))

(defun insert-node-newline ()
  ;; 1. Add newline node; pointer will move right
  ;; 2. Find beginning of line - loop preds until nil
  ;; 3. Update newline slot: pos-start-line
  ;; 3. Move pointer to newline
  
  ;; Seq-key is +xk-return+ = 65293
  ;; Pass newline char however
  (let* ((node-nl (add-node (char-code #\Newline) nil))
	 (node-start (find-node-line-start node-nl :in)))

    ;; If node not found, i.e. no chars except newline
    ;; use newline pos

    ;; Might help with debugging that something is wrong
    (when (eq node-start node-nl)
      (warn (format nil "insert-node-newline -> CHAR NODES NOT FOUND")))

    ;; Move right of node and then y down
    ;; Use pos with adjustments:
    ;; x: undo left bounds shift
    ;; y: shift down a line space -
    ;; adjust original pos to baseline first by subtracting bounds

    (let* ((bounds-origin (bounds-origin (gethash (char-code (data node-start)) *metrics*))))
      (translate-node-to-node *node-pointer*
			      node-start
			      :offset (vec3 (- (* (aref bounds-origin 0) *scale-node*))
    					    (+ (- (* (aref bounds-origin 1) *scale-node*))
    					       (- (* +linegap+ *scale-node*)))
    					    0.0)))
    
    (enqueue-node-pointer))

  ;; Store pos in newline...
  
  t)

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
      
      ;; Keep track of output objects -> use gensym

      ;; Add newline to str?
      (insert-node-newline)
      
      (loop
      	 :for char :across output-str
      	 :do (let ((node (add-node (char-code char))))
	       ;; initial data used for glyph
	       (setf (data node) output-eval)
	       t)))))

(defun draw-graph ()
  (digraph.dot:draw *digraph*
  		    :filename (str:concat (format nil "graph-~a" (osicat:get-monotonic-time))
					  ".png")
		    :format :png))

;; (format t "*:: ~S~%" (digraph:predecessors *digraph* *node-pointer*))
;; (format t "*:: ~S~%" (digraph:successors *digraph* *node-pointer*))

;; (format t "A:: ~S <- * <- ~S~%"
;; 	    (if node-ref
;; 		(data node-ref)
;; 		nil)
;; 	    (if node-buf
;; 		(data node-buf)
;; 		nil))  

(defun cut-node ()
  ;; Cut node from left side (succ) -> right side (pred)
  
  ;; Empty buffer:
  ;; A -> B -> C <- *      | START
  ;; A -> B -> C    *      | Unlink ptr
  ;;
  ;; A -> B    C    *      | Unlink C (B-C)
  ;; A <- B <------ *    C | Link ptr B
  ;;
  ;; A -> B <------ * <- C | Link C ptr

  ;; Filled buffer:
  ;; A -> B  <- * <- C     | START
  ;; A -> B     *    C     | Unlink ptr
  ;;
  ;; A    B     *    C     | Unlink B (A-B)
  ;; A <------- *    C   B | Link ptr A
  ;;
  ;; A <------- *    B <- C | Link C B  
  ;; A <------- * <- B    C | Link B ptr (should link ptr last?)

  ;; Last item:
  ;; A <- * <-      C <- B      | START
  ;; A    *         C <- B      | Unlink ptr
  ;;
  ;;      *    A <- C <- B      | Link C A
  ;;      * <- A <- C <- B      | Link A ptr

  ;; Must be ref node to do anything

  ;; (multiple-value-bind (node-buf node-ref)
  ;; 	  (get-node-bi-ptr)
  ;; 	(format t "B:: ~S <- * <- ~S~%"
  ;; 		(if node-ref
  ;; 		    (data node-ref)
  ;; 		nil)
  ;; 		(if node-buf
  ;; 		    (data node-buf)
  ;; 		    nil)))
  
  (when-let ((node-ref (get-node-out-ptr)))
	    ;; Get in before unlinking ptr
	    (let ((node-buf (get-node-in-ptr)))
	      ;; Unlink pointer
	      (unlink-node-pointer :bi)

	      ;; (format t "ref: ~S, buf: ~S~%" node-ref node-buf)
	      
	      ;; Handle left side
	      
	      ;; Unlink node-ref if needed
	      (when-let ((node-ref-new (get-node-in node-ref)))
			(when node-ref-new
			  ;; Unlink
			  (unlink-node node-ref-new node-ref :in)
			  ;; Link pointer to node-buf in
			  (link-node-pointer node-ref-new :out)))

	      ;; This assumes node is appended left side of right side
	      (link-node-pointer node-ref :in)

	      ;; Handle right side
	      
	      (if node-buf
		  (progn
		    (link-node node-buf node-ref :in)

		    ;; Move node-ref left of node-buf
  		    (advance-node-of-node node-ref
  					  node-buf
  					  :-)
		    ;; Move ptr right (of new-ref-new)
		    (if-let ((node-ref-new (get-node-out-ptr)))
			    (advance-node-of-node *node-pointer*
  						  node-ref-new
  						  1.0)
  			    (advance-node-of-node *node-pointer*
  						  *node-pointer*
  						  -1.0)))
		  
		  (progn
		    ;; Move directly right of pointer
		    (advance-node-of-node node-ref
  					  *node-pointer*
  					  :+)
		    ;; Then move ptr to right of out
		    ;; Else move ptr left
		    ;; This will maintain a gap
		    (if-let ((node-ref-new (get-node-out-ptr)))
			    (advance-node-of-node *node-pointer*
  						  node-ref-new
  						  1.0)
  			    (advance-node-of-node *node-pointer*
  						  *node-pointer*
						  -1.0))))

	      (when node-buf
  		(enqueue-node node-buf))
	      (enqueue-node node-ref)
	      (enqueue-node-pointer))))
	      
(defun paste-node ()
  ;; Take node from right side (pred) -> left side (succ)

  ;; nil node-ref:
  ;;      * <- A <- B <- C | START
  ;;      *    A <- B <- C | Unlink ptr
  ;;      *    A    B <- C | Unlink A (B-A)
  ;;
  ;; A <- * <------ B <- C | Link B ptr  
  ;; A <- *         B <- C | Link ptr A


  ;; t node-ref:
  ;; A <------- * <- B <- C | START
  ;; A          *    B <- C | Unlink ptr
  ;; A          *    B    C | Unlink B (C-B)
  ;;
  ;; A    B     * <------ C | Link C ptr
  ;; A    B <-- * <------ C | Link ptr B
  ;;
  ;; A -> B <-- * <------ C | Link A B

  ;; (format t "* <- ~S <- ~S~%"
  ;; 	    (if node-buf
  ;; 		(data node-buf)
  ;; 		nil)
  ;; 	    (if node-buf
  ;; 		(if (get-node-in node-buf)
  ;; 		    (data (get-node-in node-buf))
  ;; 		    nil)
  ;; 		nil))

  ;; Must be buffer node to do anything
  (when-let ((node-buf (get-node-in-ptr)))
	    (let ((node-ref (get-node-out-ptr)))
	      ;; Unlink pointer
	      (unlink-node-pointer :bi)
	      
	      ;; Handle right side of ptr first
	      	      
	      ;; Unlink node-buf if needed
	      (when-let ((node-buf-new (get-node-in node-buf)))
			;; Unlink
			(unlink-node node-buf-new node-buf :in)
			;; Link pointer to node-buf in
			(link-node-pointer node-buf-new :in))

	      (link-node-pointer node-buf :out)
	      
	      ;; Handle left side of ptr
	      (if node-ref
		  (progn
		    ;; Link node-buf
		    (link-node node-ref node-buf :in)
		    ;; Move node-buf right of node-ref
		    (advance-node-of-node node-buf
  		    			  node-ref
  		    			  1.0)
		    ;; Move ptr right of node-buf
		    (advance-node-of-node *node-pointer*
  		    			  node-buf
  		    			  1.0))
		  (progn
		    ;; Move ptr right
  		    (advance-node-of-node *node-pointer*
  	  				  *node-pointer*
  	  				  1.0)
		    ;; Position node-buf left of ptr
		    (advance-node-of-node node-buf
  					  *node-pointer*
  					  -1.0))))
	    
	    (enqueue-node node-buf)
	    (enqueue-node-pointer)))

(defun copy-node () ;;-to-pointer
  ;; Copy node from left side (succ) -> right side (pred)
  ;; Remainder same as cut node
  ;;
  ;; Procedure:
  ;; 1. Copy node
  ;; 2. Link ptr node-new
  ;; 3. advance-node-of-node node-new ptr :+

  (when-let ((node-src (get-node-out-ptr))) ; aka node-ref
    (let ((node-ref (copy-node-to-node node-src))
	  (node-buf (get-node-in-ptr)))
      ;; Only unlink right side of pointer
      (unlink-node-pointer :in)

      ;; Remainder of this is right side handling from cut

      ;; This assumes node is appended left side of right side
      (link-node-pointer node-ref :in)  
      
      (if node-buf
	  (progn
	    (link-node node-buf node-ref :in)
	    ;; Move node-ref left of node-buf
  	    (advance-node-of-node node-ref
  				  node-buf
  				  -1.0)
	    ;; Move ptr right (of new-ref-new)
	    (if-let ((node-ref-new (get-node-out-ptr)))
		    (advance-node-of-node *node-pointer*
  					  node-ref-new
  					  1.0)
  		    (advance-node-of-node *node-pointer*
  					  *node-pointer*
  					  -1.0)))
	  
	  (progn
	    ;; Move directly right of pointer
	    (advance-node-of-node node-ref
  				  *node-pointer*
  				  1.0)
	    ;; Then move ptr to right of out
	    ;; Else move ptr left
	    ;; This will maintain a gap
	    (if-let ((node-ref-new (get-node-out-ptr)))
		    (advance-node-of-node *node-pointer*
  					  node-ref-new
  					  1.0)
  		    (advance-node-of-node *node-pointer*
  					  *node-pointer*
  					  -1.0))))
      
      (when node-buf
	(enqueue-node node-buf))
      (enqueue-node node-ref)
      (enqueue-node-pointer))))

;; (defun link-nodes (node-a node-b)
;;   ;; Need not pos argument - user switches args
;;   (insert-edge node-a node-b))

;; (defun unlink-nodes (node-a node-b)
;;   (remove-edge node-a node-b))

(defun swap-nodes (node-src node-dest)
  ;; Swap links
  ;; - option to swap positions?
  
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

(defun show-node-ids ()
  ;; Procedure:
  t)


;; Pass starting position, i.e. node-pointer position
;; For nodes, index to calculate
;; Have to set index serially...
;; - faster way would be to use spatial relationship to calculate index
;;   -> only works when nodes are sequentially positioned
;;   -> radial would be better
;;

;; pos = start + (advance * index)
;; adjust-pos by bounds

;; 	(digraph:mapc-vertices (lambda (vert)
;; 				 (sb-concurrency:enqueue
;; 				  (list nil
;; 					(make-symbol (format nil "node-~a~%" (index vert)))
;; 					'()
;; 					(lambda ()
;; 					  (format t "~S~%" (translation (model-matrix vert)))
;; 					  ;; (funcall #'randomize-color-node vert)
;; 					  ))
;; 				  *queue-anim*))
;; 			       *digraph*))
