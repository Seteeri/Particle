(in-package :protoform.model)

;; core functions - triggered by callbacks or meant to be called by user

(defun add-node (code &optional (move-pointer t))
  
  ;; Split into
  ;; node new
  ;; digraph
  ;; pointer
  
  (let ((node (init-node-msdf (translation (model-matrix *node-pointer*))
			      *scale-node*
			      (digraph:count-vertices *digraph*)
			      (code-char code))))
    
    ;; Why repeat when init-node does this?
    (update-transform (model-matrix node))

    (insert-vertex node)

    (insert-node node)
    
    (when move-pointer
      (move-node-x-of-node *node-pointer* node :+))
    
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
      (move-node-to-node *node-pointer*
			 node-start
			 (vec3 (- (* (aref bounds-origin 0) *scale-node*))
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
  ;; A <------- * <- B    C | Link B ptr (should link ptr last?)
  ;; A <------- * <- B <- C | Link C B

  ;; Last item:
  ;; A <- * <-      C <- B      | START
  ;; A    *         C <- B      | Unlink ptr
  ;;
  ;;      *    A <- C <- B      | Link C A
  ;;      * <- A <- C <- B      | Link A ptr
  
  ;; Default to FIFO
  (multiple-value-bind (node-buf node-ref)
      (unlink-node-pointer :bi)

    ;; (format t "*:: ~S~%" (digraph:predecessors *digraph* *node-pointer*))
    ;; (format t "*:: ~S~%" (digraph:successors *digraph* *node-pointer*))
    
    ;; (format t "A:: ~S <- * <- ~S~%"
    ;; 	    (if node-ref
    ;; 		(data node-ref)
    ;; 		nil)
    ;; 	    (if node-buf
    ;; 		(data node-buf)
    ;; 		nil))

    ;; Ensure node to cut
    (when node-ref
      (let ((node-ref-new (get-node-in node-ref)))

	;; (format t "  node-ref-new: ~S~%"
	;; 	(if node-ref-new (data node-ref-new) nil))

	;; Position pointer first since later moves are relative to pointer
	(if node-ref-new
	    (progn
	      (unlink-nodes node-ref-new node-ref)
	      (link-nodes *node-pointer* node-ref-new)
  	      (move-node-x-of-node *node-pointer*
  	  			   node-ref-new
  	  			   :+
  	  			   :ignore-y t))
  	    (move-node-x-of-node *node-pointer*
  	  			 *node-pointer*
  	  			 :-
  	  			 :ignore-y t))
	
	;; This part depends on whether buffer filled or not
	;; POSS: Use insert function?
	(if node-buf
	    (progn
	      (link-nodes node-buf node-ref)
	      (link-nodes node-ref *node-pointer*)
	      ;; Move buf right of itself
	      (move-node-x-of-node node-buf
  				   node-buf
  				   :+
  				   :ignore-y t)	      
	      ;; Then move ref left of buf
	      (move-node-x-of-node node-ref
  				   node-buf
  				   :-
  				   :ignore-y t))
	    (progn
	      (link-nodes node-ref *node-pointer*)
	      (move-node-x-of-node node-ref
				   *node-pointer*
  				   :+
  				   :ignore-y t))))
      
      ;; (multiple-value-bind (node-buf node-ref)
      ;; 	  (get-node-bi-ptr)
      ;; 	(format t "B:: ~S <- * <- ~S~%"
      ;; 		(if node-ref
      ;; 		    (data node-ref)
      ;; 		nil)
      ;; 		(if node-buf
      ;; 		    (data node-buf)
      ;; 		    nil)))
      
      (when node-buf
	(enqueue-node node-buf))
      (enqueue-node node-ref)
      (enqueue-node-pointer))))  

(defun copy-node ()
  ;; Copy node from left side (succ) -> right side (pred)
  ;;
  ;; Procedure:
  ;; 1. Copy node
  ;; 2. Link ptr node-new
  ;; 3. move-node-x-of-node node-new ptr :+
  t)

(defun paste-node ()
  ;; Take node from right side (pred) -> left side (succ)

  ;; Empty buffer:
  ;;      * <- A <- B <- C | START
  ;;      *    A <- B <- C | Unlink ptr
  ;;      *    A <- B <- C | Unlink B A

  ;; Unlink ptr
  ;; in out
  ;; Given a<-*<-b:
  ;; in = b, out = a
  ;; b = buffer node
  (multiple-value-bind (node-tgt node-*)
      (unlink-node-pointer :bi)
    
    (when node-tgt
      
      ;; Link buffer remainder to ptr
      (when-let ((1-node-* (get-node-in node-tgt)))
		;; (format t "LINKING ~S to ~S~%" (data *-node-1) (data *-node))
		(unlink-nodes 1-node-* node-tgt)
    		(link-node-pointer 1-node-*
    				   :in))

      ;; Link pred node after pointer
      ;; - later use swap function
      (link-node-pointer node-tgt)
      
      ;; Link copy node to pointed node if possible
      (if node-*
	  (progn
	    (link-node node-* node-tgt)
	    ;; Move copy node right of pointed node
	    (move-node-x-of-node node-tgt
				 node-*
				 :+
				 :ignore-y t)
	    ;; Move ptr node right of copy node
	    (move-node-x-of-node *node-pointer*
				 node-tgt
				 :+
				 :ignore-y t))
	  (progn
	    ;; No pointed node so move ptr right
	    ;; Then move copy node left of ptr
	    (move-node-x-of-node *node-pointer*
				 *node-pointer*
				 :+
				 :ignore-y t)
	    (move-node-x-of-node node-tgt
				 *node-pointer*
				 :-
				 :ignore-y t)))
            
      (enqueue-node node-tgt)
      (enqueue-node-pointer))))

(defun link-nodes (node-a node-b)
  ;; Need not pos argument - user switches args
  (insert-edge node-a node-b))

(defun unlink-nodes (node-a node-b)
  (remove-edge node-a node-b))

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


;; 	;; Pass starting position, i.e. node-pointer position
;; 	;; For nodes, index to calculate
;; 	;; Have to set index serially...
;; 	;; - faster way would be to use spatial relationship to calculate index
;; 	;;   -> only works when nodes are sequentially positioned
;; 	;;   -> radial would be better
;; 	;;

;; 	;; pos = start + (advance * index)
;; 	;; adjust-pos by bounds

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
