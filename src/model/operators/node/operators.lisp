(in-package :protoform.model)

;; user-facing functions - triggered by callbacks or commands typed and eval'd by user
;; Responsibilities:
;; - enqueue shm ops
;; - append vcs

(defun thunder () 'flash)

(defun draw-graph ()
  ;; Create new graph using data...
  (digraph.dot:draw *digraph-main*
  		    :filename (str:concat (format nil "graph-~a" (osicat:get-monotonic-time))
					  ".png")
		    :format :png))

(defun add-node-ascii (char &optional (move-pointer t))
  (let ((node-new (add-node char)))
    
    ;; Handle when char exists
    ;; (when-let* ((node-ptr-out (get-node-ptr-out))
    ;; 		(node-type (get-node-type node-ptr-out)))
    ;; 	       t)
    
    ;; Attach node to ptr
    (insert-node node-new *node-ptr-main* :out)
    ;; Advance pointer
    (advance-node-of-node *node-ptr-main*
    			  node-new
    			  1.0)
    
    (send-node *node-ptr-main* nil)
    (send-node node-new)
    
    ;; Add event to VCS graph
    (add-node-vcs)
    
    node-new))

(defun backspace-node-ascii ()
  (multiple-value-bind (node-del
			node-del-in
			node-del-out)
      (delete-node)
    (when node-del
      (send-node-zero node-del) ; refactor this func
      (send-node *node-ptr-main*)
      (when node-del-in
	(send-node node-del-in))
      (when node-del-out
	(send-node node-del-out)))))

(defun add-node-tab ()
  (dotimes (n (1- +spaces-tab+))
    (add-node (char-code #\Tab) nil))
  ;; On last add-node move pointer
  (add-node (char-code #\Tab)))

(defun add-node-newline ()
  ;; 1. Add newline node; pointer will move right
  ;; 2. Find beginning of line - loop preds until nil
  ;; 3. Update newline slot: pos-start-line
  ;; 3. Move pointer to newline
  
  ;; Seq-key is +xk-return+ = 65293
  ;; Pass newline char however
  (let* ((node-nl (add-node-ascii #\Newline))
	 (node-start (find-node-line-start node-nl :in)))

    ;; If node not found, i.e. no chars except newline
    ;; use newline pos

    ;; Debugging indication
    (when (eq node-start node-nl)
      (warn "[insert-node-newline] unlinked char"))

    ;; Move right of node and then y down
    ;; Use pos with adjustments:
    ;; x: undo left bounds shift
    ;; y: shift down a line space -
    ;; adjust original pos to baseline first by subtracting bounds
    
    (translate-node-to-node *node-ptr-main*
			    node-start
			    :offset (vec3 0.0
    					  (+ (- (* +linegap+ *scale-node*)))
    					  0.0))
    
    (send-node *node-ptr-main* nil))

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

  (eval '(in-package #:protoform.model))
  
  (let* ((str (build-string-from-nodes))
	 (data-input (read-from-string str))
	 (output-eval (eval data-input))
	 (output-str  (format nil "~S" output-eval)))

    (fmt-model t "eval-node" "Input Str (to eval): ~S~%" str)
    (fmt-model t "eval-node" "Output Str (from eval): ~S~%" output-str)
    (fmt-model t "eval-node" "ID (monotonic time): ~S~%" (osicat:get-monotonic-time))

    (when create-node-output

      ;; Add newline to ptr
      (add-node-newline)

      (build-nodes-from-string output-str)
      ;; (setf (data-obj node) output-eval)

      t)))

(defun link (tgt)
  (cond ((eq tgt :near)
	 t)

	((eq tgt :end)
	 t)

	(t
	 t)))

(defun unlink (&optional (tgt nil))
  (cond ((eq tgt nil) ; default to unlinking ptr
	 (unlink-node-ptr))

	;; otherwise unlink tgt node
	(t
	 (warn "unlink cond t not implemented"))))

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

(defun move-node-ptr (dir) ; pass seq-event?
  
  ;; left = in
  ;; right = out

  (let* ((node-ref (get-node-ptr-out)))
    
    (if (not node-ref)
	;; euclidean move
	(progn
	  (with-slots (model-matrix)
	      *node-ptr-main*
	    (translate-node-ptr nil
				(lambda (value-new) ; update fn
				  (setf (vx3 (translation model-matrix)) value-new)
				  (send-node *node-ptr-main*))
				(vx3 (translation model-matrix)) ; start
				(* 96
				   *scale-node*
				   (cond ((eq dir :in)
					  -1.0)
					 ((eq dir :out)
					  1.0)))
				'move-pointer-x))
	  (send-node *node-ptr-main*))

	(when-let ((node-nxt (get-node-dir node-ref dir)))

		  (unlink-node-ptr)
		  (link-node-ptr node-nxt)
		  
		  ;; move ptr
  		  ;; (advance-node-of-node *node-ptr-main*
  		  ;; 			   *node-ptr-main*
  		  ;; 			   (cond ((eq dir :in)
		  ;; 				  -1.0)
		  ;; 				 ((eq dir :out)
		  ;; 				  1.0)))

		  ;; move ref node right
  		  ;; (advance-node-of-node node-ref
  		  ;; 			   node-ref
  		  ;; 			   (cond ((eq dir :in)
		  ;; 				  1.0)
		  ;; 				 ((eq dir :out)
		  ;; 				  -1.0)))
		  
		  (translate-node-to-node *node-ptr-main*
	     				  node-nxt)

		  (fmt-model t "move-node-ptr" "* -> ~S = ~S~%" node-nxt (data node-nxt))
		  
		  ;; (enqueue-node node-ref)
		  (send-node *node-ptr-main* nil)))))

(defun translate-node-ptr (seq-event
			   fn-new
			   start
			   delta
			   id)
  (when nil
    (fmt-model t "translate-node-ptr" "~a, ~a -> ~a~%"
	       id
	       start
	       (+ start delta)))
  (let ((anim (make-instance 'animation
			     :id id
			     :fn-easing #'easing:linear ;in-exp ;cubic
			     :fn-new fn-new
			     :value-start start
			     :value-delta delta)))
    (sb-concurrency:enqueue (list id
				  (lambda ()
				    (funcall #'run-anim
					     seq-event
					     anim)))
			    *queue-tasks-sync*)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (format t "*:: ~S~%" (digraph:predecessors *digraph-main* *node-ptr-main*))
;; (format t "*:: ~S~%" (digraph:successors *digraph-main* *node-ptr-main*))

;; (format t "A:: ~S <- * <- ~S~%"
;; 	    (if node-ref
;; 		(data node-ref)
;; 		nil)
;; 	    (if node-buf
;; 		(data node-buf)
;; 		nil))  

(defun cut-node ()
  ;; Cut node from left side (succ) -> right side (pred)
  
  ;; Cases (same as del)

  ;; Left Side:
  ;;
  ;; A -> B -> C
  ;;           |
  ;;           * ...
  ;;

  ;; A -> B -> C
  ;;           |
  ;;           * ...  

  ;; Right Side:
  ;;
  ;; Empty buffer
  ;; A -> B -> C
  ;;           |
  ;;           *
  ;;
  ;;
  ;; Filled buffer
  ;; A -> B -> C
  ;;           |
  ;;           * <- D <- E <- F
  ;;

  ;; Two parts
  ;; 1. Pop node from left side
  ;;    - Relink remainder
  ;; 2. Insert node on right side
  ;;    - Relink remainder
  
  ;; (multiple-value-bind (node-pop
  ;; 			node-pop-in
  ;; 			node-pop-out)
  ;;     (pop-node :update-transform nil)
  ;;   (when node-pop
  ;;     (insert-node node-pop
  ;; 		   *node-ptr-main*
  ;; 		   :in)

  ;;     (when node-pop-in
  ;; 	(enqueue-node node-pop-in))
  ;;     (when node-pop-out
  ;; 	(enqueue-node node-pop-out))
	
  ;;     (enqueue-node node-pop)
  ;;     (enqueue-node-ptr)))
      

  ;; (return-from cut-node)
    
  
  (when-let ((node-ref (get-node-ptr-out)))
	    ;; Get node-buf before unlinking ptr bi
	    (let ((node-buf (get-node-ptr-in)))
	      ;; Unlink pointer
	      (unlink-node-ptr :bi)
	      
	      ;; Handle left side
	      
	      ;; Unlink node-ref if needed
	      (when-let ((node-ref-new (get-node-in node-ref)))
			(when node-ref-new
			  ;; Unlink
			  (unlink-node node-ref-new node-ref :in)
			  ;; Link pointer to node-buf in
			  (link-node-ptr node-ref-new :out)))

	      ;; This assumes node is appended left side of right side
	      (link-node-ptr node-ref :in)

	      ;; Handle right side
	      
	      (if node-buf
		  (progn
		    (link-node node-buf node-ref :in)

		    ;; Move node-ref left of node-buf
  		    (advance-node-of-node node-ref
  					  node-buf
  					  -1.0)
		    ;; Move ptr right (of new-ref-new)
		    (if-let ((node-ref-new (get-node-ptr-out)))
			    (advance-node-of-node *node-ptr-main*
  						  node-ref-new
  						  1.0)
  			    (advance-node-of-node *node-ptr-main*
  						  *node-ptr-main*
  						  -1.0)))
		  
		  (progn
		    ;; Move directly right of pointer
		    (advance-node-of-node node-ref
  					  *node-ptr-main*
  					  1.0)
		    ;; Then move ptr to right of out
		    ;; Else move ptr left
		    ;; This will maintain a gap
		    (if-let ((node-ref-new (get-node-ptr-out)))
			    (advance-node-of-node *node-ptr-main*
  						  node-ref-new
  						  1.0)
  			    (advance-node-of-node *node-ptr-main*
  						  *node-ptr-main*
						  -1.0))))

	      (when node-buf
  		(send-node node-buf))
	      (send-node node-ref)
	      (send-node *node-ptr-main*))))
	      
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
  (when-let ((node-buf (get-node-ptr-in)))
	    (let ((node-ref (get-node-ptr-out)))
	      ;; Unlink pointer
	      (unlink-node-ptr :bi)
	      
	      ;; Handle right side of ptr first
	      	      
	      ;; Unlink node-buf if needed
	      (when-let ((node-buf-new (get-node-in node-buf)))
			;; Unlink
			(unlink-node node-buf-new node-buf :in)
			;; Link pointer to node-buf in
			(link-node-ptr node-buf-new :in))

	      (link-node-ptr node-buf :out)
	      
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
		    (advance-node-of-node *node-ptr-main*
  		    			  node-buf
  		    			  1.0))
		  (progn
		    ;; Move ptr right
  		    (advance-node-of-node *node-ptr-main*
  	  				  *node-ptr-main*
  	  				  1.0)
		    ;; Position node-buf left of ptr
		    (advance-node-of-node node-buf
  					  *node-ptr-main*
  					  -1.0))))
	    
	    (send-node node-buf nil)
	    (send-node *node-ptr-main* nil)))

(defun copy-node () ;;-to-pointer
  ;; Copy node from left side (succ) -> right side (pred)
  ;; Remainder same as cut node
  ;;
  ;; Procedure:
  ;; 1. Copy node
  ;; 2. Link ptr node-new
  ;; 3. advance-node-of-node node-new ptr :+

  (when-let ((node-src (get-node-ptr-out))) ; aka node-ref
    (let ((node-ref (copy-node-to-node node-src))
	  (node-buf (get-node-ptr-in)))
      ;; Only unlink right side of pointer
      (unlink-node-ptr :in)

      ;; Remainder of this is right side handling from cut

      ;; This assumes node is appended left side of right side
      (link-node-ptr node-ref :in)  
      
      (if node-buf
	  (progn
	    (link-node node-buf node-ref :in)
	    ;; Move node-ref left of node-buf
  	    (advance-node-of-node node-ref
  				  node-buf
  				  -1.0)
	    ;; Move ptr right (of new-ref-new)
	    (if-let ((node-ref-new (get-node-ptr-out)))
		    (advance-node-of-node *node-ptr-main*
  					  node-ref-new
  					  1.0)
  		    (advance-node-of-node *node-ptr-main*
  					  *node-ptr-main*
  					  -1.0)))
	  
	  (progn
	    ;; Move directly right of pointer
	    (advance-node-of-node node-ref
  				  *node-ptr-main*
  				  1.0)
	    ;; Then move ptr to right of out
	    ;; Else move ptr left
	    ;; This will maintain a gap
	    (if-let ((node-ref-new (get-node-ptr-out)))
		    (advance-node-of-node *node-ptr-main*
  					  node-ref-new
  					  1.0)
  		    (advance-node-of-node *node-ptr-main*
  					  *node-ptr-main*
  					  -1.0))))
      
      (when node-buf
	(send-node node-buf))
      (send-node node-ref nil)
      (send-node *node-ptr-main*))))
