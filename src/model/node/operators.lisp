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
	 (node-start (find-node-line-start node-nl :before)))

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

(defun show-node-ids ()
  ;; Procedure:
  t)

(defun cut-node ()
  ;; Cut node from left side (succ) -> right side (pred)
  ;;
  ;; Procedure:
  ;; 1. Get node-*
  ;; 2. Get node-**
  ;; 3. Unlink ** and *
  ;; 4. Relink pointer to node-**
  ;; 5. Link pointer to node-* (+x)
  ;; 6. Move to right of pointer
  ;;
  ;; A -> B -> C <- *
  ;;
  ;; A -> B <- * <- C
  ;;
  ;; A <- * <- C <- B
  ;;
  ;; * <- C <- B  <- A
  
  (let* ((node-* (get-node-pointer-reference))
	 (node-** node-*))
    
    (when node-*
      
      ;; Get node-**
      (let ((nbhrs (digraph:predecessors *digraph* node-*)))
	(dolist (node-i nbhrs)
	  (unless (eq node-i *node-pointer*)
	    (setf node-** node-i))))

      ;; (format t "** = ~S; * = ~S~%" (data node-**) (data node-*))

      ;; Unlink node-* from everything...aka isolate
      ;; - pass nil to unlink function to isolate? or make explicit function?
      ;;
      ;; Unlink node-** and node-*
      (unlink-node node-** node-*)
      ;; Unlink node-* <- ptr
      ;; Link node-** <- ptr
      (if (eq node-** node-*)
	  (progn
	    ;; If same:
	    ;; - actions cancel out so just unlink
	    ;; - move pointer to its position	    
	    (unlink-node-pointer)
	    ;; passing self will move it 1x advance
	    (move-node-x-of-node *node-pointer* *node-pointer* :- :ignore-y t))
	  (progn
	    (relink-node-pointer node-**)
	    ;; Move pointer to right of node-**
	    (move-node-x-of-node *node-pointer* node-** :+ :ignore-y t)))

      ;; - for moves, ignore y adjustment?
      ;;   - or readjust for new character - but should already be aligned to baseline so need not
      ;; do anything
            
      (let ((node-ptr-end-r (find-node-end *node-pointer*
					   :before
					   :before)))
	;; Link ptr/node <- node-*
	(link-node node-* node-ptr-end-r)
	;; Move node-* (copy) to right of ptr
	(move-node-x-of-node node-* node-ptr-end-r :+ :ignore-y t))

      ;; - Must move everything...can run || given index assuming monospace
      ;; (do-node (node *node-pointer* :after :before)
      ;; 	(format t "n = ~S:~S~%" node (data node)))
      
      (enqueue-node node-*)
      (enqueue-node node-**)
      (enqueue-node-pointer)
      
      t)))

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
  ;;
  ;; Procedure:
  ;; 1. Unlink ptr node-paste
  ;; 2. Insert node-ptr
  ;; 3. move-node-x-of-node node-paste node-ptr :+
  ;; 4. move-node-x-of-node node-ptr node-paste
  t)

(defun link-node (node-a node-b)
  ;; Need not pos argument - user switches args
  (insert-edge node-a node-b))

(defun unlink-node (node-a node-b)
  (remove-edge node-a node-b))

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
