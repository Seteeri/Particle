(in-package :protoform.model)

;; From Solaris Red
;; 220  50  47
(defparameter *color-default-ptr* (list (coerce (/ 181 255) 'single-float)
					(coerce (/ 137 255) 'single-float)
					(coerce (/ 0   255) 'single-float)
					(coerce (/ 255 255) 'single-float)
					
					(coerce (/ 181 255) 'single-float)
					(coerce (/ 137 255) 'single-float)
					(coerce (/ 0   255) 'single-float)
					(coerce (/ 255 255) 'single-float)
					
					(coerce (/ 181 255) 'single-float)
					(coerce (/ 137 255) 'single-float)
					(coerce (/ 0   255) 'single-float)
					(coerce (/ 255 255) 'single-float)
					
					(coerce (/ 181 255) 'single-float)
					(coerce (/ 137 255) 'single-float)
					(coerce (/ 0   255) 'single-float)
					(coerce (/ 255 255) 'single-float)))

(defun init-node-pointer ()
  (let ((node-ptr (init-node-msdf (vec3 0 0 0)
				  *scale-node*
				  0
				  #\*
				  *color-default-ptr*)))
    (update-transform (model-matrix node-ptr))
    ;; Remember to dec on removal
    (sb-ext:atomic-incf (car *vertices-digraph*))
    node-ptr))

(defun init-node-pointer-graph-shm ()
  (let ((node-pointer (init-node-pointer)))
    (digraph:insert-vertex *digraph*
			   node-pointer)
    (copy-nodes-to-shm)
    node-pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun delete-node (&optional
		      (node-ptr *node-pointer*)
		      (pos :before))

  ;; Linking Process:
  ;;
  ;; From:
  ;; [a]->[b]<-[*] (start)
  ;; To:
  ;; [a]<-[*] (start)
  ;;
  ;; #1 - Break b edges
  ;; [a] [*]
  ;; [b] 
  ;;
  ;; #2 - Link pointer to a
  ;; [a]<-[*]
  ;;
  ;; #3 - Remove [b]

  (let ((fns (cond ((eq pos :before)
		    (list #'digraph:successors
			  #'digraph:predecessors
			  #'digraph:successors))
		   ((eq pos :after)
		    (list #'digraph:predecessors
			  #'digraph:successors
			  #'digraph:predecessors))
		   (t
		    (error (format nil "delete-node -> unknown option: pos = ~S" pos))))))
    
    (let ((node-* (first (funcall (first fns) *digraph* node-ptr)))
	  (node-** nil))
      
      (when node-*

	;; Note:
	;; preds - nodes that point to it (pointer)
	;; succs - nodes that it points to (prev chars)
	
	;; Remove node-* preds
	;; - char node(s), pointer(s)
	;; - Assume 1 char for now...
	;; - Multi ptrs possible which we ignore
	;; Get node-** first before removing etc
	(let ((nbhrs (funcall (second fns) *digraph* node-*)))
	  (dolist (node-i nbhrs)
	    (remove-edge node-i node-*)
	    (unless (eq node-i node-ptr)
	      (setf node-** node-i))))

	;; Remove node-* succs
	;; - char node(s)
	;; - should not be pointers
	(let ((nbhrs (funcall (third fns) *digraph* node-*)))
	  (dolist (node-i nbhrs)
	    (remove-edge node-* node-i)))

	(when node-**
	  (insert-edge node-ptr node-**))
	
	(remove-vertex node-*)

	;; TODO:
	;; * Refactor move-node-right-of... to take :before/after arg
	
	;; If not node-**, that means first char just deleted
	;; so use its position instead
	(if node-**
	    ;; Update pointer to right of node-** (instead of node-* pos)
	    (if (char-equal (data node-**) #\Newline)
		(move-node-to-node node-ptr node-*)
		(move-node-right-of-node node-ptr node-**))
	    (move-node-to-node node-ptr node-*)))

      ;; Return deleted node
      ;; Caller should not store this so it can be GC'd
      node-*)))

(defun insert-node (node
		    &optional
		      (node-ptr *node-pointer*)
		      (pos :before))
  ;; Linking Process:
  ;;
  ;; (ins here)
  ;;    |
  ;; [a]<-[*] (start)
  ;; [b]
  ;;
  ;; #1
  ;; [a] [*]
  ;; [b] 
  ;;
  ;; #2
  ;; [a]->[b] [*]
  ;;
  ;; #3
  ;; [a]->[b]<-[*]

  ;; Ordered this way in case there is no node-*
  
  ;; Really inserting node before pointer
  (let ((node-* (first (digraph:successors *digraph* node-ptr))))
    (when node-*
      ;; 1. Remove edge between node-*   and ptr
      ;; 2. Insert edge between node-*   and node-new
      (digraph:remove-edge *digraph* node-ptr node-*)
      (digraph:insert-edge *digraph* node-*   node)))
  
  ;; 3. Insert edge between node new and ptr
  (digraph:insert-edge *digraph* node-ptr node))

(defun link-node-pointer (node &optional (unlink-preds nil))
  ;; 1. Remove edge between node-*
  ;; 2. Insert edge between node new
  (let ((node-* (first (digraph:successors *digraph* *node-pointer*))))
    (when node-*
      (when unlink-preds
	(digraph:remove-edge *digraph*
			     *node-pointer*
			     node-*))
      (digraph:insert-edge *digraph*
			   *node-pointer*
			   node))))

(defun translate-pointer (seq-event
			  ptree
			  queue
			  fn-new
			  start
			  delta
			  id)
  (fmt-model t "translate-pointer" "~a, ~a -> ~a~%"
	     id
	     start
	     (+ start
		delta))
  
  (let ((anim (make-instance 'animation
			     :id id
			     :fn-easing #'easing:in-exp ;cubic
			     :fn-new fn-new
			     :value-start start
			     :value-delta delta)))
    ;; in animation.lisp
    (enqueue-anim anim
		  id
		  (lambda ()
		    (funcall #'run-anim
			     seq-key
			     anim)))))

(defun translate-pointer-left-cb (seq-event
				  ptree
				  queue)
  (with-slots (model-matrix)
      *node-pointer*
    (translate-pointer seq-event
		       ptree
		       queue
		       (lambda (value-new) ; update fn
			 (setf (vx3 (translation model-matrix)) value-new)
			 (enqueue-node-pointer))
		       (vx3 (translation model-matrix)) ; start
		       (- (* 96 *scale-node*))          ; delta
		       'move-pointer-x)))

(defun translate-pointer-right-cb (seq-event
				   ptree
				   queue)
  (with-slots (model-matrix)
      *node-pointer*
    (translate-pointer seq-event
		       ptree
		       queue
		       (lambda (value-new)
			 (setf (vx3 (translation model-matrix)) value-new)
			 (enqueue-node-pointer))
		       (vx3 (translation model-matrix))
		       (* 96 *scale-node*)
		       'move-pointer-x)))

(defun translate-pointer-up-cb (seq-event
				ptree
				queue)
  (with-slots (model-matrix)
      *node-pointer*
    (translate-pointer seq-event
		       ptree
		       queue
		       (lambda (value-new)
			 (setf (vy3 (translation model-matrix)) value-new)
			 (enqueue-node-pointer))
		       (vy3 (translation model-matrix))
		       (* +linegap+ *scale-node*)
		       'move-pointer-y)))

(defun translate-pointer-down-cb (seq-event
				  ptree
				  queue)
  (with-slots (model-matrix)
      *node-pointer*
    (translate-pointer seq-event
		       ptree
		       queue
		       (lambda (value-new)
			 (setf (vy3 (translation model-matrix)) value-new)
			 (enqueue-node-pointer))
		       (vy3 (translation model-matrix))
		       (- (* +linegap+ *scale-node*))
		       'move-pointer-y)))
