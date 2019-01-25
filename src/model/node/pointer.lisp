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
    node-ptr))

(defun init-node-pointer-graph-shm ()
  (let ((node-pointer (init-node-pointer)))
    (insert-vertex node-pointer)
    (copy-nodes-to-shm)
    node-pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-node-pointer-reference (&optional
				     (pos :after)
				     (node-ptr *node-pointer*))
  ;; TODO:
  ;; - Add arg to get first or all
  (cond ((eq pos :after)
	 (first (digraph:successors *digraph* node-ptr)))
	((eq pos :before)
	 (first (digraph:predecessors *digraph* node-ptr)))
	(t
	 (error "get-node-pointer-reference: pos invalid"))))

 ;; Specific functions for pointer-context linking

(defun link-node-pointer (node &optional (pos :after))
  ;; pos:
  ;; before = node -> *
  ;; after = * -> node
  (cond ((eq pos :before)
	 (insert-edge node *node-pointer*))
	((eq pos :after)
	 (insert-edge *node-pointer* node))
	(t
	 (error "link-node-pointer: pos invalid"))))

(defun relink-node-pointer (node &optional
				   (pos-old :after)
				   (pos-new :after))
  ;; Return edges?
  (unlink-node-pointer pos-old)
  (link-node-pointer node pos-new))

(defun unlink-node-pointer (&optional (pos :after))
  (cond ((eq pos :before)
	 (when-let ((node-* (get-node-pointer-reference pos)))
		   (remove-edge node-* *node-pointer*)
		   node-*))
	((eq pos :after)
	 (when-let ((node-* (get-node-pointer-reference pos)))
		   (remove-edge *node-pointer* node-*)
		   node-*))
	((eq pos :bi)
	 (let* ((node-* (get-node-pointer-reference :before))
		(*-node (get-node-pointer-reference :after)))
	   (when node-*
	     (remove-edge node-* *node-pointer*))
	   (when *-node
	     (remove-edge *node-pointer* node-*))
	   (values node-* *-node)))
	(t
	 (error "unlink-node-pointer: pos invalid"))))

;; translate block

(defun translate-pointer (seq-event
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
			     seq-event
			     anim)))))

(defun edge-pointer ()
  ;; Move before or after
  t)
