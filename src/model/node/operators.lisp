(in-package :protoform.model)

(defun enqueue-node (node &optional (pointer t))
  (when pointer
    (enqueue-node-pointer))
  (sb-concurrency:enqueue (list *channel*
				*shm-nodes*
				(serialize-node node)
				(* (index node)
				   +size-struct-instance+))
			  *queue-shm*))

(defun enqueue-node-pointer ()
  (sb-concurrency:enqueue (list *channel*
				*shm-nodes*
				(serialize-node *node-pointer*)
				(* (index *node-pointer*)
				   +size-struct-instance+))
			  *queue-shm*))  

(defun enqueue-node-zero (index)
  (sb-concurrency:enqueue (list *channel*
				*shm-nodes*
				*data-zero-node*
				(* index
				   +size-struct-instance+))
			  *queue-shm*))

(defun displace-node-x (node
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

(defun displace-node-y (node
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

(defun eval-node-msdf (seq-key)
  ;; For eval, if code modifies data used by a thread
  ;; will result in corruption...how to detect?
  ;; - Warn user before eval - see if id exist in graph
  
  ;; To eval, build up string from predecessors
  (let ((node-tgt (first (digraph:successors *digraph* *node-pointer*)))
        (chrs nil))
    (loop
       :for pred := node-tgt :then (digraph:predecessors *digraph* pred)
       :while pred
       :do (progn
	     (when (listp pred)
	       (if (eq (first pred) *node-pointer*)
		   (setf pred (second pred))
		   (setf pred (first pred))))
	     (push (data pred) chrs)
	     (when nil (format t "~a: ~a~%" pred (data pred)))))

    (fmt-model t "eval-node-msdf" "Eval: ~a~%" (eval (read-from-string (with-output-to-string (stream)
									 (dolist (c chrs)
									   (write-char c stream))))))
    (when nil
      (digraph.dot:draw digraph :filename "digraph.png" :format :png))))
