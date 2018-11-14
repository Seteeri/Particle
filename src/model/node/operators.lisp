(in-package :protoform.model)

(defun displace-node-x (node
		    displacement
		    type-displace
		    &optional
		      (copy-to-shm t)
		      (copy-to-cache t))
    (with-slots (model-matrix
		 index)
	node
      (cond ((eq type-displace :absolute)
	     (setf (vx3 (translation model-matrix)) displacement))
	    ((eq type-displace :relative)
	     (incf (vx3 (translation model-matrix)) displacement))
	    (t
	     (error "Unknown type-displace")))
      (update-transform model-matrix)
      (when copy-to-shm
	(copy-node-to-shm node
			  (* index
			     (/ +size-struct-instance+ 4))))
      (when copy-to-cache
	(memcpy-shm-to-cache-flag* (list (list "nodes"
				       	       0
      				       	       (* +size-struct-instance+ (+ (digraph:count-vertices *digraph*)
				       					    (digraph:count-edges *digraph*)))))))))

(defun displace-node-y (node
		    displacement
		    type-displace
		    &optional
		      (copy-to-shm t)
		      (copy-to-cache t))
    (with-slots (model-matrix
		 index)
	node
      (cond ((eq type-displace :absolute)
	     (setf (vy3 (translation model-matrix)) displacement))
	    ((eq type-displace :relative)
	     (incf (vy3 (translation model-matrix)) displacement))
	    (t
	     (error "Unknown type-displace")))
      (update-transform model-matrix)
      (when copy-to-shm
	(copy-node-to-shm node
			  (* index
			     (/ +size-struct-instance+ 4))))
      (when copy-to-cache
	(memcpy-shm-to-cache-flag* (list (list "nodes"
				       	       0
      				       	       (* +size-struct-instance+ (+ (digraph:count-vertices *digraph*)
				       					    (digraph:count-edges *digraph*)))))))))

(defun eval-node-msdf (seq-key)
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
