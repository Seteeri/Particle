(in-package :protoform.model)

(defun init-node-pointer ()
    (let ((node-ptr (init-node-msdf (vec3 -11.5199995 14.127416 0)
				    *scale-node*
				    0
				    #\*
				    *color-default-ptr*)))
      (update-transform (model-matrix node-ptr))
      node-ptr))

(defun init-node-pointer-graph-shm ()
  (let ((node-pointer (init-node-pointer)))
    (digraph:insert-vertex *digraph*
			   node-pointer)
    (copy-nodes-to-shm)
    node-pointer))

(defun move-pointer-left (seq-key)
    (move-node-x *node-pointer*
		 (- (* 96 *scale-node*))
		 :relative)
    (fmt-model t "move-pointer-*" "~a~%" (translation (model-matrix *node-pointer*))))

(defun move-pointer-up (seq-key)
    (move-node-y *node-pointer*
		 (* +linegap+ *scale-node*) ; add more spacing due to bl adjustments
		 :relative)
    (fmt-model t "move-pointer-*" "~a~%" (translation (model-matrix *node-pointer*))))

(defun move-pointer-right (seq-key)
    (move-node-x *node-pointer*
		 (* 96 *scale-node*)
		 :relative)
    (fmt-model t "move-pointer-*" "~a~%" (translation (model-matrix *node-pointer*))))  

(defun move-pointer-down (seq-key)
    (move-node-y *node-pointer*
		 (- (* +linegap+ *scale-node*)) ; add more spacing due to bl adjustments
		 :relative)
    (fmt-model t "move-pointer-*" "~a~%" (translation (model-matrix *node-pointer*))))  
