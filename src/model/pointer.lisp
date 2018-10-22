(in-package :protoform.model)

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
