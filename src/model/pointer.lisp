(in-package :protoform.model)

(defun move-pointer-left (seq-key)
  (with-slots (node-pointer
	       scale-node)
      *model*
    (move-node-x node-pointer
		 (- (* 96 scale-node))
		 :relative)
    (fmt-model t "move-pointer-*" "~a~%" (translation (model-matrix node-pointer)))))

(defun move-pointer-up (seq-key)
  (with-slots (node-pointer
	       scale-node)
      *model*
    (move-node-y node-pointer
		 (* +linegap+ scale-node) ; add more spacing due to bl adjustments
		 :relative)
    (fmt-model t "move-pointer-*" "~a~%" (translation (model-matrix node-pointer)))))

(defun move-pointer-right (seq-key)
  (with-slots (node-pointer
	       scale-node)
      *model*
    (move-node-x node-pointer
		 (* 96 scale-node)
		 :relative)
    (fmt-model t "move-pointer-*" "~a~%" (translation (model-matrix node-pointer)))))    

(defun move-pointer-down (seq-key)
  (with-slots (node-pointer
	       scale-node)
      *model*
    (move-node-y node-pointer
		 (- (* +linegap+ scale-node)) ; add more spacing due to bl adjustments
		 :relative)
    (fmt-model t "move-pointer-*" "~a~%" (translation (model-matrix node-pointer)))))    
