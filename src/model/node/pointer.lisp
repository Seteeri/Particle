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
  (displace-node-x *node-pointer*
		   (- (* 96 *scale-node*))
		   :rel)
  (enqueue-node-pointer)
  (fmt-model t "move-pointer-*" "~a~%" (translation (model-matrix *node-pointer*))))

(defun move-pointer-up (seq-key)
  (displace-node-y *node-pointer*
		   (* +linegap+ *scale-node*) ; add more spacing due to bl adjustments
		   :rel)
  (enqueue-node-pointer)
  (fmt-model t "move-pointer-*" "~a~%" (translation (model-matrix *node-pointer*))))

(defun move-pointer-right (seq-key)
  (displace-node-x *node-pointer*
		   (* 96 *scale-node*)
		   :rel)
  (enqueue-node-pointer)
  (fmt-model t "move-pointer-*" "~a~%" (translation (model-matrix *node-pointer*))))  

(defun move-pointer-down (seq-key)
  (displace-node-y *node-pointer*
		   (- (* +linegap+ *scale-node*)) ; add more spacing due to bl adjustments
		   :rel)
  (enqueue-node-pointer)
  (fmt-model t "move-pointer-*" "~a~%" (translation (model-matrix *node-pointer*))))  
