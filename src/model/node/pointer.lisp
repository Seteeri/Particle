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

;; POSS: Use add-node instead?
(defun init-node-ptr (graph
		      verts
		      pos)
  (let ((node-ptr (init-node-msdf pos
				  *scale-node*
				  (pop *stack-i-nodes*)
				  #\*
				  *color-default-ptr*)))
    (update-transform (model-matrix node-ptr))
    (insert-vertex node-ptr
		   graph
		   verts)
    (spatial-trees:insert node-ptr *r-tree*)
    node-ptr))

(defun init-node-ptr-shm (graph
			  verts
			  pos)
  (let ((node-ptr (init-node-ptr graph
				 verts
				 pos)))
    (copy-nodes-to-shm graph)
    node-ptr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-node-ptr-in  (&optional (* *node-pointer*)) (get-node-in *))
(defun get-node-ptr-out (&optional (* *node-pointer*)) (get-node-out *))
(defun get-node-ptr-bi  (&optional (* *node-pointer*)) (values (get-node-in *) (get-node-out *)))

(defun link-node-ptr (node
		      &optional
			(* *node-pointer*)
			(dir :out))
  (link-node node * dir))

(defun unlink-node-ptr (&optional
			  (* *node-pointer*)
			  (dir :out))
  (unlink-node-first * dir))

(defun deref-ptr (&optional
		    (* *node-pointer*)
		    (dir :out))
  ;; Find final pointer or nth...
  t)
