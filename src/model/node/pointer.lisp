(in-package :protoform.model)

;; From Solaris Red
;; 220  50  47
(defparameter *color-default-ptr* (make-array (* 4 4) ; or use vec4
					      :adjustable nil
					      :fill-pointer nil
					      :element-type 'single-float
					      :initial-contents  (list (coerce (/ 181 255) 'single-float)
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
								       (coerce (/ 255 255) 'single-float))))

(defun init-node-ptr (graph
		      verts
		      pos)
  (let ((node-ptr (pop *stack-i-nodes*)))
      
    ;; update scale...
    (update-rgba-node node-ptr *color-default-ptr*)
    (update-translation-node node-ptr (vcopy3 pos))
    (update-glyph-node node-ptr #\*)
    (update-transform-node node-ptr)
    
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

(defun get-node-ptr-in  (&optional (* *node-ptr-main*)) (get-node-in *))
(defun get-node-ptr-out (&optional (* *node-ptr-main*)) (get-node-out *))
(defun get-node-ptr-bi  (&optional (* *node-ptr-main*)) (values (get-node-in *) (get-node-out *)))

(defun link-node-ptr (node
		      &optional
			(* *node-ptr-main*)
			(dir :out))
  (link-node node * dir))

(defun unlink-node-ptr (&optional
			  (* *node-ptr-main*)
			  (dir :out))
  (unlink-node-first * dir))

(defun deref-ptr (&optional
		    (* *node-ptr-main*)
		    (dir :out))
  ;; Find final pointer or nth...
  t)
