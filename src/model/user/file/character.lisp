(in-package :protoform.model)

(defun make-list-to-node-syms (list
			       pen)
  t)

(defun make-list-to-node (list
			  pen)
  t)

(defun move-pen-newline (pen baseline)
  (setf (vx3 pen) (vx3 baseline))
  (decf (vy3 pen) (* +linegap+ *scale-node*))
  pen)

(defun make-string-to-node (string
			    pen
			    &key
			      (data nil)
			      (node-prev nil))
  ;; Return last node
  ;; Don't increment pen...
  (loop
     :for char :across string
     :for i :upfrom 0
     :do (let ((node (make-char-to-node char
					(v+ pen
					    (vec3 (* 9.375 +scale-msdf+ *scale-node* i)
						  0
						  0))
					:data data)))
	   ;; Link to previous node
	   (when node-prev
	     (insert-edge node-prev node))
	   (setf node-prev node))))

(defun make-char-to-node (char
			  baseline
			  &key (data nil))
  (let ((node (add-node char
			baseline)))
    (setf (data-obj node) (if data data char))
    ;; Set color of nodes to indicate type
    (send-node node nil)
    (add-node-vcs)
    node))
