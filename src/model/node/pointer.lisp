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

(defun get-node-ptr-in (&optional (* *node-pointer*))
  (get-node-in *))

(defun get-node-ptr-out (&optional (* *node-pointer*))
  (get-node-out *))

(defun get-node-ptr-bi (&optional (* *node-pointer*))
  (values (get-node-in *)
	  (get-node-out *)))

(defun link-node-ptr (node &optional (dir :out))
  (link-node node *node-pointer* dir))

(defun unlink-node-ptr (&optional (dir :out))
  (unlink-node-first *node-pointer* dir))

(defun relink-node-ptr (node &optional
			       (dir-old :out)
			       (dir-new :out))
  ;; Return edges?
  (unlink-node-ptr dir-old)
  (link-node-ptr node dir-new))

;; translate block

(defun translate-node-ptr (seq-event
			   fn-new
			   start
			   delta
			   id)
  (fmt-model t "translate-node-ptr" "~a, ~a -> ~a~%"
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
