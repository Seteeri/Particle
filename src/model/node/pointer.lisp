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
    ;; Remember to dec on removal
    (sb-ext:atomic-incf (car *vertices-digraph*))
    node-ptr))

(defun init-node-pointer-graph-shm ()
  (let ((node-pointer (init-node-pointer)))
    (digraph:insert-vertex *digraph*
			   node-pointer)
    (copy-nodes-to-shm)
    node-pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-pointer (seq-event
			  ptree
			  queue
			  fn-new
			  start
			  delta
			  id)
  (fmt-model t "translate-pointer" "~a, ~a -> ~a~%"
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
			     seq-key
			     anim)))))

(defun translate-pointer-left-cb (seq-event
				  ptree
				  queue)
  (with-slots (model-matrix)
      *node-pointer*
    (translate-pointer seq-event
		       ptree
		       queue
		       (lambda (value-new) ; update fn
			 (setf (vx3 (translation model-matrix)) value-new)
			 (enqueue-node-pointer))
		       (vx3 (translation model-matrix)) ; start
		       (- (* 96 *scale-node*))          ; delta
		       'move-pointer-left)))

(defun translate-pointer-right-cb (seq-event
				   ptree
				   queue)
  (with-slots (model-matrix)
      *node-pointer*
    (translate-pointer seq-event
		       ptree
		       queue
		       (lambda (value-new)
			 (setf (vx3 (translation model-matrix)) value-new)
			 (enqueue-node-pointer))
		       (vx3 (translation model-matrix))
		       (* 96 *scale-node*)
		       'move-pointer-right)))

(defun translate-pointer-up-cb (seq-event
				ptree
				queue)
  (with-slots (model-matrix)
      *node-pointer*
    (translate-pointer seq-event
		       ptree
		       queue
		       (lambda (value-new)
			 (setf (vy3 (translation model-matrix)) value-new)
			 (enqueue-node-pointer))
		       (vy3 (translation model-matrix))
		       (* +linegap+ *scale-node*)
		       'move-pointer-up)))

(defun translate-pointer-down-cb (seq-event
				  ptree
				  queue)
  (with-slots (model-matrix)
      *node-pointer*
    (translate-pointer seq-event
		       ptree
		       queue
		       (lambda (value-new)
			 (setf (vy3 (translation model-matrix)) value-new)
			 (enqueue-node-pointer))
		       (vy3 (translation model-matrix))
		       (- (* +linegap+ *scale-node*))
		       'move-pointer-down)))
