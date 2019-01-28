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

(defun move-node-ptr (dir) ; pass seq-event?
  
  ;; left = in
  ;; right = out

  (let* ((node-ref (get-node-ptr-out)))
    
    (if (not node-ref)
	;; euclidean move
	(progn
	  (with-slots (model-matrix)
	      *node-pointer*
	    (translate-node-ptr nil
				(lambda (value-new) ; update fn
				  (setf (vx3 (translation model-matrix)) value-new)
				  (enqueue-node-ptr))
				(vx3 (translation model-matrix)) ; start
				(* 96
				   *scale-node*
				   (cond ((eq dir :in)
					  -1.0)
					 ((eq dir :out)
					  1.0)))
				'move-pointer-x))
	  (enqueue-node-ptr))

	(when-let ((node-nxt (get-node-dir node-ref dir)))

		  (unlink-node-ptr)
		  (link-node-ptr node-nxt)
		  
		  ;; move ptr
  		  ;; (advance-node-of-node *node-pointer*
  		  ;; 			   *node-pointer*
  		  ;; 			   (cond ((eq dir :in)
		  ;; 				  -1.0)
		  ;; 				 ((eq dir :out)
		  ;; 				  1.0)))

		  ;; move ref node right
  		  ;; (advance-node-of-node node-ref
  		  ;; 			   node-ref
  		  ;; 			   (cond ((eq dir :in)
		  ;; 				  1.0)
		  ;; 				 ((eq dir :out)
		  ;; 				  -1.0)))
		  
		  (translate-node-to-node *node-pointer*
	     				  node-nxt)

		  (fmt-model t "move-node-ptr" "* -> ~S = ~S~%" node-nxt (data node-nxt))
		  
		  ;; (enqueue-node node-ref)
		  (enqueue-node-ptr)))))

