(in-package :protoform.model)

;; See notes in dispatch

(defun print-graph-cb (seq-key)
  (fmt-model t "print-graph" "~a~%" seq-key)
  (sb-concurrency:enqueue (list nil
				'backspace-node
				'()
				(lambda ()
				  (digraph:mapc-vertices
				   (lambda (node)
				     (format t "~S = ~S~%" node (data node)))
				   *digraph*)
				  (funcall #'draw-graph)))
			  *queue-anim*))

(defun add-node-cb (seq-key)
  (fmt-model t "add-node" "~a~%" seq-key)
  (sb-concurrency:enqueue (list nil
				'add-node ;; (make-symbol (format nil "~a~%" seq-key))
				'()
				(lambda ()
				  (funcall #'add-node (second (reverse (second seq-key)))))) ; create aux fn for this
			  *queue-anim*))

(defun backspace-node-cb (seq-key)
  (fmt-model t "backspace-node" "~a~%" seq-key)
  (sb-concurrency:enqueue (list nil
				'backspace-node
				'()
				(lambda ()
				  (funcall #'backspace-node)))
			  *queue-anim*))

(defun insert-node-tab-cb (seq-key)
  (fmt-model t "insert-node-tab" "~a~%" seq-key)
  (sb-concurrency:enqueue (list nil
				'insert-node-tab
				'()
  				(lambda ()
  				  (funcall #'insert-node-tab)))
			  *queue-anim*))

(defun insert-node-newline-cb (seq-key)
  (fmt-model t "insert-node-newline" "~a~%" seq-key)
  (sb-concurrency:enqueue (list nil
				'insert-node-newline
				'()
  				(lambda ()
  				  (funcall #'insert-node-newline)))
			  *queue-anim*))
  
(defun eval-node-cb (seq-key)
  (fmt-model t "eval-node" "~a~%" seq-key)
  (sb-concurrency:enqueue (list nil
				'eval-node
				'()
				(lambda ()
				  (funcall #'eval-node)))
			  *queue-anim*))

(defun show-node-ids-cb (seq-key)
  (fmt-model t "show-node-ids" "~a~%" seq-key)
  (sb-concurrency:enqueue (list nil
				'show-node-ids
				'()
				(lambda ()
				  (funcall #'show-node-ids)))
			  *queue-anim*))

(defun cut-node-cb (seq-key)
  (fmt-model t "cut-node" "~a~%" seq-key)
  (sb-concurrency:enqueue (list nil
				'cut-node
				'()
				(lambda ()
				  (funcall #'cut-node)))
			  *queue-anim*))

(defun copy-node-cb (seq-key)
  (fmt-model t "copy-node" "~a~%" seq-key)
  (sb-concurrency:enqueue (list nil
				'copy-node
				'()
				(lambda ()
				  (funcall #'copy-node)))
			  *queue-anim*))

(defun paste-node-cb (seq-key)
  (fmt-model t "paste-node" "~a~%" seq-key)
  (sb-concurrency:enqueue (list nil
				'paste-node
				'()
				(lambda ()
				  (funcall #'paste-node)))
			  *queue-anim*))

(defun translate-node-ptr-left-cb (seq-event)
  (with-slots (model-matrix)
      *node-pointer*
    (translate-node-ptr seq-event
		       (lambda (value-new) ; update fn
			 (setf (vx3 (translation model-matrix)) value-new)
			 (enqueue-node-pointer))
		       (vx3 (translation model-matrix)) ; start
		       (- (* 96 *scale-node*))          ; delta
		       'move-pointer-x)))

(defun translate-node-ptr-right-cb (seq-event)
  (with-slots (model-matrix)
      *node-pointer*
    (translate-node-ptr seq-event
		       (lambda (value-new)
			 (setf (vx3 (translation model-matrix)) value-new)
			 (enqueue-node-pointer))
		       (vx3 (translation model-matrix))
		       (* 96 *scale-node*)
		       'move-pointer-x)))

(defun translate-node-ptr-up-cb (seq-event)
  (with-slots (model-matrix)
      *node-pointer*
    (translate-node-ptr seq-event
		       (lambda (value-new)
			 (setf (vy3 (translation model-matrix)) value-new)
			 (enqueue-node-pointer))
		       (vy3 (translation model-matrix))
		       (* +linegap+ *scale-node*)
		       'move-pointer-y)))

(defun translate-node-ptr-down-cb (seq-event)
  (with-slots (model-matrix)
      *node-pointer*
    (translate-node-ptr seq-event
		       (lambda (value-new)
			 (setf (vy3 (translation model-matrix)) value-new)
			 (enqueue-node-pointer))
		       (vy3 (translation model-matrix))
		       (- (* +linegap+ *scale-node*))
		       'move-pointer-y)))

(defun move-node-ptr-in-cb (seq-event)
  (fmt-model t "move-node-ptr-in" "~a~%" seq-event)
  (sb-concurrency:enqueue (list nil
				'move-node-ptr-in
				'()
				(lambda ()
				  (funcall #'move-node-ptr :in)))
			  *queue-anim*))

(defun move-node-ptr-out-cb (seq-event)
  (fmt-model t "move-node-ptr-out" "~a~%" seq-event)
  (sb-concurrency:enqueue (list nil
				'move-node-ptr-out
				'()
				(lambda ()
				  (funcall #'move-node-ptr :out)))
			  *queue-anim*))

(defun scale-ortho-down-cb (seq-event)
  ;; zoom in
  (scale-ortho seq-event
	       (lambda (value-new)
		 (setf (scale-ortho *projview*) value-new)
		 (enqueue-mat-proj))
	       (- (vz3 (displace *projview*)))
	       'scale-ortho))

(defun scale-ortho-up-cb (seq-event)
  ;; zoom out
  (scale-ortho seq-event
	       (lambda (value-new)
		 (setf (scale-ortho *projview*) value-new)
		 (enqueue-mat-proj))
	       (vz3 (displace *projview*))
	       'scale-ortho))

(defun translate-camera-left-cb (seq-event)
  (with-slots (pos
	       displace)
      *projview*
    (translate-camera seq-event
		      (lambda (value-new)
			(setf (vx3 pos) value-new)
			(enqueue-mat-view))
		      (vx3 pos)
		      (- (vx3 displace))
		      'translate-camera-x)))

(defun translate-camera-right-cb (seq-event)
  (with-slots (pos
	       displace)
      *projview*
    (translate-camera seq-event
		      (lambda (value-new)
			(setf (vx3 pos) value-new)
			(enqueue-mat-view))
		      (vx3 pos)
		      (vx3 displace)
		      'translate-camera-x)))

(defun translate-camera-up-cb (seq-event)
  (with-slots (pos
	       displace)
      *projview*
    (translate-camera seq-event
		      (lambda (value-new)
			(setf (vy3 pos) value-new)
			(enqueue-mat-view))
		      (vy3 pos)
		      (vy3 displace)
		      'translate-camera-y)))

(defun translate-camera-down-cb (seq-event)
  (with-slots (pos
	       displace)
      *projview*
    (translate-camera seq-event
		      (lambda (value-new)
			(setf (vy3 pos) value-new)
			(enqueue-mat-view))
		      (vy3 pos)
		      (- (vy3 displace))
		      'translate-camera-y)))
