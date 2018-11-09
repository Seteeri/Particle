(defun set-projview ()
  (declare (:rw
	    (:r :width :height)
	    (:w :*projview*)))
  (setf *projview* (make-instance 'projview
				  :width width
				  :height height
				  :type-proj 'orthographic)))

(defun set-controller ()
  (declare (:rw
	    (:r nil)
	    (:w :*controller*))) ; anim stuff  
  (setf *controller* (init-controller))
  (register-keyboard-callbacks))

(defun set-metrics ()
  (declare (:rw
	    (:r nil) ; i/o
	    (:w :*metrics*)))  
  (setf *metrics* (init-metrics)))

(defun set-digraph ()
  (declare (:rw
	    (:r nil)
	    (:w :*digraph*)))  
  (setf *digraph* (digraph:make-digraph)))

(defun set-shm-projview ()
  (declare (:rw
	    (:r :*projview*)
	    (:w :*shm-projview*)))  
  (setf *shm-projview* (init-shm-projview)))

(defun set-shm-nodes ()
  (declare (:rw
	    (:r nil)
	    (:w :*shm-nodes*)))  
  (setf *shm-nodes* (init-shm-nodes)))

(defun set-shm-atomic-counter ()
  (declare (:rw
	    (:r nil)
	    (:w :*shm-atomic-counter*)))  
  (setf *shm-atomic-counter* (init-shm-atomic-counter)))

(defun set-shm-vertices ()
  (declare (:rw
	    (:r nil)
	    (:w :*shm-vertices*)))  
  (setf *shm-vertices* (init-shm-vertices)))

(defun set-shm-element ()
  (declare (:rw
	    (:r nil)
	    (:w :*shm-element*)))  
  (setf *shm-element* (init-shm-element)))

(defun set-shm-draw-indirect ()
  (declare (:rw
	    (:r nil)
	    (:w :*shm-draw-indirect*)))  
  (setf *shm-draw-indirect* (init-shm-draw-indirect)))

(defun set-shm-texture-glyphs ()
  (declare (:rw
	    (:r nil)
	    (:w :*shm-texture*)))  
  (setf *shm-texture-glyphs* (init-shm-texture-glyphs)))

(defun set-node-pointer ()
  (declare (:rw
	    (:r :*digraph* :*shm-nodes* :*metrics*)
	    (:w :*digraph* :*shm-nodes* :*node-pointer*)))
  (setf *node-pointer* (init-node-pointer-graph)))

;; purely for side effects
(defun init-conn-rpc-view ()
  (declare (:rw
	    (:r :*shm-projview*
		;; :*shm-nodes*
		:*shm-atomic-counter*
		:*shm-vertices*
		:*shm-element*
		:*shm-draw-indirect*
		:*shm-texture*
		:*node-pointer*
		:*controller*)
	    (:w :*buffer-sock-ptr* :*sock-view*))) ; place sock in write since read is assumed parallelizeable
  (init-view))
