(in-package :protoform.model)

(defun enqueue-node (node &optional (pointer t))
  (when pointer
    (enqueue-node-pointer))
  (sb-concurrency:enqueue (list *channel*
				*shm-nodes*
				(lambda ()
				  (update-transform (model-matrix node))
				  (serialize-node node))
				(* (index node)
				   +size-struct-instance+))
			  *queue-shm*))

(defun enqueue-node-pointer ()
  (sb-concurrency:enqueue (list *channel*
				*shm-nodes*
				(lambda ()
				  (update-transform (model-matrix *node-pointer*))
				  (serialize-node *node-pointer*))
				(* (index *node-pointer*)
				   +size-struct-instance+))
			  *queue-shm*))  

(defun enqueue-node-zero (index)
  (sb-concurrency:enqueue (list *channel*
				*shm-nodes*
				(lambda ()
				  *data-zero-node*)
				(* index
				   +size-struct-instance+))
			  *queue-shm*))

(defun copy-node-to-shm (node &optional (offset-ptr 0))
  
  (with-slots (ptr size)
      *shm-nodes*
    
    (with-slots (data
		 model-matrix
		 rgba
		 offset-texel-texture
		 dims-texture
		 uv
		 flags)
	node

      (let ((marr (marr (matrix model-matrix))))
	(setf (mem-aref ptr :float offset-ptr)        (aref marr 0)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 1)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 2)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 3)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 4)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 5)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 6)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 7)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 8)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 9)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 10)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 11)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 12)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 13)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 14)
	      (mem-aref ptr :float (incf offset-ptr)) (aref marr 15)))

      (setf (mem-aref ptr :float (incf offset-ptr)) (aref rgba 0)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 1)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 2)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 3)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 4)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 5)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 6)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 7)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 8)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 9)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 10)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 11)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 12)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 13)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 14)
	    (mem-aref ptr :float (incf offset-ptr)) (aref rgba 15)
	    
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 0)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 1)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 2)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 3)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 4)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 5)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 6)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 7)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 8)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 9)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 10)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 11)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 12)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 13)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 14)
	    (mem-aref ptr :float (incf offset-ptr)) (aref uv 15)
      
	    ;; http://www.lispworks.com/documentation/lcl50/aug/aug-90.html#HEADING90-0
            (mem-aref ptr :int (incf offset-ptr)) offset-texel-texture  ; tex offset
	    (mem-aref ptr :int (incf offset-ptr)) (aref dims-texture 0) ; tex dim x
	    (mem-aref ptr :int (incf offset-ptr)) (aref dims-texture 1) ; tex dim y
	    (mem-aref ptr :int (incf offset-ptr)) flags))))

;; TODO: Refactor to pass offsets, range, etc.
(defun copy-nodes-to-shm ()
  (digraph:mapc-vertices (lambda (node)
			   (copy-node-to-shm node
					     (* (index node)
						(/ +size-struct-instance+ 4))))
			 *digraph*))

(defun zero-node-to-shm (&optional (offset-ptr 0))
  (with-slots (ptr size)
      *shm-nodes*
    (dotimes (i (/ +size-struct-instance+ 4))
      (setf (mem-aref ptr :int (+ offset-ptr i)) 0))))
