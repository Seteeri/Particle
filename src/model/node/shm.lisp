(in-package :protoform.model)

;; move out of shm
(defun send-node (node)
  (enqueue-node node
		(progn
		  (update-transform (model-matrix node))
		  (serialize-node node)))
  (send-memcpy-node node))  

(defun send-node-zero (node)
  (enqueue-node node
		*data-zero-node*)
  (send-memcpy-node node))

(defun enqueue-node (node data)
  (copy-data-to-shm *shm-nodes* 
		    data
		    (* (index node)
		       +size-struct-instance+)))

(defun send-memcpy-node (node)
  (let* ((start (* (index node)
		   +size-struct-instance+))
	 (end (+ start +size-struct-instance+)))
  (memcpy-shm-to-cache-flag*
   `(("nodes"    ,start ,end)))))

;; rename to serialize-node-to-shm
(defun copy-node-to-shm (node &optional (offset-ptr 0))
  ;; see serialize-node
  (with-slots (ptr size)
      *shm-nodes*    
    (with-slots (model-matrix
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

(defun copy-nodes-to-shm (graph)
  (digraph:mapc-vertices (lambda (node)
			   (copy-node-to-shm node
					     (* (index node)
						(/ +size-struct-instance+ 4))))
			 graph))

(defun zero-node-to-shm (&optional (offset-ptr 0))
  (with-slots (ptr size)
      *shm-nodes*
    (dotimes (i (/ +size-struct-instance+ 4))
      (setf (mem-aref ptr :int (+ offset-ptr i)) 0))))

(defun copy-data-to-shm (shm data offset-ptr)
  (declare (type (array (unsigned-byte 8)) data))
  ;; faster way to do this?
  (with-slots (ptr size)
      shm
    (loop
       :for c :across data
       :for i :upfrom 0
       :do (setf (mem-aref ptr
    			   :uchar
    			   (+ offset-ptr i))
    		 c)))
  (length data))
