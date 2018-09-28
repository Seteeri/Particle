(in-package :protoform.view)

(defclass mapping-base ()
  ((name :accessor name :initarg :name :initform nil)
   (boa :accessor boa :initarg :boa :initform nil)
   (mmap :accessor mmap :initarg :mmap :initform nil)))

(defun init-mapping-base (params-shm)

  ;; Correspond with:
  ;; model: init-mapping-base
  ;; view:  init-buffers-compute name and bind-layout
  ;; view:  update-compute-bindings

  (init-mapping-buffer :uniform-buffer
		       "projview"
		       :float
		       (+ 16 16 16)
		       1
		       0) ; same for compute/raster

  ;; use compute shader input binding
  (init-mapping-buffer :shader-storage-buffer
		       "instance"
		       :float   ; 4
		       1        ; ignore for now since going for max size, (/ 208 4)
		       (/ 134217728 4) ; inst-max
		       1)

  ;; compute shader doesn't modify this
  (init-mapping-buffer :texture-buffer
		       "texture"
    		       :unsigned-byte
    		       4
    		       (/ 134217728 4) ; get size from model			 
		       0)) ;  not used - same as init-buffers-raster

(defun init-mapping-buffer (target
			    name
			    type
			    size
			    count
			    binding-layout)

  ;; Move this buffer-object into rotational buffers for consistency?

  ;; TODO: Split this
  
  (with-slots (program-compute
	       mapping-base)
      *view*
    (let* ((buffer (init-buffer-object program-compute
				       target
				       name
				       type
				       size
				       count
				       binding-layout
				       t
				       :buffering 'single))
	   (size (size-buffer buffer))
	   (mmap (init-mmap (format nil "/protoform-~a.shm" name)
			    size
			    nil
			    :data (make-array size
					      :element-type '(unsigned-byte 8)
					      :initial-element (coerce 0 '(unsigned-byte 8)))))
	   (inst (make-instance 'mapping-base
				:name name
				:boa buffer
				:mmap mmap)))
      ;; (format t "[init-mapping-buffer] Set hash for ~S~%" name)
      (setf (gethash name mapping-base) inst))))

(defun clean-up-mapping-base (msdf)
  (loop 
     :for key :being :the :hash-keys :of (mapping-base msdf)
     :using (hash-value value)
     :do (progn
	   (let* ((buffer (boa value)))
	     (clean-up-buffer-object buffer)
	     (format t "[clean-up-mapping-base] Deleted ~a: ~a~%" key buffer))
	   (let* ((mmap (mmap value)))
	     (cleanup-mmap mmap t)
	     (format t "[clean-up-mapping-base] Deleted ~a~%" mmap)))))
