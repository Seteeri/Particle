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

  ;; (list "projview"       "/protoform-projview"      (align-size (* (+ 16 16 16) 4 1)))
  ;; (list "instance"       "/protoform-instance"      134217728)
  ;; (list "texture"        "/protoform-texture"       134217728)
  ;; (list "element"        "/protoform-element"       (* 4 6)) ; 4 bytes/int * 6 ints or indices
  ;; (list "draw-indirect"  "/protoform-draw-indirect" (* 4 6)) ; 6 ints
  
  (dolist (params params-shm)
    (fmt-view t "init-mapping-base" "~a~%" params)
    (destructuring-bind (target name path size bind) params
      (init-mapping-buffer target
  			   name
  			   path
  			   size
			   bind))))
  
  ;; 		       "projview":0
  ;; 		       "instance":1
  ;; 		       "texture":-1

(defun init-mapping-buffer (target
			    name
			    path
			    size
			    binding-layout)

  ;; TODO:
  ;; * Move this buffer-object into rotational buffers for consistency?
  ;; * Split mmap and gl creation
  
  (with-slots (program-compute
	       mapping-base)
      *view*
    (let* ((buffer (init-buffer-object program-compute
				       target
				       name
				       size
				       binding-layout
				       t
				       :buffering 'single))
	   (size (size-buffer buffer))
	   (mmap (init-mmap path
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
