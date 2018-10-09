(in-package :protoform.view)

(defclass handle-shm ()
  ((name :accessor name :initarg :name :initform nil)
   (mmap :accessor mmap :initarg :mmap :initform nil)))

(defun init-handles-shm (params-model)
  (dolist (params params-model)
      (destructuring-bind (target
			   name
			   path
			   size
			   bind-cs
			   bind-vs
			   count-buffer
			   flag-copy
			   &rest rest)
	  params
      (init-handle-shm name
  		       path
  		       size))))

(defun init-handle-shm (name
			path
			size)
  (with-slots (handles-shm) *view*
    (let* ((mmap (init-mmap path
			    size
			    nil
			    :data (make-array size
					      :element-type '(unsigned-byte 8)
					      :initial-element (coerce 0 '(unsigned-byte 8)))))
	   (inst (make-instance 'handle-shm
				:name name
				:mmap mmap)))
      ;; (format t "[init-mapping-buffer] Set hash for ~S~%" name)
      (setf (gethash name handles-shm) inst))))

(defun clean-up-handles-shm ()
  (loop 
     :for key :being :the :hash-keys :of (handles-shm *view*)
     :using (hash-value value)
     :do (progn
	   (let* ((buffer (boa value)))
	     (clean-up-buffer-object buffer)
	     (fmt-view t "clean-up-handles-shm" "Deleted ~a: ~a~%" key buffer))
	   (let* ((mmap (mmap value)))
	     (cleanup-mmap mmap t)
	     (fmt-view t "clean-up-handles-shm" "Deleted ~a~%" mmap)))))
