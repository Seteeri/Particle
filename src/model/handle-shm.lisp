(in-package :protoform.model)

(defun init-handle-shm (handles-shm
			params-shm)
  (dolist (params params-shm)
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
      (let ((mmap (init-mmap path
			     size
			     t ; create - replace these types with symbols
			     :data (make-array size
					       :element-type '(unsigned-byte 8)
					       :initial-element (coerce 0 '(unsigned-byte 8))))))
	(setf (gethash name handles-shm) mmap)
	(fmt-model t "init-handle-shm" "~S, ~S bytes~%" path size)))))

(defun clean-up-handles-shm ()
  (loop 
     :for key :being :the :hash-keys :of (handles-shm *model*)
     :using (hash-value mmap)
     :do (cleanup-mmap mmap)))
