(in-package :protoform.render)

(defclass handle-shm ()
  ((name :accessor name :initarg :name :initform nil)
   (mmap :accessor mmap :initarg :mmap :initform nil)))

(defun init-handles-shm (params-model)
  (with-slots (handles-shm)
      *render*
    (dolist (params params-model)
      (destructuring-bind (target
			   path
			   size
			   bind-cs
			   bind-vs
			   count-buffer
			   flag-copy
			   &rest rest)
	  params
	(setf (gethash path handles-shm)
	      (make-instance 'handle-shm
			     :name path
			     :mmap (init-mmap path
					      size
					      nil)))
	(fmt-render "init-handles-shm" "Created handle-shm for ~S~%" path)))))

(defun clean-up-handles-shm ()
  (loop 
     :for name :being :the :hash-keys :of (handles-shm *render*)
     :using (hash-value handle)
     :do (clean-up-mmap (mmap handle))))
