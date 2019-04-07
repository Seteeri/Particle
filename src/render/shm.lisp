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
	(fmt-render t "init-handles-shm" "Set hash for ~S~%" path)))))

(defun clean-up-handles-shm ()
  (loop 
     :for key :being :the :hash-keys :of (handles-shm *render*)
     :using (hash-value value)
     :do (progn
	   (let* ((buffer (boa value)))
	     (clean-up-buffer-object buffer)
	     (fmt-render t "clean-up-handles-shm" "Deleted ~a: ~a~%" key buffer))
	   (let* ((mmap (mmap value)))
	     (cleanup-mmap mmap t)
	     (fmt-render t "clean-up-handles-shm" "Deleted ~a~%" mmap)))))
