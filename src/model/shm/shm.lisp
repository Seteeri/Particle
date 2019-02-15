(in-package :protoform.model)

;; (defun clean-up-handles-shm ()
;;   (loop 
;;      :for key :being :the :hash-keys :of *handles-shm*
;;      :using (hash-value mmap)
;;      :do (cleanup-mmap mmap)))

;; rename mmap -> shm
(defun init-shm (name)
  (destructuring-bind (target
		       path
		       size
		       bind-cs
		       bind-vs
		       count-buffer
		       flag-copy
		       &rest rest)
      (getf *params-shm* name)
    (init-mmap path
	       size
	       t)))
