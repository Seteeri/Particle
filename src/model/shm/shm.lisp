(in-package :protoform.model)

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
