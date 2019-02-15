(in-package :protoform.model)

(defparameter *shm-nodes* nil)

(defparameter *params-nodes-shm* (list :shader-storage-buffer
				       "/protoform-nodes"
				       "/protoform-nodes"
				       (/ 134217728 4)
				       2 3
				       :triple
				       0))

(defun init-shm-nodes ()
  (init-shm '*shm-nodes*))
