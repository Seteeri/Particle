(in-package #:protoform.model)

(defun memcpy-shm-to-cache (name &optional size)
  (with-slots (conn-swank) *model*
    (with-slots (ptr size) (gethash name (handles-shm *model*))
      ;; (fmt-model t "main-model" "(memcpy-shm-to-cache ~S ~S ~S)~%" name name size)
      (eval-sync
       conn-swank
       (format nil "(memcpy-shm-to-cache ~S ~S ~S)" name name size)))))
      
(defun memcpy-shm-to-cache* (names)
  (with-slots (conn-swank) *model*    
    (eval-sync
     conn-swank
     (with-output-to-string (stream)
       (format stream "(progn ")
       (dolist (name names)
	 (with-slots (ptr size)
	     (gethash name (handles-shm *model*))
	   (format stream "(memcpy-shm-to-cache ~S ~S ~S) " name name size)))
       (format stream ")")))))

(defun set-cache-dirty (name value)
  (with-slots (conn-swank) *model*
    (eval-sync
     conn-swank
     (format nil "(set-cache-dirty ~S ~S)" name value))))
