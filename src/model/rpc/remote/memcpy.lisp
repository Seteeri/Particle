(in-package #:protoform.model)

(defparameter *path-socket-view* "/tmp/protoform-view.socket")

;; TODO: Use macro

(defun send-memcpy-shm-to-cache (name
				 shm
				 &optional
				   (offset 0)
				   (size-cpy nil))
  (with-slots (ptr size)
      shm
    ;; (fmt-model t "main-model" "(memcpy-shm-to-cache ~S ~S ~S)~%" name name size)
    (send-message *sock-view*
    		  *buffer-sock-ptr*
		  (format nil "(memcpy-shm-to-cache ~S ~S ~S ~S)" name name offset size-cpy))))

(defun send-set-cache-dirty (name value)
  (send-message *sock-view*
    		*buffer-sock-ptr*
		(format nil "(set-cache-flag-copy ~S ~S)" name value)))

(defun send-memcpy-shm-to-cache-flag* (caches)
  (send-message *sock-view*
    		*buffer-sock-ptr*
		(with-output-to-string (stream)
		  (format stream "(")
		  (dolist (cache caches)
		    (destructuring-bind (shm
					 name-cache
					 offset-cache
					 size-cache)
			cache
		      (with-slots (ptr size)
			  shm
			;; Pass offsets
			(format stream "(memcpy-shm-to-cache ~S ~S ~S ~S) "
				name-cache
				name-cache
				offset-cache
				size-cache)
			(format stream "(set-cache-flag-copy ~S 3) " name-cache))))
		  (format stream ")"))))
