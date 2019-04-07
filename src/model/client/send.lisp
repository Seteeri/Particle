(in-package #:protoform.model)

(defun send-init-view-buffers ()
  (send-message *sock-view*
		*buffer-sock-ptr*
		(with-output-to-string (stream)
		  (format stream "(setup-render (")
		  (loop
		     :for (name params) :on *params-shm* :by #'cddr
		     :do (format stream "~S " params))
		  (format stream "))"))))

(defun send-serving (value)
  (send-message *sock-view*
		*buffer-sock-ptr*
		(with-output-to-string (stream)
		  (format stream "(set-serving ~s)" value))))

(defun send-draw (value)
  (send-message *sock-view*
		*buffer-sock-ptr*
		(format nil "(set-draw ~s)" value)))
