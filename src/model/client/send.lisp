(in-package #:protoform.model)

(defun send-setup-render ()
  (send-message *sock-render*
		*buffer-sock-ptr*
		(with-output-to-string (stream)
		  (format stream "(setup-render (")
		  (loop
		     :for (name params) :on *params-shm* :by #'cddr
		     :do (format stream "~S " params))
		  (format stream "))"))))

(defun send-serving (value)
  (send-message *sock-render*
		*buffer-sock-ptr*
		(with-output-to-string (stream)
		  (format stream "(set-serving ~s)" value))))

(defun send-draw (value)
  (send-message *sock-render*
		*buffer-sock-ptr*
		(format nil "(set-draw ~s)" value)))

(defun send-clean-up-render ()
  (send-message *sock-render*
		*buffer-sock-ptr*
		"(clean-up-render)"))
