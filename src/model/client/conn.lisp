(in-package #:protoform.model)

(defparameter *path-socket-view* "/tmp/protoform-view.socket")

(defun serve-socket ()
  (loop
     (let ((message (recv-message *sock-view*
				  *buffer-sock-ptr*)))
       (when message
	 ;; (fmt-model t "serve-client" "Message: ~S~%" message)
	 ;; (print (eval message))
	 ;; (force-output)

	 ;; Add option for self-evaluating symbol to avoid fn call
	 (if (listp (first message))
	     (dolist (n message)
	       (apply (symbol-function (find-symbol (string (first n)) :protoform.model))
		      (cdr n)))
	     (apply (symbol-function (find-symbol (string (first message)) :protoform.model))
		    (cdr message)))))))

(defun init-conn-rpc-view (&rest deps)
  (declare (ignore deps))

  (setf *buffer-sock-ptr* (foreign-alloc :unsigned-char :count 212992)
	*buffer-sock-array* (make-array 212992
					:adjustable nil
					:fill-pointer nil
					:element-type '(unsigned-byte 8))
	*sock-view* (init-sock-client *path-socket-view* :block))

  ;; (format t "~a~%" (with-output-to-string (stream)
  ;; 		     (format stream "(init-view-buffers (")
  ;; 		     (loop
  ;; 			:for (name params) :on *params-shm* :by #'cddr
  ;; 			:do (format stream "~S " params))
  ;; 		     (format stream "))")))
  
  (send-init-view-buffers)

  (loop
     :for (sym params) :on *params-shm* :by #'cddr
     :do (send-memcpy-shm-to-cache (second params)
				   (symbol-value sym)))
  
  (send-draw t)

  (send-serving nil)

  t)