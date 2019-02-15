(in-package #:protoform.model)

(defparameter *path-socket-view* "/tmp/protoform-view.socket")

(defun init-conn-rpc-view (&rest deps)
  (declare (ignore deps))

  (setf *buffer-sock-ptr* (foreign-alloc :unsigned-char :count 212992)
	*buffer-sock-array* (make-array 212992
					:adjustable nil
					:fill-pointer nil
					:element-type '(unsigned-byte 8)))
  
  (setf *sock-view* (init-sock-client *path-socket-view* :block))

  ;; Combine all of below into single call
  
  (send-init-view-buffers)

  ;; See get-sym-shm-from-string
  (loop
     :for (name params) :on *params-shm* :by #'cddr
     :for name2 := (string-downcase (symbol-name name))
     :do (memcpy-shm-to-cache name2
			      (symbol-value (find-symbol (str:concat "shm-" name2) :protoform.model))))
  
  (send-draw t)

  (send-serving nil)

  t)

(defun serve-client ()
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
