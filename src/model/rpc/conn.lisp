(in-package #:protoform.model)

(defparameter *path-socket-view* "/tmp/protoform-view.socket")

;; (defun init-sym-to-shm ()
;;   (setf *sym-to-shm* (make-hash-table :size (length *params-shm*)))
;;   (loop
;;      :for (sym params) :on *params-shm* :by #'cddr
;;      :do (setf (gethash sym *sym-to-shm*)
;; 	       (symbol-value sym))))

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
