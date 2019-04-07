(in-package :protoform.render)

;; 144 hz
(defconstant +server-sleep+ (/ 1 144))

(defun run-server-sleep ()
  (sleep +server-sleep+)
  (run-server))

(defun run-server (&optional (flags 0))
  (with-slots (sock-server sock-client)
      *render*
    (if sock-client
	(progn
	  (set-serving t)
	  (serve-socket flags))
	(multiple-value-bind (sock-accept errno)
	    (accept4 sock-server :nonblock) ;non block
	  (when (/= sock-accept -1)
	    (fmt-render "main-view" "Accepted connection: ~a~%" sock-accept)
	    (setf sock-client sock-accept))))))

(defun serve-socket (&optional (flags 0))
  (with-slots (sock-client
	       buffer-sock-ptr)
      *render*
    (loop
       :while *serving*
       :do (let ((message (recv-message sock-client
					buffer-sock-ptr
					flags)))
	     (when message
	       ;; (fmt-render "serve-client" "Message: ~S~%" message)
	       ;; (print (eval message))
	       ;; (force-output)
	       
	       ;; To do multiple check if first is a list
	       (if (listp (first message))
		   (dolist (n message)
		     (apply (symbol-function (find-symbol (string (first n)) :protoform.render))
			    (cdr n)))
		   (apply (symbol-function (find-symbol (string (first message)) :protoform.render))
			  (cdr message))))))))

(defun pass ())
