(in-package :protoform.conn)

(defun start-swank-server-for-emacs (port)
  "Starts a Swank server thread, listening on PORT of the host's loopback
interface, to handle Emacs/Slime connection requests."
  (swank:create-server :port port :dont-close t))

(defun start-swank-server-for-swank-client (port)
  "Starts a Swank server thread, listening on PORT of the host's network
interface, to handle Swank Client connection requests."
  (let ((swank::*loopback-interface* (sb-unix:unix-gethostname)))
    (swank:create-server :port port :dont-close t)))

(defun swank-thread ()
  "Returns a thread that's acting as a Swank server."
  ;; (format t "~a~%" (sb-thread:list-all-threads))
  (dolist (thread (sb-thread:list-all-threads))
    ;; (sb-thread:thread-name thread)
    (return thread)))

(defun wait-for-swank-thread ()
  "Wait for the Swank server thread to exit."
  (let ((swank-thread (swank-thread)))
    (when swank-thread
      (sb-thread:join-thread swank-thread))))

(defun start-swank-server (port)
  (setf swank:*log-events* nil)
  (setf swank:*global-debugger* nil)
  (setf swank:*configure-emacs-indentation* nil)
  (setf swank:*use-dedicated-output-stream* nil)
  (setf swank:*communication-style* :fd-handler)
  (start-swank-server-for-swank-client port))
  ;; (wait-for-swank-thread)

(defun init-swank-conn (host port)
  (let ((conn (swank-protocol:make-connection host port)))
    (swank-protocol:connect conn)
    (swank-protocol:request-connection-info conn)
    (let ((msg-init (swank-protocol:read-message conn)))
      (when nil
	(format t "~a~%" msg-init)))
    conn))

  
  
