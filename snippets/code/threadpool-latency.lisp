;; (defun add-integer (a b)
;;   (declare (integer a b))
;;   (the integer (+ a b)))

(defun poll-queue ()
  (loop
     :for msg := (sb-concurrency:receive-message *queue*)
     :do (let* ((time-task      (first msg))
		(time-delta     (- (osicat:get-monotonic-time) time-task))
		(time-delta-mil (* time-delta 1000))
		(time-delta-mic (truncate (* time-delta 1000000))))
	   (declare (double-float time-delta time-delta-mil)
		    (fixnum time-delta-mic))
	   (format t "~3$ ms == ~s us ~%~%" time-delta-mil time-delta-mic))))

(defparameter *queue* (sb-concurrency:make-mailbox))

(defun run-test ()
  
  ;; Create servants
  (dotimes (i 4)
    (bordeaux-threads:make-thread #'poll-queue))
  (sleep 1)

  ;; 1. Callback
  ;;    1. Loop across string, create node for each character
  ;; 2. create-node-for-char
  ;;    1. send-message: pop-from-pool -> fn send-message: pop-from-pool return
  ;;       ...what if all threads waiting for pop-from-pool, then deadlock
  ;;       ...thread pool must traverse dependency graph
  ;;       ...need language
  ;;       ...with dependency graph, dags are passing information between each other
  ;;       ...has execution order
  ;;       ...find way to reuse ptree
  ;;    2. recv-message: node
  ;;    2. recv msg from input
  ;;    3. send msg with node
  ;;    4. modify-node
  ;;    5. serialize-node
  ;; ...
  ;; 5. Memcpy
  ;; 6. Send memcpy
  
  ;; Create owners - nodes
  (loop
     :for x :across "this is a test"
     :do (progn
	   (sb-concurrency:send-message *queue*
					(list (get-monotonic-time)
					      'create-node-for-char))
	   (sleep 0.0167)))
  
  ;; (bordeaux-threads:join-thread thread-1)
  
  (sleep 10))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

(defparameter *mb-in* (sb-concurrency:make-mailbox))
(defparameter *mb-out* (sb-concurrency:make-mailbox))
(defparameter *mb-1* (sb-concurrency:make-mailbox))
(defparameter *mb-2* (sb-concurrency:make-mailbox))
(defparameter *mb-3* (sb-concurrency:make-mailbox))
(defparameter *mb-4* (sb-concurrency:make-mailbox))

(defun fn-worker ()
  (loop
     :for msg := (sb-concurrency:receive-message *mb-work*)
     :do (progn
	   (sb-concurrency:send-message *mb-1* msg)
	   (sb-concurrency:send-message *mb-2* msg)
	   (sb-concurrency:send-message *mb-3* msg)
	   (sb-concurrency:send-message *mb-4* msg)
	   (sb-concurrency:send-message *mb-output* msg))))

(defun run-test ()
  
  ;; Create servants
  (bordeaux-threads:make-thread
   (lambda ()
     (loop
	:for msg := (sb-concurrency:receive-message *mb-in*)
	:do (when (not (eq msg 'sync))
	      (sb-concurrency:send-message *mb-out* msg)))))

  (bordeaux-threads:make-thread
   (lambda ()
     (loop
	:for msg := (sb-concurrency:receive-message *mb-in*)
	:do (when (not (eq msg 'sync))
	      (sb-concurrency:send-message *mb-out* msg)))))

  (bordeaux-threads:make-thread
   (lambda ()
     (loop
	:for msg := (sb-concurrency:receive-message *mb-in*)
	:do (when (not (eq msg 'sync))
	      (sb-concurrency:send-message *mb-out* msg)))))

  (bordeaux-threads:make-thread
   (lambda ()
     (loop
	:for msg := (sb-concurrency:receive-message *mb-in*)
	:do (when (not (eq msg 'sync))
	      (sb-concurrency:send-message *mb-out* msg)))))

  ;; main: send msg
  ;; worker: recv msg - proccess - send msg to output
  ;; main: send sync all - share data
  ;; worker: copy data - send msg out
  ;; main: recv msg
  ;; = 4 round trips @ 25*2 microseconds = 200 microsecs = 0.2 ms
  
  (let ((t-start (osicat:get-monotonic-time)))

    (dotimes (i 4)
      (sb-concurrency:send-message *mb-in* (osicat:get-monotonic-time)))
    (format t "sent in...~%")

    (dotimes (i 4)
      (format t "recv out: ~a~%" (sb-concurrency:receive-message *mb-out*)))

    ;; each thread must use select on two FD's
    ;; 1. work
    ;; 2. personal
    (sb-concurrency:send-message *mb-1* (osicat:get-monotonic-time))
    (sb-concurrency:send-message *mb-2* (osicat:get-monotonic-time))
    (sb-concurrency:send-message *mb-3* (osicat:get-monotonic-time))
    (sb-concurrency:send-message *mb-4* (osicat:get-monotonic-time))

    (format t "sent sync...~%")
    
    (format t "recv  out: ~a~%" (sb-concurrency:receive-message *mb-out*))
    
    (format t "time: ~s~%" (- (osicat:get-monotonic-time) t-start))))
|#
