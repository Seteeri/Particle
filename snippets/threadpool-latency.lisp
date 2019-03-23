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
