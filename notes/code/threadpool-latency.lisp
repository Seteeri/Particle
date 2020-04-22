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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-prototype ()
 
  ;; Controller - aka RPC thread
  ;;
  ;; * Use sockets for messaging
  ;; * Use Redis for data...
  ;;   * Could use Redis for publish/subscribe also
  (fork (lambda ()
	  ;; Setup/init server sockets and connect to worker clients
	  ;;
	  ;; - For sync, wait until frame time from view, loop over worker sockets to see if ready
	  ;; - Workers will send ready msg to controller on init and upon task completion	  
	  ;; - Frame time will trigger work for idle workers
	  ;; - Controller will skip busy workers
	  ;; - If anim count known, could batch anims and split across processes to minimize copying data
	  (loop
	     :do (progn
		   ;; Wait on frame time, then...
		   ;; Loop over workers, find idle, send frame time
		   ;; * What to do with output/results?
		   ;;   -> Workers write data to shm: NO - need to sync with view to prevent simultaneous read/write
		   ;;   -> Workers push data to view socket: YES - need to write view handling
		   ;;      * Model -> Workers -> View
		   ;;   -> Workers push data to redis, view pulls: YES - view could delay if workers pushing data too fast to redis
		   ;; ...
		   
		   ;; Workers will read/write/push/pull data from (shared memory or redis)
		   ;; Shared memory: read, deserialize, process, serialize, write
		   ;; However, shared memory requires synchornization, whereas redis is single threaded
		   ;; Redis will have more memcpy but speed may be the same due to shm requireing sync
		   ;; If we built a server around the shm, it would essentially be recreating redis
		   ;; We also need the node memory managed separately to alleviate pressure on the GC
		   
		   (sleep 0.0167)))))
  
  ;; Worker 1...4
  (fork (lambda ()
	  ;; Setup/init client socket to controller
	  ;; Send init ready msg
	  (loop
	     :do (progn
		   (sleep 1)))))
  
  ;; (let ((repeat-timer (make-timer (lambda ()
  ;; 				    (format t "HERE!!!!!!!!!!!!!!!!!!!!!!~%")))))
  ;;   (schedule-timer repeat-timer
  ;; 		    1))

  t)

;; Current model (limited concurrency):
;; - Wait for view
;; - Run tasks
;; - Serialize/Send

;; http://highscalability.com/blog/2013/5/13/the-secret-to-10-million-concurrent-connections-the-kernel-i.html

;; Invert the model:
;; - Use process pools
;;   - One pool for async and one for sync
;;     - sync will wait on frame time (block on socket)
;;       - even if num procs > CPU and all procs received frame time at once,
;;         it would not matter since execution limited to CPU count.
;;     - async will keep going
;;       - should not block on I/O...disk reads should be appropriately chunked or moved to a thread
;; - For reoccuring animations, reuse process instead of exiting loop
;;   - Each "task" process can receive codes:
;;     - time
;;     - end
;;     - renew
;;   - Or set timeout for process to exit if unused

;; Issue is model/data and view boundaries...cannot integrate them
;; Model/Ctrl <-> Redis/Data(shm) -> View
;; model push/pull to/from redis
;; view pulls from redis only
;; - The limiting factor should be the nodes since easier
;; to increase throughput by separating/delay communication between
;; the data and its representation - also maintains responsiveness
;; - Can use shm for now as long as operations/anims/tasks are not performed
;; at the same time on the same nodes otherwise undefined results, not necessarily
;; low-level corruption but more unexpected visual effects

;; If model and data in same process then need locks...
;; begs the question - which is more efficient? redis IPC vs lock contention
;; Also with threads, GC shared = GC delay proportional to number of threads
