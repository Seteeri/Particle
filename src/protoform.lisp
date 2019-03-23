(in-package #:protoform)

;; (declaim (optimize (speed 3) (space 0) (debug 0) (safety 0)))

;; (sb-ext:save-lisp-and-die "pf" :toplevel #'protoform:main :executable t)

(defun fork (fn-child)

  ;; https://stackoverflow.com/questions/26405391/linux-difference-between-forking-twice-and-daemonise
  ;; uiop:run-program
  ;; (sb-ext:run-program "/usr/bin/sbcl" (list "--help") :output *standard-output*)
  ;; double-fork to launch independent process  

  (let ((pid (sb-posix:fork)))
    
    (cond
      
      ((plusp pid)
       ;; parent A
       ;; only need wait if we want to spawn child processes (through grandchildren)
       ;; and must wait for child to finish
       ;; (sb-posix:waitpid pid 0)

       ;; otherwise just return
       t)

      ((zerop pid)
       ;; child process B
       (let ((pid (sb-posix:fork)))
         (cond
           ((plusp pid)
            ;; child process B
            ;; exit to orphan child
            (sb-ext:exit))
           ((zerop pid)
            ;; child process C
            ;; execvp - run a program
            (funcall fn-child))
           (t
            (error "fork -1")))))
      
      (t
       (error "fork -1")))))

(defun main (width
	     height
	     inst-max)

  ;; https://askubuntu.com/questions/125062/how-can-i-find-which-desktop-enviroment-i-am-using  
  ;; https://www.emacswiki.org/emacs/StumpWM
  ;; (format t "~v@{~A~:*~}~%" 64 "-")
  
  (format t "arguments: ~a~%" sb-ext:*posix-argv*)

  ;; (run-test)
  ;; (sb-ext:exit)
  
  (when t
    (fork (lambda () (protoform.view:run-view width height
    					      inst-max
    					      nil))))
  
  (when t
    (fork (lambda () (protoform.model:init-model width height
    						 inst-max
    						 nil))))
  
  (sb-ext:exit))

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
