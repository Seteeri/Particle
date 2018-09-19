(in-package #:protoform)

(declaim (optimize (speed 3) (debug 0) (safety 0)))

;; (sb-ext:save-lisp-and-die "pf" :toplevel #'protoform:main :executable t)

(defmacro set-signal-handler (signo &body body)
  (let ((handler (gensym "HANDLER")))
    `(progn
       (defcallback ,handler :void ((signo :int))
                         (declare (ignore signo))
                         ,@body)
       (foreign-funcall "signal" :int ,signo :pointer (callback ,handler)))))

;; (defun random-from-range (start end)
;;   (+ start (random (+ 1 (- end start)))))

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

(defun main ()

  ;; https://askubuntu.com/questions/125062/how-can-i-find-which-desktop-enviroment-i-am-using
  ;; $ sbcl my-command-line-arg
  ;; *posix-argv*
  ;;("sbcl" "my-command-line-arg")

  ;; Slowdown occurs @ 400-500 sprites (instances) in C
  ;; 2**21 = 2097152 (poor)
  ;; 2**20 = 1048576 (poor)
  ;; 2**19 = 524288  (mediocre)
  ;; 2**18 = 262144  (good)
  ;; 2**17 = 131072  (excellent)
  ;; 2**16 = 65536   (60)
  ;; 2**15 = 32768   
  ;; 2**13 = 8192
  ;; 2**5  = 32
  
  (let ((width (/ 2560 2))
        (height 1600)
        (inst-max (expt 2 17))
        (path-server-view "/tmp/protoform-render.socket")
        (path-server-model "/tmp/protoform-model.socket"))

    (when t
      (fork (lambda () (protoform.model:main-model width height
						   inst-max
						   path-server-model))))

    (when t
      (fork (lambda () (protoform.view:main-view width height
						 inst-max
						 path-server-model))))
    
    (when t
      (fork (lambda () (protoform.controller:main-controller path-server-model))))
    
    (sb-ext:exit)))
