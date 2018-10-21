(in-package #:protoform)

(declaim (optimize (speed 3) (space 0) (debug 0) (safety 0)))

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

(defun main ()

  ;; https://askubuntu.com/questions/125062/how-can-i-find-which-desktop-enviroment-i-am-using
  ;; $ sbcl my-command-line-arg
  ;; *posix-argv*
  ;;("sbcl" "my-command-line-arg")
  
  (let ((width (/ 2560 2)) ; 1280
        (height 1600) ; 1600
        (inst-max (expt 2 16)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Init/bootstrapper system ;)

    (when t
      (fork (lambda () (protoform.view:main-view width height
    						 inst-max
    						 nil))))
    
    (sleep 2)
    
    (when t
      (fork (lambda () (protoform.model:main-model width height
    						   inst-max
    						   nil))))

    ;; (swank-protocol:request-listener-eval connection "(+ 2 2)")
    ;; (format t "~a~%" (swank-protocol:read-message connection)) ; blocks
    ;; (format t "~a~%" (swank-protocol:read-all-messages connection))
    ;; (let ((conn (init-swank-conn "skynet" 10001)))
    ;;   (swank-protocol:request-listener-eval conn
    ;; 					  "t"))
    
    ;; https://www.emacswiki.org/emacs/StumpWM

    ;; (format t "~v@{~A~:*~}~%" 64 "-")
    
    ;; Explicitly exit after loading code
    ;; (format t "[main] Exiting...")
    (sb-ext:exit)))
