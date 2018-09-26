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

  (let ((width (/ 2560 2)) ; 1280
        (height 1600) ; 1600
        (inst-max (expt 2 17))
        (path-server-view "/tmp/protoform-view.socket")
        (path-server-model "/tmp/protoform-model.socket"))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Init/bootstrapper system ;)

    ;; Init: load
    
    ;; Model: swank server/client
    ;; View: swank server/client

    ;; Launch swank servers
    (fork (lambda () (protoform.view:main-view width height
    					       inst-max
    					       path-server-model)))
    (sleep 1)
    (fork (lambda () (protoform.model:main-model width height
    						 inst-max
    						 path-server-model)))
    ;; (when nil (fork (lambda () (protoform.controller:main-controller path-server-model))))

    ;; (swank-protocol:request-listener-eval connection "(+ 2 2)")
    ;; (format t "~a~%" (swank-protocol:read-message connection)) ; blocks
    ;; (format t "~a~%" (swank-protocol:read-all-messages connection))
    ;; (let ((conn (init-swank-conn "skynet" 10001)))
    ;;   (swank-protocol:request-listener-eval conn
    ;; 					  "t"))
    
    ;; https://www.emacswiki.org/emacs/StumpWM

    ;; (format t "~v@{~A~:*~}~%" 64 "-")
    
    ;; Explicitly exit after loading code
    (sb-ext:exit)))
