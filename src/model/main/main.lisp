(in-package :protoform.model)

(defun run-model-ptree ()
  ;; Build frames - aka ptrees
  ;; 1. Pull tasks until empty
  ;;    * Set cap or timer...
  ;; 2. Add to ptree
  ;; 3. Execute ptree
  (loop
     ;; Block on first message
     ;; Afterwards, poll messages until empty
     :for msg-first := (sb-concurrency:receive-message *mb-model*)
     :do (let ((ptree (make-ptree)))
	   ;; Process msg-first
	   (loop
	      :do (multiple-value-bind (msg flag-recv)
		      (sb-concurrency:receive-message-no-hang *mb-model*)
		    (unless flag-recv (return))
		    (format t "Processed msg: ~s~%" msg)))
	   ;; Exec ptree
	   ;; (call-ptree 'finish tree)
	   (format t "~6$ | Frame done~%" (osicat:get-monotonic-time)))))

(defun run-model ()
  (execute-mb-tasks *mb-model*))

(defun init-model (width height
		   inst-max
		   addr-swank-view)

  ;; Should start recording actions for undo
  
  (sb-ext:gc :full t)
  
  (setf *kernel*        (make-kernel 4)
	*channel*       (make-channel)
	*channel-input* (make-channel) ; rename to -controller
	
	;; Simply set here since no fn required
	*width*         width
	*height*        height
	*inst-max*      inst-max)

  ;; Run init fns
  (run-ptree-init)
    
  (sb-ext:gc :full t)

  (setf (sb-ext:bytes-consed-between-gcs) (* 2 1024))
  
  ;; (push #'(lambda ()
  ;; 	    (let ((time-gc (coerce (/ sb-ext:*gc-run-time* internal-time-units-per-second)
  ;; 				   'single-float)))
  ;; 	      (format t
  ;; 		      "[model] Time GC: ~4$ ms, ~4$ s~%"
  ;; 		      (* (- time-gc *time-gc-last*) 1000)
  ;; 		      time-gc)
  ;; 	      (setf *time-gc-last* time-gc)
  ;; 	      ;; (sleep 2)
  ;; 	      t))
  ;; 	sb-kernel::*after-gc-hooks*)

  ;; Init globals
  ;; - Create graphs
  ;; - Create text
  
  (init-nodes-default)
  
  (init-threads)
  
  (fmt-model t "main-init" "Finished model initialization~%"))

(defun run-ptree-init ()
  (let ((tree (make-ptree)))
    ;;        var(id)             input(args)     fn

    ;; Initial variables
    (ptree-fn 'projview
	      '()
	      #'set-projview
	      tree)

    (ptree-fn 'controller
              '()
	      #'set-controller
	      tree)

    (ptree-fn 'metrics
	      '()
              #'set-metrics
	      tree)

    (ptree-fn 'cc
	      '()
              #'set-cc
	      tree)	
    
    (ptree-fn 'r-tree
	      '()
              #'set-r-tree
	      tree)

    (ptree-fn 'stack-node
	      '()
              #'set-stack-node
	      tree)    
    
    ;; Shm functions
    (ptree-fn 'shm-projview
	      '(projview)
	      #'set-shm-projview
	      tree)

    (ptree-fn 'shm-nodes
	      '()
	      #'set-shm-nodes
	      tree)

    (ptree-fn 'shm-atomic-counter
	      '()
	      #'set-shm-atomic-counter
	      tree)

    (ptree-fn 'shm-vertices
	      '()
	      #'set-shm-vertices
	      tree)

    (ptree-fn 'shm-element
	      '()
	      #'set-shm-element
	      tree)

    (ptree-fn 'shm-draw-indirect
	      '()
	      #'set-shm-draw-indirect
	      tree)
    
    (ptree-fn 'shm-texture-glyphs
	      '()
	      #'set-shm-texture-glyphs
	      tree)

    ;; Pre-view
    
    (ptree-fn 'init-conn-rpc-view
    	      '(projview
		controller
		metrics
		cc
		r-tree
		stack-node
		shm-projview
    		shm-nodes
    		shm-atomic-counter
    		shm-vertices
    		shm-element
    		shm-draw-indirect
    		shm-texture-glyphs
    		controller)
    	      #'init-conn-rpc-view
	      ;; (lambda (&rest stuff))
    	      tree)
    
    (call-ptree 'init-conn-rpc-view tree)))

(defun fmt-model (dst ctx-str ctl-str &rest rest)
  ;; Add space opt
  (apply #'format
	 dst
	 (str:concat (format nil "[MODEL:~a][~a] " (sb-posix:getpid) ctx-str)
		     ctl-str)
	 rest))
