(in-package :protoform.model)

(defun run-model-ptree ()
  ;; Build frames - aka ptrees
  ;; 1. Pull tasks until empty
  ;;    * Set cap or timer...
  ;; 2. Add to ptree
  ;; 3. Execute ptree

  ;; Issues
  ;; - Cannot pause ptree execution -> require modifying ptree...
  
  (loop
     ;; Block on first message
     ;; Afterwards, poll messages until empty
     :for msg-first := (sb-concurrency:receive-message *mb-model*)
     :do (let* ((ptree (make-ptree))
		(ids (poll-and-process ptree)))

	   (push (process-msg msg-first ptree)
		 ids)

	   (format t "ids: ~a~%" ids)
	   
	   ;; Add finish node
	   (ptree-fn 'finish
		     ids
		     (lambda (&rest ids)
		       (declare (ignore ids)))
		     ptree)
       
	   ;; Exec ptree
	   (call-ptree 'finish ptree)
	   
	   (format t "~6$ | Frame done~%" (osicat:get-monotonic-time)))))

(defun poll-and-process (ptree)
  (let ((ids ()))
    (loop
       :do (multiple-value-bind (msg flag-recv)
	       (sb-concurrency:receive-message-no-hang *mb-model*)
	     (unless flag-recv (return))
	     (push (process-msg msg) ids)))
    ids))

(defun process-msg (msg ptree)
  (cond ((typep msg 'function)
	 ;; return final id
	 (funcall msg ptree))
	(t ;; ptree node args
	 (destructuring-bind (id args-in fn-out)
	     msg
	   (ptree-fn id args-in fn-out ptree)))))

(defun run-async ()
  (execute-mb-tasks *mb-model*))

(defun run-io ()
  (loop (sleep 1)))

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

  ;; (setf (sb-ext:bytes-consed-between-gcs) (* 2 1024))
  
  (init-nodes-default)
  
  (init-threads)
  
  (fmt-model t "main-init" "Finished model initialization~%"))

(defun run-ptree-init ()
  (let ((tree (make-ptree)))

    ;; Initial variables
    ;; Read only after init
    (ptree-fn 'projview
	      '()
	      #'set-projview
	      tree)

    (ptree-fn 'controller
              '()
	      #'set-controller
	      tree)

    ;; Read only after init
    (ptree-fn 'metrics
	      '()
              #'set-metrics
	      tree)

    ;; sb-concurrency
    (ptree-fn 'cc
	      '()
              #'set-cc
	      tree)	

    (ptree-fn 'rtree-model
	      '()
              #'set-rtree-model
	      tree)
    
    (ptree-fn 'rtree-view
	      '()
              #'set-rtree-view
	      tree)

    (ptree-fn 'nodes-model
	      '()
              #'set-nodes-model
	      tree)    

    (ptree-fn 'nodes-view
	      '()
              #'set-nodes-view
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
		rtree-view
		rtree-model
		nodes-view
		nodes-model
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
