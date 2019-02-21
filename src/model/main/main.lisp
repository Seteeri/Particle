(in-package :protoform.model)

(defun fmt-model (dst ctx-str ctl-str &rest rest)
  ;; Add space opt
  (apply #'format
	 dst
	 (str:concat (format nil "[MODEL:~a][~a] " (sb-posix:getpid) ctx-str)
		     ctl-str)
	 rest))

(defun run-model ()
  (loop
     :do (sb-concurrency:receive-message *mb-model*)))

(defun init-model (width height
		   inst-max
		   addr-swank-view)
  
  (setf *kernel*        (make-kernel 4)
	*channel*       (make-channel)
	*channel-input* (make-channel) ; rename to -controller

	*mb-model*      (sb-concurrency:make-mailbox)
	
	;; Simply set here since no fn required
	*width*         width
	*height*        height
	*inst-max*      inst-max)

  ;; Run init fns
  (run-ptree-init)
  
  ;; Then start permanent threads
  (init-threads)
  
  (sb-ext:gc :full t)
  ;; (room t)
  ;; (force-output)
  
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
    
    ;; graphs
    
    (ptree-fn 'digraph
	      '()
              #'set-digraph
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

    ;; Create initial ptr nodes for graphs
    
    (ptree-fn 'node-pointer
	      '(cc
		stack-node
		r-tree
		digraph
		shm-nodes
		metrics)
	      #'set-node-pointer
	      tree)
    
    (ptree-fn 'sock-view
    	      '(shm-projview
    		shm-nodes
    		shm-atomic-counter
    		shm-vertices
    		shm-element
    		shm-draw-indirect
    		shm-texture-glyphs
    		node-pointer
    		controller)
    	      #'init-conn-rpc-view
	      ;; (lambda (&rest stuff))
    	      tree)

    ;; Make sure all nodes computed
    ;; (dolist (id '(projview
    ;; 		  controller
    ;; 		  metrics
    ;; 		  digraph
    ;; 		  shm-projview
    ;; 		  shm-nodes
    ;; 		  shm-atomic-counter
    ;; 		  shm-vertices
    ;; 		  shm-element
    ;; 		  shm-draw-indirect
    ;; 		  shm-texture-glyphs
    ;; 		  node-pointer
    ;; 		  sock-view)
    ;; 	     (when (not (ptree-computed-p id tree))
    ;; 	       (call-ptree id tree))))
    
    (call-ptree 'sock-view tree)))
