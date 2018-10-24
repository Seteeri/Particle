(in-package :protoform.view)

(defun init-program-compute ()
  (let* ((program (gl:create-program)))
    (let* ((dir-sys-src (asdf:system-source-directory :protoform))
	   (shaders (list (merge-pathnames #P"glsl/structs.glsl" dir-sys-src)
			  (merge-pathnames #P"glsl/node.cs.glsl" dir-sys-src)))
	   (log-main (cad-shader :compute-shader
				 program
				 shaders)))
      (if (> (length log-main) 0)
	  (fmt-view t "init-program-compute" "Shader log: ~%~a~%" log-main)
	  (fmt-view t "init-program-compute" "Compiled and attached compute shader sucessfully~%")))
    
    (gl:link-program program)
    
    (let ((log-prog (gl:get-program-info-log program)))
      (if (> (length log-prog) 0)
	  (fmt-view t "init-program-compute" "Program log: ~%~a~%" log-prog)
	  (fmt-view t "init-program-compute" "Compiled program successfully~%")))
    
    program))

(defun init-buffers-compute (params-shm)

  (with-slots (program-compute
	       handles-shm
	       bo-cache
	       bo-counter)
      *view*

    (gl:use-program program-compute)

    ;; These are input bindings/buffers
    ;; Cache need only bound once on init since all single buffered
    (loop 
       :for name :being :the :hash-keys :of bo-cache
       :using (hash-value cache)
       :do (let* ((buffer (buffer cache))
		  (bl (binding-layout buffer)))
	     ;; Bind if allowed
	     ;; Single buffered so always index 0
	     (when (> bl -1)
	       (fmt-view t "init-buffers-compute" "Binding ~a to ~a~%" name bl)
	       (update-binding-buffer buffer 0))))))

(defun update-compute-bindings ()
  (with-slots (bo-step
	       ix-fence)
      *view*
    
    ;; These output bindings/buffers
    ;; Need only be done for those specified in the compute shader
    ;; These bindings only apply to compute program
    ;; Input buffers, aka cache buffers, are single so need not rebind
    ;;
    ;; Note, atomic counter is re-bound here also...
    (loop 
       :for name :being :the :hash-keys :of bo-step
       :using (hash-value buffer)
       :do (progn
	     (let ((bl (binding-layout buffer)))
	       ;; Bind if allowed
	       ;; Single buffered so always index 0
	       (when (> bl -1)
		 ;; (format t "~a, ~a~%" name bl)
		 (update-binding-buffer buffer ix-fence)))))))

(defun update-compute-buffers-ptr (&optional (force nil))
  (with-slots (bo-cache
	       ix-fence)
      *view*
    (loop 
       :for name :being :the :hash-keys :of bo-cache
       :using (hash-value cache)
       :do (with-slots (buffer flag-copy)
	       cache
	     (when (or (/= flag-copy 0)
		       force)
	       ;; Can also use gl function to copy between buffers
	       ;; (when (and (not force) (/= flag-copy -1))
	       ;; 	 (fmt-view t "update-compute-buffers" "Cache status: ~a, ~a~%" name flag-copy))
	       (memcpy-cache-to-step name ix-fence
    				     name
				     nil
				     nil) ; no print
	       (when (> flag-copy 0)
		 (decf flag-copy)))))))

(defun update-compute-buffers-gl (&optional (force nil))
  (with-slots (bo-cache
	       ix-fence)
      *view*
    (loop 
       :for name :being :the :hash-keys :of bo-cache
       :using (hash-value cache)
       :do (with-slots (buffer flag-copy)
	       cache
	     (when (or (/= flag-copy 0)
		       force)
	       ;; Can also use gl function to copy between buffers
	       ;; (when (and (not force) (/= flag-copy -1))
	       ;; 	 (fmt-view t "update-compute-buffers" "Cache status: ~a, ~a~%" name flag-copy))
	       (copy-buffer (aref (buffers (get-cache-buffer name)) 0)
			    (aref (buffers (gethash name (bo-step *view*))) ix-fence)
			    (size-buffer (gethash name (bo-step *view*))))
	       (when (> flag-copy 0)
		 (decf flag-copy)))))))

(defun run-compute-bypass ()

  (with-slots (program-compute
	       bo-step
	       inst-max
	       ix-fence)
      *view*
    
    ;; (gl:use-program program-compute)
    
    (update-compute-bindings)
    (update-compute-buffers-ptr)
    
    ;; Set to render all instances
    (setf (mem-aref (aref (ptrs-buffer (gethash "draw-indirect" bo-step)) ix-fence) :uint 1)
    	  inst-max)

    t))

(defun run-compute ()

  (with-slots (program-compute
	       bo-cache
	       bo-step
	       inst-max
	       ix-fence)
      *view*
    
    (gl:use-program program-compute)

    ;; Double check output binding is being set - update-compute-buffers
    ;; Ensure these correlate with raster VAO
    (update-compute-bindings)
    
    ;; Reset counter before every dispatch
    (setf (mem-aref (aref (ptrs-buffer (gethash "atomic-counter" bo-step)) ix-fence)
		    :uint 0)
	  0)
    
    ;; <= 65535 (GL_MAX_COMPUTE_WORK_GROUP_COUNT)
    ;; 1792 max local threads/invocations per work group
    ;; 65535*1792 = 117,438,720 - n instances; greater than this value means multiple dispatches (and switching buffers)
    ;;
    ;; TODO: Instead of inst-max use count from view
    ;;
    ;; Indirect version exists...
    (%gl:dispatch-compute (ceiling (/ inst-max 1792))
			  1
			  1)

    ;; Ensure compute shader finishes before grabbing values
    (protoform.opengl::sync-gl)

    (when nil
      (fmt-view t "run-compute" "Counter: ~a~%"
    		(mem-aref (aref (ptrs-buffer (gethash "atomic-counter" bo-step)) ix-fence)
    			  :uint 0)))
    
    ;; Update indirect primCount with counter
    (setf (mem-aref (aref (ptrs-buffer (gethash "draw-indirect" bo-step)) ix-fence) :uint 1)
	  (mem-aref (aref (ptrs-buffer (gethash "atomic-counter" bo-step)) ix-fence) :uint 0))))
