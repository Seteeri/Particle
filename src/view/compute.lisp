(in-package :protoform.view)

(defun init-program-compute ()
  (let* ((program (gl:create-program)))
    (let* ((dir-sys-src (asdf:system-source-directory :protoform))
	   (path-struct (merge-pathnames #P"glsl/structs.glsl" dir-sys-src))
	   (path-main (merge-pathnames #P"glsl/msdf.cs.glsl" dir-sys-src))
	   (log-main (cad-shader :compute-shader
				 program
				 (list path-struct
				       path-main))))
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
    
    ;; Cache need only bound once on init since all single buffered
    (loop 
       :for name :being :the :hash-keys :of bo-cache
       :using (hash-value cache)
       :do (let* ((buffer (buffer cache))
		  (bl (binding-layout buffer)))
	     ;; Bind if allowed
	     ;; Single buffered so always index 0
	     (when (> bl -1)
	       ;; (fmt-view t "init-buffers-compute" "Binding ~a~%" name)
	       (update-binding-buffer buffer 0))))))

(defun update-compute-bindings ()
  (with-slots (bo-step
	       ix-fence)
      *view*
    
    ;; These are the output buffers for the compute shader
    ;; Need only be done for those specified in the compute shader
    ;; These bindings only apply to compute program
    ;; Input buffers, aka cache buffers, are single so need not rebind
    (loop 
       :for name :being :the :hash-keys :of bo-step
       :using (hash-value buffer)
       :do (progn
	     (let ((bl (binding-layout buffer)))
	       ;; Bind if allowed
	       ;; Single buffered so always index 0
	       (when (> bl -1)
		 (update-binding-buffer buffer ix-fence)))))))

(defun update-compute-buffers ()
  (with-slots (bo-cache
	       ix-fence)
      *view*
    (loop 
       :for name :being :the :hash-keys :of bo-cache
       :using (hash-value cache)
       :do (with-slots (buffer dirty)
	       cache
	     (when (> dirty 0)
	       ;; Can also use gl function to copy between buffers
	       (fmt-view t "update-compute-buffers" "Cache dirt: ~a, ~a~%" name dirty)
	       (memcpy-cache-to-step name ix-fence
    				     name)
	       (decf dirty))))))

(defun run-compute-copy ()

  (with-slots (program-compute
	       bo-step
	       inst-max
	       ix-fence)
      *view*

    (gl:use-program program-compute)
    
    (update-compute-bindings)
    (update-compute-buffers)
    
    ;; Set to render all instances
    (setf (mem-aref (aref (ptrs-buffer (gethash "draw-indirect" bo-step)) ix-fence) :uint 1)
	  inst-max)))

(defun run-compute ()

  (with-slots (program-compute
	       bo-counter
	       bo-step
	       inst-max
	       ix-fence)
      *view*
    
    (gl:use-program program-compute)

    ;; Double check output binding is being set - update-compute-buffers
    (update-compute-bindings)
    (update-compute-buffers)
    
    ;; Reset counter before every dispatch
    (setf (mem-aref (aref (ptrs-buffer bo-counter) 0) :uint 0) 0)
    
    ;; <= 65535 (GL_MAX_COMPUTE_WORK_GROUP_COUNT)
    ;; 1792 max local threads/invocations per work group
    ;; 65535*1792 = 117,438,720 - n instances; greater than this value means multiple dispatches (and switching buffers)
    ;;
    ;; TODO: Instead of inst-max use count from view
    (%gl:dispatch-compute (ceiling (/ inst-max 1792))
			  1
			  1) ;indirect version exists...

    ;; Ensure compute shader finishes before grabbing values
    (sync-gl)
    
    ;; Update indirect primCount with counter
    (setf (mem-aref (aref (ptrs-buffer (gethash "draw-indirect" bo-step)) ix-fence) :uint 1)
	  inst-max)))
    	  ;; (mem-aref (aref (ptrs-buffer bo-counter) 0) :uint 0))))
