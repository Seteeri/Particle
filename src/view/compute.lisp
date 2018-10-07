(in-package :protoform.view)

(defun init-program-compute ()
  (let* ((program (gl:create-program)))
    (let* ((dir-sys-src (asdf:system-source-directory :protoform))
	   (path-struct (merge-pathnames #P"glsl/structs.glsl" dir-sys-src))
	   (path-cull-frustum (merge-pathnames #P"glsl/msdf.cs.glsl" dir-sys-src)))
      (attach-shader :compute-shader
		     program
		     (list path-struct
			   path-cull-frustum)))
    (gl:link-program program)
    (fmt-view t "init-program-compute" "Program info log: ~a~%" (gl:get-program-info-log program))
    program))

(defun init-buffers-compute (params-shm)

  (with-slots (program-compute
	       handles-shm
	       bo-cache
	       bo-counter)
      *view*

    (gl:use-program program-compute)
    
    ;; Uniform is only bound once also

    ;; Texture exists but not bound since compute shader does not use it
    ;; Texture need only do a shm-ptr copy per frame as needed

    ;; Not needed since start of frame will do this?
    ;; Specifically for compute program
    ;; Go through all and check flag
    (dolist (name '("projview"
		    "vertices"
		    "instance"))
      (fmt-view t "init-buffers-compute" "Binding ~a~%" name)
      ;; These are single so always 0
      (update-binding-buffer (get-cache-buffer name) 0))

    ;; Bound on init only
    (setf bo-counter (init-buffer-object :atomic-counter-buffer
					 "atomic-counter-buffer"
					 6 ; why 6 counters needed?
					 4 ; bind
					 t ; pmap
					 :buffering 'single))))

(defun update-compute-bindings ()
  (with-slots (bo-step
	       ix-fence)
      *view*
    ;; These are the output buffers for the compute shader
    ;; Need only be done for those specified in the compute shader
    ;; These bindings only apply to compute program
    ;; Input buffers, aka cache buffers, are single so need not rebind
    (dolist (name '("projview"
		    "vertices"
		    "instance"))
      (update-binding-buffer (gethash name bo-step) ix-fence))))

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
	       ;; (fmt-view t "update-compute-buffers" "COPYING CACHE->STEP!~%")
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
