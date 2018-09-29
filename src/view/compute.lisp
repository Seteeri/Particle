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
    (format t "[init-program-compute] Program info log: ~a~%" (gl:get-program-info-log program))
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
    (dolist (name '("projview"
		    "instance"))
      (fmt-view t "init-buffers-compute" "Binding ~a~%" name)
      ;; These are single so always 0
      (update-binding-buffer (gethash name bo-cache) 0))

    ;; Bound on init only
    (setf bo-counter (init-buffer-object program-compute
					 :atomic-counter-buffer
					 "atomic-counter-buffer"
					 6 ; why 6 counters needed?
					 3 ; bind
					 t ; pmap
					 :buffering 'single))))

(defun update-compute-buffers ()
  
  (with-slots (handles-shm
	       bo-cache
	       bo-step
	       ix-fence)
      *view*

    ;; Need not bind texture since it is not used by compute shader
    (dolist (name '("projview"
		    "instance"))
      (update-binding-buffer (gethash name bo-step) ix-fence))
    
    ;; Refactor to be on-demand by model

    ;; Currently, assume changing every frame for now...
    ;; Memcpy from compute ptrs to raster ptrs since compute shader doesn't compute anything for it
    (let ((step-projview (gethash "projview" bo-step))
	  (base-projview (gethash "projview" bo-cache)))
      (c-memcpy (aref (ptrs-buffer step-projview) ix-fence)
    		(aref (ptrs-buffer base-projview) 0)
    		(size-buffer step-projview))) ; use dest

    ;; Memcpy texture also - shm ptr to gl ptr
    ;; Binding only required for shader usage
    ;; Need only do on-demand - not every frame
    (let ((step-tex (gethash "texture" bo-step))
	  (base-tex (gethash "texture" bo-cache)))
      (c-memcpy (aref (ptrs-buffer step-tex) ix-fence)
    		(aref (ptrs-buffer base-tex) 0)
    		(size-buffer step-tex)))))

(defun run-compute ()

  (with-slots (program-compute
	       bo-counter
	       bo-step
	       inst-max
	       ix-fence)
      *view*
    
    (gl:use-program program-compute)
    
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

(defun run-compute-copy ()

  (with-slots (handles-shm
	       bo-cache
	       bo-step
	       bo-counter
	       inst-max
	       ix-fence)
      *view*

    ;; Need not use program since this just copies memory
    
    (update-compute-buffers)
    
    ;; Do only if dirty
    (let ((step-instance (gethash "instance" bo-step))
	  (base-instance (gethash "instance" bo-cache)))
      (copy-buffer (aref (buffers base-instance) 0)
		   (aref (buffers step-instance) ix-fence)
		   (size-buffer step-instance)))
      
    ;; Set to render all instances
    (setf (mem-aref (aref (ptrs-buffer (gethash "draw-indirect" bo-step)) ix-fence) :uint 1)
	  inst-max)))
