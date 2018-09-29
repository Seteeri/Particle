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
	       mapping-base
	       bo-counter)
      *view*

    (gl:use-program program-compute)
    
    ;; Uniform is only bound once also

    ;; Texture exists but not bound since compute shader does not use it
    ;; Texture need only do a shm-ptr copy per frame as needed

    ;; Not needed since start of frame will do this?
    ;; Specifically for compute program
    (when t
      (dolist (name '("projview"
		      "instance"))
	(fmt-view t "init-buffers-compute" "Binding ~a: ~a~%" name (gethash name mapping-base))
	(let ((boa (boa (gethash name mapping-base))))
	  (update-binding-buffer boa 0))))

    ;; Bound on init only
    ;; TODO: Move creation to raster or integrate elsewhere
    (setf bo-counter (init-buffer-object program-compute
					 :atomic-counter-buffer
					 "atomic-counter-buffer"
					 6 ; why 6 counters needed?
					 3 ; bind
					 t ; pmap
					 :buffering 'single))))


(defun update-compute-buffers ()

  ;; This function binds the raster buffers to the *_out bindings in the shader
  ;; Should group them like mapping base is?
  
  (with-slots (mapping-base
	       bo-step
	       ix-fence)
      *view*

    ;; Need not bind texture since it is not used by compute shader
    (dolist (name '("projview"
		    "instance"))
      (update-binding-buffer (gethash name bo-step) ix-fence))
    
    ;; Tentative placement:

    ;; Currently, assume changing every frame for now...
    ;; Memcpy from compute ptrs to raster ptrs since compute shader doesn't compute anything for it
    (let ((step-projview (gethash "projview" bo-step))
	  (base-projview (boa (gethash "projview" mapping-base))))
      (c-memcpy (aref (ptrs-buffer step-projview) ix-fence)
    		(aref (ptrs-buffer base-projview) 0)
    		(size-buffer step-projview))) ; use dest

    ;; Memcpy texture also - shm ptr to gl ptr
    ;; Binding only required for shader usage
    ;; Need only do on-demand - not every frame

    ;; Don't need yet
    (when nil
      (c-memcpy (aref (ptrs-buffer bo-texture) ix-fence)
    		(aref (ptrs-buffer (boa (gethash "texture" mapping-base))) 0)
    		(size-buffer bo-texture))))

  t)
  

(defun run-compute ()

  (with-slots (program-compute
	       mapping-base
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

  (with-slots (program-compute
	       mapping-base
	       bo-step
	       bo-counter
	       inst-max
	       ix-fence)
      *view*

    ;; Need not use program since this just copies memory
    
    (update-compute-buffers)
    
    ;; Do only if dirty
    (let ((step-instance (gethash "instance" bo-step))
	  (base-instance (boa (gethash "instance" mapping-base))))
      (copy-buffer (aref (buffers base-instance) 0)
		   (aref (buffers step-instance) ix-fence)
		   (size-buffer step-instance)))
      
    ;; Set to render all instances
    (setf (mem-aref (aref (ptrs-buffer (gethash "draw-indirect" bo-step)) ix-fence) :uint 1)
	  inst-max)))

(defun copy-buffer (buffer-read
		    buffer-write
		    size
		    &key
		      (offset-read 0)
		      (offset-write 0))
  
  (%gl:bind-buffer :copy-read-buffer buffer-read)
  (%gl:bind-buffer :copy-write-buffer buffer-write)
  (%gl:copy-buffer-sub-data :copy-read-buffer
			    :copy-write-buffer
			    offset-read ; r off
			    offset-write ; w off
			    size))
