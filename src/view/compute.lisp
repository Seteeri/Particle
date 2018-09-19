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

(defun init-buffers-compute (msdf)

  (with-slots (program-compute
	       mapping-base
	       bo-counter)
      msdf

    (gl:use-program program-compute)

    ;; Uniform is only bound once also
    ;; Texture also exists but not bound since compute shader does not use it
    ;; Texture need only do a shm-ptr copy
    (dolist (name '("projview"
		    "instance"))
      (progn
	(format t "[init-buffers-compute] Binding ~a: ~a~%" name (gethash name mapping-base))
	(let ((boa (boa (gethash name mapping-base))))
	  (update-binding-buffer boa 0))))

    ;; Note: this is initially bound once
    (setf bo-counter (init-buffer-object program-compute
					     :atomic-counter-buffer
					     "atomic-counter-buffer"
					     :int
					     6 ; 6 ints
					     1 ; 1 counter
					     3 ; 3=binding
					     t ; yes map
					     :buffering 'single))

    t))


(defun update-compute-buffers (msdf)

  ;; This function binds the raster buffers to the *_out bindings in the shader
  ;; Should group them like mapping base is?
  
  (with-slots (mapping-base
	       bo-projview
	       bo-instance
	       bo-texture
	       ix-fence)
      msdf

    ;; Need not bind texture since it is not used by compute shader
    (dolist (boa (list bo-projview
		       bo-instance))
      (update-binding-buffer boa ix-fence))
    
    ;; Tentative placement:
    
    ;; Memcpy from compute ptrs to raster ptrs since compute shader doesn't compute anything for it
    (c-memcpy (aref (ptrs-buffer bo-projview) ix-fence)
    	      (aref (ptrs-buffer (boa (gethash "projview" mapping-base))) 0)
    	      (size-buffer bo-projview))

    ;; Memcpy texture also - shm ptr to gl ptr
    ;; Binding only required for shader usage
    (c-memcpy (aref (ptrs-buffer bo-texture) ix-fence)
    	      (aref (ptrs-buffer (boa (gethash "texture" mapping-base))) 0)
    	      (size-buffer bo-texture))))
  

(defun run-compute (msdf)

  (with-slots (program-compute
	       mapping-base
	       bo-counter
	       bo-indirect
	       inst-max
	       ix-fence)
      msdf
    
    (gl:use-program program-compute)
    
    (update-compute-buffers msdf)
    
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
    (setf (mem-aref (aref (ptrs-buffer bo-indirect) ix-fence) :uint 1)
	  inst-max)))
    	  ;; (mem-aref (aref (ptrs-buffer bo-counter) 0) :uint 0))))

(defun run-compute-copy (msdf)

  (with-slots (program-compute
	       mapping-base

	       bo-instance
	       
	       bo-counter
	       bo-indirect
	       inst-max
	       ix-fence)
      msdf

    ;; Need not use program
    
    (update-compute-buffers msdf)
    
    ;; base -> raster
    (copy-buffer (aref (buffers (boa (gethash "instance" mapping-base))) 0)
		 (aref (buffers bo-instance) ix-fence)
		 (size-buffer bo-instance))

    ;; Set to render all instances
    (setf (mem-aref (aref (ptrs-buffer bo-indirect) ix-fence) :uint 1)
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
