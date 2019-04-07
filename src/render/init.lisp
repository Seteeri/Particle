(in-package :protoform.render)

(defun init-gl-env (width height)
  
  (fmt-render t "init-gl-env" "GL Vendor: ~a~%" (gl:get* :vendor))
  (fmt-render t "init-gl-env" "GL Renderer: ~a~%" (gl:get* :renderer))
  (fmt-render t "init-gl-env" "GL Version: ~a~%" (gl:get* :version))
  (fmt-render t "init-gl-env" "GLSL Version: ~a~%" (gl:get* :shading-language-version))
  
  ;; Get screen dimensions from drm
  (gl:viewport 0 0 width height)
  
  (gl:enable :cull-face)
  (gl:enable :depth-test)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)

  ;; for testing - semi-transparent
  ;; (gl:blend-func :one :one-minus-src-alpha)
  
  (%gl:clear-color 0.0
		   (coerce (/ 43 255) 'single-float)
		   (coerce (/ 54 255) 'single-float)
		   0.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  
  (loop
     :for pair :in (get-gl-maxes)
     :do (fmt-render t "init-gl-env" "~a = ~a~%" (first pair) (second pair))))
  
(defun init-bo-step (params-shm)

  (with-slots (width height
		     boav-main
		     bo-step
		     inst-max)
      *render*

    ;; Notes:
    ;; * Some buffers have a different bind layout per shader stage
    ;; * Texture requires setting fmt after and other ops
    ;; * Set initial data for buffers element and draw-indirect
    ;; * glMapNamedBuffer is unavailable so
    ;;   * to persistently map the buffer, it needs to be bound...
    ;;   * to bind a buffer, requires an appropriate program
    ;; * What is the behavior when binding a buffer with no program bound?
    ;;  * Doesn't matter here...
    
    (dolist (params params-shm)
      (destructuring-bind (target
			   path
			   size
			   bind-cs
			   bind-vs
			   count-buffer
			   flag-copy
			   &rest rest)
	  params
	
	(let ((bo (init-buffer-object target
    				      path
    				      size
    				      (if (> bind-vs -1) bind-vs bind-cs)
    				      t ; pmap
    				      :buffering count-buffer)))
	  (setf (gethash path bo-step)
		bo)

	  (when (eq target :texture-buffer)

	    ;; texturei max - GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
	    ;; Already active...
	    (gl:active-texture :texture0)
	    
	    (dotimes (i (count-buffers bo))
	      (update-binding-buffer bo i)
	      (%gl:tex-buffer :texture-buffer
			      (first rest) ; rgba8
			      (aref (buffers bo) i)))
	    
	    t))))))

(defun init-view-buffers (params-model)

  ;; (format t "~S~%" params-model)
  
  (fmt-render t "init-view-buffers" "Initializing shm handles~%")
  (init-handles-shm params-model)

  (fmt-render t "init-view-buffers" "Initializing buffer object caches~%")
  (init-bo-caches params-model)

  (fmt-render t "init-view-buffers" "Initializing buffer object steps~%")
  (init-bo-step params-model)

  (fmt-render t "init-view-buffers" "Initializing shader bindings~%")
  ;; Shader specific initialization
  (init-buffers-raster-default params-model)
  (init-buffers-raster-msdf params-model)
  (init-buffers-compute params-model)

  ;; At this point, shm already has data loaded by model
  ;; so copy to OpenGL buffers
  (memcpy-shm-to-all)

  (fmt-render t "init-view-buffers" "Finished initializing shm handles~%"))
