(in-package :protoform.render)

(defun init-gl-env (width height)
  
  (fmt-render "init-gl-env" "GL Vendor: ~a~%" (gl:get* :vendor))
  (fmt-render "init-gl-env" "GL Renderer: ~a~%" (gl:get* :renderer))
  (fmt-render "init-gl-env" "GL Version: ~a~%" (gl:get* :version))
  (fmt-render "init-gl-env" "GLSL Version: ~a~%" (gl:get* :shading-language-version))
  
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
     :do (fmt-render "init-gl-env" "~a = ~a~%" (first pair) (second pair))))
  
(defun setup-render (params-model)

  ;; (format t "~S~%" params-model)
  
  (fmt-render "setup-render" "Initializing shm handles~%")
  (init-handles-shm params-model)

  (fmt-render "setup-render" "Initializing buffer object caches~%")
  (init-bo-caches params-model)

  (fmt-render "setup-render" "Initializing buffer object steps~%")
  (init-bo-step params-model)

  (fmt-render "setup-render" "Initializing shader bindings~%")
  ;; Shader specific initialization
  (gl:use-program (prog-rast-msdf *render*))
  (init-buff-rast-msdf params-model)
  (gl:use-program (prog-compute *render*))
  (init-buff-compute params-model)

  ;; At this point, shm already has data loaded by model
  ;; so copy to OpenGL buffers
  (memcpy-shm-to-all)

  (fmt-render "setup-render" "Finished initializing shm handles~%"))
