(in-package :protoform.render)

(defparameter *shad-rast-msdf-vert* (list (merge-pathnames #P"glsl/structs.glsl"
								(asdf:system-source-directory :protoform))
					       (merge-pathnames #P"glsl/node.vs.glsl"
								(asdf:system-source-directory :protoform))))

(defparameter *shad-rast-msdf-frag* (list (merge-pathnames #P"glsl/structs.glsl"
								(asdf:system-source-directory :protoform))
					       (merge-pathnames #P"glsl/filter-bilinear.fs.glsl"
								(asdf:system-source-directory :protoform))
					       (merge-pathnames #P"glsl/msdf.fs.glsl"
								(asdf:system-source-directory :protoform))))

(defun init-prog-rast-msdf ()
  (let* ((program (gl:create-program)))
    (let* ((log-vert (cad-shader :vertex-shader
				 program
				 *shad-rast-msdf-vert*))
	   (log-frag (cad-shader :fragment-shader
				 program
				 *shad-rast-msdf-frag*)))
      (if (> (length log-vert) 0)
	  (fmt-render "init-prog-rast-msdf" "Shader log: ~%~a~%" log-vert)
	  (fmt-render "init-prog-rast-msdf" "Compiled and attached vertex shader sucessfully~%"))
      (if (> (length log-frag) 0)
	  (fmt-render "init-prog-rast-msdf" "Shader log: ~%~a~%" log-frag)
	  (fmt-render "init-prog-rast-msdf" "Compiled and attached fragment shader sucessfully~%")))
    
    (gl:link-program program)
    
    (let ((log-prog (gl:get-program-info-log program)))
      (if (> (length log-prog) 0)
	  (fmt-render "init-prog-rast-msdf" "Program log: ~%~a~%" log-prog)
      	  (fmt-render "init-prog-rast-msdf" "Compiled program sucessfully~%")))
    
    program))

(defun init-buff-rast-msdf (params-shm)
  ;; VAO per program?
  (with-slots (vaos)
      *render*
    ;; Setup vaos/bindings for each step
    ;; 1 bind vao > many bind buffer calls
    (let ((vao (init-vao)))
      (vector-push vao vaos))))

(defun update-rast-buff-bindings ()
  (with-slots (bo-step
	       vaos
	       ix-fence)
      *render*
    ;; Setup vaos/bindings for each step
    ;; 1 bind vao > many bind buffer calls
    (loop 
       :for name :being :the :hash-keys :of bo-step
       :using (hash-value buffer)
       :do (update-binding-buffer buffer ix-fence))))

;; https://stackoverflow.com/questions/50807645/how-does-opengl-differentiate-binding-points-in-vao-from-ones-defined-with-glbin
;; Particle Systems with Compute & Geometry Shader. Institute of Computer Graphics and Algorithms Vienna University of Technology
;; http://web.engr.oregonstate.edu/~mjb/cs519/Handouts/compute.shader.1pp.pdf
;; https://github.com/OpenGLInsights/OpenGLInsightsCode
;; (defun init-buffers-raster (params-shm)
;;   (with-slots (bo-step
;; 	       vaos)
;;       *render*
;;     ;; Setup vaos/bindings for each step
;;     ;; 1 bind vao > many bind buffer calls
;;     (dotimes (i 3)
;;       (let ((vao (init-vao)))
;; 	(vector-push vao vaos)
;; 	(loop 
;; 	   :for name :being :the :hash-keys :of bo-step
;; 	   :using (hash-value buffer)
;; 	   :do (update-binding-buffer buffer i))))))

;; Causes flickering probably because bindings aren't being updated
;; Binding points created via VAO and glBindBufferBase completely seperate things
;; (defun update-rast-buff-bindings-2 ()
;;   (with-slots (vaos
;; 	       ix-fence)
;;       *render*
;;     ;; Use different pre-setup VAOs instead of individual bindings
;;     (gl:bind-vertex-array (aref vaos ix-fence))))

(defun run-raster-msdf ()

  (gl:use-program (prog-rast-msdf *render*))

  (update-rast-buff-bindings)
    
  ;; (let* ((time (osicat:get-monotonic-time)))

  (gl:clear :color-buffer-bit :depth-buffer-bit)

  ;; (let* ((time-final (osicat:get-monotonic-time))
  ;; 	   (time-delta (- time-final time)))
  ;;   (format t "delta: ~s~%" (* time-delta 1000))))
  
  ;; (%gl:draw-elements :triangles 6 :unsigned-int (null-pointer))
  ;; (%gl:draw-elements-instanced :triangles 6 :unsigned-int (null-pointer) 64)
  (%gl:draw-elements-indirect :triangles
			      :unsigned-int
			      0)) ;; nullptr or offset into buffer
