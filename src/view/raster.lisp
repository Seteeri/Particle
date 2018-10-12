(in-package :protoform.view)

(defun init-program-default ()
  ;; Create defparameters for the paths - or model can pass it
  (let* ((program (gl:create-program)))
    (let* ((dir-sys-src (asdf:system-source-directory :protoform))
	   (shaders-vert (list (merge-pathnames #P"glsl/structs.glsl" dir-sys-src)
			       (merge-pathnames #P"glsl/default.vs.glsl" dir-sys-src)))
	   (shaders-frag (list (merge-pathnames #P"glsl/structs.glsl" dir-sys-src)
			       (merge-pathnames #P"glsl/filter-bilinear.fs.glsl" dir-sys-src)
			       (merge-pathnames #P"glsl/default.fs.glsl" dir-sys-src)))
	   (log-vert (cad-shader :vertex-shader
				 program
				 shaders-vert))
	   (log-frag (cad-shader :fragment-shader
				 program
				 shaders-frag)))
      (if (> (length log-vert) 0)
	  (fmt-view t "init-program-default" "Shader log: ~%~a~%" log-vert)
	  (fmt-view t "init-program-default" "Compiled and attached vertex shader sucessfully~%"))
      (if (> (length log-frag) 0)
	  (fmt-view t "init-program-default" "Shader log: ~%~a~%" log-frag)
	  (fmt-view t "init-program-default" "Compiled and attached fragment shader sucessfully~%")))
    
    (gl:link-program program)
    
    (let ((log-prog (gl:get-program-info-log program)))
      (if (> (length log-prog) 0)
	  (fmt-view t "init-program-default" "Program log: ~%~a~%" log-prog)
      	  (fmt-view t "init-program-default" "Compiled program sucessfully~%")))
    
    program))

(defun init-buffers-raster (params-shm)
  (with-slots (vaos)
      *view*
    ;; Setup vaos/bindings for each step
    ;; 1 bind vao > many bind buffer calls
    (let ((vao (init-vao)))
      (vector-push vao vaos))))

(defun update-raster-buffer-bindings ()
  (with-slots (bo-step
	       vaos
	       ix-fence)
      *view*
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
;;       *view*
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
;; (defun update-raster-buffer-bindings-2 ()
;;   (with-slots (vaos
;; 	       ix-fence)
;;       *view*
;;     ;; Use different pre-setup VAOs instead of individual bindings
;;     (gl:bind-vertex-array (aref vaos ix-fence))))

(defun run-raster ()

  (gl:use-program (program-default *view*))

  (update-raster-buffer-bindings)

  (gl:clear :color-buffer-bit :depth-buffer-bit)
  
  ;; (%gl:draw-elements :triangles 6 :unsigned-int (null-pointer))
  ;; (%gl:draw-elements-instanced :triangles 6 :unsigned-int (null-pointer) 64)
  (%gl:draw-elements-indirect :triangles
			      :unsigned-int
			      0) ;; nullptr or offset into buffer
  
  t)
