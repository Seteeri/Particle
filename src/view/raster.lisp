(in-package :protoform.view)

(defun init-program-raster ()
  ;; Create defparameters for the paths - or model can pass it
  (let* ((program (gl:create-program)))
    (let* ((dir-sys-src (asdf:system-source-directory :protoform))
	   (path-struct (merge-pathnames #P"glsl/structs.glsl" dir-sys-src))
	   (path-vert (merge-pathnames #P"glsl/msdf.vs.glsl" dir-sys-src))
	   (path-frag (merge-pathnames #P"glsl/msdf.fs.glsl" dir-sys-src))
	   (log-vert (cad-shader :vertex-shader
				 program
				 (list path-struct
				       path-vert)))
	   (log-frag (cad-shader :fragment-shader
				 program
				 (list path-struct
				       path-frag))))
      (if (> (length log-vert) 0)
	  (fmt-view t "init-program-raster" "Shader log: ~%~a~%" log-vert)
	  (fmt-view t "init-program-raster" "Compiled and attached vertex shader sucessfully~%"))
      (if (> (length log-frag) 0)
	  (fmt-view t "init-program-raster" "Shader log: ~%~a~%" log-frag)
	  (fmt-view t "init-program-raster" "Compiled and attached fragment shader sucessfully~%")))
    
    (gl:link-program program)
    
    (let ((log-prog (gl:get-program-info-log program)))
      (if (> (length log-prog) 0)
	  (fmt-view t "init-program-raster" "Program log: ~%~a~%" log-prog)
      	  (fmt-view t "init-program-raster" "Compiled program sucessfully~%")))
    
    program))

(defun init-buffers-raster (params-shm)
  (with-slots (bo-step
	       vaos)
      *view*
    ;; Setup vaos/bindings for each step
    ;; 1 bind vao > many bind buffer calls
    (dotimes (i 3)
      (let ((vao (init-vao)))
	(vector-push vao vaos)
	(loop 
	   :for name :being :the :hash-keys :of bo-step
	   :using (hash-value buffer)
	   :do (update-binding-buffer buffer i))))))

(defun update-raster-buffer-bindings ()
  (with-slots (vaos
	       ix-fence)
      *view*
    ;; Use different pre-setup VAOs instead of individual bindings
    (gl:bind-vertex-array (aref vaos ix-fence))))

(defun run-raster ()

  (gl:use-program (program-raster *view*))

  (update-raster-buffer-bindings)

  (gl:clear :color-buffer-bit :depth-buffer-bit)
  
  ;; (%gl:draw-elements :triangles 6 :unsigned-int (null-pointer))
  ;; (%gl:draw-elements-instanced :triangles 6 :unsigned-int (null-pointer) 64)
  (%gl:draw-elements-indirect :triangles
			      :unsigned-int
			      0) ;; nullptr or offset into buffer
  
  t)
