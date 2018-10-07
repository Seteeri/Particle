(in-package :protoform.view)

(defun init-program-raster ()
  ;; Create defparameters for the paths - or model can pass it
  (let* ((program (gl:create-program)))
    (let* ((dir-sys-src (asdf:system-source-directory :protoform))
	   (path-struct (merge-pathnames #P"glsl/structs.glsl" dir-sys-src))
	   (path-vert (merge-pathnames #P"glsl/msdf.vs.glsl" dir-sys-src))
	   (path-frag (merge-pathnames #P"glsl/msdf.fs.glsl" dir-sys-src)))
      (attach-shader :vertex-shader
		     program
		     (list path-struct
			   path-vert))
      (attach-shader :fragment-shader
		     program
		     (list path-struct
			   path-frag)))
    (gl:link-program program)
    (fmt-view t "init-program-raster" "Program info log: ~a~%" (gl:get-program-info-log program))
    program))

(defun init-buffers-raster (params-shm)
  (with-slots (boav-main)
      *view*
    (setf boav-main (init-boav-main))))

(defun update-raster-buffer-bindings ()
  (with-slots (bo-step
	       ix-fence)
      *view*
    (loop 
       :for name :being :the :hash-keys :of bo-step
       :using (hash-value buffer)
       :do (update-binding-buffer buffer ix-fence))))

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
