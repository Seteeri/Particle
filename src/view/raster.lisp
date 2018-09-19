(in-package :protoform.view)

(defun init-program-raster ()
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
    (format t "[init-program-raster] Program info log: ~a~%" (gl:get-program-info-log program))
    program))

(defun init-buffers-raster (msdf)

  (with-slots (width height
		     program-raster
		     boav-main
		     bo-projview
		     bo-instance
		     boa-element
		     bo-indirect
		     bo-texture
		     inst-max)
      msdf

    (gl:use-program program-raster)
    
    ;; Upload msdf glyphs
    (gl:active-texture :texture0)
    (setf bo-texture (init-buffer-texture program-raster
    					  :texture-buffer
    					  "texture-buffer"
    					  :unsigned-byte
    					  4
    					  (* 96 96 255)
    					  0
    					  t
    					  :buffering 'single)) ;triple if editing and also must rotate
    (%gl:uniform-1i (gl:get-uniform-location program-raster "msdf") 0)

    

    
    (setf boav-main (init-boav-main))

    ;; static
    (setf boa-element (init-boa-element program-raster
					:element-array-buffer
					"element-array-buffer"
					:int
					1
					6
					-1
					nil
					:buffering 'single
					:usage :static-draw
					:data (make-array 6
							  :element-type '(unsigned-byte 32)
							  :initial-contents (list 0 2 1 0 3 2))))
    ;; :initial-contents (list 2 0 3 2 1 0))))
    
    ;; bind-layout uses the output buffers so they should not be the same
    ;; as those in init-mapping-base/init-buffers-compute, excluding uniforms
    
    (setf bo-projview (init-buffer-object program-raster
						  :uniform-buffer
						  "projview"
						  :float
						  (+ 16 16 16)
						  1
						  0 ; same for compute/raster
						  t
						  :buffering 'triple))
    ;; (with-foreign-string (foo "model")
    ;;   (format t "ssb: ~a~%" (%gl:get-program-resource-location-index program-raster :program-output foo)))

    ;; shared between compute output and raster input
    (setf bo-instance (init-buffer-object program-raster
						  :shader-storage-buffer
						  "instance"
						  :float
						  (/ 208 4)
						  inst-max
						  2 ; output binding
						  t
						  :buffering 'triple))
    
    (setf bo-indirect (init-buffer-draw-indirect program-raster
						 :draw-indirect-buffer
						 "draw-indirect-buffer"
						 :int
						 6
						 1
						 -1 ; no binding
						 t
						 :buffering 'triple))
    (set-bo-indirect bo-indirect
		     6 inst-max 0 0 0)))

(defun update-raster-buffer-bindings (msdf)
  (with-slots (bo-projview
	       bo-instance
	       bo-indirect
	       ix-fence)
      msdf
    
    (when (> (count-buffers bo-projview) 1)
      (update-binding-buffer bo-projview ix-fence))
    
    (when (> (count-buffers bo-instance) 1)
      (update-binding-buffer bo-instance ix-fence))
    
    (when (> (count-buffers bo-indirect) 1)
      (update-binding-buffer bo-indirect ix-fence))))

(defun run-raster (msdf)

  (gl:use-program (program-raster msdf))

  (update-raster-buffer-bindings msdf)

  (gl:clear :color-buffer-bit :depth-buffer-bit)
  
  ;; (%gl:draw-elements :triangles 6 :unsigned-int (null-pointer))
  ;; (%gl:draw-elements-instanced :triangles 6 :unsigned-int (null-pointer) 64)
  (%gl:draw-elements-indirect :triangles
			      :unsigned-int
			      0) ;; nullptr or offset into buffer
  
  t)
