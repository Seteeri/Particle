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

  (with-slots (width height
		     program-raster
		     boav-main
		     bo-step
		     inst-max)
      *view*

    (gl:use-program program-raster)

    (setf boav-main (init-boav-main))

    ;; Notes:
    ;; * Some buffers have a different bind layout per shader stage
    ;; * Texture requires setting fmt after and other ops
    ;; * Set initial data for buffers element and draw-indirect
    
    (dolist (params params-shm)
      (destructuring-bind (target name path size bind-cs bind-vs &rest rest) params
	(let ((bo (init-buffer-object target
    				      name
    				      size
    				      bind-vs
    				      t ; pmap
    				      :buffering 'triple)))
	  (setf (gethash name bo-step)
		bo)

	  (when (eq target :texture-buffer)

	    ;; texturei max - GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
	    ;; Already active...
	    ;; (gl:active-texture :texture0)
	    ;; Parse glyph images into texture: (parse-glyphs-ppm bo-texture)
	    
	    ;; Pass additional params for texture - format type
	    (dotimes (i (count-buffers (gethash "texture" bo-step)))
	      (%gl:tex-buffer :texture-buffer
			      :rgba8
			      (aref (buffers (gethash "texture" bo-step)) i)))

	    ;; uniform samplerBuffer msdf;
	    ;; rename to something more relevant...
	    ;; (%gl:uniform-1i (gl:get-uniform-location program-raster "msdf") 0)
	    
	    t))))

    ;; Set data in model and do memcpy
    (let ((data-element (make-array 6
      				    :element-type '(unsigned-byte 32)
      				    :initial-contents (list 0 2 1 0 3 2))))
      (set-bo-element (gethash "element" bo-step)
		      data-element))
    
    ;; Update draw-indirect
    (set-bo-draw-indirect (gethash "draw-indirect" bo-step)
			  6 inst-max 0 0 0)))

(defun update-raster-buffer-bindings ()
  (with-slots (bo-step
	       ix-fence)
      *view*
    ;; indirect needs to be rotated also
    (dolist (name '("projview"
		    "instance"
		    "draw-indirect"))
      (let ((bo (gethash name bo-step)))
	(when (> (count-buffers bo) 1)
	  (update-binding-buffer bo ix-fence))))))

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


(defun read-ppm (filepath)
  (let ((bitmap (make-array (* 96 96 4)
			    :fill-pointer 0
			    :element-type '(unsigned-byte 8))))
    (with-open-file (stream filepath)

      ;; Skip first 3 lines
      (dotimes (i 3)
	(read-line stream nil))
      
      ;; OpenGL textures are row-major order = row/column,  e.g.3x2:
      ;; -> -> -> \n
      ;; -> -> ->
      ;; Line 0: (0,0)
      ;; Line 1: (1,0)
      ;; Line 2: (2,0)
      ;; Line 3: (0,1)
      ;; Line 4: (1,1)
      ;; Line 5: (2,1)
      ;; Note, however, OpenGL matrices are column-major order,
      ;; somewhat by historical accident
      
      ;; Each line has 12 space delimited values
      ;; 12 / 3 components per pixel = 4 pixels every line

      (loop 
         :for pixels := (read-line stream nil)
	 :while pixels
	 :do (loop :for c :in (str:split-omit-nulls " " pixels)
		:with i := 0
		:do (progn
		      (vector-push (parse-integer c) bitmap)
		      (incf i)
		      (when (= i 3)
			(vector-push 255 bitmap)
			(setf i 0))))))
    bitmap))

(defun parse-glyphs-ppm (buffer)
  (loop
     :for code :from 32 :to 255
     :with msdf-glyphs-path := (merge-pathnames #P"glyphs-msdf/" (asdf:system-source-directory :protoform))
     :for ppm-path := (merge-pathnames (make-pathname :name (write-to-string code) :type "ppm")
				       msdf-glyphs-path)
     :with i := 0
     :with ptr := (aref (ptrs-buffer buffer) 0)
     :do (let* ((bmp (read-ppm ppm-path)))
	   
    	   (assert (= (length bmp) (* 96 96 4)))

	   (loop 
	      :for c :across bmp
	      :do (progn
		    (setf (mem-aref ptr :unsigned-char i) c)
		    (incf i))))))  
