(in-package :protoform.view)

(defun fmt-view (dst ctx-str ctl-str &rest rest)
  ;; Add space opt
  (apply #'format
	 dst
	 (str:concat (format nil "[VIEW:~a][~a] " (sb-posix:getpid) ctx-str)
		     ctl-str)
	 rest))

(defparameter *view* nil)
(defparameter *draw* nil)

(defclass view ()
  ;; Create a base for these 3 slots?
  ((width :accessor width :initarg :width :initform nil)
   (height :accessor height :initarg :height :initform nil)
   (inst-max :accessor inst-max :initarg :inst-max :initform nil)
   
   ;; Programs
   (program-raster :accessor program-raster :initarg :program-raster :initform nil)
   (program-compute :accessor program-compute :initarg :program-compute :initform nil)
   
   (handles-shm :accessor handles-shm :initarg :handles-shm :initform (make-hash-table :size 6 :test 'equal))
   
   ;; VAO
   (vaos :accessor vaos :initarg :vaos :initform (make-array 3 :adjustable nil :fill-pointer 0))
   ;; Persistently mapped: projview, instance, texture, element, indirect
   (bo-step :accessor bo-step :initarg :bo-step :initform (make-hash-table :size 6 :test 'equal))
   ;; Cache buffer objects - modified by mmap and input for compute shader
   (bo-cache :accessor bo-cache :initarg :bo-cache :initform (make-hash-table :size 6 :test 'equal))
   
   ;; Sync
   (sync :accessor sync :initarg :sync :initform nil)
   (fences :accessor fences :initarg :fences :initform nil)
   (ix-fence :accessor ix-fence :initarg :ix-fence :initform 0)))

(defun clean-up-view (view)
  (with-slots (program-raster
	       program-compute
	       boav-main
	       boa-uniform-projview
	       fences)
      view
    (describe view)

    ;; do fence/wait first if running
    (%gl:memory-barrier :all-barrier-bits)
    (let ((sync (%gl:fence-sync :sync-gpu-commands-complete 0)))
      (wait-buffer sync)
      (%gl:delete-sync sync))
    
    ;; below fn includes mmaps
    (clean-up-handles-shm view)
    (clean-up-buffer-objects view)
    
    (gl:delete-vertex-arrays (list boav-main))
    (format t "[clean-up-view] Deleted vertex array ~a~%" boav-main)

    (%gl:use-program 0)
    
    (%gl:delete-program program-raster)
    (format t "[clean-up-view] Deleted program ~a~%" program-raster)
    (%gl:delete-program program-compute)
    (format t "[clean-up-view] Deleted program ~a~%" program-compute)

    (loop 
       :for fence :across fences
       :do (unless (null-pointer-p fence)
	     (%gl:delete-sync fence)
	     (format t "[clean-up-view] Deleted fence ~a~%" fence)))))

(defun clean-up-buffer-objects (view)
  (dolist (boa (list (bo-projview view)
		     (bo-instance view)
		     (bo-element view)
		     (bo-texture view)))
    do(clean-up-buffer-object boa)))

(defun init-gl-env (width height)
  
  (format t "[init-gl] GL Vendor: ~a~%" (gl:get* :vendor))
  (format t "[init-gl] GL Renderer: ~a~%" (gl:get* :renderer))
  (format t "[init-gl] GL Version: ~a~%" (gl:get* :version))
  (format t "[init-gl] GLSL Version: ~a~%" (gl:get* :shading-language-version))
  
  ;; Get screen dimensions from drm
  (gl:viewport 0 0 width height)
  
  (gl:enable :cull-face)
  (gl:enable :depth-test)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  
  (%gl:clear-color 0.0
		   (coerce (/ 43 255) 'single-float)
		   (coerce (/ 54 255) 'single-float)
		   0.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  
  (print-gl-max))

(defun init-view-programs (width
			   height
			   inst-max)
  (make-instance 'view
		 :width width
		 :height height
		 :fences (make-array 3
				     :adjustable nil
				     :initial-element (null-pointer))
		 :program-raster (init-program-raster)
		 :program-compute (init-program-compute)
		 :inst-max inst-max))

(defun init-bo-step (params-shm)

  (with-slots (width height
		     boav-main
		     bo-step
		     inst-max)
      *view*

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
			   name
			   path
			   size
			   bind-cs
			   bind-vs
			   count-buffer
			   &rest rest)
	  params
	
	(let ((bo (init-buffer-object target
    				      name
    				      size
    				      (if (> bind-vs -1) bind-vs bind-cs)
    				      t ; pmap
    				      :buffering count-buffer)))
	  (setf (gethash name bo-step)
		bo)

	  (when (eq target :texture-buffer)

	    ;; texturei max - GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
	    ;; Already active...
	    ;; (gl:active-texture :texture0)
	    ;; Parse glyph images into texture: (parse-glyphs-ppm bo-texture)
	    
	    (dotimes (i (count-buffers (gethash "texture" bo-step)))
	      (%gl:tex-buffer :texture-buffer
			      (first rest) ; rgba8
			      (aref (buffers (gethash "texture" bo-step)) i)))
	    
	    t))))))

(defun init-view-buffers (params-model)

  (fmt-view t "init-view" "Initializing shm handles~%")
  (init-handles-shm params-model)

  (fmt-view t "init-view" "Initializing buffer object caches~%")
  (init-bo-caches params-model)

  (fmt-view t "init-view" "Initializing buffer object steps~%")
  (init-bo-step params-model)

  (fmt-view t "init-view" "Initializing shader bindings~%")
  ;; Shader specific initialization
  (init-buffers-raster params-model)
  (init-buffers-compute params-model)

  ;; At this point, shm already has data loaded by model
  ;; so copy to OpenGL buffers
  (memcpy-shm-to-all))

(defun main-view (width
		  height
		  inst-max
		  path-server)

  (start-swank-server 10001)
    
  (glfw:with-init-window (:title "Protoform"
			  :width width
			  :height height
			  :client-api :opengl-es-api
			  :context-version-major 3
			  :depth-bits 16)
    (glfw:set-window-size-callback 'update-viewport)
    
    (init-gl-env width height)

    (calc-opengl-usage)
    
    ;; Model will connect and execute code in this process      
    ;; Cannot actually draw until buffers

    (format t "[view] Begin loop...~%")
    
    (loop 
       ;; :until (glfw:window-should-close-p)
       :do (progn

	     ;; Recv evals from model
	     ;; http://3bb.cc/tutorials/cl-opengl/getting-started.html
	     ;; (format t "[view] Serve-all-events~%")
	     (sb-sys:serve-all-events 0)
	     
	     (if *draw*
		 (progn
		   ;; (fmt-view t "main-view" "Running...~%")
		   (render-frame)
		   ;;(glfw:poll-events)
		   (glfw:swap-buffers))
		 (sleep 0.0167))))))

(defun render-frame ()
  (with-slots (sync fences ix-fence)
      *view*
    
    ;; Create with-* macro for this
    ;; if sync:
    ;;    need one sync/fence per group of vbos (as long as they are using same index)
    ;;       since vbo is inserted after the draw commands are done
    ;;    remember this is waiting on previous fence
    (when sync
      (let ((fence (aref fences ix-fence)))
	;; check for null...
    	(protoform.opengl::wait-buffer fence)
    	(%gl:delete-sync fence)
    	(setf (aref fences ix-fence) (null-pointer))))

    (run-compute-copy)
    
    (run-raster)
    
    (when sync
      ;; Create fence, which previous portion will check for
      (setf (aref fences ix-fence)
    	    (%gl:fence-sync :sync-gpu-commands-complete 0)))

    (setf ix-fence (mod (+ ix-fence 1) 3)) ; 3 defconstant +fences-max+
    
    ;; Caller swaps...
    t))

(defun calc-opengl-usage ()

  ;; Avg src code file = 512 lines @ 100 graphical chars = 51200
  (let* ((size-per-char 208)
	 (buffer-size 134217728)
	 (inst-mm (floor (/ buffer-size
			    size-per-char))))
      
      ;; State: SHM -> Base -> Rotate Buffers (DRV/CPU/GPU)
      ;; Process:
      ;; 1. Memcpy base buffers (add/rem instances) - not used in drawing
      ;; 2. Compute buffers' (draw flag or cull) - copy from base buffers to draw buffers
      ;; 3. Queue buffers'' in driver (call draw commands)
      ;; 4. Runs program using buffers''' in GPU
      (let ((total-quad (* buffer-size 2 5))) ; n types * n buffers
	(format t "Total RAM (3x buffer): ~A MB~%" (/ total-quad 1024 1024)))
      (format t "Max planes @ 134217728 bytes or 134ish megabytes: ~:d~%" inst-mm)
      (format t "Triangles: ~:d~%" (* inst-mm 2))
      (format t "Vertices: ~:d~%" (* inst-mm 6))      
      (format t "~%")
      (format t "Max glyphs @ 134217728 bytes or 134ish megabytes, (20x56px): ~A ~%" (/ buffer-size 4480))) ; 29,959

  ;; Using overall 1.5 GB with all buffers + SBCL etc.
  
  ;; Limits is either instances or textures (unique glyphs) - whichever is smaller
  ;; 1 char = 20x56 px * = 1120 px^2 * 4 bytes = 4480 bytes
  ;; 536,870,912 / 4480 = 119,837 chars = 27x this todo
  ;; Words could be reused:
  ;; - english - common words
  ;; - code - keywords, symbols, variable names
  

  (format t "-------------------~%"))
