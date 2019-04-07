(in-package :protoform.render)

(defparameter *view* nil)
(defparameter *time-frame-last* 0)
(defparameter *time-gc-last* 0.0)
(defparameter *draw* nil)
(defparameter *serving* t)

(defun fmt-view (dst ctx-str ctl-str &rest rest)
  ;; Add space opt
  (apply #'format
	 dst
	 (str:concat (format nil "[VIEW:~a][~a] " (sb-posix:getpid) ctx-str)
		     ctl-str)
	 rest))

(defclass view ()
  ;; Create a base for these 3 slots?
  ((width :accessor width :initarg :width :initform nil)
   (height :accessor height :initarg :height :initform nil)
   (inst-max :accessor inst-max :initarg :inst-max :initform nil)

   (sock-server :accessor sock-server :initarg :sock-server :initform nil)
   (sock-client :accessor sock-client :initarg :sock-client :initform nil)
   (buffer-sock-ptr :accessor buffer-sock-ptr :initarg :buffer-sock-ptr :initform (foreign-alloc :unsigned-char :count 212992))
   (buffer-sock-array :accessor buffer-sock-array :initarg :buffer-sock-array :initform (make-array 212992
												    :adjustable nil
												    :fill-pointer nil
												    :element-type '(unsigned-byte 8)))
   
   (handles-shm :accessor handles-shm :initarg :handles-shm :initform (make-hash-table :size 6 :test 'equal))
   
   ;; Programs
   ;; program-* -> array/list
   (program-default :accessor program-default :initarg :program-default :initform nil)
   (program-msdf :accessor program-msdf :initarg :program-msdf :initform nil)
   (program-compute :accessor program-compute :initarg :program-compute :initform nil)
      
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
  (with-slots (program-default
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
    (fmt-view t "clean-up-view" "Deleted vertex array ~a~%" boav-main)

    (%gl:use-program 0)
    
    (%gl:delete-program program-default)
    (fmt-view t "clean-up-view" "Deleted program ~a~%" program-default)
    (%gl:delete-program program-compute)
    (fmt-view t "clean-up-view" "Deleted program ~a~%" program-compute)

    (loop 
       :for fence :across fences
       :do (unless (null-pointer-p fence)
	     (%gl:delete-sync fence)
	     (fmt-view t "clean-up-view" "Deleted fence ~a~%" fence)))))

(defun clean-up-buffer-objects (view)
  (dolist (boa (list (bo-projview view)
		     (bo-instance view)
		     (bo-element view)
		     (bo-texture view)))
    do(clean-up-buffer-object boa)))

(defun init-gl-env (width height)
  
  (fmt-view t "init-gl-env" "GL Vendor: ~a~%" (gl:get* :vendor))
  (fmt-view t "init-gl-env" "GL Renderer: ~a~%" (gl:get* :renderer))
  (fmt-view t "init-gl-env" "GL Version: ~a~%" (gl:get* :version))
  (fmt-view t "init-gl-env" "GLSL Version: ~a~%" (gl:get* :shading-language-version))
  
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
     :do (fmt-view t "init-gl-env" "~a = ~a~%" (first pair) (second pair))))
  
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
  
  (fmt-view t "init-view-buffers" "Initializing shm handles~%")
  (init-handles-shm params-model)

  (fmt-view t "init-view-buffers" "Initializing buffer object caches~%")
  (init-bo-caches params-model)

  (fmt-view t "init-view-buffers" "Initializing buffer object steps~%")
  (init-bo-step params-model)

  (fmt-view t "init-view-buffers" "Initializing shader bindings~%")
  ;; Shader specific initialization
  (init-buffers-raster-default params-model)
  (init-buffers-raster-msdf params-model)
  (init-buffers-compute params-model)

  ;; At this point, shm already has data loaded by model
  ;; so copy to OpenGL buffers
  (memcpy-shm-to-all)

  (fmt-view t "init-view-buffers" "Finished initializing shm handles~%"))

(defun run-view (width
		  height
		  inst-max
		  path-server)
    
  (glfw:with-init-window (:title "Protoform"
			  :width width
			  :height height
			  :client-api :opengl-es-api
			  :context-version-major 3
			  :depth-bits 16)
    (glfw:set-window-size-callback 'update-viewport)
    (fmt-view t "run-view" "Window Position: ~S~%" (glfw:get-window-position (glfw:get-current-context)))
    
    (init-gl-env width height)
    (calc-opengl-usage)
    
    (setf *view* (make-instance 'view
				:width width
				:height height
				:inst-max inst-max
				:sock-server (init-sock-server "/tmp/protoform-view.socket" :nonblock)
				:program-default (init-program-default)
				:program-msdf (init-program-msdf)
				:program-compute (init-program-compute)
				:fences (make-array 3
						    :adjustable nil
						    :initial-element (null-pointer))))

    ;; (push #'(lambda ()
    ;; 	      (let ((time-gc (coerce (/ sb-ext:*gc-run-time* internal-time-units-per-second)
    ;; 				     'single-float)))
    ;; 		(format t
    ;; 			"[view] Time GC: ~4$ ms, ~4$ s~%"
    ;; 			(* (- time-gc *time-gc-last*) 1000)
    ;; 			time-gc)
    ;; 		(setf *time-gc-last* time-gc)
    ;; 		;; (sleep 2)
    ;; 		t))
    ;; 	  sb-kernel::*after-gc-hooks*)

    (sb-ext:gc :full t)    
    ;; (setf (sb-ext:bytes-consed-between-gcs) (* 2 1024))
    
    (loop 
       (if *draw*
	   (render-frame)
	   (run-server-sleep)))))

(defun run-server-sleep ()
  (sleep (/ 1 144))
  (run-server))

(defun render-frame ()
  (run-programs-shader)
  (glfw:poll-events)
  (glfw:swap-buffers)

  ;; This should be fine as long as memory is not exhausted - exceptional case?
  ;; Goal: GC during glClear since that's where process blocks on GPU
  ;; (sb-ext::without-gcing
  
  (let ((time-frame (osicat:get-monotonic-time)))
    ;; (format t
    ;; 	    "Time frame: ~4$ ms~%"
    ;; 	    (* (- time-frame *time-frame-last*) 1000))
    (setf *time-frame-last* time-frame)

    ;; Send frame
    (send-message (sock-client *view*)
    		  (buffer-sock-ptr *view*)
		  (format nil "(handle-view-sync ~S)" time-frame)))

  ;; GC before waiting on model process
  ;; (sb-ext:gc)
  
  ;; MSG_WAITALL	0x100
  (run-server #x100))

(defun run-programs-shader ()
  (with-slots (sync
	       fences
	       ix-fence)
      *view*

    ;; (fmt-view t "run-programs-shader" "ix-fence: ~a~%" ix-fence)
    
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

    ;; Server will do shm->cache
    
    (if t
	(run-compute-bypass)
	(progn
	  ;; Manually do uniforms/projview
	  (update-cache-to-step-pv)	  
	  (run-compute)))
  
    ;; Have run-raster run all programs in a specified order
    (run-raster)
    
    (when sync
      ;; Create fence, which previous portion will check for
      (setf (aref fences ix-fence)
    	    (%gl:fence-sync :sync-gpu-commands-complete 0)))

    ; 3 defconstant +fences-max+
    (setf ix-fence (mod (+ ix-fence 1) 3))))

(defun set-draw (value)
  (setf *draw* value))

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

(defun run-server (&optional (flags 0))
  (with-slots (sock-server sock-client)
      *view*
    (if sock-client
	(progn
	  (set-serving t)
	  (serve-client flags))
	(multiple-value-bind (sock-accept errno)
	    (accept4 sock-server :nonblock) ;non block
	  (when (/= sock-accept -1)
	    (fmt-view t "main-view" "Accepted connection: ~a~%" sock-accept)
	    (setf sock-client sock-accept))))))

(defun serve-client (&optional (flags 0))
  (with-slots (sock-client
	       buffer-sock-ptr)
      *view*
    (loop
       :while *serving*
       :do (let ((message (recv-message sock-client
					buffer-sock-ptr
					flags)))
	     (when message
	       ;; (fmt-view t "serve-client" "Message: ~S~%" message)
	       ;; (print (eval message))
	       ;; (force-output)
	       
	       ;; To do multiple check if first is a list
	       (if (listp (first message))
		   (dolist (n message)
		     (apply (symbol-function (find-symbol (string (first n)) :protoform.render))
			    (cdr n)))
		   (apply (symbol-function (find-symbol (string (first message)) :protoform.render))
			  (cdr message))))))))

(defun set-serving (value)
  (setf *serving* value))

(defun pass ())
;; check errno at end?
;; handle special forms
	
(defun update-cache-to-step ()
  (with-slots (bo-cache
	       ix-fence)
      *view*
    ;; https://stackoverflow.com/questions/28704818/how-can-i-write-to-a-texture-buffer-object
    ;; Shader can't copy instance textures due to different image sizes
    ;; Indices remain the same
    ;; Exception below is instance
    (loop 
       :for name :being :the :hash-keys :of bo-cache
       :using (hash-value cache)
       :do ;; (unless (string= name "/protoform-nodes") ;; why???
       	     (with-slots (buffer flag-copy)
       		 cache
       	       (unless (zerop flag-copy)
       		 (fmt-view t "update-cache-to-step" "Cache status: ~a, ~a~%" name flag-copy)
       		 (memcpy-cache-to-step name ix-fence
       				       name
       				       nil
       				       nil) ; no print
       		 (when (> flag-copy 0)
       		   (decf flag-copy)))))))

(defun update-cache-to-step-pv ()
  (with-slots (bo-cache
	       ix-fence)
      *view*
    ;; https://stackoverflow.com/questions/28704818/how-can-i-write-to-a-texture-buffer-object
    ;; Shader can't copy instance textures due to different image sizes
    ;; Indices remain the same
    ;; Exception below is instance
    (let ((cache (gethash "/protoform-projview" bo-cache)))
      (with-slots (buffer flag-copy)
       	  cache
       	(unless (zerop flag-copy)
       	  (fmt-view t "update-cache-to-step" "~a, ~a~%" "/protoform-projview" flag-copy)
       	  (memcpy-cache-to-step "/protoform-projview" ix-fence
       				"/protoform-projview"
       				nil
       				nil) ; no print
       	  (when (> flag-copy 0)
       	    (decf flag-copy)))))))
