(in-package :protoform.view)

(defparameter *view* nil)
(defparameter *draw* nil)

(defun fmt-view (dst ctx-str ctl-str &rest rest)
  ;; Add space opt
  (format dst (str:concat "[model:" ctx-str "] " ctl-str) rest))

(defclass view ()
  ;; Create a base for these 3 slots?
  ((width :accessor width :initarg :width :initform nil)
   (height :accessor height :initarg :height :initform nil)
   (inst-max :accessor inst-max :initarg :inst-max :initform nil)
   
   ;; Programs
   (program-raster :accessor program-raster :initarg :program-raster :initform nil)
   (program-compute :accessor program-compute :initarg :program-compute :initform nil)

   ;; memcpy has 2 options
   ;; 1. mmap to compute (base)
   ;; 2. mmap to raster
   
   ;; Base buffers - modified by mmap and input for compute shader
   ;;
   ;; POSS: Initialize table with default values - make explicit so easier to understand
   ;; POSS: Integrate with step values
   ;;
   ;; Only bound to ssbo
   (mapping-base :accessor mapping-base :initarg :mapping-base :initform (make-hash-table :size 1 :test 'equal))

   ;; flags - compute only...
   ;; counter - compute only...
   
   ;; Counter is not a base since it is not modified by mmap; however it is used by compute shader
   ;; Counter does not step since it is in sync with compute shader
   ;; Mapped single
   ;; Only bound to ssbo
   ;; Maybe merge into mapping-base and add flag to decide whether to update binding
   (bo-counter :accessor bo-counter :initarg :bo-counter :initform nil)
   
   ;; VAO
   ;; Note, VAOs not shared between contexts
   (boav-main :accessor boav-main :initarg :boav-main :initform nil)
   
   ;; Persistently mapped
   (bo-element :accessor bo-element :initarg :bo-element :initform nil)
   (bo-projview :accessor bo-projview :initarg :bo-projview :initform nil)      
   (bo-instance :accessor bo-instance :initarg :bo-instance :initform nil)
   (bo-texture :accessor bo-texture :initarg :bo-texture :initform nil)
   (bo-indirect :accessor bo-indirect :initarg :bo-indirect :initform nil)

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
    (clean-up-mapping-base view)
    (clean-up-buffer-objects view)
    
    (gl:delete-vertex-arrays (list boav-main))
    (format t "[clean-up-msdf] Deleted vertex array ~a~%" boav-main)

    (%gl:use-program 0)
    
    (%gl:delete-program program-raster)
    (format t "[clean-up-msdf] Deleted program ~a~%" program-raster)
    (%gl:delete-program program-compute)
    (format t "[clean-up-msdf] Deleted program ~a~%" program-compute)

    (loop 
       :for fence :across fences
       :do (unless (null-pointer-p fence)
	     (%gl:delete-sync fence)
	     (format t "[clean-up-msdf] Deleted fence ~a~%" fence)))))

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

(defun init-view-buffers (params)

  ;; (format t "[init-view] params: ~S~%" params)

  (format t "[init-view] Initializing base buffers...~%")
  (init-mapping-base params)
  
  (format t "[init-view] Initializing raster buffers...~%")
  (init-buffers-raster params)
    
  (format t "[init-view] Initializing compute buffers...~%")
  (init-buffers-compute params))

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
		   (run-view)
		   ;;(glfw:poll-events)
		   (glfw:swap-buffers))
		 (sleep 0.0167))))))

(defun run-view ()
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

    ;; Dispatch compute shader; process instances from base buffer to render buffers
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
