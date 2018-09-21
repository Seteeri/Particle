(in-package :protoform.view)

(defun calc-opengl-usage ()
  ;; 243,148 / 51200 = 4.75 files

  ;; If text is placed in a gridlike structure then transforms can be reused
  ;; For example, if using coordinates for the text:
  ;; First line = (x,0)
  ;; Second line = (x,1)
  ;; Third line = (x,2)

  ;; "uniform"
  ;; - vertex position = 4 XYZW
  ;; Max data per char
  ;; - Model Matrix (16*4)(1 per inst)
  ;;   - translation
  ;;   - scaling
  ;;   - rotation - doesn't change
  ;; - RGBA (16)(4 bytes per vert)
  ;;   - remove alpha? padding might make it 4 anyhow
  ;; - UV (32)(8 bytes per vert)
  ;; - W-UV (4)(1 per inst)
  ;;   - byte unless using more than 256 glyphs
  ;;   - if > 256 then short = 2 bytes
  ;;
  ;; Avg src code file = 512 lines @ 100 graphical chars = 51200
  (let ((size-per-char (+ (* 16 4)
			  (* 4 4)
			  (* 8 4)
			  (* 4 1)
			  (* 4 1)))
	(buffer-size-lower (* 16 1024 1024))
	(buffer-size-upper (* 256 1024 1024))
	(buffer-size-max (* 256 1024 1024))
	(buffer-size-memcpy (* 192 1024 1024)))

    ;; model matrix limiting factor since its biggest per instance data
    (let* ((inst-mm (floor (/ buffer-size-memcpy
			      (+ 64 16 32 4 4)))) ; size per inst

	   (size-mm (/ (* 16 4 inst-mm) 1024 1024))
	   (size-align-mm (+ size-mm (- 4 (mod size-mm 4))))

	   (size-rgba (/ (* 4 4 inst-mm) 1024 1024))
	   (size-align-rgba (+ size-rgba (- 4 (mod size-rgba 4))))

	   (size-uv (/ (* 8 4 inst-mm) 1024 1024))
	   (size-align-uv (+ size-uv (- 4 (mod size-uv 4))))

	   (size-w-uv (/ (* 4 1 inst-mm) 1024 1024))
	   (size-align-w-uv (+ size-w-uv (- 4 (mod size-w-uv 4))))

	   (size-flags (/ (* 4 1 inst-mm) 1024 1024))
	   (size-align-flags (+ size-flags (- 4 (mod size-flags 4)))))

      ;; ~53 source code files open

      (format t "Maximums @192 MB:~%")
      (format t "Quads: ~:d (instances/chars)~%" inst-mm)
      (format t "Triangles: ~:d~%" (* inst-mm 2))
      (format t "Vertices: ~:d~%" (* inst-mm 6))
      (format t "Buffer Object Requirements @192 MB:~%")
      (format t "Model Matrix (16 floats): ~:d MB~%" size-align-mm)
      (format t "RGBA (16 floats): ~:d MB~%" size-align-rgba)
      (format t "UV (8 floats): ~:d MB~%" size-align-uv)
      (format t "W-UV (1 int): ~:d MB~%" size-align-w-uv)
      (format t "Flags (1 int): ~:d MB~%" size-align-flags)
      
      ;; State: Base (Compute + Driver + GPU)
      ;; Process:
      ;; 1. Memcpy base buffers (add/rem instances) - not used in drawing
      ;;    - 192MB for all buffers combined
      ;;    - 192 = MM + RGBA + GI
      ;;    - (* 192 1024 1024) = (* 64 MM) + (* 4b MM) + (* 1b MM) | Total = 64MM + 2MM + 4MM = 70MM
      ;; 2. Compute buffers' (draw flag or cull) - copy from base buffers to draw buffers
      ;; 3. Queue buffers'' in driver (call draw commands)
      ;; 4. Runs program using buffers''' in GPU
      (let ((total-quad (* (+ 3 1) (+ size-align-mm
				      size-align-rgba
				      size-align-uv
				      size-align-w-uv
				      size-align-flags))))
	(format t "Total VRAM required with complete triple buffering: ~A MB~%" total-quad)))


    (format t "-------------------~%")))

;; Rename to view?
(defclass msdf ()
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
   (boav-main :accessor boav-main :initarg :boav-main :initform nil)

   ;; Raster (static)
   ;; Unmapped single
   (boa-element :accessor boa-element :initarg :boa-element :initform nil)
   
   ;; Raster (dynamic)
   (bo-projview :accessor bo-projview :initarg :bo-projview :initform nil)      
   (bo-instance :accessor bo-instance :initarg :bo-instance :initform nil)
   (bo-texture :accessor bo-texture :initarg :bo-texture :initform nil)
   (bo-indirect :accessor bo-indirect :initarg :bo-indirect :initform nil)

   ;; Sync
   (sync :accessor sync :initarg :sync :initform nil)
   (fences :accessor fences :initarg :fences :initform nil)
   (ix-fence :accessor ix-fence :initarg :ix-fence :initform 0)))

(defun clean-up-msdf (msdf)
  (with-slots (program-raster
	       program-compute
	       boav-main
	       boa-uniform-projview
	       fences)
      msdf
    (describe msdf)

    ;; do fence/wait first if running
    (%gl:memory-barrier :all-barrier-bits)
    (let ((sync (%gl:fence-sync :sync-gpu-commands-complete 0)))
      (wait-buffer sync)
      (%gl:delete-sync sync))
    
    ;; below fn includes mmaps
    (clean-up-mapping-base msdf)
    (clean-up-buffer-objects msdf)
    
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

(defun clean-up-buffer-objects (msdf)
  (dolist (boa (list (bo-projview msdf)
		     (bo-instance msdf)
		     (boa-element msdf)
		     (bo-texture msdf)))
    do(clean-up-buffer-object boa)))

(defun init-msdf (width
		  height
		  inst-max)

  (when nil
    (format t "[init-msdf] Instances: ~:D~%" inst-max)
    (format t "[init-msdf] Triangles: ~:D~%" (* inst-max 2))
    (format t "[init-msdf] Vertices: ~:D~%" (* inst-max 6))
    (calc-opengl-usage))
  
  (let ((msdf (make-instance 'msdf
			     :width width
			     :height height
			     :fences (make-array 3
						 :adjustable nil
						 :initial-element (null-pointer))
			     :program-raster (init-program-raster)
			     :program-compute (init-program-compute)
			     :inst-max inst-max)))

    (format t "[init-msdf] Initializing raster buffers...~%")
    (init-buffers-raster msdf)
    
    (format t "[init-msdf] Initializing base buffers...~%")
    (init-mapping-base msdf)
    
    (format t "[init-msdf] Initializing compute buffers...~%")
    (init-buffers-compute msdf)
    
    msdf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main-view (width
		  height
		  inst-max
		  path-server)

  (glfw:with-init-window (:title "Protoform"
				 :width width ;1440
				 :height height ;900
				 :client-api :opengl-es-api
				 :context-version-major 3
				 :depth-bits 16)
    ;; :monitor (glfw:get-primary-monitor))
    ;; (setf %gl:*gl-get-proc-address* #'get-proc-address)
    ;; (glfw:set-key-callback 'quit-on-escape)
    (glfw:set-window-size-callback 'update-viewport)

    ;; Init view
    (let* ((conn-model (init-conn-client path-server))
	   (gles (init-gles width height))
	   (msdf (init-msdf width
			    height
			    inst-max)))

      ;; Connect to server before running
      (setf (sock conn-model) (init-socket-client path-server t))
      ;; Send client type
      (send-message (sock conn-model)
		    (buffer-ptr-send conn-model)
		    ":view")

      (loop 
	 ;; :until (glfw:window-should-close-p)
	 :do (progn

	       (request-server conn-model
			       msdf)
	       (run-view msdf
			 conn-model)

	       ;;(glfw:poll-events)
	       (glfw:swap-buffers))))))

(defun run-view (msdf
		 conn-client)

  (with-slots (sync fences ix-fence)
      msdf
    
    ;; Create with-* macro for this
    ;; if sync:
    ;;    need one sync/fence per group of vbos (as long as they are using same index)
    ;;       since vbo is inserted after the draw commands are done
    ;;    remember this is waiting on previous fence
    (when sync
      (let ((fence (aref fences ix-fence)))
	;; check for null...
    	(wait-buffer fence)
    	(%gl:delete-sync fence)
    	(setf (aref fences ix-fence) (null-pointer))))

    ;; Dispatch compute shader; process instances from base buffer to render buffers
    (run-compute-copy msdf)
    
    (run-raster msdf)
    
    (when sync
      ;; Create fence, which previous portion will check for
      (setf (aref fences ix-fence)
    	    (%gl:fence-sync :sync-gpu-commands-complete 0)))

    (setf ix-fence (mod (+ ix-fence 1) 3)) ; 3 defconstant +fences-max+
    
    ;; Caller swaps...
    t))

(defun sync-gl ()
  ;; Compute shader performs "incoherent memory accesses":
  ;; - Writes (atomic or otherwise) via Image Load Store
  ;; - Writes (atomic or otherwise) via Shader Storage Buffer Objects
  ;; - Writes to variables declared as shared in Compute Shaders (but not output variables in Tessellation Control Shaders)    
  ;; Issuing a barrier ensure writes are completed
  ;; Issue memory-barrier + fence, then wait for fence
  ;; (%gl:memory-barrier :vertex-attrib-array-barrier-bit)
  ;; (%gl:memory-barrier :atomic-counter-barrier-bit)
  ;; (%gl:memory-barrier :shader-storage-barrier-bit)
  (%gl:memory-barrier :all-barrier-bits)
  (let ((sync (%gl:fence-sync :sync-gpu-commands-complete 0)))
    (wait-buffer sync)
    (%gl:delete-sync sync)))

;; REFACTOR BELOW
;; Rename to wait-fence
(defun wait-buffer (sync)
  (unless (null-pointer-p sync)
    ;; check first then wait
    (let* ((wait-flags 0)
	   (wait-duration 0)
	   (wait-return nil))
      (loop
	 :do(progn
	      
	      (setf wait-return (%gl:client-wait-sync sync wait-flags wait-duration))

	      (when (or (eq wait-return :already-signaled-apple) (eq wait-return :condition-satisfied-apple))
		(return))
	      (when (eq wait-return :wait-failed-appled)
		(error "wait-buffer: client-wait-sync returned :wait-failed")
		(return))
	      
	      ;; After the first time, need to start flushing, and wait for a looong time.
	      (setf wait-flags :sync-flush-commands-bit-apple)
	      (setf wait-duration #xFFFFFFFFFFFFFFFF))))))

(defun wait-buffer-2 (sync)
  (unless (null-pointer-p sync)
    (loop :while t
       :for wait-return := (%gl:client-wait-sync sync :sync-flush-commands-bit 1)
       :do (when (or (eq wait-return :already-signaled-apple)
		     (eq wait-return :condition-satisfied-apple))
		(return)))))
