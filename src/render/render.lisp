(in-package :protoform.render)

(defparameter *render* nil)
(defparameter *time-frame-last* 0)
(defparameter *time-gc-last* 0.0)
(defparameter *draw* nil)
(defparameter *serving* t)

(defun fmt-render (str-ctx
		   str
		   &rest rest)
  (apply #'format
	 t ; default to std-out
	 (str:concat (format nil "[RENDER:~a][~a] " (sb-posix:getpid) str-ctx)
		     str)
	 rest))

(defun set-draw (value)
  (setf *draw* value))

(defun set-serving (value)
  (setf *serving* value))

(defclass render ()
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
   (prog-rast-msdf :accessor prog-rast-msdf :initarg :prog-rast-msdf :initform nil)
   (prog-compute :accessor prog-compute :initarg :prog-compute :initform nil)
      
   ;; VAO
   (vaos :accessor vaos :initarg :vaos :initform (make-array 3 :adjustable nil :fill-pointer 0))
   ;; Persistently mapped: projview, instance, texture, element, indirect
   (bo-step :accessor bo-step :initarg :bo-step :initform (make-hash-table :size 6 :test 'equal))
   ;; Cache buffer objects - modified by mmap and input for compute shader
   (bo-cache :accessor bo-cache :initarg :bo-cache :initform (make-hash-table :size 6 :test 'equal))
   
   ;; Sync
   (sync :accessor sync :initarg :sync :initform nil)
   (ix-fence :accessor ix-fence :initarg :ix-fence :initform 0)
   (fences :accessor fences :initarg :fences :initform nil)))

(defun clean-up-render ()
  (with-slots (sock-server
	       sock-client
	       buffer-sock-ptr
	       handles-shm
	       prog-rast-msdf
	       prog-compute
	       vaos
	       bo-step
	       bo-cache
	       fences)
      *render*

    ;; debug info
    (describe *render*)
   
   ;; do fence/wait first if running
   (%gl:memory-barrier :all-barrier-bits)
   (let ((sync (%gl:fence-sync :sync-gpu-commands-complete 0)))
     (wait-buffer sync)
     (%gl:delete-sync sync))
   (loop 
      :for fence :across fences
      :do (unless (null-pointer-p fence)
	    (%gl:delete-sync fence)))
    
    (%gl:use-program 0)
    (%gl:delete-program prog-rast-msdf)
    (%gl:delete-program prog-compute)
        
    (gl:delete-vertex-arrays (loop :for v :across vaos :collect v))

    (loop
       :for name :being :the :hash-keys :of bo-step
       :using (hash-value bo)
       :do (clean-up-buffer-object bo))

    (loop
       :for name :being :the :hash-keys :of bo-cache
       :using (hash-value cache)
       :do (clean-up-buffer-object (buffer cache)))
    
    ;; clean up shm
    (clean-up-handles-shm)
    
    ;; clean up server last
    (when sock-client
      (c-shutdown sock-client +shut-rdwr+)
      (c-close sock-client))
    (when sock-server
      (c-shutdown sock-server +shut-rdwr+)
      (c-close sock-server))
    (foreign-free buffer-sock-ptr))

  (fmt-render "clean-up-render" "Render process exiting!~%")
  (force-output)
  (sb-ext:exit))

(defun run-render (width
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
    (fmt-render "run-view" "Window Position: ~S~%" (glfw:get-window-position (glfw:get-current-context)))
    
    (init-gl-env width height)
    (calc-opengl-usage)
    
    (setf *render*
	  (make-instance 'render
			 :width width
			 :height height
			 :inst-max inst-max
			 :sock-server (init-sock-server "/tmp/protoform-view.socket" :nonblock)
			 :prog-rast-msdf (init-prog-rast-msdf)
			 :prog-compute (init-prog-compute)
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
    (send-message (sock-client *render*)
    		  (buffer-sock-ptr *render*)
		  (format nil "(handle-view-sync ~S)" time-frame)))

  ;; GC before waiting on model process
  ;; (sb-ext:gc)
  
  ;; MSG_WAITALL	0x100
  (run-server #x100))

(defun run-programs-shader ()
  (with-slots (sync
	       fences
	       ix-fence)
      *render*

    ;; (fmt-render "run-programs-shader" "ix-fence: ~a~%" ix-fence)
    
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

    ;; Server will do shm->cache
    
    (if t
	(run-compute-bypass)
	(progn
	  ;; Manually do uniforms/projview
	  (update-cache-to-step-pv)	  
	  (run-compute)))
  
    ;; Have run-raster run all programs in a specified order
    (run-raster-msdf)
    
    (when sync
      ;; Create fence, which previous portion will check for
      (setf (aref fences ix-fence)
    	    (%gl:fence-sync :sync-gpu-commands-complete 0)))

    ; 3 defconstant +fences-max+
    (setf ix-fence (mod (+ ix-fence 1) 3))))
	
(defun update-cache-to-step ()
  (with-slots (bo-cache
	       ix-fence)
      *render*
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
       		 (fmt-render "update-cache-to-step" "Cache status: ~a, ~a~%" name flag-copy)
       		 (memcpy-cache-to-step name ix-fence
       				       name
       				       nil
       				       nil) ; no print
       		 (when (> flag-copy 0)
       		   (decf flag-copy)))))))

(defun update-cache-to-step-pv ()
  (with-slots (bo-cache
	       ix-fence)
      *render*
    ;; https://stackoverflow.com/questions/28704818/how-can-i-write-to-a-texture-buffer-object
    ;; Shader can't copy instance textures due to different image sizes
    ;; Indices remain the same
    ;; Exception below is instance
    (let ((cache (gethash "/protoform-projview" bo-cache)))
      (with-slots (buffer flag-copy)
       	  cache
       	(unless (zerop flag-copy)
       	  (fmt-render "update-cache-to-step" "~a, ~a~%" "/protoform-projview" flag-copy)
       	  (memcpy-cache-to-step "/protoform-projview" ix-fence
       				"/protoform-projview"
       				nil
       				nil) ; no print
       	  (when (> flag-copy 0)
       	    (decf flag-copy)))))))

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
