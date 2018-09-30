(in-package :protoform.model)

;; /proc/sys/net/core/rmem_default for recv and /proc/sys/net/core/wmem_default
;; 212992
;; They contain three numbers, which are minimum, default and maximum memory size values (in byte), respectively.

(defun align-size (size &optional (boundary 4))
  (+ size (- boundary (mod size boundary))))

(defun fmt-model (dst ctx-str ctl-str &rest rest)
  ;; Add space opt
  (apply #'format
	 dst
	 (str:concat (format nil "[PID:~a,model][~a] " (sb-posix:getpid) ctx-str)
		     ctl-str)
	 rest))

;; rename to graph or root?
(defclass model ()
  ((width :accessor width :initarg :width :initform nil)
   (height :accessor height :initarg :height :initform nil)
   (inst-max :accessor inst-max :initarg :inst-max :initform nil)

   (handles-shm :accessor handles-shm :initarg :handles-shm :initform (make-hash-table :size 6 :test 'equal))

   (dpi-glyph :accessor dpi-glyph :initarg :dpi-glyph :initform nil)
   (scale-glyph :accessor scale-glyph :initarg :scale-glyph :initform nil)
   (cursor :accessor cursor :initarg :cursor :initform nil)
   
   ;; Integrate these into corresponding slot - data-model
   (sl-chrs :accessor sl-chrs :initarg :sl-chrs :initform nil)
   (lock-sl-chrs :accessor lock-sl-chrs :initarg :lock-sl-chrs :initform (bt:make-lock))
   (projview :accessor projview :initarg :projview :initform nil)))

   ;; Make class - integrate with conn? (inc cohesion)
   ;; (epoll-events :accessor epoll-events :initarg :epoll-events :initform nil)
   ;; (epoll-fd :accessor epoll-fd :initarg :epoll-fd :initform nil)
   ;; (queue-chrs :accessor queue-chrs :initarg :queue-chrs :initform (make-queue))
   ;; (queue-pv :accessor queue-pv :initarg :queue-pv :initform (make-queue))   
   ;; (metrics :accessor metrics :initarg :metrics :initform nil)
   ;; (dpi-glyph :accessor dpi-glyph :initarg :dpi-glyph :initform nil)
   ;; (scale-glyph :accessor scale-glyph :initarg :scale-glyph :initform nil)
   ;; (cursor :accessor cursor :initarg :cursor :initform nil))

;; For now, determine these through view - maybe model can request from view?
;; GL_MAX_SHADER_STORAGE_BLOCK_SIZE = 134217728 = 134.217728 MBs
;; GL_MAX_TEXTURE_BUFFER_SIZE       = 134217728 = 134.217728 MBs
;;
;; Or pass 0/-1 to determine max?
;;
;; Make class slots?
;; Add buffering: single double triple
(defparameter *params-shm* (list (list :uniform-buffer
				       "projview"
				       "/protoform-projview"
				       (align-size (* (+ 16 16 16) 4 1))
				       0 0) ; cs-in, vs-in
				 (list :shader-storage-buffer
				       "instance"
				       "/protoform-instance"
				       134217728				       
				       1 2)
				 (list :texture-buffer ; requires fmt type
				       "texture"
				       "/protoform-texture"
				       134217728
				       -1 -1)
				 (list :element-array-buffer
				       "element"
				       "/protoform-element"
				       (* 4 6)  ; 4 bytes/int * 6 ints or indices
				       -1 -1)
				 (list :draw-indirect-buffer
				       "draw-indirect"
				       "/protoform-draw-indirect"
				       (* 4 6)  ; 6 ints/params
				       -1 -1))) 
;; Do atomic counter also?


(defun init-model (width
		   height
		   inst-max
		   path-server-model)

  (let* ((model (make-instance 'model
			       :width width
			       :height height
			       :projview (make-instance 'projview
							:width width
							:height height
							:projection-type 'orthographic)
			       :dpi-glyph (/ 1 90)
			       :scale-glyph 10.0			       
			       :inst-max inst-max))
	 (handles-shm (handles-shm model))
	 (projview (projview model)))
    
    ;; Init shms, request view to mmap
    (fmt-model t "main-model" " Initializing mmaps...~%")
    (init-handle-shm handles-shm *params-shm*)
    
    ;; Init data for shms
    (with-slots (ptr size)
	(gethash "projview" handles-shm)
      (update-projection-matrix projview)
      (update-view-matrix projview)
      ;; (write-matrix (view-matrix (projview model)) t)
      (set-projection-matrix ptr (projection-matrix projview))
      (set-view-matrix ptr (view-matrix projview))
      (let ((b (+ 16 16))
	    (data (init-vector-position 1)))
	(dotimes (i (length data))
	  (setf (mem-aref ptr :float (+ b i))
		(aref data i)))))

    ;; Init other data
    ;; ;; Pass initial data for these in separate RPC call from model
    ;; ;; Well, it'll do shm copy before draw flag
    ;; ;; Update element
    ;; (let ((data-element (make-array 6
    ;; 				      :element-type '(unsigned-byte 32)
    ;; 				      :initial-contents (list 0 2 1 0 3 2))))
    ;; 	(set-bo-element (gethash "element" bo-step)
    ;; 			data-element))
    
    ;; ;; Update draw-indirect
    ;; (set-bo-draw-indirect (gethash "draw-indirect" bo-step)
    ;; 			    6 inst-max 0 0 0))))
    
    model))

(defun init-text (model)

  (let* ((cursor (vec3 0.0 0.0 0.0))
	 (inst-node (init-node cursor
			       (scale-glyph model)
			       #\X))
	 (offset-ptr 0))
    
    (with-slots (ptr size)
	(gethash "instance" (handles-shm model))
      (with-slots (chr
		   model-matrix
		   rgba
		   uv
		   flags)
	  inst-node
	
	(loop
	   :for c :across (marr (matrix model-matrix))
	   :for c-i :upfrom 0
	   :do (setf (mem-aref ptr :float (+ offset-ptr c-i))
		     c))
	(incf offset-ptr 16)
	
	(loop
	   :for c :across rgba
	   :for c-i :upfrom 0
	   :do (setf (mem-aref ptr :float (+ offset-ptr c-i))
		     c))
	(incf offset-ptr 16)

	;; TODO:
	;; Adjust UVs based on texture rather than from metrics
	;; (u v s t) * 4
	(loop
	   :for c :across uv
	   :for c-i :upfrom 0
	   :do (setf (mem-aref ptr :float (+ offset-ptr c-i))
		     c))
	(incf offset-ptr 8)
	
	;; Glyph, Flags, pad, pad
	(setf (mem-aref ptr :int (+ offset-ptr 0)) (- (char-code chr) 32))
	(setf (mem-aref ptr :int (+ offset-ptr 1)) flags)
	(incf offset-ptr 4)))))  

(defun init-layout (model)

  ;; Create a cairo context for pango layout
  (let* ((width-pango (foreign-alloc :unsigned-int))
	 (height-pango (foreign-alloc :unsigned-int))
	 (surface-temp (cairo:create-image-surface :argb32 0 0))
	 (context-layout (cairo:create-context surface-temp))
	 (layout (pango:pango_cairo_create_layout (slot-value context-layout 'cairo:pointer))))
    ;; Pango layout
    ;; This function is the most convenient way to use Cairo with Pango,
    ;; however it is slightly inefficient
    ;; since it creates a separate PangoContext object for each layout.
    ;; This might matter in an application that was laying out large amounts of text.

    ;; To reuse pango context:
    ;; - Create pango context: pango_cairo_create_context(fontmap) - use default font map
    ;;   - Lower: pango_cairo_font_map_get_default(void) -> pango_font_map_create_context(fontmap)
    ;; - create layout:        pango_layout_new(ctx)

    ;; Every node will have its own layout (so effectively each text node is a pango layout?)
    ;; but will share context/fontmap, unless context becomes its own node also
    ;; Ultimately user can refine this process by attaching a context/fontmap node to layout
    
    ;; Destroy the surface as it is not actually needed
    (cairo:destroy surface-temp)
    
    ;; Set the font/text
    
    ;; (pango:pango_layout_set_text layout "R" -1)
    ;; (let* ((desc (pango:pango_font_description_from_string "Inconsolata-g 72"))) ;"Sans Bold 72")))
    ;;   (pango:pango_layout_set_font_description layout desc)
    ;;   (pango:pango_font_description_free desc))
    
    (let ((text-2 "<span foreground=\"#FFCC00\" font=\"Inconsolata-g 72\" strikethrough=\"true\">X</span>")) ; 100, 80, 0
      ;; b = 100, g = 80, r = 0
      (pango:pango_layout_set_markup layout text-2 -1))
    
    ;; Once text is laidout, get dimensions
    (pango:pango_layout_get_size layout
				 width-pango
				 height-pango)
    ;; Divide by pango scale to get dimensions in pixels
    (setf (mem-ref width-pango :unsigned-int) (/ (mem-ref width-pango :unsigned-int) pango:PANGO_SCALE))
    (setf (mem-ref height-pango :unsigned-int) (/ (mem-ref height-pango :unsigned-int) pango:PANGO_SCALE))

    ;; typedef enum _cairo_format {
    ;;     CAIRO_FORMAT_INVALID   = -1,
    ;;     CAIRO_FORMAT_ARGB32    = 0,
    ;;     CAIRO_FORMAT_RGB24     = 1,
    ;;     CAIRO_FORMAT_A8        = 2,
    ;;     CAIRO_FORMAT_A1        = 3,
    ;;     CAIRO_FORMAT_RGB16_565 = 4
    ;; } cairo_format_t;
    
    ;; Create cairo context/surface to render to
    (let* ((size-data (* (mem-ref width-pango :unsigned-int)
			 (mem-ref height-pango :unsigned-int)
			 4))
	   ;; (data-surface-render (foreign-alloc :unsigned-char :count size-data))
	   ;; (stride (* 4 (mem-ref width-pango :unsigned-int)))
	   (stride (cairo::cairo_format_stride_for_width 0 (mem-ref width-pango :unsigned-int))) ; send patch upstream
	   (surface-render (cairo:create-image-surface-for-data (ptr (gethash "texture" (handles-shm model))) ;data-surface-render
								:argb32
								(mem-ref width-pango :unsigned-int)
								(mem-ref height-pango :unsigned-int)
								stride))
	   (context-render (cairo:create-context surface-render)))
      
      ;; 0.002803 seconds = 2.2 to 9.3 ms
      
      ;; Set surface color - similar to glClear
      (cairo:set-source-rgba 1 1 1 1 context-render)

      ;; Draw
      (pango:pango_cairo_show_layout (slot-value context-render 'cairo:pointer) layout)

      ;; Ensure surface ops are flushed before accessing memory
      ;; (cairo:surface-flush surface-render)
      ;; (cairo:surface-write-to-png surface "/home/user/pango-test.png")
      ;; (sb-ext:exit)

      ;; Watchout for memory layout:
      ;; OpenGL: RGBA
      ;; CAIRO_FORMAT_ARGB32: BGRA

      ;; Copy surface ptr to shm ptr
      ;; Can also render directly to ptr
      
      ;; (with-slots (ptr size)
      ;;     (gethash "texture" (mapping-base model))
      ;;   (assert (<= size-data size))
      ;;   (c-memcpy ptr
      ;; 	    (cairo:image-surface-get-data (cairo:image-surface-create-from-png "/home/user/pango-test2.png")
      ;; 					  :pointer-only t)
      ;; 	    size-data))

      (when nil
	(with-slots (ptr size)
	    (gethash "texture" (handles-shm model))
	  (assert (<= size-data size))
	  (c-memcpy ptr
		    data-surface-render
		    size-data)))

      ;; Clean up
      ;; (foreign-free data-surface-render)
      
      (pango:g_object_unref layout)
      (cairo:destroy context-layout)
      (cairo:destroy context-render)
      (cairo:destroy surface-render))))

(defun setup-view (model)
  
  ;; Init view buffers and start loop
  (let ((conn (init-swank-conn "skynet" 10001)))
    
    (setf (swank-protocol::connection-package conn) "protoform.view")

    ;; (format t "[model] Send eval~%")
    
    ;; view should not concern itself with inst-max...
    (with-slots (width height inst-max) model
	(swank-protocol:request-listener-eval conn
      					      (format nil "(setf *view* (init-view-programs ~S ~S ~S))" width height inst-max)))
    ;; (format t "[model] Wait for eval~%")
    (fmt-model t "main-model" "~a~%" (swank-protocol:read-message conn))
    
    ;; Init buffers
    ;; Need to standardize view buffer creation params
    (swank-protocol:request-listener-eval conn
					  (format nil "(init-view-buffers `(~S ~S ~S ~S ~S))"
						  (first *params-shm*)
						  (second *params-shm*)
						  (third *params-shm*)
						  (fourth *params-shm*)
						  (fifth *params-shm*)))
    ;; (format t "[model] Wait for eval~%")
    (fmt-model t "main-model" "~a~%" (swank-protocol:read-message conn))
    
    ;; Do progn to chain them?
    (dolist (params *params-shm*)
      (destructuring-bind (target name path size bind-cs bind-vs) params      
        (with-slots (ptr size)
    	    (gethash name (handles-shm model))
	  (fmt-model t "main-model" "(serve-memcpy ~S ~S ~S)~%" name name size)
	  (swank-protocol:request-listener-eval conn
						(format nil "(serve-memcpy ~S ~S ~S)" name name size))
	  (fmt-model t "main-model" "~a~%" (swank-protocol:read-message conn)))))

    ;; Enable draw flag for view loop
    (swank-protocol:request-listener-eval conn
					  (format nil "(setf *draw* t)"))
    ;; (format t "[model] Wait for eval~%")
    (fmt-model t "main-model" "~a~%" (swank-protocol:read-message conn))))

(defun main-model (width height
		   inst-max
		   path-server-model)

  (start-swank-server 10000)
  
  (let* ((model (init-model width
			    height
			    inst-max
			    path-server-model)))

    ;; TODO:
    ;; 1. Test live texture updates
    ;; 2. Use parameters from Pango to set texture size

    (fmt-model t "main-model" "Init data~%")
    (init-text model)
    (init-layout model)
    
    (fmt-model t "main-model" "Init conn to view swank server~%")
    (setup-view model)
    
    (loop (sleep 0.0167))))

(defun handle-escape (model
		      keysym)
  
  (clean-up-model model)
  (glfw:set-window-should-close))

(defun clean-up-model (model)
  (loop 
     :for key :being :the :hash-keys :of (handles-shm model)
     :using (hash-value mmap)
     :do (cleanup-mmap mmap))

  (request-exit (conn-model model))
  
  (format t "[handle-escape] Model/Controller exiting!~%")

  ;; Only for DRM?
  (sb-ext:exit))
