(in-package :protoform.model)

(defun align-size (size &optional (boundary 4))
  (+ size (- boundary (mod size boundary))))

(defclass model ()
  ((width :accessor width :initarg :width :initform nil)
   (height :accessor height :initarg :height :initform nil)
   ;; still relevant?
   (inst-max :accessor inst-max :initarg :inst-max :initform nil)
   (projview :accessor projview :initarg :projview :initform nil)
   
   (handles-shm :accessor handles-shm :initarg :handles-shm :initform (make-hash-table :size 6 :test 'equal))

   ;; Textures - list of Texture instances wich store tex parameters
   ;; Use skip list? -> For now use vector
   ;; Hmm, when texture is removed need to recopy all (to "defragment")
   (offset-textures :accessor offset-textures :initarg :offset-textures :initform 0)
   (textures :accessor textures :initarg :textures :initform (make-array 64 :adjustable t))
   
   (cursor :accessor cursor :initarg :cursor :initform (vec3 0 0 0))
   ;; move to node? used in conjunction with scale-node
   (dpi-glyph :accessor dpi-glyph :initarg :dpi-glyph :initform (/ 1 90))
   ;; rename to scale-default-node
   (scale-node :accessor scale-node :initarg :scale-node :initform 1.0)))

;; For now, determine these through view - maybe model can request from view?
;; GL_MAX_SHADER_STORAGE_BLOCK_SIZE = 134217728 = 134.217728 MBs
;; GL_MAX_TEXTURE_BUFFER_SIZE       = 134217728 = 134.217728 MBs
;;
;; Or pass 0/-1 to determine max?
;;
;; Make class slots? -> Harder to be dynamic
;; Add buffering: single double triple - default to triple
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
				       -1 -1
				       :rgba8)
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
			       :inst-max inst-max))
	 (handles-shm (handles-shm model))
	 (projview (projview model)))
    
    ;; Init shms, request view to mmap
    (fmt-model t "main-model" " Initializing shm...~%")
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
    
    ;; Set other buffer data
    
    model))

(defun generate-text-texture (model
			 text-pm)

  ;; Return index, width, height
  
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
    
    ;; (let ((text-2 "<span foreground=\"#FFCC00\" font=\"Inconsolata-g 12\" strikethrough=\"true\">X</span>")) ; 100, 80, 0
    ;;   ;; b = 100, g = 80, r = 0
    ;;   (pango:pango_layout_set_markup layout text-2 -1))

    (pango:pango_layout_set_markup layout text-pm -1)
    
    ;; Once text is laidout, get dimensions
    (pango:pango_layout_get_size layout
				 width-pango
				 height-pango)
    ;; Divide by pango scale to get dimensions in pixels
    (setf (mem-ref width-pango :unsigned-int) (/ (mem-ref width-pango :unsigned-int) pango:PANGO_SCALE))
    (setf (mem-ref height-pango :unsigned-int) (/ (mem-ref height-pango :unsigned-int) pango:PANGO_SCALE))
    ;; (fmt-model t "init-layout" "pango layout size: ~a, ~a~%" (mem-ref width-pango :unsigned-int) (mem-ref height-pango :unsigned-int))

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
	   (ptr (ptr (gethash "texture" (handles-shm model))))
	   (surface-render (cairo:create-image-surface-for-data (inc-pointer ptr
									     (offset-textures *model*)) ;data-surface-render
								:argb32
								(mem-ref width-pango :unsigned-int)
								(mem-ref height-pango :unsigned-int)
								stride))
	   (context-render (cairo:create-context surface-render)))

      ;; Update offset by size of texture
      (incf (offset-textures *model*) size-data)
      
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
      (cairo:destroy surface-render)
    
      (values
       (- (offset-textures *model*) size-data) ; want start
       (vec2 (mem-ref width-pango :unsigned-int) (mem-ref height-pango :unsigned-int))))))

(defun setup-view (model)
  
  ;; Init view buffers and start loop
  (let ((conn (init-swank-conn "skynet" 10001)))
    
    (setf (swank-protocol::connection-package conn) "protoform.view")

    ;; (format t "[model] Send eval~%")
    
    ;; view should not concern itself with inst-max...
    (with-slots (width height inst-max) model
	(swank-protocol:request-listener-eval conn (format nil "(setf *view* (init-view-programs ~S ~S ~S))" width height inst-max)))

    (fmt-model t "main-model" "~%~a~%" (swank-protocol:read-message conn))
    
    ;; Init buffers
    (swank-protocol:request-listener-eval conn
					  (with-output-to-string (stream)
					    (format stream "(init-view-buffers `(")
					    (dolist (param *params-shm*)
					      (format stream "~S " param))
					    (format stream "))")))
    ;; (format t "[model] Wait for eval~%")
    (fmt-model t "main-model" "~%~a~%" (swank-protocol:read-message conn))
    
    ;; Do progn to chain them?
    (dolist (params *params-shm*)
      (destructuring-bind (target name path size bind-cs bind-vs &rest rest) params      
        (with-slots (ptr size)
    	    (gethash name (handles-shm model))
	  (fmt-model t "main-model" "(memcpy-shm-to-cache ~S ~S ~S)~%" name name size)
	  (swank-protocol:request-listener-eval conn (format nil "(memcpy-shm-to-cache ~S ~S ~S)" name name size))
	  (fmt-model t "main-model" "~%~a~%" (swank-protocol:read-message conn)))))

    ;; Enable draw flag for view loop
    (swank-protocol:request-listener-eval conn (format nil "(setf *draw* t)"))
    ;; (format t "[model] Wait for eval~%")
    (fmt-model t "main-model" "~%~a~%" (swank-protocol:read-message conn))))

(defun main-model (width height
		   inst-max
		   path-server-model)

  (start-swank-server 10000)
  
  (let* ((model (init-model width
			    height
			    inst-max
			    path-server-model)))

    (defparameter *model* model)
    
    (fmt-model t "main-model" "Init data~%")
    (let ((node (init-node (cursor model)
			   (scale-node model)
			   #\X)))
      
      (multiple-value-bind (offset-texture dims-texture)
	  (generate-text-texture model
				 "<span foreground=\"#FFCC00\" font=\"Inconsolata-g 12\" strikethrough=\"true\">X</span>")
	(setf (offset-texture node) offset-texture)
	(setf (dims-texture node) dims-texture))

      ;; Copy to shm before sending signal to view
      (copy-node-to-shm node))
    
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

;; /proc/sys/net/core/rmem_default for recv and /proc/sys/net/core/wmem_default
;; 212992
;; They contain three numbers, which are minimum, default and maximum memory size values (in byte), respectively.

;; move elsewhere to misc.lisp or util.lisp
(defun fmt-model (dst ctx-str ctl-str &rest rest)
  ;; Add space opt
  (apply #'format
	 dst
	 (str:concat (format nil "[PID:~a,model][~a] " (sb-posix:getpid) ctx-str)
		     ctl-str)
	 rest))
