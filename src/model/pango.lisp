(in-package :protoform.model)

(defclass texture ()
  ((data :accessor data :initarg :data :initform nil)
   (size :accessor size :initarg :size :initform nil)
   (dim :accessor dim :initarg :dim :initform nil)))

(defun copy-textures-to-shm ()
  (let ((offset 0))
    (with-slots (ptr size)
	(gethash "texture" (handles-shm *model*))
      (loop
	 :for texture :across (textures *model*)
	 :do (progn
	       ;; assert
	       (c-memcpy (inc-pointer ptr offset)
			 (data texture)
			 (size texture))
	       (incf offset (size texture)))))))

(defun init-node (cursor
		  scale
		  ix
		  data)
  (let ((node (make-instance 'node
			     :data data
			     :index ix
			     :model-matrix (make-instance 'model-matrix
							  :scale scale
							  :translation cursor))))
    
    (update-node-texture node
			 data)
    
    ;; Update transform
    (update-transform (model-matrix node))
    
    node))

(defun update-node-texture (node
			    data)

  (setf (data node) data)
  
  ;; Separate this function maybe
  (multiple-value-bind (offset-texel-texture
			dims-texture
			data-size)
      (convert-pm-to-texture
       (format nil "<span foreground=\"#FFCC00\" font=\"Inconsolata-g 59\" strikethrough=\"false\">~A</span>" data))
    
    (setf (offset-texel-texture node) offset-texel-texture)
    (setf (dims-texture node) dims-texture)

    (fmt-model t "update-node-texture"
	       (with-output-to-string (stream)
		 (write-char #\Newline stream)
		 (format stream "  Name: ~S~%" data)
		 (format stream "  Dims: ~S~%" dims-texture)
		 (format stream "  Start Texels Offset: ~S texels~%" offset-texel-texture)
		 (format stream "  Data Size: ~S bytes~%" data-size)
		 (format stream "  Current Offset: ~S bytes~%" (offset-bytes-textures *model*)))))

  ;; Update scale to match texture
  (setf (vx3 (scale (model-matrix node))) (* (/ 1 96) (vx2 (dims-texture node))))
  (setf (vy3 (scale (model-matrix node))) (* (/ 1 96) (vy2 (dims-texture node)))))

(defun convert-pm-to-texture (text-pm)
  
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

    ;; typedef enum _cairo_format {
    ;;     CAIRO_FORMAT_INVALID   = -1,
    ;;     CAIRO_FORMAT_ARGB32    = 0,
    ;;     CAIRO_FORMAT_RGB24     = 1,
    ;;     CAIRO_FORMAT_A8        = 2,
    ;;     CAIRO_FORMAT_A1        = 3,
    ;;     CAIRO_FORMAT_RGB16_565 = 4
    ;; } cairo_format_t;
    
    ;; Create cairo context/surface to render to
    (let* ((dim (vec2 (mem-ref width-pango :unsigned-int)
		      (mem-ref height-pango :unsigned-int)))
	   (size-data (* (mem-ref width-pango :unsigned-int)
			 (mem-ref height-pango :unsigned-int)
			 4)) ; 4 bytes per pixel
	   (data (foreign-alloc :unsigned-char :count size-data))
	   (stride (cairo::cairo_format_stride_for_width 0 (mem-ref width-pango :unsigned-int))) ; send patch upstream
	   ;; (ptr (ptr (gethash "texture" (handles-shm *model*))))
	   ;; (surface-render (cairo:create-image-surface-for-data (inc-pointer ptr
	   ;; 								     (offset-bytes-textures *model*))
	   (surface-render (cairo:create-image-surface-for-data data
								:argb32
								(mem-ref width-pango :unsigned-int)
								(mem-ref height-pango :unsigned-int)
								stride))
	   (context-render (cairo:create-context surface-render)))
      
      ;; Set surface color - similar to glClear
      (cairo:set-source-rgba 1 1 1 1 context-render)
      ;; Draw
      (pango:pango_cairo_show_layout (slot-value context-render 'cairo:pointer) layout)
      ;; Ensure surface ops are flushed before accessing memory
      (cairo:surface-flush surface-render)

      ;; Update offsets by size of texture
      (incf (offset-texel-textures *model*) (* (mem-ref width-pango :unsigned-int)
					       (mem-ref height-pango :unsigned-int)))
      (incf (offset-bytes-textures *model*) size-data)
      ;; Store data - free later
      (vector-push (make-instance 'texture
				  :data data
				  :size size-data
				  :dim dim)
		   (textures *model*))
      
      ;; (cairo:surface-write-to-png surface "/home/user/pango-test.png")
      ;; (sb-ext:exit)

      ;; Clean up
      ;; (foreign-free data-surface-render)
      (foreign-free width-pango)
      (foreign-free height-pango)
      (pango:g_object_unref layout)
      (cairo:destroy context-layout)
      (cairo:destroy context-render)
      (cairo:destroy surface-render)

      ;; Free pango dim ptrs

      ;; Watchout for memory layout:
      ;; OpenGL: RGBA
      ;; CAIRO_FORMAT_ARGB32: BGRA
      
      (values
       (- (offset-texel-textures *model*) (truncate (* (vx2 dim) (vy2 dim))))
       dim
       size-data))))

(defun update-node-text (seq-key)

  (with-slots (digraph)
      *model*
  
    (fmt-model t "main-model" "Updating root node~%")

    (let ((node-root (first (digraph:roots digraph))))
    
      ;; Generate texture directly to shm
      ;; Update node
      ;; Tell view to copy to cache

      ;; Must replace old texture...by deleting old texture
      ;; Then this causes recopy subsequent

      ;; (format nil "~v@{~A~:*~}" 9 (code-char keysym))
      (update-node-texture node-root (format nil "~a~a" (data node-root) (code-char keysym)))
      (update-transform (model-matrix node-root))
      
      (copy-textures-to-shm)
      (copy-node-to-shm node-root
			(* (index node-root)
			   (/ +size-struct-instance+ 4)))

      ;;;;;;;;;;;;;;;
      ;; Make atomic
      ;; otherwise will result in possible delay or "tearing"

      ;; Set flags so cache->step
      ;; - Important to make sure all steps are updated otherwise flickering will occur
      ;; - Simplest method is to set a counter and copy every frame until counter is 0
      ;; - Specify size?

      ;; (return-from update-node-text)

      ;; Commenting out nodes = no flicker/update
      ;; so memcpy texture is not the problem

      ;; Removing bytes will stay permablack
      
      (memcpy-shm-to-cache-flag* (list (list "texture"
					     0
      					     (offset-bytes-textures *model*))
      				       (list "nodes"
				       	     0
      				       	     (* +size-struct-instance+ (+ (digraph:count-vertices digraph)
				       					  (digraph:count-edges digraph)))))))))


(defun init-graph ()
  ;; Create DAG
  (let ((digraph (digraph:make-digraph)))

    (setf (digraph *model*) digraph)

    ;; Create vertices/edge then generate nodes
    ;; Normally user will create these through input (controller)
    
    ;; Node 1
    (let ((n-0 (init-node (vec3 -8 8 1)
			  (scale-node *model*)
			  0
			  "hello world!"))
	  (n-1 (init-node (vec3 0 0 0)
			  (scale-node *model*)
			  1
			  " ")) ; "‖↑↓"
	  (n-2 (init-node (vec3 0 -1 1)
			  (scale-node *model*)
			  2
			  "MASTERMIND")))

      ;; (setf (scale (model-matrix n-1))
      ;; 	    (vec3 10.0 10.0 1.0))
      ;; (update-transform (model-matrix n-1))

      ;; * Adjust transforms
      ;; * Position - center
      ;; * Rotation - Z
      ;; * Scale - Y=dist,X=thickness
      (let* ((mm-0 (model-matrix n-0))
	     (sca-0 (scale mm-0))
	     (pos-0 (translation mm-0))
	     (mm-2 (model-matrix n-2))
	     (sca-2 (scale mm-2))
	     (pos-2 (translation mm-2))
	     (dist (vdistance pos-0 pos-2))
	     (direction (v- pos-0 pos-2))
	     (bottom-center (- (vy3 pos-0) (* 0.5 (vy3 sca-0))))
	     (top-center (+ (vy3 pos-2) (* 0.5 (vy3 sca-2))))
	     (ang-direct (atan (vy3 direction) (vx3 direction))))

	(with-slots (translation
		     rotation
		     scale)
	    (model-matrix n-1)

	  #|
	  _[]
	  |
	  |_[]
	  |#
	  
	  ;; Set halfway - nodes can have different scales

	  ;; Origin is bottom left

	  (format t "vangle: ~a~%" ang-direct)

	  ;; rads
	  (setf rotation (vec3 0.0
			       0.0
			       (- ang-direct (/ pi 2))))
	  
	  (setf translation (vec3 0.0
				  top-center
				  0.0))
	  (setf scale (vec3 0.2
	  		    dist
	  		    1.0))
	  t))
      
      (update-transform (model-matrix n-1))
      
      (digraph:insert-vertex digraph n-0)
      (digraph:insert-vertex digraph n-1)
      (digraph:insert-vertex digraph n-2)
      
      (digraph:insert-edge digraph n-0 n-1)
      (digraph:insert-edge digraph n-1 n-2)
      
      (copy-nodes-to-shm)
      (copy-textures-to-shm)
      
      (fmt-model t "main-model" "Init conn to view swank server~%"))))
