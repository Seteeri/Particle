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

(defun init-glyph-data ()

  ;; Convert ppm to bytes offline for faster loading
  ;; Or turn into lisp data and make a vector
  
  (with-slots (ptr size)
      (gethash "glyphs-msdf" (handles-shm *model*))
    
    (loop
       :for code :from 32 :to 255
       :with msdf-glyphs-path := (merge-pathnames #P"glyphs-msdf/" (asdf:system-source-directory :protoform))
       :for ppm-path := (merge-pathnames (make-pathname :name (write-to-string code) :type "ppm")
					 msdf-glyphs-path)
       :with i := 0
       :do (let* ((bmp (read-ppm ppm-path)))
	     
    	     (assert (= (length bmp) (* 96 96 4)))

	     (fmt-model t "init-glyph-data" "Loading ~S~%" ppm-path)
	     
	     (loop 
		:for c :across bmp
		:do (progn
		      (setf (mem-aref ptr :unsigned-char i) c)
		      (incf i)))))))

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
	 :do (loop
		:for c :in (str:split-omit-nulls " " pixels)
		:with i := 0
		:do (progn
		      (vector-push (parse-integer c) bitmap)
		      (incf i)
		      (when (= i 3)
			(vector-push 255 bitmap)
			(setf i 0))))))
    bitmap))
