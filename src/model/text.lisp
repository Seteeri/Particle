(in-package :protoform.model)

(defun generate-text-texture (text-pm)

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
			 4)) ; 4 bytes per pixel
	   ;; (data-surface-render (foreign-alloc :unsigned-char :count size-data))
	   ;; (stride (* 4 (mem-ref width-pango :unsigned-int)))
	   (stride (cairo::cairo_format_stride_for_width 0 (mem-ref width-pango :unsigned-int))) ; send patch upstream
	   (ptr (ptr (gethash "texture" (handles-shm *model*))))
	   (surface-render (cairo:create-image-surface-for-data (inc-pointer ptr
									     (offset-bytes-textures *model*)) ;data-surface-render
								:argb32
								(mem-ref width-pango :unsigned-int)
								(mem-ref height-pango :unsigned-int)
								stride))
	   (context-render (cairo:create-context surface-render)))

      ;; Update offsets by size of texture
      (incf (offset-texel-textures *model*) (* (mem-ref width-pango :unsigned-int)
					       (mem-ref height-pango :unsigned-int)))
      (incf (offset-bytes-textures *model*) size-data)
      
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

      ;; free pango dim ptrs
      
      (values
       (- (offset-texel-textures *model*) (* (mem-ref width-pango :unsigned-int)
					     (mem-ref height-pango :unsigned-int)))
       (vec2 (mem-ref width-pango :unsigned-int) (mem-ref height-pango :unsigned-int))))))
