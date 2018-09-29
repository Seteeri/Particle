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

   ;; Integrate these into corresponding slot - data-model
   (sl-chrs :accessor sl-chrs :initarg :sl-chrs :initform nil)
   (lock-sl-chrs :accessor lock-sl-chrs :initarg :lock-sl-chrs :initform (bt:make-lock))
   (projview :accessor projview :initarg :projview :initform nil)
   ;; Rename mapping-base -> data-shm
   (mapping-base :accessor mapping-base :initarg :mapping-base :initform (make-hash-table :size 6 :test 'equal))
   
   (conn-model :accessor conn-model :initarg :conn-model :initform nil)
   ;; Make class - integrate with conn? (inc cohesion)
   (epoll-events :accessor epoll-events :initarg :epoll-events :initform nil)
   (epoll-fd :accessor epoll-fd :initarg :epoll-fd :initform nil)

   ;; Make class
   (queue-chrs :accessor queue-chrs :initarg :queue-chrs :initform (make-queue))
   (queue-pv :accessor queue-pv :initarg :queue-pv :initform (make-queue))
      
   ;; Make class - layout (inc cohesion)
   (metrics :accessor metrics :initarg :metrics :initform nil)
   (dpi-glyph :accessor dpi-glyph :initarg :dpi-glyph :initform nil)
   (scale-glyph :accessor scale-glyph :initarg :scale-glyph :initform nil)
   (cursor :accessor cursor :initarg :cursor :initform nil)))

;; For now, determine these through view - maybe model can request from view?
;; GL_MAX_SHADER_STORAGE_BLOCK_SIZE = 134217728 = 134.217728 MBs
;; GL_MAX_TEXTURE_BUFFER_SIZE       = 134217728 = 134.217728 MBs
;;
;; Or pass 0/-1 to determine max?
;;
;; Make class slots?
(defparameter *params-shm* (list (list :uniform-buffer
				       "bo-projview"
				       "/protoform-projview"
				       (align-size (* (+ 16 16 16) 4 1))
				       0 0) ; cs-in, vs-in
				 (list :shader-storage-buffer
				       "bo-instance"
				       "/protoform-instance"
				       134217728				       
				       1 2)
				 (list :texture-buffer ; requires fmt type
				       "bo-texture"
				       "/protoform-texture"
				       134217728
				       -1 -1)
				 (list :element-array-buffer
				       "bo-element"
				       "/protoform-element"
				       (* 4 6)  ; 4 bytes/int * 6 ints or indices
				       -1 -1)
				 (list :draw-indirect-buffer
				       "bo-draw-indirect"
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
			       :inst-max inst-max
			       :conn-model nil))) ;(init-conn-server path-server-model)))
    
    (setf (dpi-glyph model) (/ 1 90))
    (setf (scale-glyph model) 10.0)
    
    ;; Init shms, request view to mmap
    ;; TODO:
    ;; Read cfg file (s-exp) to set parameters for mmaps
    ;; View also needs a cfg file since it corresponds for mmaps - or gen automatically?
    ;;
    ;; Note, element-array and draw-indirect buffers exist
    (fmt-model t "main-model" " Initializing mmaps...~%")
    (init-mapping-base (mapping-base model)
		       *params-shm*)
    
    ;; Init data for shms
    (with-slots (ptr size)
	(gethash "projview" (mapping-base model))
      (update-projection-matrix (projview model))
      (update-view-matrix (projview model))
      ;; (write-matrix (view-matrix (projview model)) t)
      (set-projection-matrix ptr (projection-matrix (projview model)))
      (set-view-matrix ptr (view-matrix (projview model)))
      (let ((b (+ 16 16))
	    (data (init-vector-position 1)))
	(dotimes (i (length data))
	  (setf (mem-aref ptr :float (+ b i))
		(aref data i)))))    
        
    model))

(defun init-text (model)

  (let* ((cursor (vec3 0.0 0.0 0.0))
	 (inst-node (init-node cursor
			       (scale-glyph model)
			       #\X))
	 (offset-ptr 0))
    
    (with-slots (ptr size)
	(gethash "instance" (mapping-base model))
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
	   (surface-render (cairo:create-image-surface-for-data (ptr (gethash "texture" (mapping-base model))) ;data-surface-render
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
	    (gethash "texture" (mapping-base model))
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

(defun main-model (width height
		   inst-max
		   path-server-model)

  (start-swank-server 10000)
  
  (let* ((model (init-model width
			    height
			    inst-max
			    path-server-model)))

    ;; NODES
    ;; * Context -> Default FB
    ;; * Default FN -> Pipeline -> Buffers [SHOW ENTIRE ENGINE LAYOUT IN REALTIME]
    ;; * Render into a node...or select a framebuffer

    ;; TODO:
    ;; 1. Rewrite/refactor IPC - model will send setup to view
    ;; 2. Test live texture updates
    ;; 3. Use parameters from Pango to set texture size

    (format t "[model] Init data~%")
    (init-text model)
    (init-layout model)
    
    (format t "[model] Init conn to view swank server~%")
    
    ;; Init view buffers and start loop
    (let ((conn (init-swank-conn "skynet" 10001)))      
      (setf (swank-protocol::connection-package conn) "protoform.view")

      ;; (format t "[model] Send eval~%")
      
      ;; view should not concern itself with inst-max...
      (swank-protocol:request-listener-eval conn
      					    (format nil "(setf *view* (init-view-programs ~S ~S ~S))" width height inst-max))
      ;; (format t "[model] Wait for eval~%")
      (format t "~a~%" (swank-protocol:read-message conn))
      
      ;; Init buffers
      ;; Need to standardize view buffer creation params
      (swank-protocol:request-listener-eval conn
					    (format nil "(init-view-buffers `(~S ~S ~S))"
						    (first *params-shm*)
						    (second *params-shm*)
						    (third *params-shm*)))
      ;; (format t "[model] Wait for eval~%")
      (format t "~a~%" (swank-protocol:read-message conn))
      
      ;; Do progn to chain them?
      (dolist (name (list "projview"
    			  "instance"
    			  "texture"))
        (with-slots (ptr size)
    	    (gethash name (mapping-base model))
	  (format t "(serve-memcpy2 \"~a\" \"~a\" ~a)~%" name name size)
	  (swank-protocol:request-listener-eval conn
						(format nil "(serve-memcpy2 \"~a\" \"~a\" ~a)" name name size))
	  (format t "~a~%" (swank-protocol:read-message conn))))

      ;; Enable draw flag for view loop
      (swank-protocol:request-listener-eval conn
					    (format nil "(setf *draw* t)"))
      ;; (format t "[model] Wait for eval~%")
      (format t "~a~%" (swank-protocol:read-message conn)))      
    
    (loop (sleep 0.0167))))

(defun handle-escape (msdf
		      keysym)
  
  (clean-up-model msdf)
  (glfw:set-window-should-close))

(defun clean-up-model (msdf)
  (loop 
     :for key :being :the :hash-keys :of (mapping-base msdf)
     :using (hash-value mmap)
     :do (cleanup-mmap mmap))

  (request-exit (conn-model msdf))
  
  (format t "[handle-escape] Model/Controller exiting!~%")

  ;; Only for DRM?
  (sb-ext:exit))

(defun wait-epoll (msdf)
  (with-slots (conn-model
	       epoll-fd
	       epoll-events)
      msdf
    ;; -1 = timeout = block/infinite
    ;; 0 = return if nothing

    ;; number of events should equal number of connections or max connections

    ;; store callback in data or get func from hash table
    
    (let ((fds-n (c-epoll-wait epoll-fd epoll-events 3 -1)))
      (when (> fds-n 0)
	(loop
	   :for i :from 0 :below fds-n
	   :for fd := (foreign-slot-value (foreign-slot-value (mem-aptr epoll-events '(:struct event) 0) '(:struct event) 'data)
					  '(:union data)
					  'fd)
	   :do (cond ((= fd (sock conn-model))
		      ;; Handle server
		      
		      (let* ((conn-client (accept-client conn-model msdf))
			     (sock-client (sock conn-client)))

			(format t "[wait-epoll] Client ~a connected~%" sock-client)

			;; Add to epoll
			(ctl-epoll epoll-fd
				   sock-client
				   (logior #x0001 #x008 #x010) ; EPOLLIN, EPOLLERR, EPOLLHUP
				   :add)
			
			;; Store in hash-table
			(setf (gethash sock-client (clients conn-model))
			      conn-client)
			    			
			;; TEMP
			(if (eq (id conn-client) :view)
			    (setf (view conn-model) conn-client)
			    (setf (controller conn-model) conn-client))

			;; LET VIEW REQUEST THIS
			;; Send data/code to view to initialize
			;; (when (eq (id conn-client) :view)
			;;   (dolist (name (list "projview"
			;; 		      "instance"
			;; 		      "texture"))
			;;     (with-slots (ptr size)
			;; 	(gethash name (mapping-base msdf))
			;;       (request-memcpy conn-client name name size nil)))
			;;   ;; sync
			;;   (request-sync conn-client))

			t)
		      
		      t)
		   

		     ;; Handle client
		     ;; Primarily watch for requests from controller
		     ;; View connection is used to send requests
		     ;; View has no reason to modify model - that is done through controller
		     (t
		      
		      ;; (format t "[wait-epoll] Client ~a sent data, ~a~%" fd (gethash fd (clients conn-model)))

		      (serve-client (gethash fd (clients conn-model))
				    msdf)
		      
		      t)))))))


;; REFACTOR SKIP-LIST: Iterate from passed node
;; Could cache loop node here to skip O log(N) operation
;; -> high probability nodes are sequential

(defun run-thread-chrs (msdf)
  ;; Needs client
  (with-slots (conn-model
	       mapping-base
	       sl-chrs
	       metrics
	       dpi-glyph
	       scale-glyph
	       cursor
	       queue-chrs
	       lock-sl-chrs)
      msdf
    (loop
       :for task := (pop-queue queue-chrs)
       :do (when (>= task 0)

	     (bt:with-lock-held (lock-sl-chrs)

	       (format t "[run-thread-chrs] Starting task @ ~a~%" task)
	       
	       (let* ((offset-ptr 0)
		      (counter-batch 0)
		      (size-batch 19) ; 208 bytes
		      (offset-batch 0)
		      (cursor (vec3 0.0 0.0 0.0)))

		 ;; lock skiplist?
		 
		 (skip-list:doskiplist
		  (sl-chr sl-chrs)

		  ;; Check queue for cancel
		  (when (not (queue-empty-p queue-chrs))
		    ;; (format t "[run-thread-chrs] Cancelling current task~%")
		    (return))
		  
		  ;; Otherwise process char
		  (multiple-value-bind (new-offset)
		      (update-shm-from-chr sl-chr
					   mapping-base
					   metrics
					   dpi-glyph
					   scale-glyph
					   cursor
					   offset-ptr)
		    (setf offset-ptr new-offset))

		  ;; Update batch
		  (incf counter-batch)
		  (when (>= counter-batch size-batch)
		    (request-memcpy (view conn-model)
				    "instance" "instance"
				    (* counter-batch 208)
				    nil
				    :offset-dest offset-batch
				    :offset-src  offset-batch)
		    (incf offset-batch (* counter-batch 208))
		    (setf counter-batch 0))		 
		  ;; skip list end
		  t)
		 
		 ;; Update incomplete batch
		 (when (> counter-batch 0)
		   (request-memcpy (view conn-model)
		 		  "instance" "instance"
		 		  (* counter-batch 208)
		 		  t
		 		  :offset-dest offset-batch
		 		  :offset-src  offset-batch)
		   (incf offset-batch (* counter-batch 208))
		   (setf counter-batch 0))
		 
		 (format t "[run-thread-chrs] Finished updating~%")

		 t))))))

(defun update-shm-from-chr (sl-chr
			    mapping-base
			    metrics
			    dpi-glyph
			    scale-glyph
			    cursor
			    offset-ptr)

  (let* ((metrics-space (gethash 32 metrics))
	 (uv-space (uv metrics-space)))

    (with-slots (ptr size)
	(gethash "instance" mapping-base)
    
      (with-slots (chr
		   model-matrix
		   rgba
		   flags)
	  sl-chr
	
	(cond ((char-equal chr #\Tab)
	       t)

	      ;; ascii
	      (t

	       ;; (format t "[update-chrs] ~a~%" chr)

	       (update-transform-chr cursor
				     (if (char-equal chr #\Newline)
					 metrics-space
					 (gethash (char-code chr) metrics))
				     scale-glyph
				     dpi-glyph
				     sl-chr)
	       
	       (when (and (char-equal chr #\Newline) t)
		 ;; After new chr, move down, reset x for next char
		 ;; Treat it like a space...
		 (setf (vx3 cursor) 0)
		 (decf (vy3 cursor) (* 9.375 2.0 scale-glyph)))
	       
	       (loop
		  :for c :across (marr4 (matrix model-matrix))
		  :for i :upfrom 0
		  :do (setf (mem-aref ptr :float (+ offset-ptr i))
			    c))
	       (incf offset-ptr 16)
	       
	       (loop
		  :for c :across rgba
		  :for i :upfrom 0
		  :do (setf (mem-aref ptr :float (+ offset-ptr i))
			    c))
	       (incf offset-ptr 16)
	       
	       ;; (u v s t) * 4
	       (loop
		  :for c :across (if (char-equal chr #\Newline)
				     uv-space
				     (uv (gethash (char-code chr) metrics)))
		  :for i :upfrom 0
		  :do (setf (mem-aref ptr :float (+ offset-ptr i))
			    c))
	       (incf offset-ptr 16)
	       
	       ;; Glyph, Flags, pad, pad
	       (setf (mem-aref ptr :int (+ offset-ptr 0)) (- (char-code chr) 32))
	       (setf (mem-aref ptr :int (+ offset-ptr 1)) flags)
	       (incf offset-ptr 4))))))

  offset-ptr)
