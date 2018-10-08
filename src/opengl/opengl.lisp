(in-package :protoform.opengl)

(defparameter *map-buffer-range-access* (logior #x0002 #x0040 #x0080))

(defun copy-buffer (buffer-read
		    buffer-write
		    size
		    &key
		      (offset-read 0)
		      (offset-write 0))
  
  (%gl:bind-buffer :copy-read-buffer buffer-read)
  (%gl:bind-buffer :copy-write-buffer buffer-write)
  (%gl:copy-buffer-sub-data :copy-read-buffer
			    :copy-write-buffer
			    offset-read ; r off
			    offset-write ; w off
			    size))

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

(defun get-gl-maxes ()
  (loop 
     :for name :in (list "GL_MAX_VERTEX_SHADER_STORAGE_BLOCKS"
			 "GL_MAX_GEOMETRY_SHADER_STORAGE_BLOCKS"
			 "GL_MAX_TESS_CONTROL_SHADER_STORAGE_BLOCKS"
			 "GL_MAX_TESS_EVALUATION_SHADER_STORAGE_BLOCKS"
			 "GL_MAX_FRAGMENT_SHADER_STORAGE_BLOCKS"
			 "GL_MAX_COMPUTE_SHADER_STORAGE_BLOCKS"
			 "GL_MAX_COMBINED_SHADER_STORAGE_BLOCKS"
			 "GL_MAX_SHADER_STORAGE_BUFFER_BINDINGS"
			 "GL_MAX_SHADER_STORAGE_BLOCK_SIZE"
			 "GL_SHADER_STORAGE_BUFFER_OFFSET_ALIGNMENT"
			 "GL_MAX_ARRAY_TEXTURE_LAYERS"
			 "GL_MAX_TEXTURE_SIZE"
			 "GL_MAX_TEXTURE_BUFFER_SIZE"
			 "GL_MAX_TEXTURE_IMAGE_UNITS"
			 "GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS"
			 "GL_MAX_ELEMENTS_VERTICES"
			 "GL_MAX_ELEMENTS_INDICES"
			 "GL_MAX_COMPUTE_WORK_GROUP_COUNT"
			 "GL_MAX_COMPUTE_WORK_GROUP_SIZE"
			 "GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS"
			 "GL_MAX_COMPUTE_SHARED_MEMORY_SIZE"
			 "GL_MAX_VERTEX_UNIFORM_BLOCKS"
			 "GL_MAX_GEOMETRY_UNIFORM_BLOCKS"
			 "GL_MAX_FRAGMENT_UNIFORM_BLOCKS"
			 "GL_MAX_UNIFORM_BLOCK_SIZE")
     :collect (progn
		(setf (char name 2) #\:)
		(let* ((k2 (string-downcase (str:replace-all "_" "-" (subseq name 2)))))
		  (list k2 (gl:get* (read-from-string k2)))))))

(defun cad-shader (type
		   program
		   paths)
  (multiple-value-bind (shader log-shader)
      (compile-shader type
		      paths)
    (attach-shader program shader)
    (delete-shader shader)
    log-shader))
  
(defun compile-shader (type
		       paths)
  (let* ((shader (gl:create-shader type)))
    ;; read paths into strings into list
    (gl:shader-source shader
		      (loop 
		         :for path :in paths
			 :collect (read-file-string path)))
    (gl:compile-shader shader)
    ;; Return log also
    (values shader
	    (gl:get-shader-info-log shader))))

(defun attach-shader (program shader)
  (gl:attach-shader program shader))

(defun delete-shader (shader)
  (gl:delete-shader shader))

;; Move to msdf?
(defun make-perspective-vector (width height)
  (mperspective 45.0 ; arg
		(/ width height)
		0.1 ; arg
		100.0)) ; arg

(defun make-orthographic-vector (width height scale near far)
  (let* ((aspect-ratio (float (/ width height)))
	 (size-x (* scale 0.5))
	 (size-y (/ (* scale 0.5 ) aspect-ratio)))    
    (mortho (- size-x)
	    size-x
	    (- size-y)
	    size-y
	    near
	    far)))

(defun init-vao ()
  ;; VAOs store all of the links between the attributes and VBOs
  ;; including raw vertex data, etc.
  (let ((vao (first (gl:gen-vertex-arrays 1))))
    (gl:bind-vertex-array vao)
    vao))

(defun type-cffi-to-gl (type)
  (ecase type
    ((:char :signed-char :signed-byte) '%gl:byte)
    ((:uchar :unsigned-char :unsigned-byte) '%gl:ubyte)
    ((:short :signed-short) '%gl:short)
    ((:ushort :unsigned-short) '%gl:ushort)
    ((:int :signed-int) '%gl:int)
    ((:uint :unsigned-int) '%gl:uint)
    (:float '%gl:float)
    (:double '%gl:double)))

(defun extract-clip-planes (mvp)
  
  ;; mcref4 mat y x

  ;; col, row = y x
  ;; near
  ;; m(3,1) + m(4,1),
  ;; m(3,2) + m(4,2),
  ;; m(3,3) + m(4,3),
  ;; m(3,4) + m(4,4));

  ;; far
  ;; -m(3,1) + m(4,1),
  ;; -m(3,2) + m(4,2),
  ;; -m(3,3) + m(4,3),
  ;; -m(3,4) + m(4,4));

  ;; bottom
  ;; m(2,1) + m(4,1),
  ;; m(2,2) + m(4,2),
  ;; m(2,3) + m(4,3),
  ;; m(2,4) + m(4,4));

  ;; top
  ;; -m(2,1) + m(4,1),
  ;; -m(2,2) + m(4,2),
  ;; -m(2,3) + m(4,3),
  ;; -m(2,4) + m(4,4));

  ;; left
  ;; m(1,1) + m(4,1),
  ;; m(1,2) + m(4,2),
  ;; m(1,3) + m(4,3),
  ;; m(1,4) + m(4,4));

  ;; right
  ;; -m(1,1) + m(4,1),
  ;; -m(1,2) + m(4,2),
  ;; -m(1,3) + m(4,3),
  ;; -m(1,4) + m(4,4));  

  ;; // set the normal vector
  ;; normal.set(a,b,c);
  ;; //compute the lenght of the vector
  ;; float l = normal.length();
  ;; // normalize the vector
  ;; normal.set(a/l,b/l,c/l);
  ;; // and divide d by th length as well
  ;; this->d = d/l;
  
  ;;      near
  (values (plane-normalize (+ (mcref4 mvp 2 0) (mcref4 mvp 3 0))
			   (+ (mcref4 mvp 2 1) (mcref4 mvp 3 1))
			   (+ (mcref4 mvp 2 2) (mcref4 mvp 3 2))
			   (+ (mcref4 mvp 2 3) (mcref4 mvp 3 3)))

	  ;; far
	  (plane-normalize (+ (- (mcref4 mvp 2 0)) (mcref4 mvp 3 0))
			   (+ (- (mcref4 mvp 2 1)) (mcref4 mvp 3 1))
			   (+ (- (mcref4 mvp 2 2)) (mcref4 mvp 3 2))
			   (+ (- (mcref4 mvp 2 3)) (mcref4 mvp 3 3)))

	  ;; bottom
	  (plane-normalize (+ (mcref4 mvp 1 0) (mcref4 mvp 3 0))
			   (+ (mcref4 mvp 1 1) (mcref4 mvp 3 1))
			   (+ (mcref4 mvp 1 2) (mcref4 mvp 3 2))
			   (+ (mcref4 mvp 1 3) (mcref4 mvp 3 3)))

	  ;; top
	  (plane-normalize (+ (- (mcref4 mvp 1 0)) (mcref4 mvp 3 0))
			   (+ (- (mcref4 mvp 1 1)) (mcref4 mvp 3 1))
			   (+ (- (mcref4 mvp 1 2)) (mcref4 mvp 3 2))
			   (+ (- (mcref4 mvp 1 3)) (mcref4 mvp 3 3)))

	  ;; left
	  (plane-normalize (+ (mcref4 mvp 0 0) (mcref4 mvp 3 0))
			   (+ (mcref4 mvp 0 1) (mcref4 mvp 3 1))
			   (+ (mcref4 mvp 0 2) (mcref4 mvp 3 2))
			   (+ (mcref4 mvp 0 3) (mcref4 mvp 3 3)))

	  ;; right
	  (plane-normalize (+ (- (mcref4 mvp 0 0)) (mcref4 mvp 3 0))
			   (+ (- (mcref4 mvp 0 1)) (mcref4 mvp 3 1))
			   (+ (- (mcref4 mvp 0 2)) (mcref4 mvp 3 2))
			   (+ (- (mcref4 mvp 0 3)) (mcref4 mvp 3 3)))))
