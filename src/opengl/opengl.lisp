(in-package :protoform.opengl)

(declaim (inline rad-to-deg))
(defun rad-to-deg (rad)
  (/ (* rad 180.0) pi))

(declaim (inline deg-to-rad))
(defun deg-to-rad (deg)
  (/ (* deg pi) 180.0))

(declaim (inline read-file-string))
(defun read-file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defclass gles ()
  ((programs :accessor programs :initarg :programs :initform nil)))

(defun init-gles (width height)

  (defparameter *map-buffer-range-access* (logior #x0002 #x0040 #x0080))
  
  (format t "[init-gles] GL Vendor: ~a~%" (gl:get* :vendor))
  (format t "[init-gles] GL Renderer: ~a~%" (gl:get* :renderer))
  (format t "[init-gles] GL Version: ~a~%" (gl:get* :version))
  (format t "[init-gles] GLSL Version: ~a~%" (gl:get* :shading-language-version))
  
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  
  ;; Get screen dimensions from drm
  (gl:viewport 0 0 width height)
  (gl:enable :cull-face)
  (gl:enable :depth-test)

  (%gl:clear-color 0.0
		   (coerce (/ 43 255) 'single-float)
		   (coerce (/ 54 255) 'single-float)
		   0.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  
  (print-gl-max))

(defun print-gl-max ()

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
     :do (progn
	   (setf (char name 2) #\:)
	   (let* ((k2 (string-downcase (str:replace-all "_" "-" (subseq name 2)))))
	     (format t "[init-gles] ~a: ~a~%" k2 (gl:get* (read-from-string k2)))))))

(defun attach-shader (type
		      program
		      paths
		      &optional
			(attach t)
			(delete t))
  
  (let* ((shader (gl:create-shader type)))
    
    ;; read paths into strings into list
    (gl:shader-source shader
		      (loop 
		         :for path :in paths
			 :collect (read-file-string path)))
    
    (gl:compile-shader shader)
    
    (format t "[attach-shader] Shader Info Log:~%")
    ;; (format t "[attach-shader]    ~a~%" paths)
    (format t "[attach-shader]    \"~a\"~%" (gl:get-shader-info-log shader))
    
    (when attach
      (gl:attach-shader program shader))
    
    (when delete
      (gl:delete-shader shader))
    
    shader))

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

(defun init-boav-main ()
  ;; VAOs store all of the links between the attributes and VBOs
  ;; including raw vertex data, etc.
  ;; VAO attributes can be linked to different VBOs, essentially
  ;; storing pointers to bound VBOs when glEnableVertexAttrib called
  (let ((boav-main (first (gl:gen-vertex-arrays 1))))
    (gl:bind-vertex-array boav-main)
    boav-main))

(defun vertex-attrib-pointer (index size type norm stride offset vbo)
  (%gl:enable-vertex-attrib-array index)
  ;; Bind vbo first
  (%gl:vertex-attrib-format index
			    size
			    type
			    norm
			    0)
  (%gl:vertex-attrib-binding index index)
  ;; 0 will unbind vbo
  ;; Need not bind vbo for this func
  (%gl:bind-vertex-buffer index vbo offset stride))

(defun vertex-attrib-divisor (index divisor)
  (%gl:vertex-attrib-binding index index)
  (%gl:vertex-binding-divisor index divisor))

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

(declaim (inline normalize))
(defun normalize (a b c d)
  (v/ (vec4 a b c d) (vlength (vec3 a b c))))
