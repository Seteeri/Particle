(in-package :protoform.model)

;; For now, determine these through view - maybe model can request from view?
;; GL_MAX_SHADER_STORAGE_BLOCK_SIZE = 134217728 = 134.217728 MBs
;; GL_MAX_TEXTURE_BUFFER_SIZE       = 134217728 = 134.217728 MBs
;;
;; Or pass 0/-1 to determine max?
;;
;; Make class slots? -> Harder to be dynamic
;; Add buffering: single double triple - default to triple
;;
;; Cache/compute will use cs-in
;; Step/raster will use vs-in
(defparameter *params-shm* (list (list :uniform-buffer
				       "projview"
				       "/protoform-projview"
				       (* (+ 16 16) 4)
				       0 0  ; cs-in (cache), vs-in (raster)
				       :triple)
				 (list :uniform-buffer
				       "vertices"
				       "/protoform-vertices"
				       (* 16 4)
				       1 1
				       :triple)
				 (list :shader-storage-buffer
				       "instance"
				       "/protoform-instance"
				       134217728
				       2 3
				       :triple)
				 (list :texture-buffer
				       "texture"
				       "/protoform-texture"
				       134217728
				       -1 -1
				       :triple		       
				       :rgba8) ; requires fmt type
				 (list :element-array-buffer
				       "element"
				       "/protoform-element"
				       (* 4 6)  ; 4 bytes/int * 6 ints or indices
				       -1 -1
				       :triple)
				 (list :draw-indirect-buffer
				       "draw-indirect"
				       "/protoform-draw-indirect"
				       (* 4 6)  ; 6 ints/params
				       -1 -1
				       :triple)
				 (list :atomic-counter-buffer
				       "atomic-counter"
				       "/protoform-atomic-counter"
				       (* 4 6)  ; 6 ints/params
				       4 -1
				       :triple)))

(defconstant +size-struct-instance+ 208)

(defclass model ()
  ((width :accessor width :initarg :width :initform nil)
   (height :accessor height :initarg :height :initform nil)
   (conn-swank :accessor conn-swank :initarg :conn-swank :initform nil)
   (handles-shm :accessor handles-shm :initarg :handles-shm :initform (make-hash-table :size 6 :test 'equal))

   (projview :accessor projview :initarg :projview :initform nil)
   
   ;; Instances...
   (inst-max :accessor inst-max :initarg :inst-max :initform nil)
   (digraph :accessor digraph :initarg :digraph :initform nil)
   
   ;; Textures - list of Texture instances wich store tex parameters
   ;; Use skip list? -> For now use vector
   ;; Hmm, when texture is removed need to recopy all (to "defragment")
   (offset-texel-textures :accessor offset-texel-textures :initarg :offset-texel-textures :initform 0) ; sum of wxh
   (offset-bytes-textures :accessor offset-bytes-textures :initarg :offset-bytes-textures :initform 0) ; sum of bytes
   (textures :accessor textures :initarg :textures :initform (make-array 64 :adjustable t :fill-pointer 0))
   
   (cursor :accessor cursor :initarg :cursor :initform (vec3 0 0 0))
   ;; move to node? used in conjunction with scale-node
   (dpi-glyph :accessor dpi-glyph :initarg :dpi-glyph :initform (/ 1 90))
   ;; rename to scale-default-node
   (scale-node :accessor scale-node :initarg :scale-node :initform (vec3 1.0 1.0 1.0))))

;; TODO: Move elsewhere
(defun init-vector-position ()
  (make-array (* 4 4) :element-type 'single-float
	      ;; top right, bottom right, bottom left, top left
	      ;;
	      ;; 3---0
	      ;; | / |
	      ;; 2---1
	      ;;
	      ;; ccw: 0 2 1 0 3 2
	      :initial-contents (list 1.0  1.0  0.0  1.0
				      1.0  0.0  0.0  1.0
				      0.0  0.0  0.0  1.0
				      0.0  1.0  0.0  1.0)))

(defun set-matrix (ptr-dest matrix-src offset)
  (let ((matrix-arr (marr (mtranspose matrix-src))))
    (dotimes (i 16)
      (setf (mem-aref ptr-dest :float (+ offset i))
	    (aref matrix-arr i)))))

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
			       :inst-max inst-max)))
    (init-handle-shm (handles-shm model)
		     *params-shm*)
    model))

(defun init-shm-data ()
  (with-slots (inst-max
	       projview
	       handles-shm)
      *model*

    ;; instance and texture is done later

    ;; Move some of this out
    (with-slots (ptr size)
	(gethash "projview" handles-shm)
      (update-projection-matrix projview)
      (update-view-matrix projview)
      ;; (write-matrix (view-matrix (projview model)) t)
      (set-projection-matrix ptr (projection-matrix projview))
      (set-view-matrix ptr (view-matrix projview)))

    (with-slots (ptr size)
	(gethash "vertices" handles-shm)
      (let ((data (init-vector-position)))
	(dotimes (i (length data))
	  (setf (mem-aref ptr :float i)
		(aref data i)))))
    
    (with-slots (ptr size)
	(gethash "element" handles-shm)
      (let ((data (make-array 6
      			      :element-type '(unsigned-byte 32)
      			      :initial-contents (list 0 2 1 0 3 2))))
	(dotimes (i (length data))
	  (setf (mem-aref ptr ::uint i)
		(aref data i)))))

    (with-slots (ptr size)
	(gethash "draw-indirect" handles-shm)
      (let ((data (make-array 5
      			      :element-type '(unsigned-byte 32)
      			      :initial-contents (list 6 inst-max 0 0 0))))
	(dotimes (i (length data))
	  (setf (mem-aref ptr ::uint i)
		(aref data i)))))))

(defun setup-view ()
  
  (let ((conn (init-swank-conn "skynet" 10001)))

    (setf (conn-swank *model*) conn)
    (setf (swank-protocol::connection-package conn) "protoform.view")
    
    ;; view should not concern itself with inst-max...
    (with-slots (width height inst-max)
	*model*
      (eval-sync conn
		 (format nil "(setf *view* (init-view-programs ~S ~S ~S))" width height inst-max)))
    
    ;; Init buffers
    (eval-sync conn
	       (with-output-to-string (stream)
		 (format stream "(init-view-buffers `(")
		 (dolist (param *params-shm*)
		   (format stream "~S " param))
		 (format stream "))")))

    ;; Use progn to do all at once
    (memcpy-shm-to-cache* (loop :for params :in *params-shm*
			     :collect (second params)))
    
    ;; Enable draw flag for view loop
    (eval-sync conn (format nil "(setf *draw* t)"))))

(defun init-graph ()
  ;; Create DAG
  (let ((digraph (digraph:make-digraph)))

    (setf (digraph *model*) digraph)
    
    ;; Node 1
    (let ((n-0 (init-node (vec3 0 0 0)
			  (scale-node *model*)
			  0
			  "LISP"))
	  (n-1 (init-node (vec3 0 1 0)
			  (scale-node *model*)
			  1
			  "C"))
	  (n-2 (init-node (vec3 0 2 0)
			  (scale-node *model*)
			  2
			  "GLSL")))

      (digraph:insert-vertex digraph n-0)
      (digraph:insert-vertex digraph n-1)
      (digraph:insert-vertex digraph n-2)
      
      (digraph:insert-edge digraph n-0 n-1)
      (digraph:insert-edge digraph n-1 n-2)
      
      (copy-nodes-to-shm)
      (copy-textures-to-shm)
      
      (fmt-model t "main-model" "Init conn to view swank server~%")
      (setup-view)

      ;; TEST LIVE TEXTURE
      ;; Change texture after 5 seconds
      
      (sleep 6)

      (fmt-model t "main-model" "Updating texture~%")
      
      ;; Generate texture directly to shm
      ;; Update node
      ;; Tell view to copy to cache
      (update-node-texture n-0 "1234")
      (update-transform (model-matrix n-0))

      ;;;;;;;;;;;;;;;
      ;; Make atomic
      ;; otherwise will result in possible delay or "tearing"
      
      (copy-textures-to-shm)
      (copy-node-to-shm n-0
			(* (index n-0)
			   (/ +size-struct-instance+ 4)))

      (memcpy-shm-to-cache "texture"
			   (offset-bytes-textures *model*))
      (memcpy-shm-to-cache "instance"
			   (* +size-struct-instance+ (digraph:count-vertices digraph)))

      ;; Set flags so cache->step
      ;; Important to make sure all steps are updated
      ;; otherwise flickering will occur
      ;; Simplest method is to set a counter and
      ;; copy every frame until counter is 0
      ;; Make this function more congruent with memcpy
      (set-cache-dirty "texture" 3)
      (set-cache-dirty "instance" 3))))

(defun main-model (width height
		   inst-max
		   path-server-model)

  (start-swank-server 10000)

  (defparameter *model* (init-model width
				    height
				    inst-max
				    path-server-model))
  
  (fmt-model t "main-model" "Init shm data~%")

  (init-shm-data)

  (fmt-model t "main-model" "Init graph~%")

  (init-graph)
  
  (loop (sleep 0.0167)))

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
