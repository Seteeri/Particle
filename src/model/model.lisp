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
   (offset-texel-textures :accessor offset-texel-textures :initarg :offset-texel-textures :initform 0) ; sum of wxh
   (offset-bytes-textures :accessor offset-bytes-textures :initarg :offset-bytes-textures :initform 0) ; sum of bytes
   (textures :accessor textures :initarg :textures :initform (make-array 64 :adjustable t))
   
   (cursor :accessor cursor :initarg :cursor :initform (vec3 0 0 0))
   ;; move to node? used in conjunction with scale-node
   (dpi-glyph :accessor dpi-glyph :initarg :dpi-glyph :initform (/ 1 90))
   ;; rename to scale-default-node
   (scale-node :accessor scale-node :initarg :scale-node :initform (vec3 1.0 1.0 1.0))))

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

    ;; Node 1
    (fmt-model t "main-model" "Init data~%")
    (let ((node (init-node (vec3 0 0 0)
			   (scale-node model)
			   "QWERTY")))
      ;; Copy to shm before sending signal to view
      (copy-node-to-shm node 0))

    (let ((node (init-node (vec3 0 1 0)
			   (scale-node model)
			   "ASDF")))
      ;; Copy to shm before sending signal to view
      (copy-node-to-shm node (/ 208 4)))
    
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
