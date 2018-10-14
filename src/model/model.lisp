(in-package :protoform.model)

(defun fmt-model (dst ctx-str ctl-str &rest rest)
  ;; Add space opt
  (apply #'format
	 dst
	 (str:concat (format nil "[MODEL:~a][~a] " (sb-posix:getpid) ctx-str)
		     ctl-str)
	 rest))

;; For now, determine these through view - maybe model can request from view?
;; GL_MAX_SHADER_STORAGE_BLOCK_SIZE = 134217728 = 134.217728 MBs
;; GL_MAX_TEXTURE_BUFFER_SIZE       = 134217728 = 134.217728 MBs
;;
;; Cache/compute will use cs-in
;; Step/raster will use vs-in
(defparameter *projview-shm* (list :uniform-buffer
				   "projview"
				   "/protoform-projview"
				   (* (+ 16 16) 4)
				   0 0  ; cs-in (cache), vs-in (raster)
				   :triple
				   -1)) ; copy every frame

(defparameter *vertices-shm* (list :uniform-buffer
				   "vertices"
				   "/protoform-vertices"
				   (* 16 4)
				   1 1
				   :triple
				   0))

(defparameter *nodes-shm* (list :shader-storage-buffer
				"nodes"
				"/protoform-nodes"
				(/ 134217728 4)
				2 3
				:triple
				0))

;; (defparameter *glyphs-msdf-shm* (list :texture-buffer
;; 				      "glyphs-msdf"
;; 				      "/protoform-glyphs-msdf"
;; 				      16465920 ; size of all ppm glyphs
;; 				      -1 -1
;; 				      :triple
;; 				      0
;; 				      :rgba8))

(defparameter *texture-shm* (list :texture-buffer
				  "texture"
				  "/protoform-texture"
				  (/ 134217728 4)
				  -1 -1
				  :triple
				  0
				  :rgba8)) ; requires fmt type

(defparameter *element-shm* (list :element-array-buffer
				  "element"
				  "/protoform-element"
				  (* 4 6)  ; 4 bytes/int * 6 ints or indices
				  -1 -1
				  :triple
				  0))

(defparameter *draw-indirect-shm* (list :draw-indirect-buffer
					"draw-indirect"
					"/protoform-draw-indirect"
					(* 4 6)  ; 6 ints/params
					-1 -1
					:triple
					0))

(defparameter *atomic-counter-shm* (list :atomic-counter-buffer
					 "atomic-counter"
					 "/protoform-atomic-counter"
					 (* 4 6)  ; 6 ints/params
					 4 -1
					 :triple
					 0))

(defparameter *params-shm* (list *projview-shm*
				 *vertices-shm*
				 *nodes-shm*
				 ;; *glyphs-msdf-shm*
				 *texture-shm*
				 *element-shm*
				 *draw-indirect-shm*
				 *atomic-counter-shm*))

(defconstant +size-struct-instance+ 208)

;; From Solaris Red
;; 220  50  47
(defparameter *color-default-ptr* (list  (coerce (/ 220 255) 'single-float)
					 (coerce (/ 50 255)  'single-float)
					 (coerce (/ 47 255)  'single-float)
					 (coerce (/ 255 255) 'single-float)
					 
					 (coerce (/ 220 255) 'single-float)
					 (coerce (/ 50 255)  'single-float)
					 (coerce (/ 47 255)  'single-float)
					 (coerce (/ 255 255) 'single-float)
					 
					 (coerce (/ 220 255) 'single-float)
					 (coerce (/ 50 255)  'single-float)
					 (coerce (/ 47 255)  'single-float)
					 (coerce (/ 255 255) 'single-float)
					 
					 (coerce (/ 220 255) 'single-float)
					 (coerce (/ 50 255)  'single-float)
					 (coerce (/ 47 255)  'single-float)
					 (coerce (/ 255 255) 'single-float)))

(defclass model ()
  ((conn-swank :accessor conn-swank :initarg :conn-swank :initform nil)
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

   (metrics :accessor metrics :initarg :metrics :initform nil)
   (node-pointer :accessor node-pointer :initarg :node-pointer :initform nil)
   (cursor :accessor cursor :initarg :cursor :initform (vec3 -10 0 0))
   (dpi-glyph :accessor dpi-glyph :initarg :dpi-glyph :initform (/ 1 90))
   (scale-node :accessor scale-node :initarg :scale-node :initform 0.008)))

;; Do atomic counter also?
(defun init-model (width
		   height
		   inst-max)

  (let* ((model (make-instance 'model
			       :projview (make-instance 'projview
							:width width
							:height height
							:projection-type 'orthographic)
			       :inst-max inst-max)))
    
    ;; (setf (scale-node model) (* 5.8239365 (dpi-glyph model)))
  
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

    ;; top right, bottom right, bottom left, top left
    ;;
    ;; 3---0
    ;; | / |
    ;; 2---1
    ;;
    ;; ccw: 0 2 1 0 3 2    
    (with-slots (ptr size)
	(gethash "vertices" handles-shm)
      (let ((data (make-array (* 4 4)
			      :element-type 'single-float
			      :initial-contents (list 1.0  1.0  0.0  1.0
						      1.0  0.0  0.0  1.0
						      0.0  0.0  0.0  1.0
						      0.0  1.0  0.0  1.0))))
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

(defun setup-view (addr-swank-view)

  ;; Get below from addr-swank-view
  
  (let ((conn (init-swank-conn "skynet" 10001)))

    (setf (conn-swank *model*) conn)
    (setf (swank-protocol::connection-package conn) "protoform.view")
    
    ;; view should not concern itself with inst-max...
    (with-slots (projview inst-max)
	*model*
      (with-slots (width height)
	  projview
	(eval-sync conn
		   (format nil "(setf *view* (init-view-programs ~S ~S ~S))" width height inst-max))))
    
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

(defun init-graph-msdf ()
  (with-slots (cursor
	       scale-node
	       digraph
	       node-pointer)
      *model*

    (setf digraph (digraph:make-digraph))

    ;; Create pointer node
    (let ((node-ptr (init-node-msdf (vcopy3 cursor)
				    scale-node
				    0
				    #\>
				    *color-default-ptr*)))
      
      (update-transform (model-matrix node-ptr))
      
      (digraph:insert-vertex digraph node-ptr)
      
      (copy-nodes-to-shm)
      ;; (copy-textures-to-shm)

      (setf node-pointer node-ptr))))

(defun main-model (width height
		   inst-max
		   addr-swank-view)

  (start-swank-server 10000)
  
  (defparameter *model* (init-model width
				    height
				    inst-max))
  
  (fmt-model t "main-model" "Init shm data~%")
  (init-shm-data)
  (init-glyph-data)
  (setf (metrics *model*) (init-metrics))
  
  (fmt-model t "main-model" "Init graph~%")
  (init-graph-msdf)

  (fmt-model t "main-model" "Init conn to view swank server~%")
  (setup-view addr-swank-view)
  
  (defparameter *controller* (init-controller))

  (register-keyboard-callbacks)
  
  (loop
     (progn

       ;; Poll for input events
       ;; Dispatch event handlers to update status
       (wait-epoll)

       ;; Dispatch callbacks in response to status
       ;; Reset keyboard keys
       ;; Question: Perform dispatch after updating all events or per event?
       (dispatch-all-seq-key)
       (reset-release-keys))))

(defun register-keyboard-callbacks ()

  (with-slots (key-callbacks)
      *controller*

    ;; +xk-return+    
    ;; +xk-escape+
    ;; +xk-backspace+
    ;; +xk-delete+
    ;; +xk-left+
    ;; +xk-right+
    ;; +xk-up+
    ;; +xk-down+
    ;; +xk-minus+
    ;; +xk-equal+
    ;; +xk-up+
    ;; +xk-down+
    ;; +xk-left+
    ;; +xk-right+
    
    (register-callback key-callbacks
		       (list +xk-escape+)
		       (list :press)
		       (lambda (seq-key)
			 (clean-up-model)
			 (let ((sock-swank (swank-protocol:connection-socket (conn-swank *model*))))
			   (usocket:socket-shutdown sock-swank :io)
			   (usocket:socket-close sock-swank))
			 (fmt-model t "handle-escape" "Model process exiting!~%")
			 (sb-ext:exit)))

    (register-callback key-callbacks
		       (list +xk-backspace+)
		       (list :press)
		       #'backspace-node-msdf)
    
    (when t
      (loop
	 :for keysym :from 32 :to 255
	 :do (progn
	       (register-callback key-callbacks
				  (list keysym)
				  (list :press)
				  #'add-node-msdf))))

    ;; ;;; /* Modifiers */
    ;; (defconstant +xk-shift-l+ #xffe1) ;  Left shift 
    ;; (defconstant +xk-shift-r+ #xffe2) ;  Right shift 
    ;; (defconstant +xk-control-l+ #xffe3) ;  Left control 
    ;; (defconstant +xk-control-r+ #xffe4) ;  Right control 
    ;; (defconstant +xk-caps-lock+ #xffe5) ;  Caps lock 
    ;; (defconstant +xk-shift-lock+ #xffe6) ;  Shift lock 
    ;; (defconstant +xk-meta-l+ #xffe7) ;  Left meta 
    ;; (defconstant +xk-meta-r+ #xffe8) ;  Right meta 
    ;; (defconstant +xk-alt-l+ #xffe9) ;  Left alt 
    ;; (defconstant +xk-alt-r+ #xffea) ;  Right alt 
    ;; (defconstant +xk-super-l+ #xffeb) ;  Left super 
    ;; (defconstant +xk-super-r+ #xffec) ;  Right super 
    ;; (defconstant +xk-hyper-l+ #xffed) ;  Left hyper 
    ;; (defconstant +xk-hyper-r+ #xffee) ;  Right hyper 

    ;; Modifier keys remain in press state instead of repeat

    ;; Test Ctrl-X
    (when nil
      (register-callback key-callbacks
			 (list +xk-control-l+ +xk-x+)
			 (list :press :press)
			 (lambda (seq-key)
			   (format t "CALLBACK: ~a~%" seq-key))))
    
    ;; Print hashtable
    (when nil
      (maphash (lambda (key value)
		 (format t "Seq-key: ~S = ~S~%" key value)
		 (maphash (lambda (key value)
			    (format t "  Seq-state: ~S = ~S~%" key value)
			    (maphash (lambda (key value)
				       (format t "    Callback: ~S = ~S~%" key value))
				     value))
			  value))
	       key-callbacks))
    t))
