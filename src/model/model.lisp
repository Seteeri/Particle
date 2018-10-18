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
   (dpi-glyph :accessor dpi-glyph :initarg :dpi-glyph :initform (/ 1 90))
   (scale-node :accessor scale-node :initarg :scale-node :initform 0.008)))

(defun init-shm ()
  (init-handle-shm (handles-shm *model*)
		   *params-shm*)
  ;; instance and texture is done later
  (copy-projview-to-shm nil)
  (copy-shm-vertices)
  (copy-shm-element)
  (copy-shm-draw-indirect))

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
  (with-slots (scale-node
	       digraph
	       node-pointer)
      *model*

    (setf digraph (digraph:make-digraph))

    ;; Create pointer node
    (let ((node-ptr (init-node-msdf (vec3 -11.5199995 14.127416 0)
				    scale-node
				    0
				    #\*
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

  ;; (setf (scale-node model) (* 5.8239365 (dpi-glyph model)))
  (defparameter *model* (make-instance 'model
				       :projview (make-instance 'projview
								:width width
								:height height
								:projection-type 'orthographic)
				       :inst-max inst-max))
  
  (fmt-model t "main-model" "Init shm data~%")
  (init-shm)

  (fmt-model t "main-model" "Init glyph data~%")
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

       ;; Update states
       ;; - For key events, copy index 0 to index 1, set index 0
       ;; Dispatch state callbacks
       ;; - For key states in index 0, do callbacks
       ;; Update states
       ;; - rel -> up
       ;; - press -> down
       
       (dispatch-events-input)

       (dispatch-all-seq-event)

       (reset-states-key)

       t)))

(defun register-callback-down (keysym cb)
  (with-slots (key-callbacks)
      *controller*
    (register-callback (list keysym :press)
		       ()
		       :exclusive
		       cb)
    (register-callback (list keysym :repeat)
		       ()
		       :exclusive
		       cb)))
  
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

    ;; +xk-shift-l+ #xffe1   ;  Left shift 
    ;; +xk-shift-r+ #xffe2   ;  Right shift 
    ;; +xk-control-l+ #xffe3   ;  Left control 
    ;; +xk-control-r+ #xffe4   ;  Right control 
    ;; +xk-caps-lock+ #xffe5   ;  Caps lock 
    ;; +xk-shift-lock+ #xffe6   ;  Shift lock 
    ;; +xk-meta-l+ #xffe7   ;  Left meta 
    ;; +xk-meta-r+ #xffe8   ;  Right meta 
    ;; +xk-alt-l+ #xffe9   ;  Left alt 
    ;; +xk-alt-r+ #xffea   ;  Right alt 
    ;; +xk-super-l+ #xffeb   ;  Left super 
    ;; +xk-super-r+ #xffec   ;  Right super 
    ;; +xk-hyper-l+ #xffed   ;  Left hyper 
    ;; +xk-hyper-r+ #xffee   ;  Right hyper 

    ;; +xk-left+ #xff51   ;  Move left, left arrow 
    ;; +xk-up+ #xff52   ;  Move up, up arrow 
    ;; +xk-right+ #xff53   ;  Move right, right arrow 
    ;; +xk-down+ #xff54   ;  Move down, down arrow 

    (when t
      (register-callback (list +xk-escape+ (list :press))
    			 :exclusive
    			 (lambda (seq-key)
    			   (clean-up-handles-shm)
    			   (let ((sock-swank (swank-protocol:connection-socket (conn-swank *model*))))
    			     (usocket:socket-shutdown sock-swank :io)
    			     (usocket:socket-close sock-swank))
    			   (fmt-model t "handle-escape" "Model process exiting!~%")
    			   (sb-ext:exit))))
    
    (when t
      ;; handlers in node
      (loop
	 :for keysym :from 32 :to 255
	 :do (register-callback (list keysym (list :press :repeat))
				:exclusive
				#'add-node-msdf)))

    (when t
      ;; handlers in node
      (dolist (seq-event (list (list +xk-left+       #'move-pointer-left)
			       (list +xk-up+         #'move-pointer-up)
			       (list +xk-right+      #'move-pointer-right)
			       (list +xk-down+       #'move-pointer-down)
			       (list +xk-backspace+  #'backspace-node-msdf)
			       (list +xk-return+     #'return-node-msdf)))
	  (register-callback (list (first seq-event) (list :press :repeat))
			     :exclusive
			     (second seq-event))))
    
    (when t
      ;; handlers in projview
      (dolist (seq-event (list (list +xk-left+       #'move-camera-left)
			       (list +xk-up+         #'move-camera-up)
			       (list +xk-right+      #'move-camera-right)
			       (list +xk-down+       #'move-camera-down)))
	  (register-callback (list +xk-control-l+ (list :press :down) (first seq-event) (list :press :repeat))
			     :exclusive
			     (second seq-event))))
    
    ;; Print hashtable
    (when nil
      (maphash (lambda (key value)
		 (fmt-model t "register-keyboard..." "Seq-event: ~S = ~S~%" key value))
	       key-callbacks))
    t))
