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
  ((sock-view :accessor sock-view :initarg :sock-view :initform nil)
   (buffer-sock-ptr :accessor buffer-sock-ptr :initarg :buffer-sock-ptr :initform (foreign-alloc :unsigned-char :count 212992))
   (buffer-sock-array :accessor buffer-sock-array :initarg :buffer-sock-array :initform (make-array 212992
												    :adjustable nil
												    :fill-pointer nil
												    :element-type '(unsigned-byte 8)))

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

(defun main-model (width height
		   inst-max
		   addr-swank-view)

  (fmt-model t "main-model" "Init kernel lparallel~%")
  (init-kernel-lparallel)
		 
  (submit-task *channel*
	       (lambda ()
		 (defparameter *model* (make-instance 'model
						      :projview (make-instance 'projview
									       :width width
									       :height height
									       :type-proj 'orthographic)
						      :inst-max inst-max))

		 ;; Load glyphs and metrics independently?
		 
		 (fmt-model t "main-model" "Init shm data~%")
		 (init-shm)
		 
		 (fmt-model t "main-model" "Init glyph data~%")
		 (init-glyph-data)
		 (setf (metrics *model*) (init-metrics))
		 
		 (fmt-model t "main-model" "Init graph~%")
		 (init-graph-msdf)

		 (fmt-model t "main-model" "Init conn to view~%")
		 (init-view)))

  (submit-task *channel*
	       (lambda ()  
		 (defparameter *controller* (init-controller))
		 (register-keyboard-callbacks)
		 t))
  
  (dotimes (i 2) (receive-result *channel*))

  ;; sock view loop
  (submit-task *channel*
	       #'serve-client)

  ;; input loop
  (submit-task *channel*
	       (lambda ()
		 (loop
		    (dispatch-events-input)
		    (dispatch-all-seq-event)
		    (update-states-keyboard-continuous))))
  
  ;; Block forever
  (loop (receive-result *channel*)))

(defun init-kernel-lparallel ()

  ;; Swank, Input, Wayland
  ;; (bordeaux-threads:make-thread (lambda () (sleep 1)))

  (setf *kernel* (make-kernel (+ 2 4)))
  (setf *channel* (make-channel))
  (setf *chan-anim* (make-channel))
  (setf *queue-anim* (make-queue)))

(defun init-view ()
  (with-slots (sock-view
	       buffer-sock-ptr
	       projview
	       inst-max)
      *model*
    
    (setf sock-view (init-sock-client "/tmp/protoform-view.socket" :block))
    
    ;; Init buffers
    (send-message sock-view
		  buffer-sock-ptr
		  (with-output-to-string (stream)
		    (format stream "(init-view-buffers (")
		    (dolist (param *params-shm*)
		      (format stream "~S " param))
		    (format stream "))")))

    (memcpy-shm-to-cache* (loop :for params :in *params-shm*
			     :collect (second params)))
  
    ;; Enable draw flag for view loop
    (send-message sock-view
		  buffer-sock-ptr
		  (format nil "(set-draw t)"))))

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

    (when nil
      (register-callback (list +xk-escape+ (list :press))
    			 :exclusive
    			 (lambda (seq-key)
    			   (clean-up-handles-shm)
    			   (c-shutdown sock-view)
    			   (c-close sock-view))
    			   (fmt-model t "handle-escape" "Model process exiting!~%")
    			   (sb-ext:exit)))
    
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
	(register-callback (list +xk-control-l+ (list :press :down)
				 (first seq-event) (list :press :repeat))
			   :exclusive
			   (second seq-event)))

      (dolist (seq-event (list (list +xk-left+       #'update-scale-ortho-out)
			       (list +xk-up+         #'update-scale-ortho-in)
			       (list +xk-right+      #'update-scale-ortho-in)
			       (list +xk-down+       #'update-scale-ortho-out)))
	(register-callback (list +xk-control-l+ (list :press :down)
				 +xk-shift-l+ (list :press :down)
				 (first seq-event) (list :press :repeat))
			   :exclusive
			   (second seq-event))))

    (register-callback (list +xk-f7+ (list :press))
		       :exclusive
		       (lambda (seq-event)
			 (setf *fn-anim* #'easing:in-cubic)
			 (setf *value-start* (vx3 (pos (projview *model*))))
			 (setf *time-start* (osicat:get-monotonic-time))
			 (setf *time-end* (+ *time-start* (/ (* 60 4) 60))) ; (/ frame count fps)
			 (setf *time-duration* (- *time-end* *time-start*)) ; (/ frame-count fps)
			 (setf *time-elapsed* 0.0)
			 (setf *time-run* t)))
			 ;; (submit-task *chan-anim*
			 ;; 	      #'produce-frames-anim)))

    
    ;; Print hashtable
    (when nil
      (maphash (lambda (key value)
		 (fmt-model t "register-keyboard..." "Seq-event: ~S = ~S~%" key value))
	       key-callbacks))
    t))

;; https://gamedev.stackexchange.com/questions/48227/smooth-movement-pygame

;; (:export :linear
;; 	 :in-sine :out-sine :in-out-sine
;; 	 :in-cubic :out-cubic :in-out-cubic
;; 	 :in-quad :out-quad :in-out-quad
;; 	 :in-quart :out-quart :in-out-quart
;; 	 :in-quint :out-quint :in-out-quint
;; 	 :in-exp :out-exp :in-out-exp
;; 	 :in-circ :out-circ :in-out-circ
;; 	 :in-elastic :out-elastic :in-out-elastic
;; 	 :in-back :out-back :in-out-back
;; 	 :in-bounce :out-bounce :in-out-bounce)

(defparameter *value-start* 0)
(defparameter *time-start* 0)
(defparameter *time-end* 0)
(defparameter *time-duration* 0)
(defparameter *time-elapsed* 0)
(defparameter *time-run* nil)
(defparameter *fn-anim* nil)

(defparameter *time-last* 0)

(defun handle-view-sync (time-view)

  ;; Submit tasks in this fn for each anim - each thread will do copy-to-shm
  ;;   -> Need lock on conn unless we do one conn per thread??

  ;; Input will trigger animations

  ;; Skipping/dropping frames will result in jerky/choppy animation

  ;; Model generates frames
  ;; - Updates shm
  ;; - Model sends msg
  ;; - Waits for view to recv and reply
  
  (when nil
    (let ((time (osicat:get-monotonic-time)))
      (format t "Model: ~8$ ms~%" (* (- time *time-last*) 1000))
      (setf *time-last* time)))

  (submit-task *chan-anim*
	       (lambda ()
		 
		 (when *time-run*
		   
		   (let* ((projview (projview *model*))
			  (time-now (osicat:get-monotonic-time))
			  (time-delta (- time-now *time-start*)))

		     (incf *time-elapsed* time-delta)

		     (when nil
		       (format t "~4$ { ~4$ } ~4$ (~4$) [~4$] ~%" *time-start* *time-elapsed* *time-end* *time-duration* time-delta)
		       (format t "  ~7$~%" (osicat:get-monotonic-time)))

		     (with-slots (pos scale-ortho)
      			 projview

		       ;; normalize x/time elapsed by dividing over duration
		       ;; normalize y by multiplying by displacement
		       ;; add to begin value to get new value otherwise its just relative displacement from beginning

		       (let ((pos-new (+ *value-start*
					 (* (funcall *fn-anim* (/ *time-elapsed* *time-duration*))
      					    -4.0))))
			 ;; (format t "t = ~a, y = ~a~%" (/ *time-elapsed* *time-duration*) (easing:out-exp (/ *time-elapsed* *time-duration*)))
      			 (setf (vx3 pos) pos-new))
		       
		       t)
		     (copy-projview-to-shm)
		     
		     ;; Cap time-delta to ending time
		     (when (> *time-elapsed* *time-duration*)
		       (format t "Ending anim~%")
		       (setf *time-run* nil))))))
      
  (dotimes (i 1) (receive-result *chan-anim*))

  ;; Send signal back
  
  t)

(defun serve-client ()
  (with-slots (sock-view
	       buffer-sock-ptr)
      *model*
    (loop
       (let ((message (recv-message sock-view
				    buffer-sock-ptr)))
	 (when message
	   ;; (fmt-model t "serve-client" "Message: ~S~%" message)
	   ;; (print (eval message))
	   ;; (force-output)

	   (if (listp (first message))
	       (dolist (n message)
		 (apply (symbol-function (find-symbol (string (first n)) :protoform.model))
			(cdr n)))
	       (apply (symbol-function (find-symbol (string (first message)) :protoform.model))
		      (cdr message))))))))
