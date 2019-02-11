(in-package :protoform.drm)

(defparameter *egl-fn-ptrs* nil)

(defclass egl () 
  ((display :accessor display :initarg :display :initform nil)
   (config :accessor config :initarg :config :initform nil)
   (context :accessor context :initarg :context :initform nil)
   (surface :accessor surface :initarg :surface :initform nil)))

(defun init-egl (gbm)

  (let* ((egl (make-instance 'egl))
	 (display (egl:get-display (dev gbm))))
    
    (multiple-value-bind (major minor)
	(egl:initialize display) ;; prints out code
      (format t "[init-egl] EGL version: ~d.~d~%" major minor))
    
    (egl:bind-api :opengl-es-api)
    
    (let* ((config (egl:choose-config display
				      1
				      :surface-type :window-bit
				      :red-size 1
				      :green-size 1
				      :blue-size 1
				      :alpha-size 0
				      :renderable-type :opengl-es3-bit
				      :none))
	   (context (egl:create-context display
    					(first config)
    					(null-pointer) ; EGL_NO_CONTEXT
					:context-client-version 3
					;; :context-major-version 4
					;; :context-minor-version 5
					;; :context-opengl-profile-mask :context-opengl-core-profile-bit
					;; #x30FC #x00000002
					:none))
	   ;; GL_CONTEXT_FLAGS_KHR                   0x30FC
	   ;; EGL_CONTEXT_OPENGL_FORWARD_COMPATIBLE_BIT_KHR  0x00000002
    	   (surface (egl:create-window-surface display
    					       (first config)
    					       (surface gbm)
    					       (null-pointer))))
      
      (format t "[init-egl] config: ~a~%" config)
      (format t "[init-egl] context: ~a~%" context)
      (format t "[init-egl] surface: ~a~%" surface)
      (format t "[init-egl] make-current: ~a~%" (egl:make-current display surface surface context))

      ;; flip this
      (setf (display egl) display
	    (config egl)  config
	    (context egl) context
	    (surface egl) surface))

    ;; Move *egl-fn-ptrs* to egl?
    (get-egl-fn-ptrs)
    
    egl))

(defun get-egl-fn-ptrs ()
  
  (with-foreign-strings ((n1 "eglDupNativeFenceFDANDROID")
			 (n2 "eglCreateSyncKHR")
			 (n3 "eglDestroySyncKHR")
			 (n4 "eglWaitSyncKHR")
			 (n5 "eglClientWaitSyncKHR"))
    
    (let ((ptr (egl:get-proc-address n1)))
      (setf (getf *egl-fn-ptrs* 'eglDupNativeFenceFDANDROID) ptr)
      (when (null-pointer-p ptr)
	(error "no eglDupNativeFenceFDANDROID")))

    (let ((ptr (egl:get-proc-address n2)))
      (setf (getf *egl-fn-ptrs* 'eglCreateSyncKHR) ptr)
      (when (null-pointer-p ptr)
	(error "no eglCreateSyncKHR")))

    (let ((ptr (egl:get-proc-address n3)))
      (setf (getf *egl-fn-ptrs* 'eglDestroySyncKHR) ptr)
      (when (null-pointer-p ptr)
	(error "no eglDestroySyncKHR")))

    (let ((ptr (egl:get-proc-address n4)))
      (setf (getf *egl-fn-ptrs* 'eglWaitSyncKHR) ptr)
      (when (null-pointer-p ptr)
	(error "no eglWaitSyncKHR")))

    (let ((ptr (egl:get-proc-address n5)))
      (setf (getf *egl-fn-ptrs* 'eglClientWaitSyncKHR) ptr)
      (when (null-pointer-p ptr)
	(error"no eglClientWaitSyncKHR")))))
