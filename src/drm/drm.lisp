(in-package :protoform.drm)

(defun main-drm ()
  
  ;; ;; Use when running sbcl to prevent SysRq kill
  ;; ;; 9,17,19,23
  ;; (iter (for i in '(1 2 3 4 6 8 15 16 18 20))
  ;; 	(set-signal-handler i
  ;; 			    (format t "SIG* received~%")
  ;; 			    (quit)))
  
  (let* ((drm       (init-drm))
	 (width     (drm:get-mode-hdisplay (mode drm)))
	 (height    (drm:get-mode-vdisplay (mode drm)))
	 (gbm       (init-gbm (fd drm) width height nil))
	 (egl       (init-egl gbm))
	 (gles      (init-gles width height))
	 (dev-input (init-dev-input)))

    (init-msdf width height)
    (init-dev-input-epoll dev-input)

    (multiple-value-bind (bo fb)
	(run drm
	     gbm
	     egl
	     dev-input)
      (shutdown drm
		gbm
		egl
		bo
		fb))))


(defclass drm () 
  ((fd :accessor fd :initarg :fd :initform nil)
   (plane :accessor plane :initarg :plane :initform nil)
   (crtc :accessor crtc :initarg :crtc :initform nil)
   (connector :accessor connector :initarg :connector :initform nil)
   (crtc-index :accessor crtc-index :initarg :crtc-index :initform nil)

   ;; out - ptr
   (kms-in-fence-fd :accessor kms-in-fence-fd :initarg :kms-in-fence-fd :initform nil)
   (kms-out-fence-fd :accessor kms-out-fence-fd :initarg :kms-out-fence-fd :initform nil)

   (mode :accessor mode :initarg :mode :initform nil)
   (crtc-id :accessor crtc-id :initarg :crtc-id :initform nil)
   (connector-id :accessor connector-id :initarg :connector-id :initform nil)))


;; static void drm_fb_destroy_callback(struct gbm_bo *bo, void *data) {
;; 	int drm_fd = gbm_device_get_fd(gbm_bo_get_device(bo));
;; 	struct drm_fb *fb = data;
;; 	if (fb->fb_id) drmModeRmFB(drm_fd, fb->fb_id);
;; 	free(fb);
;; }
(defcallback destroy-user-data :void ((bo :pointer) (data :pointer))
	     (let ((drm-fd (gbm:device-get-fd (gbm:bo-get-device bo)))
		   (fb-id (foreign-slot-value data '(:struct drm:drm-fb) 'drm:fb-id)))
	       (when (> fb-id 0)
		 (drm:mode-rm-fb drm-fd fb-id))
	       (foreign-free data)))


;; TODO: Move to cl-egl

(defun create-fence (display fd)
  (with-foreign-object (attrib-list :int32 3)
    (setf (mem-aref attrib-list :int32 0) #x3145  ; :EGL-SYNC-NATIVE-FENCE-FD-ANDROID
	  (mem-aref attrib-list :int32 1) fd
	  (mem-aref attrib-list :int32 2) #x3038)  ; :EGL-NONE

    (let ((fence (foreign-funcall-pointer (getf *egl-fn-ptrs* 'eglCreateSyncKHR) nil
					  :pointer display
					  :int #x3144 ;:EGL-SYNC-NATIVE-FENCE-ANDROID
					  (:pointer :int32) attrib-list
					  :pointer)))
      (assert (not (null-pointer-p fence)))
      fence)))

;; TODO: Move to cl-drm, refactor to return respective instances
;;
;; Create macros?
;; (defmacro cfun-error var error-p error-msg)

(defun open-drm-device (path)
  (let ((fd (nix:open "/dev/dri/card0" nix:o-rdwr)))
    (when (< fd 0)
      (error "could not open drm device~%"))
    fd))

(defun init-drm ()
  (let* ((fd         (open-drm-device "/dev/dri/card0"))
	 (resources  (drm:get-resources fd))
	 (connector  (drm:find-connector fd resources))
	 (mode       (drm:find-mode fd connector))
	 (crtc-id    (drm:find-encoder-crtc-id fd resources connector))
	 (crtc-index (drm:find-crtc-index resources crtc-id))
	 (drm        (make-instance 'drm
				    :fd fd
				    :mode mode
				    :crtc-id crtc-id
				    :connector-id (foreign-slot-value connector
								      '(:struct drm:mode-connector)
								      'drm:connector-id)
				    :kms-out-fence-fd (foreign-alloc :int :initial-element -1))))
    
    ;; plane, crtc, connector are instanced later
    (unless (zerop (drm:set-client-cap fd
				       3 ; drm-client-cap-atomic
				       1))
      (error "Atomic modesetting unsupported"))

    ;; Must this be done after set-client-cap?
    (setf (plane drm)     (init-plane fd crtc-index)
	  (crtc drm)      (init-crtc fd crtc-id)
	  (connector drm) (init-connector fd connector-id))

    
    (format t "[init-drm] fd: ~a~%"           fd)
    (format t "[init-drm] resources: ~a~%"    resources)
    (format t "[init-drm] mode: ~a~%"         mode)
    (format t "[init-drm] crtc-id: ~a~%"      crtc-id)
    (format t "[init-drm] crtc-index: ~a~%"   crtc-index)
    (format t "[init-drm] connector-id: ~a~%" connector-id)
    
    drm))


(defun commit (drm fb-id flags)
  
  (let ((req (drm:mode-atomic-alloc))
	(blob-id (foreign-alloc :uint32)))

    ;; Do various properties
    (unless (zerop (logand flags #x0400))
      
      (when (< (drm:add-connector-property (props (connector drm)) (props-info (connector drm)) req (connector-id drm) "CRTC_ID" (crtc-id drm))
	       0)
	(error "drm:add-connector-property"))
      
      (when (/= (drm:mode-create-property-blob (fd drm) (mode drm) (foreign-type-size '(:struct drm:mode-mode-info)) blob-id)
		0)
	(error "drm:mode-create-property-blob"))
      
      (when (< (drm:add-crtc-property (props (crtc drm)) (props-info (crtc drm)) req (crtc-id drm) "MODE_ID" (mem-ref blob-id :uint32))
	       0)
	(error "drm:add-crtc-property"))
      
      (when (< (drm:add-crtc-property (props (crtc drm)) (props-info (crtc drm)) req (crtc-id drm) "ACTIVE" 1)
	       0)
	(error "drm:add-crtc-property")))

    
    (let ((plane-id (foreign-slot-value (plane (plane drm)) '(:struct drm:mode-plane) 'drm:plane-id))
	  (props (props (plane drm)))
	  (props-info (props-info (plane drm))))
      
      ;; Do plane properties
      (funcall (lambda (props props-info req plane-id &rest pairs)
		 (iter (for (n v) on pairs by 'cddr)
		       (apply #'drm:add-plane-property props props-info req plane-id n v ())))
	       props
	       props-info
	       req
	       plane-id
	       "FB_ID" fb-id
	       "CRTC_ID" (crtc-id drm)
	       "SRC_X" 0
	       "SRC_Y" 0
	       "SRC_W" (ash (drm:get-mode-hdisplay (mode drm)) 16)
	       "SRC_H" (ash (drm:get-mode-vdisplay (mode drm)) 16)
	       "CRTC_X" 0
	       "CRTC_Y" 0
	       "CRTC_W" (drm:get-mode-hdisplay (mode drm))
	       "CRTC_H" (drm:get-mode-vdisplay (mode drm)))
      
      ;; Do fences
      (when (/= (kms-in-fence-fd drm)
		-1)
	(drm:add-crtc-property (props (crtc drm))
			       (props-info (crtc drm))
			       req
			       (crtc-id drm)
			       "OUT_FENCE_PTR"
			       (pointer-address (kms-out-fence-fd drm)))
	(drm:add-plane-property props
				props-info
				req
				plane-id
				"IN_FENCE_FD"
				(kms-in-fence-fd drm))))
    
    ;; Do atomic commit
    (let ((ret (drm:mode-atomic-commit (fd drm) req flags (null-pointer))))
      
      (unless (zerop ret)
	(format t "mode-atomic-commit returned ~a~%" ret)
	(drm:mode-atomic-free req)
	(return-from commit ret))
      
      (when (/= (kms-in-fence-fd drm)
		-1)
	(nix:close (kms-in-fence-fd drm))
	(setf (kms-in-fence-fd drm) -1))

      (drm:mode-atomic-free req)

      ret)))

(defun wait-for-previous-commit (display kms-fence)
  (unless (null-pointer-p kms-fence)
    (loop
       (let ((status (foreign-funcall-pointer (getf *egl-fn-ptrs* 'eglClientWaitSyncKHR) ()
					      :pointer display
					      :pointer kms-fence
					      :int32 0
					      :uint64 #xFFFFFFFFFFFFFFFF ; EGLTimeKHR EGL_FOREVER_KHR 0xFFFFFFFFFFFFFFFFull
					      :int32)))
	 (when (= status #x30F6) ; :EGL-CONDITION-SATISFIED-KHR
	   (return))))
    (foreign-funcall-pointer (getf *egl-fn-ptrs* 'eglDestroySyncKHR) ()
			     :pointer display
			     :pointer kms-fence
			     :int)))

(defun run (drm gbm egl dev-input)
  
  (assert (= (mem-ref (kms-out-fence-fd drm) :int) -1))

  (let ((flags #x0200) ; DRM_MODE_ATOMIC_NONBLOCK
	(bo (null-pointer))
	(fb (null-pointer))
	(running t))
    
    (setf flags (logior flags #x0400))  ; DRM_MODE_ATOMIC_ALLOW_MODESET
    
    (loop
       :while running
       :do (let ((next-bo   (null-pointer))
		 (gpu-fence (null-pointer)) ;/* out-fence from gpu, in-fence to kms */
		 (kms-fence (null-pointer))) ;/* in-fence to gpu, out-fence from kms */
	     
	     (when (/= (mem-ref (kms-out-fence-fd drm) :int) -1)
	       (setf kms-fence
		     (create-fence (display egl)
				   (mem-ref (kms-out-fence-fd drm) :int)))
	       (assert (not (null-pointer-p kms-fence)))
	       
	       ;; /* driver now has ownership of the fence fd: */
	       (setf (mem-ref (kms-out-fence-fd drm) :int) -1)
	       
	       ;; /* wait "on the gpu" (ie. this won't necessarily block, but
	       ;;  * will block the rendering until fence is signaled), until
	       ;;  * the previous pageflip completes so we don't render into
	       ;;  * the buffer that is still on screen.
	       ;;  */
	       (foreign-funcall-pointer (getf *egl-fn-ptrs* 'eglWaitSyncKHR) ()
					:pointer (display egl)
					:pointer kms-fence
					:int32 0
					:int32))

	     ;; DO STUFF
	     ;; (gl:clear-color (- 1.0 (/ i 600.0)) (/ i 600.0) 0.0 1.0)
	     ;; (gl:clear :color-buffer-bit)
	     (input-main dev-input)
	     (when (not (msdf-run))
	       (setf running nil))
	     
	     ;; Insert fence to be singled in cmdstream.. this fence will be
	     ;; signaled when gpu rendering done
	     (setf gpu-fence (create-fence (display egl) -1)) ;:EGL-NO-NATIVE-FENCE-FD-ANDROID
	     (assert (not (null-pointer-p gpu-fence)))
	     
	     (egl:swap-buffers (display egl)
			       (surface egl))
	     
	     ;; After swapbuffers, gpu_fence should be flushed, so safe
	     ;; to get fd:
	     (setf (kms-in-fence-fd drm) (foreign-funcall-pointer (getf *egl-fn-ptrs* 'eglDupNativeFenceFDANDROID) ()
								  :pointer (display egl)
								  :pointer gpu-fence
								  :int32))
	     (foreign-funcall-pointer (getf *egl-fn-ptrs* 'eglDestroySyncKHR) ()
				      :pointer (display egl)
				      :pointer gpu-fence
				      :int)
	     (assert (/= (kms-in-fence-fd drm) -1))
	     
	     (setf next-bo (gbm:surface-lock-front-buffer (surface gbm)))
	     (when (null-pointer-p next-bo)
	       (error "Failed to lock frontbuffer~%"))

	     (setf fb (drm-fb-get-from-bo next-bo))
	     (when (null-pointer-p fb)
	       (error "Failed to get a new framebuffer BO~%"))
	     
	     ;; Wait on the CPU side for the _previous_ commit to
	     ;; complete before we post the flip through KMS, as
	     ;; atomic will reject the commit if we post a new one
	     ;; whilst the previous one is still pending.
	     (wait-for-previous-commit (display egl)
				       kms-fence)
	     
	     ;; Here you could also update drm plane layers if you want
	     ;; hw composition
	     (let ((ret (commit drm
				(foreign-slot-value fb '(:struct drm:drm-fb) 'drm:fb-id)
				flags)))
	       (unless (zerop ret)
		 (error "failed to commit modeset")))
	     
	     ;; Release last buffer to render on again
	     (unless (null-pointer-p bo)
	       (gbm:surface-release-buffer (surface gbm) bo))
	     (setf bo next-bo)

	     ;; Allow a modeset change for the first commit only
	     (setf flags (logand flags (lognot #x0400)))))

    ;; Return current bo/fb for shutdown function
    (values bo fb)))


(defun shutdown (drm gbm egl bo fb)

  (format t "[shutdown] Destroying backend~%")

  (let ((fd (fd drm))
	(crtc (crtc (crtc drm)))
	(gbm-surface (surface gbm))
	(egl-display (display egl)))
    
    ;; Restore previous crtc
    (unless (null-pointer-p crtc)
      (with-foreign-objects ((connector-id :uint32))
	(setf (mem-aref connector-id :uint32) (connector-id drm))
	(drm:mode-set-crtc fd
			   (foreign-slot-value crtc '(:struct drm:mode-crtc) 'drm:crtc-id)
			   (foreign-slot-value crtc '(:struct drm:mode-crtc) 'drm:buffer-id)
			   (foreign-slot-value crtc '(:struct drm:mode-crtc) 'drm:x)
			   (foreign-slot-value crtc '(:struct drm:mode-crtc) 'drm:y)
			   connector-id
			   1
			   (incf-pointer crtc (foreign-slot-offset '(:struct drm:mode-crtc) 'drm:mode)))
	(format t "[shutdown] Reset CRTC~%")))
    ;; (drm:mode-free-crtc crtc)) ; invalid pointer
    
    ;; Remove FB
    (unless (null-pointer-p bo)
      (drm:mode-remove-framebuffer fd (foreign-slot-value fb '(:struct drm:drm-fb) 'drm:fb-id))
      (gbm:surface-release-buffer gbm-surface bo))
    (format t "[shutdown] FB removed~%")

    ;; Destroy various objects
    (when (surface egl)
      (egl:destroy-surface egl-display (surface egl)))

    (when gbm-surface
      (gbm:surface-destroy gbm-surface))

    (when (context egl)
      (egl:destroy-context egl-display (context egl)))

    (when egl-display
      (egl:terminate2 egl-display))

    (when (dev gbm)
      (gbm:device-destroy (dev gbm)))

    (when fd
      (nix:close fd))))
