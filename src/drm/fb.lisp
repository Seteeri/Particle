(in-package :protoform.drm)

;; TODO: Move to cl-drm

(defun drm-fb-get-from-bo (bo)
  (let ((fb (gbm:bo-get-user-data bo)))
    (when fb
      (return-from drm-fb-get-from-bo fb)))

  ;; TODO
  ;; Refactor as function in cl-drm
  
  ;; allocate drm-fb; free later in callback
  (let ((drm-fd (gbm:device-get-fd (gbm:bo-get-device bo)))
	(fb     (foreign-alloc '(:struct drm:drm-fb))))
    
    ;; set bo
    (setf (foreign-slot-value fb '(:struct drm:drm-fb) 'drm:bo) bo
	  (foreign-slot-value fb '(:struct drm:drm-fb) 'drm:fb-id) 0)
    
    (with-foreign-objects ((handles :uint32 4)
			   (pitches :uint32 4)
			   (offsets :uint32 4))
      
      (setf (mem-aref handles :uint32 0) (gbm:bo-get-handle bo))
      (loop
	 :for i :from 1 :below 4
	 :do (setf (mem-aref handles :uint32 i) 0))

      (setf (mem-aref pitches :uint32 0) (gbm:bo-get-stride bo))
      (loop
	 :for i :from 1 :below 4
	 :do (setf (mem-aref pitches :uint32 i) 0))

      (loop
	 :for i :from 1 :below 4
	 :do (setf (mem-aref offsets :uint32 i) 0))
      
      (drm:mode-add-fb2 drm-fd
      			(gbm:bo-get-width bo)
      			(gbm:bo-get-height bo)
      			(logior (char-code #\X)
      				(ash (char-code #\R) 8)
      				(ash (char-code #\2) 16)
      				(ash (char-code #\4) 24)) ; DRM_FORMAT_XRGB8888
      			handles
      			pitches
      			offsets
      			(foreign-slot-pointer fb '(:struct drm:drm-fb) 'drm:fb-id)
      			0))

      ;; (drm:mode-add-framebuffer drm-fd
      ;; 				(gbm:bo-get-width bo) (gbm:bo-get-height bo)
      ;; 				24 32 (gbm:bo-get-stride bo) (gbm:bo-get-handle bo)
      ;; 				(foreign-slot-pointer fb '(:struct drm:drm-fb) 'drm:fb-id)))
    
    (gbm:bo-set-user-data bo
			  fb
			  (callback destroy-user-data))

    fb))
