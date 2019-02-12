(in-package :protoform.drm)

(defclass plane ()
  ((plane :accessor plane :initarg :plane :initform nil)
   (props :accessor props :initarg :props :initform nil)
   (props-info :accessor props-info :initarg :props-info :initform nil)))

(defun init-plane (fd crtc-index)

  (let* ((plane (make-instance 'plane))
	 (plane-id (drm:find-plane-id fd
				      crtc-index)))
    (unless plane-id
      (error "Could not find unsuitable plane"))

    (format t "[init-drm-atomic] plane-id: ~a~%" plane-id)
    
    (setf (plane plane) (drm:mode-get-plane fd
					    plane-id))
    (when (null-pointer-p (plane plane))
      (error "Could not get plane~%"))

    (setf (props plane) (drm:mode-object-get-properties fd
							plane-id
							#xeeeeeeee)) ; DRM_MODE_OBJECT_PLANE
    
    (when (null-pointer-p (props plane))
      (error "Could not get properties for plane~%"))
    
    (setf (props-info plane) (drm:get-object-properties fd
							(props plane)))

    plane))
  
