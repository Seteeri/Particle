(in-package :protoform.drm)

(defclass crtc ()
  ((crtc :accessor crtc :initarg :crtc :initform nil)
   (props :accessor props :initarg :props :initform nil)
   (props-info :accessor props-info :initarg :props-info :initform nil)))

(defun init-crtc (drm)    

  (let ((crtc (make-instance 'crtc)))

    (setf (crtc crtc) (drm:mode-get-crtc (fd drm) (crtc-id drm)))
    (when (null-pointer-p (crtc crtc))
      (error "Could not get crtc~%"))

    (setf (props crtc) (drm:mode-object-get-properties (fd drm) (crtc-id drm) #xcccccccc)) ; DRM_MODE_OBJECT_CRTC
    (when (null-pointer-p (props crtc))
      (error "Could not get properties for crtc~%"))
    (setf (props-info crtc) (drm:get-object-properties (fd drm) (props crtc)))
    
    crtc))
