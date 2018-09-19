(in-package :protoform.drm)

(defclass connector ()
  ((connector :accessor connector :initarg :connector :initform nil)
   (props :accessor props :initarg :props :initform nil)
   (props-info :accessor props-info :initarg :props-info :initform nil)))

(defun init-connector (drm)

  (let ((connector (make-instance 'connector)))
    
    (setf (connector connector) (drm:mode-get-connector (fd drm) (connector-id drm)))
    (when (null-pointer-p (connector connector))
      (error "Could not get connector~%"))
  
    (setf (props connector) (drm:mode-object-get-properties (fd drm) (connector-id drm) #xc0c0c0c0)) ;DRM_MODE_OBJECT_CONNECTOR
    (when (null-pointer-p (props connector))
      (error "Could not get properties for connector~%"))
    (setf (props-info connector) (drm:get-object-properties (fd drm) (props connector)))

    connector))
