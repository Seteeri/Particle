(in-package :protoform.drm)

(defclass gbm () 
  ((dev :accessor dev :initarg :dev :initform nil)
   (surface :accessor surface :initarg :surface :initform nil)
   (width :accessor width :initarg :width :initform nil)
   (height :accessor height :initarg :height :initform nil)))

(defun init-gbm (fd hdisplay vdisplay modifier)
  (let* ((gbm (make-instance 'gbm))
	 (device (gbm:create-device fd))
	 (surface (gbm:surface-create device
				      hdisplay vdisplay ; implement modifiers
				      0; 0=GBM_BO_FORMAT_XRGB8888, 1=GBM_BO_FORMAT_ARGB8888
				      5))) ; GBM_BO_USE_SCANOUT | GBM_BO_USE_RENDERING

    (format t "[init-gbm] hdisplay: ~a~%" hdisplay)
    (format t "[init-gbm] vdisplay: ~a~%" vdisplay)
    (format t "[init-gbm] device: ~a~%" device)
    (format t "[init-gbm] surface: ~a~%" surface)
    
    (when (null-pointer-p surface)
      (error "Failed to create gbm surface")
      (return-from init-gbm))
    
    (setf (dev gbm) device)
    (setf (surface gbm) surface)
    (setf (width gbm) hdisplay)
    (setf (height gbm) vdisplay)
    
    gbm))
