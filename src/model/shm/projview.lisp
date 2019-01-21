(in-package :protoform.model)

(defparameter *shm-projview* nil)

(defparameter *params-projview-shm* (list :uniform-buffer
					  "projview"
					  "/protoform-projview"
					  (* (+ 16 16) 4)
					  0 0  ; cs-in (cache), vs-in (raster)
					  :triple
					  0)) ; copy every frame

(defun init-shm-projview ()
  (let ((shm (init-shm :projview)))
    (setf *shm-projview* shm) ; below require this
    (update-mat-proj)
    (update-mat-view)
    (copy-mat-proj-to-shm)
    (copy-mat-view-to-shm)
    shm))
