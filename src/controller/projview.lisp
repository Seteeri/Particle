(in-package :protoform.controller)

;; Flag dirty also

(defun update-zoom-in (msdf keysym)
  (decf (ortho-scale (projview msdf)) (vz3 (camera-displacement (projview msdf))))
  (update-projview (projview msdf) (ipc-model msdf) (mapping-base msdf)))

(defun update-zoom-out (msdf keysym)
  (incf (ortho-scale (projview msdf)) (vz3 (camera-displacement (projview msdf))))
  (update-projview (projview msdf) (ipc-model msdf) (mapping-base msdf)))

(defun update-mm-left (msdf keysym)
  (with-slots (camera-position camera-displacement) (projview msdf)
    (decf (vx3 camera-position) (vx3 camera-displacement)))
  (update-projview (projview msdf) (ipc-model msdf) (mapping-base msdf)))

(defun update-mm-right (msdf keysym)
  (with-slots (camera-position camera-displacement) (projview msdf)
    (incf (vx3 camera-position) (vx3 camera-displacement)))
  (update-projview (projview msdf) (ipc-model msdf) (mapping-base msdf)))

(defun update-mm-up (msdf keysym)
  (with-slots (camera-position camera-displacement) (projview msdf)
    (incf (vy3 camera-position) (vy3 camera-displacement)))
  (update-projview (projview msdf) (ipc-model msdf) (mapping-base msdf)))

(defun update-mm-dn (msdf keysym)
  (with-slots (camera-position camera-displacement) (projview msdf)
    (decf (vy3 camera-position) (vy3 camera-displacement)))
  (update-projview (projview msdf) (ipc-model msdf) (mapping-base msdf)))
