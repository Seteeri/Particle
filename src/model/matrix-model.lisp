(in-package :protoform.model)

(defclass model-matrix ()
  ((matrix :accessor matrix :initarg :matrix :initform (mat4 1))
   (translation :accessor translation :initarg :translation :initform (vec3 0.0 0.0 0.0))
   (rotation :accessor rotation :initarg :rotation :initform (vec3 0.0 0.0 0.0))
   (scale :accessor scale :initarg :scale :initform (vec3 1.0 1.0 1.0))))
