(in-package :protoform.view)

(defclass cache ()
  ((buffer :accessor buffer :initarg :buffer :initform nil)
   (dirty :accessor dirty :initarg :dirty :initform nil)))

(defun init-bo-caches (params-model)
  ;; "projview":0
  ;; "instance":1
  ;; "texture":-1
  (dolist (params params-model)
    (destructuring-bind (target name path size bind-cs bind-vs &rest rest) params
      (init-bo-cache target
		     name
  		     size
		     bind-cs))))

(defun init-bo-cache (target
		      name
		      size
		      bind-cs)
  (with-slots (program-compute bo-cache) *view*
    (setf (gethash name bo-cache)
	  (make-instance 'cache
			 :buffer (init-buffer-object target
						     name
						     size
						     bind-cs
						     t
						     :buffering 'single)))))

(defun get-cache-buffer (name)
  (buffer (gethash name (bo-cache *view*))))
