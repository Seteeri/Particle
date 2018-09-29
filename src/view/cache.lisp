(in-package :protoform.view)

(defun init-bo-caches (params-model)
  ;; "projview":0
  ;; "instance":1
  ;; "texture":-1
  (dolist (params params-model)
    (destructuring-bind (target name path size bind-cs bind-vs) params
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
	  (init-buffer-object program-compute
			      target
			      name
			      size
			      bind-cs
			      t
			      :buffering 'single))))
