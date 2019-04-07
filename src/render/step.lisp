(in-package :protoform.render)

(defun init-bo-step (params-shm)

  (with-slots (width height
		     boav-main
		     bo-step
		     inst-max)
      *render*

    ;; Notes:
    ;; * Some buffers have a different bind layout per shader stage
    ;; * Texture requires setting fmt after and other ops
    ;; * Set initial data for buffers element and draw-indirect
    ;; * glMapNamedBuffer is unavailable so
    ;;   * to persistently map the buffer, it needs to be bound...
    ;;   * to bind a buffer, requires an appropriate program
    ;; * What is the behavior when binding a buffer with no program bound?
    ;;  * Doesn't matter here...
    
    (dolist (params params-shm)
      (destructuring-bind (target
			   path
			   size
			   bind-cs
			   bind-vs
			   count-buffer
			   flag-copy
			   &rest rest)
	  params
	
	(let ((bo (init-buffer-object target
    				      path
    				      size
    				      (if (> bind-vs -1) bind-vs bind-cs)
    				      t ; pmap
    				      :buffering count-buffer)))
	  (setf (gethash path bo-step)
		bo)

	  (when (eq target :texture-buffer)

	    ;; texturei max - GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
	    ;; Already active...
	    (gl:active-texture :texture0)
	    
	    (dotimes (i (count-buffers bo))
	      (update-binding-buffer bo i)
	      (%gl:tex-buffer :texture-buffer
			      (first rest) ; rgba8
			      (aref (buffers bo) i)))
	    
	    t))))))
