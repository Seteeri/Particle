(in-package :protoform.model)

(defun handle-event-touch (event)
  ;; Libinput coords are from top left
  (let* ((event-touch (libinput:event-get-touch-event         event))
	 (x           (libinput:event-touch-get-x-transformed event-touch 2880)) ;2560)) ; screen
	 (y           (libinput:event-touch-get-y-transformed event-touch 1920)) ;1080)) ; screen
	 (x-mm        (libinput:event-touch-get-x             event-touch))
	 (y-mm        (libinput:event-touch-get-y             event-touch))
	 (time        (libinput:event-touch-get-time          event-touch)))
    ;; (format t "~,2f ~,2f (~,2f/~,2fmm)~%" x y x-mm y-mm)

    ;; Default window position: (50 64) also from top left
    ;;
    ;; Now Windows coordinates start with (0, 0) being at the top left whereas
    ;; OpenGL coords start at the lower left.
    ;; Convert to OpenGL

    ;; Need to clamp touch to window
    
    (let* ((x-win    (- x 50))
	   (y-win    (- y 64))
	   (x-win-gl x-win)              ; need not adjust x since both are rel from left
	   (y-win-gl (- *height* y-win)) ; make rel to lower
	   (coords-touch (unproject (vec3 x-win-gl y-win-gl 0)
				    (mat-view *projview*)
				    (mat-proj *projview*)
				    (vec4 0 0 *width* *height*)))) ; x y - lower left, w h
      (format t "~,2f, ~,2f ->  ~,2f, ~,2f~%" x-win-gl y-win-gl (vx3 coords-touch) (vy3 coords-touch))

      (with-slots (model-matrix)
	  *node-pointer*
	(when t
	  (translate-node-ptr t ;seq-event
			      (lambda (value-new) ; update fn
				(setf (vx3 (translation model-matrix)) value-new)
				(enqueue-node-ptr))
			      (vx3 (translation model-matrix)) ; start
			      (- (vx3 coords-touch)
				 (vx3 (translation model-matrix)))
			      'move-pointer-x))
	(when t	
	  (translate-node-ptr t ;seq-event
			      (lambda (value-new) ; update fn
				(setf (vy3 (translation model-matrix)) value-new)
				(enqueue-node-ptr))
			      (vy3 (translation model-matrix)) ; start
			      (- (vy3 coords-touch)
				 (vy3 (translation model-matrix)))
			      'move-pointer-y)))
	
      t)))

(defun handle-event-touch-2 (event)
  (let* ((event-touch (libinput:event-get-touch-event         event))
	 (time        (libinput:event-touch-get-time          event-touch)))
    t))

(defun unproject (point modelview-matrix perspective-matrix viewport)
  (declare (type vec3 point)
           (type matrix modelview-matrix perspective-matrix)
           (type vec4 viewport))

  (let* ((inv-pm (minv (m* perspective-matrix
			   modelview-matrix)))
	 (new-point (vec4 (float
                           (1- (/ (* 2 (- (vx3 point) (vx4 viewport)))
                                  (vz4 viewport))))
                          (float
                           (1- (/ (* 2 (- (vy3 point) (vy4 viewport)))
                                  (vw4 viewport))))
                          (float (1- (* 2 (vz3 point))))
                          1.0))
         (obj (m* inv-pm new-point)))
    (v/ (vec3 (vx4 obj) (vy4 obj) (vz4 obj))
	(vw4 obj))))

  
