(in-package :protoform.model)

(defclass controller () 
  ((context :accessor context :initarg :context :initform nil)
   (xkb :accessor xkb :initarg :xkb :initform (init-xkb))
   (key-states :accessor key-states :initarg :key-states :initform (make-hash-table :size 256))
   (key-callbacks :accessor key-callbacks :initarg :key-callbacks :initform (make-hash-table :size 256))
   (ep-events :accessor ep-events :initarg :ep-events :initform nil)
   (ep-fd :accessor ep-fd :initarg :ep-fd :initform nil)))   

(defun init-controller (&rest devices)
  (let ((controller (make-instance 'controller)))
    (with-slots (context
		 key-states
		 key-callbacks
		 ep-fd
		 ep-events)
	controller
      
      ;; Initialize libinput
      (setf context (libinput:path-create-context (libinput:make-libinput-interface)
						  (null-pointer)))
      ;; (let* ((fd-li (libinput:get-fd context-li)))

      (if devices
	  (mapcar (lambda (path)
		    (libinput:path-add-device context path))
		  devices)
	  (let ((device-paths (directory "/dev/input/event*")))
	    (loop
	       :for device-path :in device-paths
	       :do (let ((device (libinput:path-add-device context
							   (namestring device-path))))
		     (when (not (null-pointer-p device))
		       (if (not (or (libinput:device-has-capability device
								    libinput:device-cap-keyboard)
				    (libinput:device-has-capability device
								    libinput:device-cap-pointer)))
			   (libinput:path-remove-device device)))))))
      
      (setf ep-fd     (init-epoll context)
            ep-events (foreign-alloc '(:struct event))))
    controller))

(defun init-epoll (context)
  (let ((ep-fd (c-epoll-create 1)))
    (ctl-epoll ep-fd
	       (libinput:get-fd context)
	       #x0001 ; TODO: defconstant
	       :add)
    ep-fd))

(defun run-controller ()
  ;; Refactor below to per-event?
  (loop
     (dispatch-events-input)             ; serial
     (dispatch-all-seq-event)            ; parallel
     (update-states-keyboard-continuous) ; parallel
     t))

(defun dispatch-events-input ()
  ;; Poss pull events and submit task instead of processing in parallel
  ;; However, would need to allocate separate ep-events for each callback
  (with-slots (context
	       ep-fd
	       ep-events)
      *controller*
    ;; -1 = timeout = block/infinite
    ;; 0 = return if nothing
    (when (> (c-epoll-wait ep-fd ep-events 1 -1) 0)
      (loop
      	 :for event := (progn
			 (libinput:dispatch context)
			 (libinput:get-event context))
      	 :until (null-pointer-p event)
      	 :do
      	   (dispatch-event-handler event)
      	   (libinput:event-destroy event)
	 :finally (libinput:dispatch context)))))

(defun dispatch-event-handler (event)

  ;; libinput:none
  ;; libinput:device-added
  ;; libinput:device-removed
  ;; libinput:keyboard-key
  ;; libinput:pointer-motion
  ;; libinput:pointer-motion-absolute
  ;; libinput:pointer-button
  ;; libinput:pointer-axis
  ;; libinput:touch-down
  ;; libinput:touch-motion
  ;; libinput:touch-up
  ;; libinput:touch-cancel
  ;; libinput:touch-frame
  ;; libinput:gesture-swipe-begin
  ;; libinput:gesture-swipe-update
  ;; libinput:gesture-swipe-end
  ;; libinput:gesture-pinch-begin
  ;; libinput:gesture-pinch-update
  ;; libinput:gesture-pinch-end
  ;; libinput:tablet-tool-axis
  ;; libinput:tablet-tool-proximity
  ;; libinput:tablet-tool-tip
  ;; libinput:tablet-tool-button
  ;; libinput:tablet-pad-button
  ;; libinput:tablet-pad-ring
  ;; libinput:tablet-pad-strip
  ;; libinput:switch-toggle

  (let ((type (libinput:event-get-type event)))
    ;; (format t "type: ~a~%" type)
    (cond
      ((= type libinput:keyboard-key)
       (handle-event-keyboard event))
      ((= type libinput:pointer-motion)
       ;; (format t "type: ~a~%" type)
       t)
      ((= type libinput:pointer-button)
       t)
      (t
       t))))
