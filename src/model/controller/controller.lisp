(in-package :protoform.model)

(defclass controller () 
  ((context :accessor context :initarg :context :initform nil)
   (xkb :accessor xkb :initarg :xkb :initform (init-xkb))
   (epoll-events :accessor epoll-events :initarg :epoll-events :initform nil)
   (epoll-fd :accessor epoll-fd :initarg :epoll-fd :initform nil)
   (key-states :accessor key-states :initarg :key-states :initform (make-hash-table :size 256))
   (key-callbacks :accessor key-callbacks :initarg :key-callbacks :initform (make-hash-table :size 256))))

(defun init-controller (&rest devices)
  (let ((controller (make-instance 'controller)))
    (with-slots (context
		 key-states
		 key-callbacks
		 epoll-fd)
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
	       :do (let ((device (libinput:path-add-device context (namestring device-path))))
		     (when (not (null-pointer-p device))
		       (if (not (or (libinput:device-has-capability device libinput:device-cap-keyboard)
				    (libinput:device-has-capability device libinput:device-cap-pointer)))
			   (libinput:path-remove-device device)))))))
      
      (init-epoll controller)
      ;; (register-callbacks controller)
      t)
    controller))
    
(defun init-epoll (controller)
  
  (let ((epoll-fd (c-epoll-create 1)))

    (ctl-epoll epoll-fd
	       (libinput:get-fd (context controller))
	       #x0001
	       :add)
    
    (setf (epoll-fd controller) epoll-fd)
    (setf (epoll-events controller) (foreign-alloc '(:struct event)))))
       
(defun wait-epoll ()
  (with-slots (context
	       epoll-fd
	       epoll-events)
      *controller*
    ;; -1 = timeout = block/infinite
    ;; 0 = return if nothing
    (let ((epoll-fds (c-epoll-wait epoll-fd epoll-events 1 -1)))
      (when (> epoll-fds 0)
	(libinput:dispatch context)
	(loop
	   :for event := (libinput:get-event context)
	   :until (null-pointer-p event)
	   :do (progn
		 (dispatch-event-handler event)
		 (libinput:event-destroy event)
		 (libinput:dispatch context)))))))

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
  
(defun register-callbacks (controller)

  (let ((key-callbacks (key-callbacks controller)))
    
    (when nil
    
      ;; Create closures for variables needed
      ;; Controller function should simply call it without arguments except k
      
      (push-callback key-callbacks +xk-escape+ 'press (lambda (controller k)
							(handle-escape controller k)))

      ;; camera pan
      
      ;; (push-callback key-callbacks +xk-left+ 'press #'update-mm-left)
      ;; (push-callback key-callbacks +xk-left+ 'repeat #'update-mm-left)

      ;; (push-callback key-callbacks +xk-right+ 'press #'update-mm-right)
      ;; (push-callback key-callbacks +xk-right+ 'repeat #'update-mm-right)
      
      ;; (push-callback key-callbacks +xk-up+ 'press #'update-mm-up)
      ;; (push-callback key-callbacks +xk-up+ 'repeat #'update-mm-up)
      
      ;; (push-callback key-callbacks +xk-down+ 'press #'update-mm-dn)
      ;; (push-callback key-callbacks +xk-down+ 'repeat #'update-mm-dn)

      ;; camera zoom
      
      (push-callback key-callbacks +xk-minus+ 'press #'update-zoom-out)
      (push-callback key-callbacks +xk-minus+ 'repeat #'update-zoom-out)

      (push-callback key-callbacks +xk-equal+ 'press #'update-zoom-in)
      (push-callback key-callbacks +xk-equal+ 'repeat #'update-zoom-in)

      ;; cursor

      (push-callback key-callbacks +xk-up+ 'press #'handle-up)
      (push-callback key-callbacks +xk-up+ 'repeat #'handle-up)
      
      (push-callback key-callbacks +xk-down+ 'press #'handle-down)
      (push-callback key-callbacks +xk-down+ 'repeat #'handle-down)

      (push-callback key-callbacks +xk-left+ 'press #'handle-left)
      (push-callback key-callbacks +xk-left+ 'repeat #'handle-left)

      (push-callback key-callbacks +xk-right+ 'press #'handle-right)
      (push-callback key-callbacks +xk-right+ 'repeat #'handle-right)
      
      ;; ascii/ctrl
      
      (push-callback key-callbacks +xk-backspace+ 'press #'handle-backspace)
      (push-callback key-callbacks +xk-backspace+ 'repeat #'handle-backspace)

      (push-callback key-callbacks +xk-delete+ 'press #'handle-delete)
      (push-callback key-callbacks +xk-delete+ 'repeat #'handle-delete)

      (push-callback key-callbacks +xk-return+ 'press #'handle-enter)
      (push-callback key-callbacks +xk-return+ 'repeat #'handle-enter))
    
    (loop
       :for keysym :from 32 :to 255
       :do (progn
	     (push-callback key-callbacks keysym 'press #'handle-ascii)
	     (push-callback key-callbacks keysym 'repeat #'handle-ascii)))))
