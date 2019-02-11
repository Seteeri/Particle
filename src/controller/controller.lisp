(in-package :protoform.controller)

;; mirror of model variables
(defparameter *controller* nil)
(defparameter *channel-input* nil)

;; TEMP
(defparameter *queue-tasks-sync* nil)
(defparameter *translate-node-rel* nil)

(defparameter *path-devices* "/dev/input/event*")

(defclass controller () 
  ((context :accessor context :initarg :context :initform nil)
   (xkb :accessor xkb :initarg :xkb :initform nil)
   (key-states :accessor key-states :initarg :key-states :initform (make-hash-table :size 256))
   (key-callbacks :accessor key-callbacks :initarg :key-callbacks :initform (make-hash-table :size 256))
   (mailbox-event :accessor mailbox-event :initarg :mailbox-event :initform (sb-concurrency:make-mailbox))
   (ep-events :accessor ep-events :initarg :ep-events :initform nil)
   (ep-fd :accessor ep-fd :initarg :ep-fd :initform nil)))   

(defun init-controller (channel-input queue-anim translate-node-rel &rest devices)
  (let ((controller (make-instance 'controller
				   :context (libinput:path-create-context (libinput:make-libinput-interface)
									  (null-pointer))
				   :xkb (init-xkb))))
    (with-slots (context
		 ep-fd
		 ep-events)
	controller

      (init-devices devices context)
      
      (setf ep-fd     (init-epoll context)
            ep-events (foreign-alloc '(:struct event))))

    (setf *controller* controller)
    (setf *channel-input* channel-input)
    (setf *queue-tasks-sync* queue-anim)
    (setf *translate-node-rel* translate-node-rel)
    
    controller))

(defun init-devices (devices context)
  (if devices
      (mapcar (lambda (path)
		(libinput:path-add-device context path))
	      devices)
      (let ((device-paths (directory *path-devices*)))
	(loop
	   :for device-path :in device-paths
	   :do (let ((device (libinput:path-add-device context
						       (namestring device-path))))
		 (unless (null-pointer-p device)
		   (if (not (or (libinput:device-has-capability device
								libinput:device-cap-keyboard)
				(libinput:device-has-capability device
								libinput:device-cap-pointer)))
		       (libinput:path-remove-device device))))))))

(defun init-epoll (context)
  (let ((ep-fd (c-epoll-create 1)))
    (ctl-epoll ep-fd
	       (libinput:get-fd context)
	       #x0001 ; TODO: defconstant
	       :add)
    ep-fd))

(defun poll-fd-li ()
  ;; Poll events ASAP and enqueue for processing by controller thread
  (with-slots (context
	       mailbox-event
	       ep-fd
	       ep-events)
      *controller*  
    (loop
       ;; -1 = timeout = block/infinite
       ;; 0 = return if nothing
       (when (> (c-epoll-wait ep-fd ep-events 1 -1) 0)
	 (libinput:dispatch context)
	 (loop
      	    :for event := (libinput:get-event context)
      	    :until (null-pointer-p event)
      	    :do (progn
		  (sb-concurrency:send-message mailbox-event
					       event)
		  (libinput:dispatch context)))))))

(defun run-controller ()
  (with-slots (mailbox-event)
      *controller*
    (loop
       :for event := (sb-concurrency:receive-message mailbox-event)
       :do (progn
	     (let ((type-event (dispatch-handler-event event)))
	       (libinput:event-destroy event)
	       ;; implement for other devices
	       ;; can run in parallel for each device
	       (when (eq type-event libinput:keyboard-key)
		 (dispatch-callbacks-keyboard)))
	     ;; always update
	     (update-states-keyboard-continuous)))))

(defun dispatch-handler-event (event)

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

      ;; ((= type libinput:pointer-button)
      ;;  t)
      
      ;; ((= type libinput:pointer-motion)
      ;;  (handle-event-pointer-motion event))
      
      ;; ((= type libinput:touch-down)
      ;;  (handle-event-touch event))

      ;; ((= type libinput:touch-motion)
      ;;  (handle-event-touch event))

      ;; ((= type libinput:touch-up)
      ;;  (handle-event-touch-2 event))

      ;; ((= type libinput:touch-cancel)
      ;;  (handle-event-touch-2 event))
      
      ;; ((= type libinput:touch-frame)
      ;;  (handle-event-touch-2 event))

      ;; ((= type libinput:tablet-tool-axis)
      ;;  (handle-event-tablet-tool-axis event))

      ;; ((= type libinput:tablet-tool-proximity)
      ;;  (handle-event-tablet-tool-proximity event))

      ;; ((= type libinput:tablet-tool-tip)
      ;;  (handle-event-tablet-tool-tip event))
      
      (t
       t))

    type))
