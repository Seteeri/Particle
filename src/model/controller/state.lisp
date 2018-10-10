(in-package :protoform.model)

(defconstant +state-event-press+ 1)
(defconstant +state-event-release+ 0)

(defun handle-repeat-timer (keysym)

  ;; Set key state:
  ;; PRESS - pressed on frame
  ;; REPEAT - repeated
  ;; RELEASE - released on frame
  ;; UP - depressed or none...
  
  ;; (format t "[handle-keyboard-timer][~a] Repeating ~a~%" (get-internal-real-time) keysym)
  ;; (force-output)
  
  (setf (gethash keysym (key-states *controller*)) 'repeat))

(defun update-repeat-timer (xkb
			    ev-state
			    keysym
			    keysym-char)

  (with-slots (repeat-char
	       repeat-timer
	       repeat-key
	       repeat-delay
	       repeat-interval)
      xkb
    
    (cond ((= ev-state +state-event-press+)
	   
	   ;; If repeatable, start timer, remove current if exists
	   ;; Repeat will only be set after a delay, i.e. user can release key before delay
	   ;; If new key is held, it replaces old one

	   ;; Only one timer allowed for all keys so remove current
	   (when repeat-timer
	     (unschedule-timer repeat-timer))

	   ;; Update repeat key
	   (setf repeat-key keysym)
	   (setf repeat-char keysym-char)
	   
	   ;; Schedule new timer
	   (setf repeat-timer (make-timer (lambda ()
					    (handle-repeat-timer keysym))))
	   (schedule-timer repeat-timer
			   (/ repeat-delay 1000)
			   :repeat-interval (/ repeat-interval 1000)))

	  
	  ((= ev-state +state-event-release+)

	   ;; Remove timer only when current repeat key is released
	   (when (eq keysym repeat-key)

	     (format t "Removed timer ~a~%" repeat-key)
	     
	     (when repeat-timer
	       (unschedule-timer repeat-timer))
	     
	     ;; Or maintain previous state...
	     (setf repeat-key nil)
	     (setf repeat-char nil))))))

(defun update-keyboard (event)

  (with-slots (context
	       xkb
	       epoll-fd
	       epoll-events
	       key-states
	       key-callbacks)
      *controller*
    
    (let* ((keyboard-event (libinput:event-get-keyboard-event event))
	   (ev-state (libinput:event-keyboard-get-key-state keyboard-event))
	   (ev-keycode (libinput:event-keyboard-get-key keyboard-event)))
      ;; event-keyboard-get-time
      ;; event-keyboard-get-time-usec

      (with-slots (context keymap state repeat-char repeat-timer repeat-key repeat-delay repeat-interval
			   mods-depressed mods-latched mods-locked)
	  xkb
	
	(let* ((keysym (xkb:xkb-state-key-get-one-sym state (+ ev-keycode 8)))
	       (repeats (if (= (xkb:xkb-keymap-key-repeats keymap (+ ev-keycode 8)) 1)
			    t
			    nil))
	       (keysym-char (xkb:get-key-character state (+ ev-keycode 8))))
	  ;; (keysym-char2 (xkb:get-keysym-character keysym)))
	  ;; (keysym-name (xkb:get-keysym-name keysym)))
	  
	  (xkb:xkb-state-update-key state
				    (+ ev-keycode 8)
				    (if (= ev-state +state-event-press+)
					+state-event-press+
					+state-event-release+))

	  (setf mods-depressed (xkb:xkb-state-serialize-mods state 1))
	  (setf mods-latched (xkb:xkb-state-serialize-mods state 2))
	  (setf mods-locked (xkb:xkb-state-serialize-mods state 4))
	  (setf mods-group (xkb:xkb-state-serialize-mods state 8))
	  ;; (format t "~a, ~a, ~a~%" mods-depressed mods-latched mods-locked)

	  (when (= ev-state +state-event-press+)
	    (fmt-model t "update-keybaord"
		       "code: ~a, sym: ~a, state: ~a, rep: ~a~%"
		       (+ ev-keycode 8) (code-char keysym) ev-state repeats))

	  ;; Update key state
	  ;; If key is repeated, state will be set to 'repeat at a later time
	  (setf (gethash keysym key-states)
		(if (= ev-state +state-event-press+)
		    'press
		    'release))

	  ;; Check timer for repeatable keys
	  (when repeats
	    (update-repeat-timer xkb
				 ev-state
				 keysym
				 keysym-char)))))))

(defun reset-release-keys (key-states)
  ;; Any keys previously set to 'release -> 'up
  ;; Keys continously pressed will maintain 'press
  ;; If press exceeds repeat delay, timer will handle it and switch to 'repeat
  (loop 
     :for k :being :the :hash-keys :of key-states
     :using (hash-value state)
     :do (when (and state
		    (eq state 'release)))
	   (setf (gethash k key-states) 'up)))

