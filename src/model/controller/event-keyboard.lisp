(in-package :protoform.model)

(defconstant +state-event-press+ 1)
(defconstant +state-event-release+ 0)

(defparameter *keysyms-modifier* (list +xk-shift-l+
				       +xk-shift-r+
				       +xk-control-l+
				       +xk-control-r+
				       +xk-caps-lock+
				       +xk-shift-lock+
				       +xk-meta-l+
				       +xk-meta-r+
				       +xk-alt-l+
				       +xk-alt-r+
				       +xk-super-l+
				       +xk-super-r+
				       +xk-hyper-l+
				       +xk-hyper-r+))

(defun handle-repeat-timer (keysym)
  ;; (format t "[handle-keyboard-timer][~a] Repeating ~a~%" (get-internal-real-time) keysym)
  (let ((state (gethash keysym (key-states *controller*))))
    (setf (aref state 1) (aref state 0))
    (setf (nth 0 (aref state 0)) :repeat)))

(defun update-repeat-timer (xkb
			    ev-state
			    keysym
			    keysym-char)

  ;; Figure out how to handle modifier keys
  ;; Possible states: PRESS RELEASE UP
  ;; Should it have REPEAT also?
  ;; How can user handle mod key pressed by itself
  
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

	     ;; (format t "Removed timer ~a~%" repeat-key)
	     
	     (when repeat-timer
	       (unschedule-timer repeat-timer))
	     
	     ;; Or maintain previous state...
	     (setf repeat-key nil)
	     (setf repeat-char nil))))))

(defun handle-event-keyboard (event)

  (with-slots (context
	       xkb
	       epoll-fd
	       epoll-events
	       key-states
	       key-states-delta
	       key-callbacks)
      *controller*
    
    (let* ((keyboard-event (libinput:event-get-keyboard-event event))
	   (ev-state (libinput:event-keyboard-get-key-state keyboard-event))
	   (ev-keycode (libinput:event-keyboard-get-key keyboard-event))
	   (ev-time (libinput:event-keyboard-get-time-usec event)))

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

	  (when nil
	    (fmt-model t "update-keyboard"
		       "[~a] keysym: ~a, code: ~a, char: #\~a, state: ~a, rep: ~a~%"
		       ev-time
		       keysym
		       (+ ev-keycode 8)
		       (code-char keysym)
		       ev-state
		       repeats))
	  
	  ;; Update key state
	  ;; If key is repeated, state will be set to 'down at a later time
	  (let ((state (gethash keysym key-states)))
	    (when (not state)
	      (setf state (make-array 2
				      :adjustable nil
				      :initial-contents (list (list :up ev-time) (list :up ev-time))))
	      (setf (gethash keysym key-states) state))
	    ;; Copy index 0 to 1
	    (setf (aref state 1) (aref state 0))
	    (setf (aref state 0)
		  (if (= ev-state +state-event-press+)
		      (list :press ev-time)
		      (list :release ev-time))))
	  
	  ;; Perform callback for key
	  ;; (dispatch-callback keysym)
	  ;; Then reset key or do after
	  
	  ;; Check timer for repeatable keys
	  (when repeats
	    (update-repeat-timer xkb
				 ev-state
				 keysym
				 keysym-char)))))))

(defun is-modifier-key (keysym)
  ;; Should use xkb function
  ;; Is this faster than using global+loop?
  (if (or (eq keysym +xk-shift-l+)
	  (eq keysym +xk-shift-r+)
	  (eq keysym +xk-control-l+)
	  (eq keysym +xk-control-r+)
	  (eq keysym +xk-caps-lock+)
	  (eq keysym +xk-shift-lock+)
	  (eq keysym +xk-meta-l+)
	  (eq keysym +xk-meta-r+)
	  (eq keysym +xk-alt-l+)
	  (eq keysym +xk-alt-r+)
	  (eq keysym +xk-super-l+)
	  (eq keysym +xk-super-r+)
	  (eq keysym +xk-hyper-l+)
	  (eq keysym +xk-hyper-r+))
      t
      nil))

(defun update-states-keyboard-continuous ()
  ;; This is done at end of frame so on next frame,
  ;; if state changes, it will override below,
  ;; otherwise it will remain the same and
  ;; callbacks will be executed
  
  (loop
     :with key-states := (key-states *controller*)
     :for keysym :being :the :hash-keys :of key-states
     :using (hash-value state)
     :for state-key := (aref state 0)       
     :do (progn
	   (cond ((eq (first state-key) :press)
		  ;; (fmt-model t "reset-states-key" "Press -> Down: ~a~%" keysym)
		  (setf (nth 0 (aref state 1)) (nth 0 state-key))
		  (setf (nth 1 (aref state 1)) (nth 1 state-key))
		  (setf (nth 0 state-key) :down))
		
		 ((eq (first state-key) :release)
		  ;; (fmt-model t "reset-states-key" "Release -> Up : ~a~%" keysym)
		  (setf (nth 0 (aref state 1)) (nth 0 state-key))
		  (setf (nth 1 (aref state 1)) (nth 1 state-key))
		  (setf (nth 0 state-key) :up))

		 ((eq (first state-key) :repeat)
		  ;; (fmt-model t "reset-states-key" "Repeat -> Down : ~a~%" keysym)
		  (setf (nth 0 (aref state 1)) (nth 0 state-key))
		  (setf (nth 1 (aref state 1)) (nth 1 state-key))
		  (setf (nth 0 state-key) :down))))))
