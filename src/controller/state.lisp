(in-package :protoform.controller)

(defun handle-keyboard-timer (key-states keysym)

  ;; Set key state:
  ;; PRESS
  ;; REPEAT
  ;; RELEASE
  ;; UP

  (setf (gethash keysym key-states) 'repeat)

  ;; (format t "[handle-keyboard-timer][~a] Repeating ~a~%" (get-internal-real-time) keysym)
  ;; (force-output)

  ;; Callback will then call repeat callbacks
  ;; Will return to normal upon release of key or another key is pressed
  
  t)

(defun update-keyboard (controller
			key-states
			key-callbacks
			xkb
			event)
  
  (with-slots (context keymap state repeat-char repeat-timer repeat-key repeat-delay repeat-interval
		       mods-depressed mods-latched mods-locked)
      xkb
    
    (let* ((keyboard-event (libinput:event-get-keyboard-event event))
	   (ev-state (libinput:event-keyboard-get-key-state keyboard-event))
	   (ev-keycode (libinput:event-keyboard-get-key keyboard-event)))
      ;; event-keyboard-get-time
      ;; event-keyboard-get-time-usec
      
      (let* ((keysym (xkb:xkb-state-key-get-one-sym state (+ ev-keycode 8)))
	     (repeats (if (= (xkb:xkb-keymap-key-repeats keymap (+ ev-keycode 8)) 1)
			  t
			  nil))
	     (keysym-char (xkb:get-key-character state (+ ev-keycode 8))))
	     ;; (keysym-char2 (xkb:get-keysym-character keysym)))
	     ;; (keysym-name (xkb:get-keysym-name keysym)))
		
	(xkb:xkb-state-update-key state
				  (+ ev-keycode 8)
				  (if (= ev-state 1) 1 0))

	(setf mods-depressed (xkb:xkb-state-serialize-mods state 1))
	(setf mods-latched (xkb:xkb-state-serialize-mods state 2))
	(setf mods-locked (xkb:xkb-state-serialize-mods state 4))
	(setf mods-group (xkb:xkb-state-serialize-mods state 8))
	;; (format t "~a, ~a, ~a~%" mods-depressed mods-latched mods-locked)

	;; (when (= ev-state 1)
	;;   (format t "code: ~a, sym: ~a, state: ~a, rep: ~a~%"
	;; 	  (+ ev-keycode 8) (code-char keysym) ev-state repeats))

	(when repeats
	  (let ((state (gethash keysym key-states))
		(callbacks (gethash keysym key-callbacks)))
	    (when (and (eq state 'press) (= ev-state 0)) ; prev=press, new=release
	      (loop :for cb :across (press callbacks) :do (funcall cb controller keysym)))))
	
	;; Repeat is handled in the thread	
	(setf (gethash keysym key-states) (if (= ev-state 1) 'press 'release))

	;; Update timer
	(cond ((= ev-state 1) ; press
	       
	       ;; If repeatable, start timer...
	       ;; When timer expires, it will check if key is still pressed
	       ;; If pressed, continue timer - set REPEAT state
	       ;; If released, remove timer - set RELEASE state
	       ;; If new key is held, it replaces old one
	       (when repeats

		 ;; Only one timer - remove current
		 (when repeat-timer
		   (unschedule-timer repeat-timer))

		 (setf repeat-key keysym)
		 (setf repeat-char keysym-char)
		 
		 ;; Schedule new timer
		 ;; Closure!
		 (setf repeat-timer (make-timer (lambda ()
						  (handle-keyboard-timer key-states keysym))))
		 (schedule-timer repeat-timer
				 (/ repeat-delay 1000)
				 :repeat-interval (/ repeat-interval 1000)))
	       t)
	      
	      ((= ev-state 0) ; release

	       ;; Remove timer only when repeat key is released
	       (when (and repeats (eq keysym repeat-key))

		 ;; (format t "Removed timer ~a" repeat-key)
		 
		 (when repeat-timer
		   (unschedule-timer repeat-timer))

		 ;; Or maintain previous state
		 (setf repeat-key nil)
		 (setf repeat-char nil)
		 t)
		 
	       ))

	t)))
  t)

(defun reset-keys (key-states)
  ;; Any keys that were set to 'release in previous frame -> set to 'up
  ;; (format t "[reset-keys] ~a~%" (gethash 65362 key-states))
  (loop 
     :for k :being :the :hash-keys :of key-states
     :using (hash-value state)
     :do (when (and state
		    (or ;(eq state 'press)
		     (eq state 'release)))
	   (setf (gethash k key-states) 'up))))

