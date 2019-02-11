(in-package :protoform.controller)

;; Data structure:
;; (mod-logic, (mods key:(state)) (norm key:(state))) : (fn1:nil, fn2:nil)

(defun dispatch-callbacks-keyboard ()
  (with-slots (key-callbacks)
      *controller*
     (loop
	:for seq-event :being :the :hash-keys :of key-callbacks
	:using (hash-value callbacks)
	:do (dispatch-seq-event-2 seq-event callbacks))))

(defun dispatch-seq-event-2 (seq-event callbacks)
  (when (is-seq-event-valid seq-event)
    (loop :for cb :being :the :hash-keys :of callbacks ; (get-callbacks seq-event)
       :do (funcall cb seq-event))))

(defun is-seq-event-valid (seq-events-key)
  (with-slots (key-states)
      *controller*
    
    (destructuring-bind (logic-mod seq-keys)
      seq-events-key

      ;; (loop
      ;;    :for (key states-tgt) :on seq-keys :by 'cddr
      ;;    :for state-key := (gethash key key-states)
      ;;    :do (when state-key (format t "~a : ~a, " key (aref state-key 0))))
      ;; (format t "~%")
	
      ;; Any of these fail, return
      (unless (is-seq-state-valid seq-keys)
	(return-from is-seq-event-valid nil))
      
      ;; Exclusive: make sure no other mods pressed than those specified
      ;; Inclusive: need not check
      (when (eq logic-mod :exclusive)
	;; Check if any non-specified keys are pressed
	;; Build list
	(loop
	   :for key-mod :in *keysyms-modifier*
	   :for state-key := (if (gethash key-mod key-states)
				 (first (aref (gethash key-mod key-states) 0))
				 :up)
	   :do (when (and (or (eq state-key :press)
			      (eq state-key :down))
			  (not (member key-mod seq-keys)))
		 (return-from is-seq-event-valid nil))))))
  
  ;; Return t
  t)

(defun is-seq-state-valid (seq-key)
  ;; Must run serially
  (with-slots (key-states)
      *controller*  
    (loop
       :for (key states-tgt) :on seq-key :by 'cddr
       :for state-key := (if (gethash key key-states)
			     (aref (gethash key key-states) 0)
			     (list :up 0)) ; use last element
       :with time-last := 0
       :do (progn
	     ;; If not key matches one of the states
	     ;; or not keys pressed in order
	     ;; - return nil
	     (when (or (not (is-state-valid states-tgt (first state-key)))
		       (< (second state-key) time-last))
	       (return-from is-seq-state-valid nil))
	     (setf time-last (second state-key)))
       :finally (return-from is-seq-state-valid t))))

(defun is-state-valid (states-tgt state)
  ;; Need only match one
  (dolist (state-tgt states-tgt)
    (when (eq state state-tgt)
      (return-from is-state-valid t))))
