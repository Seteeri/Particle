(in-package :protoform.model)

;; Data structure:
;; (mod-logic, (mods key:(state)) (norm key:(state))) : (fn1:nil, fn2:nil)

(defun add-callback (callbacks cb)
  (if (gethash cb callbacks)
      ;; Do nothing if already exists
      nil
      ;; Callback does not exist, add, return t
      (setf (gethash cb callbacks) nil)))

(defun remove-callback (callbacks cb)
  (if (gethash cb callbacks)
      ;; Callback does exist, remove
      (remhash cb callbacks)
      ;; Do nothing if not exists
      nil))

(defun get-callbacks (seq-event &optional (add nil))
  ;; Return value for seq state -> hash table of fns
  (with-slots (key-callbacks)
      *controller*
    (if (gethash seq-event key-callbacks)
	(gethash seq-event key-callbacks)
	;; Key seq does not exist, create, add fn
	(when add
	  (let ((callbacks (make-hash-table :size 1)))
	    (setf (gethash seq-event key-callbacks) callbacks)
	    callbacks)))))

(defun register-callback (seq-events-key
			  logic-mods
			  cb)
  ;; Build keys and states - order important
  ;; Then register that specific combination  
  (let* ((seq-event (list logic-mods seq-events-key))
	 (callbacks (get-callbacks seq-event t)))
    (add-callback callbacks cb)))
   
(defun unregister-callback (seq-event
			    callback)
  (let ((callbacks (get-callbacks seq-event)))
    (when callbacks
      (remove-callback callbacks cb))))

;; Dispatch functions
;; Refactor verbosity for dispatch functions

;; rename -> event
(defun dispatch-all-seq-key ()
  (loop
     :for seq-event :being :the :hash-keys :of (key-callbacks *controller*)
     ;; :using (hash-value states)
     :do (progn
	   ;; (fmt-model t "dispatch-all-seq" "~a~%" seq-event)
	   (dispatch-seq-event seq-event))))

(defun dispatch-seq-event (seq-event)
  (when (is-seq-event-valid seq-event)
    (dispatch-callbacks-for-event seq-event)))

(defun dispatch-callbacks-for-event (seq-event)
  (loop
     :for cb :being :the :hash-keys :of (get-callbacks seq-event)
     ;; :using (hash-value dummy)
     :do (funcall cb seq-event)))

(defun is-seq-event-valid (seq-events-key)
  (with-slots (key-states)
      *controller*
    
    ;; seq-event = (mod-logic, (mods key:(state)) (norm key:(state)))
    ;; 3 lists:
    ;; :inclusive
    ;; (list +xk-control-l+ (list :press :down) +xk-x+ (list :press :down :repeat))
    (destructuring-bind (logic-mod seq-keys)
      seq-events-key

      ;; (loop
      ;;    :for (key states-tgt) :on seq-keys :by 'cddr
      ;;    :for state-key := (gethash key key-states)
      ;;    :do (when state-key (format t "~a : ~a, " key (aref state-key 0))))
      ;; (format t "~%")
	
      ;; Any of these fail, return
      (when (not (is-seq-state-valid seq-keys))
	(return-from is-seq-event-valid nil))

      ;; :for key-mod :in (set-difference *keysyms-modifier* seq-mod-state)
      
      ;; Exclusive: make sure no other mods pressed than those specified
      ;; Inclusive: need not check
      (when (eq logic-mod :exclusive)
	;; Check if any non-specified keys are pressed
	;; Build list
	(loop
	   :for key-mod :in *keysyms-modifier*
	   :for state-key := (if (gethash key-mod key-states)
				 (first (aref (gethash key-mod key-states) 0))
				 (:up 0))
	   :do (when (and (or (eq state-key :press)
			      (eq state-key :down))
			  (not (member key-mod eq-keys)))
		 ;; (fmt-model t "is-seq-event-valid" "fail: ~a | ~a, ~a~%" seq-event key-mod state)
		 (return-from is-seq-event-valid nil))))))
  
  ;; Return t
  t)

(defun is-seq-state-valid (seq-key)
    ;; Return mod keys also?
  (with-slots (key-states)
      *controller*  
    (loop
       :for (key states-tgt) :on seq-key :by 'cddr
       :for state-key := (if (gethash key key-states)
			     (aref (gethash key key-states) 0)
			     (list :up 0)) ; use last element
       :with time-last := 0
       :do (progn
	     (when (or (not (is-state-valid states-tgt (first state-key)))
		       (< (second state-key) time-last)) ; key pressed before
	       (return-from is-seq-state-valid nil))
	     (setf time-last (second state-key)))
       :finally (return-from is-seq-state-valid t))))

(defun is-state-valid (states-tgt state)
  ;; Need only match one
  (dolist (state-tgt states-tgt)
    (when (eq state state-tgt)
      (return-from is-state-valid t))))
