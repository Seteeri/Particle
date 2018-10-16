(in-package :protoform.model)

;; Data structure:
;; ((mods),(key)) : ((mods,key state)) : (fn1:nil, fn2:nil)
;;
;; or combine key/state:
;; (mod-logic, (mods key:state) (norm key:state)) : (fn1:nil, fn2:nil)

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

(defun register-callback (seq-key-state
			  seq-mods
			  logic-mods
			  cb)
  ;; Build keys and states - order important
  ;; Then register that specific combination  
  (let* ((seq-event (list logic-mods seq-mods seq-key-state))
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

(defun is-seq-event-valid (seq-event)
  (with-slots (key-states
	       key-states-delta)
      *controller*
    
    ;; seq-event = :keyword (mod:status) (key:status)
    (destructuring-bind (logic-mod seq-mod-state seq-key-state)
      seq-event

      ;; Any of these fail, return
      
      ;; Check mod states
      (loop
	 :for (key state) :on seq-mod-state :by 'cddr
	 :do (when (not (eq (gethash key key-states) state))
	       (return-from is-seq-event-valid nil)))
      
      ;; Check key states
      (loop
	 :for (key state) :on seq-key-state :by 'cddr
	 :do (when (not (eq (gethash key key-states) state))
	       (return-from is-seq-event-valid nil)))
      
      ;; Check mod
      ;; Exclusive: make sure no other mods pressed than those specified
      ;; Inclusive: need not check
      (when (eq logic-mod :exclusive)
	;; Generate list on init to save time?
	;; set-difference returns a list of elements of list-1 that do not appear in list-2. 
	(loop
	   :for key-mod :in (set-difference *keysyms-modifier* seq-mod-state)
	   :for state := (gethash key-mod key-states)
	   :do (when (and state ; > 255; hash will return nil for non-existent key
			  (not (or (eq state :up) (eq state :release)))) ; double check this with reset-states
		 (fmt-model t "is-seq-event-valid" "fail: ~a | ~a, ~a~%" seq-event key-mod state)
		 (return-from is-seq-event-valid nil))))

      t)))
