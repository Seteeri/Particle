(in-package :protoform.model)

;; Data structure:
;; (mod-logic, (mods key:(state)) (norm key:(state))) : (fn1:nil, fn2:nil)

(defun dispatch-all-seq-event ()
  ;; Execute callbacks in order -> merge deps into single ptree
  ;; which will later be executed during view
  ;; - Could make separate ptree for immediate execution and/or thread
  
  (let ((ptree (make-ptree))
	(queue (sb-concurrency:make-queue)))

    ;; Fill ptree/queue
    (loop
       :for seq-event :being :the :hash-keys :of (key-callbacks *controller*)
       ;; :using (hash-value states)
       :do (submit-task *channel-input*
			#'dispatch-seq-event
			seq-event)
       :finally (dotimes (i (hash-table-count (key-callbacks *controller*)))
		  ;; Call callbacks to enqueue
		  ;; Assumption here is callbacks do not share data...
		  (dolist (cb-ev (receive-result *channel-input*))
		    (destructuring-bind (cb ev)
			cb-ev
		      (funcall cb ev ptree queue)))))

    ;; Enqueue for frame
    (unless (sb-concurrency:queue-empty-p queue)
      (sb-concurrency:enqueue (list ptree queue)
			      *queue-frame*))))

(defun dispatch-seq-event (seq-event)
  (when (is-seq-event-valid seq-event)
    (loop
       :for cb :being :the :hash-keys :of (get-callbacks seq-event)
	 ;; could also use queue
       :collect (list cb seq-event))))

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
      (when (not (is-seq-state-valid seq-keys))
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
    ;; Collect mod keys while traversing?
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
