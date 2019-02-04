(in-package :protoform.controller)

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

