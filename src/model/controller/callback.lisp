(in-package :protoform.model)

;; Use hashtables and sort
;; {<key seq>} : {<key state>} : {fn1:nil, fn2:nil}
;; nil/t is dummy
;; This means callbacks will be called in arbitrary order

(defun get-states (seq-key key-callbacks &optional (add nil))
  ;; Return value for seq key -> hash table of states
  (if (gethash seq-key key-callbacks)
      (gethash seq-key key-callbacks)
      ;; Key seq does not exist, create, add fn
      (when add
	(let ((states (make-hash-table :size 1)))
	  (setf (gethash seq-key key-callbacks) states)
	  states))))

(defun get-callbacks (seq-state states &optional (add nil))
  ;; Return value for seq state -> hash table of fns
  (if (gethash seq-state states)
      (gethash seq-state states)
      ;; Key seq does not exist, create, add fn
      (when add
	(let ((callbacks (make-hash-table :size 1)))
	  (setf (gethash seq-state states) callbacks)
	  callbacks))))

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

(defun register-callback (key-callbacks seq-key seq-state cb)
  (let* ((states (get-states seq-key key-callbacks t))
	 (callbacks (get-callbacks seq-state states t)))
    (add-callback callbacks cb)))
   
(defun unregister-callback (key-callbacks seq-key seq-state callback)
  ;; Check each binding
  (let* ((states (get-states seq-key key-callbacks))
	 (callbacks (get-callbacks seq-state states)))
    (remove-callback callbacks cb)))

;; Dispatch functions
;; Refactor verbosity for dispatch functions

(defun dispatch-seq-key (seq-key &optional (seq-state nil))
  ;; Dispatch callbacks for specific seq-key and opt seq-state
  (with-slots (key-callbacks)
      *controller*
    (let ((states (get-states seq-key key-callbacks)))
      ;; Dispatch for specific seq-state or all if none
      (if seq-state
	  (dispatch-seq-state seq-key seq-state states)
	  (loop
	     :for seq-state :being :the :hash-keys :of states
	     ;; :using (hash-value callbacks)
	     :do (dispatch-seq-state seq-key seq-state states))))))

(defun dispatch-seq-state (seq-key seq-state states)
  (with-slots (key-states)
      *controller*
    (when (is-seq-state-active seq-key seq-state)
      (dispatch-callbacks seq-key seq-state states))))

(defun dispatch-callbacks (seq-key seq-state states)
  (loop
     :for cb :being :the :hash-keys :of (get-callbacks seq-state states)
     ;; :using (hash-value dummy)
     :do (funcall cb seq-key)))

(defun is-seq-state-active (seq-key seq-state)
  (with-slots (key-states)
      *controller*
    ;; Return on first non eq
    (loop
       :for key :in seq-key
       :for state :in seq-state
       :do (when (not (eq (gethash key key-states) state))
	     (return-from is-seq-state-active nil)))
    t))
  
(defun dispatch-all-seq-key ()
  ;; Loop over key callbacks
  ;; Loop over states
  ;; Check state for each
  ;;   If states all match, execute callback
  ;;   If states don't match, skip

  ;; Can do recursive version of this by incorporating loop into dispatch-seq-key upon nil seq-key
  (loop
     :for seq-key :being :the :hash-keys :of (key-callbacks *controller*)
     ;; :using (hash-value states)
     :do (progn
	   (dispatch-seq-key seq-key))))
