(in-package :protoform.model)

;; Press should be first press

(defclass callback-key ()
  ((up :accessor up :initarg :up :initform (make-array 8 :fill-pointer 0 :adjustable t))
   (press :accessor press :initarg :press :initform (make-array 8 :fill-pointer 0 :adjustable t))
   (repeat :accessor repeat :initarg :repeat :initform (make-array 8 :fill-pointer 0 :adjustable t))
   (release :accessor release :initarg :release :initform (make-array 8 :fill-pointer 0 :adjustable t))))

(defun dispatch-callback (keysym)
  (with-slots (key-states key-callbacks)
      *controller*
    (let ((state (gethash keysym key-states)))
      (when state
	(let ((ck (gethash keysym key-callbacks)))
	  (when ck
	    (loop :for cb :across (slot-value ck (find-symbol (symbol-name state)))
	       :do (funcall cb keysym))))))))

(defun dispatch-callbacks ()
  (with-slots (key-states key-callbacks)
      *controller*
    (loop 
       :for keysym :being :the :hash-keys :of key-callbacks
       :using (hash-value ck)
       :for state := (gethash keysym key-states)
       :do (when state
	     (loop :for cb :across (slot-value ck (find-symbol (symbol-name state)))
		:do (funcall cb keysym))))))
     
(defun push-callback (key-callbacks keysym state callback)
  ;; Ignore if already registered?
  (if (gethash keysym key-callbacks)

      (vector-push-extend callback
			  (slot-value (gethash keysym key-callbacks) state))

      (let ((ck (make-instance 'callback-key)))
	(setf (gethash keysym key-callbacks) ck)
	(vector-push-extend callback (slot-value ck state)))))

(defun delete-callback (key-callbacks keysym state callback)
  ;; Delete only if it exists
  (when (gethash keysym key-callbacks)
    (let ((ck (gethash keysym key-callbacks)))
      (when ck
	(let ((callbacks (slot-value ck state)))
	  (raise "Delete-callback not yet implemented!"))))))
	  
