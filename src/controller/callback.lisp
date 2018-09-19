(in-package :protoform.controller)

;; Press should be first press

(defclass callback-key ()
  ((up :accessor up :initarg :up :initform (make-array 8 :fill-pointer 0 :adjustable t))
   (press :accessor press :initarg :press :initform (make-array 8 :fill-pointer 0 :adjustable t))
   (repeat :accessor repeat :initarg :repeat :initform (make-array 8 :fill-pointer 0 :adjustable t))
   (release :accessor release :initarg :release :initform (make-array 8 :fill-pointer 0 :adjustable t))))

(defun call-callbacks (controller)
  (with-slots (key-states key-callbacks)
      controller    
    (loop 
       :for key :being :the :hash-keys :of key-callbacks
       :using (hash-value ck)
       :for state := (gethash key key-states)
       :do (when state
	     (loop :for cb :across (slot-value ck state) :do (funcall cb controller key))))))
     
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
	  
