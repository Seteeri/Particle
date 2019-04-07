(in-package :protoform.render)

(defclass cache ()
  ((buffer :accessor buffer :initarg :buffer :initform nil)
   (flag-copy :accessor flag-copy :initarg :flag-copy :initform 0)))

;; Maybe dirty can have a way to memcpy a segment
;; Or on memcpy update slot value here like dirty range
;; Use same parameters as glCopyBufferSubData
;; (offset-dest-dirty :accessor dirty :initarg :dirty :initform nil)
;; (offset-src-dirty :accessor dirty :initarg :dirty :initform nil)
;; (size-dirty :accessor dirty :initarg :dirty :initform nil)))

(defun init-bo-caches (params-model)
  (with-slots (bo-cache)
      *render*
    (dolist (params params-model)
      (destructuring-bind (target
			   path
			   size
			   bind-cs
			   bind-vs
			   count-buffer
			   flag-copy
			   &rest rest)
	  params
	(setf (gethash path bo-cache)
	      (make-instance 'cache
			     :buffer (init-buffer-object target
							 path
							 size
							 bind-cs
							 t
							 :buffering :single)
			     :flag-copy flag-copy))))))

(defun get-cache (name)
  (gethash name (bo-cache *render*)))

(defun get-cache-buffer (name)
  (buffer (gethash name (bo-cache *render*))))

(defun get-cache-flag-copy (name)
  (flag-copy (gethash name (bo-cache *render*))))

(defun set-cache-flag-copy (name value)
  (with-slots (flag-copy)
      (gethash name (bo-cache *render*))
    (setf flag-copy value)))
