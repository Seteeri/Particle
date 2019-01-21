(in-package :protoform.view)

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
  (dolist (params params-model)
      (destructuring-bind (target
			   name
			   path
			   size
			   bind-cs
			   bind-vs
			   count-buffer
			   flag-copy
			   &rest rest)
	  params
      (init-bo-cache target
		     name
  		     size
		     bind-cs
		     flag-copy))))

(defun init-bo-cache (target
		      name
		      size
		      bind-cs
		      flag-copy)
  (with-slots (program-compute bo-cache) *view*
    (setf (gethash name bo-cache)
	  (make-instance 'cache
			 :buffer (init-buffer-object target
						     name
						     size
						     bind-cs
						     t
						     :buffering :single)
			 :flag-copy flag-copy))))

(defun get-cache (name)
  (gethash name (bo-cache *view*)))

(defun get-cache-buffer (name)
  (buffer (gethash name (bo-cache *view*))))

(defun get-cache-flag-copy (name)
  (flag-copy (gethash name (bo-cache *view*))))

(defun set-cache-flag-copy (name value)
  (with-slots (flag-copy)
      (gethash name (bo-cache *view*))
    (setf flag-copy value)))
