(in-package :protoform.model)
  
;; (defun clean-up-handles-shm ()
;;   (loop 
;;      :for key :being :the :hash-keys :of *handles-shm*
;;      :using (hash-value mmap)
;;      :do (cleanup-mmap mmap)))

;; (defun init-handles-shm ()
;;   (loop
;;      :with handles-shm := (make-hash-table :size 6 :test 'equal)
;;      :for params :in *params-shm*
;;      :do (destructuring-bind (target
;; 			      name
;; 			      path
;; 			      size
;; 			      bind-cs
;; 			      bind-vs
;; 			      count-buffer
;; 			      flag-copy
;; 			      &rest rest)
;; 	     params
;; 	   (let ((mmap (init-mmap path
;; 				  size
;; 				  t ; create - replace these types with symbols
;; 				  :data (make-array size
;; 						    :element-type '(unsigned-byte 8)
;; 						    :initial-element (coerce 0 '(unsigned-byte 8))))))
;; 	     (setf (gethash name handles-shm) mmap)
;; 	     (fmt-model t "init-handle-shm" "~S, ~S bytes~%" path size)))
;;      :finally (return handles-shm)))

;; rename mmap -> shm
(defun init-shm (name)
  (destructuring-bind (target
		       name
		       path
		       size
		       bind-cs
		       bind-vs
		       count-buffer
		       flag-copy
		       &rest rest)
      (getf *params-shm* name)
    (init-mmap path
	       size
	       t ; create - replace these types with symbols
	       :data (make-array size
				 :element-type '(unsigned-byte 8)
				 :initial-element (coerce 0 '(unsigned-byte 8))))))
