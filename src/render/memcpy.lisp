(in-package :protoform.render)

(defun memcpy-shm-to-all ()
  (with-slots (bo-cache)
      *render*
    ;; shm -> cache -> step
    ;; model triggers shm->cache
    ;; compute performs cache->step
    (loop 
       :for name :being :the :hash-keys :of bo-cache
       :using (hash-value cache)
       :do (progn
	     (memcpy-shm-to-cache name name 0 nil)
	     ;; Primarily do below for texture, other buffers will be updated by compute shader
	     (memcpy-cache-to-step-all name name)
	     t))))

(defun memcpy-shm-to-cache (name-dest
			    name-src
			    offset
			    size
			    &optional (print nil))
  ;; (format t "shm-to-cache: ~a~%" (get-cache-buffer name-dest))
  (let* ((bo-dest (get-cache-buffer name-dest))
	 (ptr-dest (aref (ptrs-buffer bo-dest) 0)) ; always 0
	 (ptr-src (ptr (mmap (gethash name-src (handles-shm *render*))))))
    (memcpy-ptr ptr-dest
		offset
		ptr-src
		offset
		(if size
		    size
		    (size-buffer bo-dest))))
  (when print
    (fmt-render t "memcpy-shm-to-cache" "~a, @~a, +~a bytes~%" name-src offset size)))

;; Can either use this function to cpy between buffers or use GL func
(defun memcpy-cache-to-step (name-dest
			     ix-dest
			     name-src
			     &optional
			       (size nil)
			       (print t))
  ;; (format t "cache-to-step: ~a~%" (get-cache-buffer name-dest))
  (let* ((bo-dest (gethash name-dest (bo-step *render*)))
	 (ptr-dest (aref (ptrs-buffer bo-dest) ix-dest))
	 (ptr-src (aref (ptrs-buffer (get-cache-buffer name-src)) 0)))
    (memcpy-ptr ptr-dest
		0
		ptr-src
		0
		(if size
		    size
		    (size-buffer bo-dest))))
  (when print
    (fmt-render t "memcpy-cache-to-step" "~a, ~a bytes~%" name-src size)))

;; Copy from cache to all step buffers
(defun memcpy-cache-to-step-all (name-dest
				 name-src
				 &optional (size nil))
  ;; (format t "cache-to-step-all: ~a~%" (get-cache-buffer shm-nodes))
  (let* ((bo-dest (gethash name-dest (bo-step *render*)))
	 (ptr-src (aref (ptrs-buffer (get-cache-buffer name-dest)) 0))
	 (size (if size
		   size
		   (size-buffer bo-dest))))
    (dotimes (i (count-buffers bo-dest))
      (memcpy-ptr (aref (ptrs-buffer bo-dest) i)
		  0
		  ptr-src
		  0
		  size))))

(defun memcpy-ptr (ptr-dest off-dest
		   ptr-src off-src
		   size)
  (let* ((ptr-dest-off (inc-pointer ptr-dest off-dest))
	 (ptr-src-off (inc-pointer ptr-src off-src)))
    (c-memcpy ptr-dest-off
	      ptr-src-off
	      size)))
