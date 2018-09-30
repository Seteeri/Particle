(in-package :protoform.view)

;; Create variant that use same name

(defun memcpy-shm-to-cache (name-dest
			    name-src
			    &optional (size nil))
  (let* ((bo-dest (gethash name-dest (bo-cache *view*)))
	 (ptr-dest (aref (ptrs-buffer bo-dest) 0)) ; always 0
	 (ptr-src (ptr (mmap (gethash name-src (handles-shm *view*))))))
    (memcpy-ptr ptr-dest
		ptr-src
		(if size
		    size
		    (size-buffer bo-dest)))))
  ;; (fmt-view t "memcpy-shm-to-cache" "~a ~a ~a~%" name-dest name-src size))

;; Can either use this function to cpy between buffers or use GL func
(defun memcpy-cache-to-step (name-dest
			     ix-dest
			     name-src
			     &optional (size nil))
  (let* ((bo-dest (gethash name-dest (bo-step *view*)))
	 (ptr-dest (aref (ptrs-buffer bo-dest) ix-dest))
	 (ptr-src (aref (ptrs-buffer (gethash name-dest (bo-cache *view*))) 0)))
    (memcpy-ptr ptr-dest
		ptr-src
		(if size
		    size
		    (size-buffer bo-dest)))))
  ;; (fmt-view t "memcpy-cache-to-step" "~a ~a ~a~%" name-dest name-src size))

;; Copy from cache to all step buffers
(defun memcpy-cache-to-step-all (name-dest
				 name-src
				 &optional (size nil))
  (let* ((bo-dest (gethash name-dest (bo-step *view*)))
	 (ptr-src (aref (ptrs-buffer (gethash name-dest (bo-cache *view*))) 0))
	 (size (if size
		   size
		   (size-buffer bo-dest))))
    (dotimes (i (count-buffers bo-dest))
      (memcpy-ptr (aref (ptrs-buffer bo-dest) i)
		  ptr-src
		  size))))

(defun memcpy-ptr (ptr-dest
		   ptr-src
		   size)
  (let* ((offset-dest 0)
	 (offset-src 0)
	 (ptr-dest-off (inc-pointer ptr-dest offset-dest))
	 (ptr-src-off (inc-pointer ptr-src offset-src)))
    (c-memcpy ptr-dest-off
	      ptr-src-off
	      size)))


(defun serve-exit (view conn-client)
  (with-slots (sock) conn-client
    (c-shutdown sock +shut-rdwr+)
    (c-close sock))
  (clean-up-msdf msdf)
  (sb-ext:exit))
      
(defun serve-query-buffer-objects (view
				   sock
				   buffer-ptr)

  ;; (let ((s (make-string-output-stream)))
  
  (let ((s (with-output-to-string (s)
	     (format s "(list")

	     (loop 
	        :for key :being :the :hash-keys :of (handles-shm msdf)
		:using (hash-value value)
		:do (let ((mmap (mmap value)))
		      (format s " (list \"~a\" \"~a\" ~a)"
			      key (path mmap) (size mmap))))
	     (format s ")")
	     (finish-output))))
    
    (send-message sock
		  buffer-ptr
		  s)))
