(in-package :protoform.view)

;; Rename since technically view is not a server socket
;; but can be implemented on any socket
(defun request-server (conn-client msdf)
  (with-slots (sock
	       buffer-ptr-recv
	       buffer-arr-recv)
      conn-client
    
    (loop

       :with requests-served = 0
       :do (multiple-value-bind (ret-recv len-recv) (recv-message sock
								  buffer-ptr-recv
								  buffer-arr-recv)
	     (if ret-recv

		 (progn
		   (incf requests-served)
		   ;; (format t "[serve-client] ~a~%" ret-recv)
		   (cond ((eq (first ret-recv) :mmap-shm)
			  t)
			 ((eq (first ret-recv) :unmmap-shm)
			  t)
			 ((eq (first ret-recv) :query-buffer-objects)
			  (serve-query-buffer-objects msdf
						      sock
						      buffer-ptr-recv))
			 ((eq (first ret-recv) :memcpy)
			  ;; Refactor for readability
			  ;; (format t "[serve-client] ~a, fence: ~a~%" ret-recv (ix-fence msdf))
			  (serve-memcpy sock
					buffer-ptr-recv
					(aref (ptrs-buffer (boa (gethash (second ret-recv) (mapping-base msdf)))) 0) ; dest - base buffer, no rot
					(ptr (mmap (gethash (third ret-recv) (mapping-base msdf))))                  ; src  - mmap shm
					(fourth ret-recv)
					(fifth ret-recv)
					(sixth ret-recv)
					(seventh ret-recv)
					requests-served)
			  t)
			 ((eq (first ret-recv) :exit)
			  (serve-exit msdf conn-client))
			 ((eq (first ret-recv) :sync)
			  (serve-sync sock
				      buffer-ptr-recv)
			  ;; (verify-instance msdf)
			  (return requests-served)
			  t)
			 (t
			  t)))

		 ;; Check for errors
		 (cond ((= len-recv 0)
			;; 11=EAGAIN
			(format t "[request-server] Server disconnected: ~a, ~a~%" sock (sb-alien:get-errno))
			(serve-exit msdf conn-client))
		       
		       (t ; -1=no-data,>0=invalid-data
			  (return requests-served)
			t)))))))

(defun serve-sync (sock buffer-ptr)
  (send-message sock
		buffer-ptr
		"t"))

(defun serve-exit (msdf conn-client)
  (with-slots (sock) conn-client
    (c-shutdown sock +shut-rdwr+)
    (c-close sock))
  (clean-up-msdf msdf)
  (sb-ext:exit))

(defun serve-memcpy2 (name-dest
		      name-src
		      size)
  (let* ((offset-dest 0)
	 (offset-src 0)
	 (ptr-dest (aref (ptrs-buffer (boa (gethash name-dest (mapping-base *view*)))) 0)) ; dest - base buffer, no rot
	 (ptr-src (ptr (mmap (gethash name-src (mapping-base *view*)))))                   ; src  - mmap shm
	 (ptr-dest-off (inc-pointer ptr-dest offset-dest))
	 (ptr-src-off (inc-pointer ptr-src offset-src)))
    (c-memcpy ptr-dest-off
	      ptr-src-off
	      size))
  (format t "[view] c-memcpy: ~a ~a ~a" name-dest name-src size))

(defun serve-memcpy (sock
		     buffer-ptr
		     ptr-dest
		     ptr-src
		     offset-dest
		     offset-src
		     size
		     ret
		     nr)

  ;; Remember, dest is GPU buffer

  (let ((ptr-dest-off (inc-pointer ptr-dest offset-dest))
	(ptr-src-off (inc-pointer ptr-src offset-src)))
    (c-memcpy ptr-dest-off
	      ptr-src-off
	      size))

  (when ret
    (send-message sock
		  buffer-ptr
		  "t")))
      
(defun serve-query-buffer-objects (msdf
				   sock
				   buffer-ptr)

  ;; (let ((s (make-string-output-stream)))
  
  (let ((s (with-output-to-string (s)
	     (format s "(list")

	     (loop 
	        :for key :being :the :hash-keys :of (mapping-base msdf)
		:using (hash-value value)
		:do (let ((mmap (mmap value)))
		      (format s " (list \"~a\" \"~a\" ~a)"
			      key (path mmap) (size mmap))))
	     (format s ")")
	     (finish-output))))
    
    (send-message sock
		  buffer-ptr
		  s)))
