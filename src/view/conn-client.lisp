(in-package :protoform.view)

;; (cond ((eq (first ret-recv) :mmap-shm)
;;        t)
;;       ((eq (first ret-recv) :unmmap-shm)
;;        t)
;;       ((eq (first ret-recv) :query-buffer-objects)
;;        (serve-query-buffer-objects msdf
;; 				   sock
;; 				   buffer-ptr-recv))
;;       ((eq (first ret-recv) :memcpy)
;;        ;; Refactor for readability
;;        ;; (format t "[serve-client] ~a, fence: ~a~%" ret-recv (ix-fence msdf))
;;        (serve-memcpy sock
;; 		     buffer-ptr-recv
;; 		     (aref (ptrs-buffer (boa (gethash (second ret-recv) (mapping-base msdf)))) 0) ; dest - base buffer, no rot
;; 		     (ptr (mmap (gethash (third ret-recv) (mapping-base msdf))))                  ; src  - mmap shm
;; 		     (fourth ret-recv)
;; 		     (fifth ret-recv)
;; 		     (sixth ret-recv)
;; 		     (seventh ret-recv)
;; 		     requests-served)
;;        t)
;;       ((eq (first ret-recv) :exit)
;;        (serve-exit msdf conn-client))
;;       ((eq (first ret-recv) :sync)
;;        (serve-sync sock
;; 		   buffer-ptr-recv)
;;        ;; (verify-instance msdf)
;;        (return requests-served)
;;        t)
;;       (t
;;        t)))

(defun serve-memcpy (name-dest
		     name-src
		     size)
  (let* ((offset-dest 0)
	 (offset-src 0)
	 (ptr-dest (aref (ptrs-buffer (gethash name-dest (bo-cache *view*))) 0)) ; dest - base buffer, no rot
	 (ptr-src (ptr (mmap (gethash name-src (handles-shm *view*)))))                   ; src  - mmap shm
	 (ptr-dest-off (inc-pointer ptr-dest offset-dest))
	 (ptr-src-off (inc-pointer ptr-src offset-src)))
    (c-memcpy ptr-dest-off
	      ptr-src-off
	      size))
  (fmt-view t "serve-memcpy" "c-memcpy: ~a ~a ~a~%" name-dest name-src size))

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
