(in-package :protoform.conn)

;; socat -v - UNIX-CLIENT:/tmp/protoform-model.socket

;; $ cat /proc/sys/net/ipv4/tcp_mem

;; $ cat /proc/sys/net/core/rmem_default
;; $ cat /proc/sys/net/core/rmem_max

;; $ cat /proc/sys/net/core/wmem_default
;; $ cat /proc/sys/net/core/wmem_max

;; $ cat /proc/sys/net/core/optmem_max

(defun init-sockaddr (path-server)
  (let ((sockaddr (foreign-alloc '(:struct sockaddr-un))))
    ;; set socket family
    (setf (foreign-slot-value sockaddr '(:struct sockaddr-un) 'sun-family)
	  1)      
    ;; set socket path
    (lisp-string-to-foreign path-server
			    (foreign-slot-pointer sockaddr
						  '(:struct sockaddr-un)
						  'sun-path)
			    (+ (length path-server) 1))
    sockaddr))

(defun init-socket (path-server nonblock)
  
  (let ((sock (c-socket protoform.libc::+af-unix+
			(if (eq nonblock :nonblock)
			    (logior protoform.libc::+sock-stream+ protoform.libc::+sock-nonblock+)
			    (logior protoform.libc::+sock-stream+))
			0)))
    
    (when (= sock -1)
      (error (get-str-errno)))
    
    (values sock
	    (init-sockaddr path-server))))

(defun set-nonblock (sock flag)
  (when (not flag)
    (sb-posix:fcntl sock
		    sb-posix::f-setfl
		    (logand (sb-posix:fcntl sock sb-posix::f-getfl)
			    (lognot sb-posix::o-nonblock)))))

(defun init-sock-server (path nonblock)
  
  (multiple-value-bind (sock sockaddr)
      (init-socket path nonblock)
    
    (with-foreign-string (ptr path)
      (c-unlink ptr))
    
    (let ((ret (c-bind sock
		       sockaddr
		       (foreign-type-size '(:struct sockaddr-un)))))
      (when (= ret -1) (error (get-str-errno))))    
    (foreign-free sockaddr)
        
    (let ((ret (c-listen sock 1)))
      (when (= ret -1) (error (get-str-errno))))
    
    (return-from init-sock-server sock)))

;; Refactor this
(defun init-sock-client (path nonblock &optional (timeout 0.0167))
  
  (multiple-value-bind (sock sockaddr)
      (init-socket path nonblock)

    ;; timeout
    ;; 0 = return immediately
    ;; > 0 = wait indefinitely with delay
    (let ((ret-connect nil))
      (if (> timeout 0)
	  (loop
	     :until (zerop (setf ret-connect (c-connect sock
							sockaddr
							(foreign-type-size '(:struct sockaddr-un)))))
	     :do (sleep timeout))
	  (setf ret-connect (c-connect sock
				       sockaddr
				       (foreign-type-size '(:struct sockaddr-un)))))
      
      (foreign-free sockaddr)

      ;; Return status
      (values sock
	      ret-connect))))

;; With flags 0, same as accept
(defun accept4 (sock nonblock)
  (with-foreign-objects ((addr '(:struct sockaddr-un))
			 (addr-len :int))
    (setf (mem-ref addr-len :int) (foreign-type-size '(:struct sockaddr-un)))
    (let ((sock-accept (c-accept4 sock
				  addr
				  addr-len
				  (if (eq nonblock :nonblock)
				      protoform.libc::+sock-nonblock+
				      0))))

      ;; For nonblocking, will either return a valid fd (> 0)
      ;; or it will fail and set errno to e-again or e-wouldblock
      ;; Any other errno indicates failure somewhere else
      ;;
      ;; Nonblocking expected by the server (view)
      ;; ((or (= (sb-alien:get-errno) protoform.libc::+e-again+)
      ;; 	   (= (sb-alien:get-errno) protoform.libc::+e-wouldblock+))

      (values sock-accept
	      (sb-alien:get-errno)))))

(defun recv-ptr (sock
		 buffer-ptr
		 &optional
		   (len 212992)
		   (flags 0))
  (declare (type fixnum sock))
  (declare (type fixnum len))
  (declare (type fixnum flags))
  ;; TODO: Use defparameter...
  ;; Similar to accept and other sock functions

  ;; Upon successful completion, recv() shall return the length of the message in bytes.
  ;; If no messages are available to be received and the peer has performed an orderly shutdown,
  ;; recv() shall return 0. Otherwise, -1 shall be returned and errno set to indicate the error. 
  
  (let ((len-recv (c-recv sock buffer-ptr len flags)))
    (values len-recv
	    (sb-alien:get-errno))))

(defun recv-message (sock
		     buffer-ptr)
  (declare (type fixnum sock))
  
  (let ((len-recv (recv-ptr sock
			    buffer-ptr
			    4)))
    (when (< len-recv 4)
      ;; (format t "[recv-message] recv-ptr returned ~a~%" len-recv)
      (return-from recv-message nil))

    (let* ((len-data (read-long-from-ptr buffer-ptr))
	   (len-recv (recv-ptr sock
			       buffer-ptr
			       len-data)))
      (when (< len-recv len-data)
	;; (format t "[recv-message] recv-ptr returned ~a < ~a~%" len-recv len-data)
	(return-from recv-message nil))
      
      ;; Bytes -> String -> Data
      (read-from-string (foreign-string-to-lisp buffer-ptr :count len-recv)))))

(defun send-message (sock
		     buffer-ptr
		     msg)
  (declare (type fixnum sock))
  (declare (type string msg))
  
  ;; send returns data copied to buffer
  ;; When data cannot fit in buffer:
  ;; nonblocking: -1 + EAGAIN/EWOULDBLOCK
  ;;   - can it return 0? -> possible if requested to write 0 bytes but again indictates disconnect/error
  ;; blocking: wait
  
  (let* ((len-msg (length msg)))
    (declare (type fixnum len-msg))
    
    ;; Write msg length to ptr
    (write-long-to-ptr buffer-ptr len-msg)
    ;; Copy msg to ptr
    ;; The lisp-string-to-foreign function copies at most bufsize-1 octets from a Lisp string
    ;; using the specified encoding into buffer+offset.
    ;; The foreign string will be null-terminated. 
    (lisp-string-to-foreign msg buffer-ptr (1+ len-msg) :offset 4)
    
    ;; assert length sent matches?
    (c-send sock
	    buffer-ptr
	    (+ len-msg 4)
	    0)))

(declaim (inline write-long-to-ptr))
(defun write-long-to-ptr (ptr long)
  (declare (type fixnum long))
  (setf (mem-aref ptr :unsigned-char 0) (ldb (byte 8 0)  long)
	(mem-aref ptr :unsigned-char 1) (ldb (byte 8 8)  long)
	(mem-aref ptr :unsigned-char 2) (ldb (byte 8 16) long)
	(mem-aref ptr :unsigned-char 3) (ldb (byte 8 24) long)))

(declaim (inline read-long-from-ptr))
(defun read-long-from-ptr (ptr)
  (let ((long 0))
    (declare (type fixnum long))
    (setf (ldb (byte 8 0)  long)  (mem-aref ptr :unsigned-char 0)
          (ldb (byte 8 8)  long)  (mem-aref ptr :unsigned-char 1)
          (ldb (byte 8 16) long)  (mem-aref ptr :unsigned-char 2)
          (ldb (byte 8 24) long)  (mem-aref ptr :unsigned-char 3))
    long))
