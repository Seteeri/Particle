(in-package :protoform.conn)

(defun write-long-to-byte-array (array integer &optional (offset 0))
  (setf (aref array offset) (ldb (byte 8 0) integer)
  (aref array (+ offset 1))(ldb (byte 8 8) integer)
  (aref array (+ offset 2))(ldb (byte 8 16) integer)
  (aref array (+ offset 3))(ldb (byte 8 24) integer)))

(defun read-long-from-byte-array (byte-array &optional (offset 0))
  (let ((long 0))
    (setf (ldb (byte 8 0) long) (aref byte-array offset)
          (ldb (byte 8 8) long) (aref byte-array (+ offset 1))
          (ldb (byte 8 16) long) (aref byte-array (+ offset 2))
          (ldb (byte 8 24) long) (aref byte-array (+ offset 3)))
    long))

(defun write-long-to-ptr (ptr integer &optional (offset 0))
  (setf (mem-aref ptr :unsigned-char offset) (ldb (byte 8 0) integer)
	(mem-aref ptr :unsigned-char (+ offset 1))(ldb (byte 8 8) integer)
	(mem-aref ptr :unsigned-char (+ offset 2))(ldb (byte 8 16) integer)
	(mem-aref ptr :unsigned-char (+ offset 3))(ldb (byte 8 24) integer)))

(defun read-long-from-ptr (ptr &optional (offset 0))
  (let ((long 0))
    (setf (ldb (byte 8 0) long) (mem-aref ptr :unsigned-char offset)
          (ldb (byte 8 8) long) (mem-aref ptr :unsigned-char (+ offset 1))
          (ldb (byte 8 16) long) (mem-aref ptr :unsigned-char (+ offset 2))
          (ldb (byte 8 24) long) (mem-aref ptr :unsigned-char (+ offset 3)))
    long))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun send-message (sock
		     buffer-send-ptr
		     cmd)

  ;; send returns data copied to buffer
  ;; when data cannot fit in buffer:
  ;; nonblocking: -1 + EAGAIN/EWOULDBLOCK
  ;; - can it return 0? -> possible if requested to write 0 bytes but again indictates disconnect/error
  ;; blocking: wait
  
  (let* ((len-string (length cmd)))

    (write-long-to-ptr buffer-send-ptr len-string)
    (dotimes (i len-string)
      (setf (mem-aref buffer-send-ptr :unsigned-char (+ i 4))
    	    (char-code (char cmd i))))
    
    (let ((len-sent (c-send sock
			    buffer-send-ptr
			    (+ len-string 4)
			    0)))
      ;; assert length sent matches
      len-sent)))

(defun recv-message (sock
		     buffer-recv-ptr
		     buffer-recv-array)

  ;; Or receive directly into the ptr

  ;; expected to always get all data
  ;; otherwise would need to loop until all data received
  ;; add error for now if not all data received

  ;; nonblocking:
  ;; 0 = client disconnected
  ;; -1 = no data (not possible for blocking socks since they wait indefinitely for data)

  ;; returns (values message|nil len-recv)
  ;; - if nil, check len-recv
  ;; or create separate function
  
  (let* ((len-recv (recv-ptr sock
			     buffer-recv-ptr
			     0
			     :end 4)))

    (when (< len-recv 4)
      ;; (warn (format nil "[recv-message] recv-message returned ~a" len-recv))
      (return-from recv-message (values nil
					len-recv)))

    (let* ((len-data (read-long-from-ptr buffer-recv-ptr))
	   (len-recv (recv-ptr sock
			       buffer-recv-ptr
			       0
			       :end len-data)))

      (when (< len-recv len-data)
	;; (warn "[recv-message] len-recv < len-data")
	(return-from recv-message (values nil
					  len-recv)))

      ;; copy to array for now
      ;; manually implement octets for speed (to avoid consing)
      (let ((start 0))
	(dotimes (i len-data)
	  (setf (aref buffer-recv-array start)
		(mem-aref buffer-recv-ptr :unsigned-char i))
	  (incf start)))

      ;; return data instead of code
      ;;
      ;; if no valid string, return nil on error
      ;; how to handle octets-to-string failure? -> warn and return nil
      (values (read-from-string (babel:octets-to-string buffer-recv-array
							:encoding :ascii
							:start 0
							:end len-data)
				nil)
	      len-recv))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Move all of these to protocol?

(defun accept (sockfd)
  (with-foreign-objects ((addr '(:struct sockaddr-un))
			 (addr-len :int))
    (setf (mem-ref addr-len :int) (foreign-type-size '(:struct sockaddr-un)))
    (let ((sock (c-accept sockfd addr addr-len)))
      (cond ((<= 0 sock)
	     (values sock addr))
	    ((or (= (sb-alien:get-errno) protoform.libc::+e-again+)
		 (= (sb-alien:get-errno) protoform.libc::+e-wouldblock+))
	     'non-blocking)
	    (t
	     (get-str-errno))))))

(defun accept4 (sockfd &optional (flags 0))
  ;; Refactor addr - memory leak?
  
  ;; PASS BELOW IN
  (with-foreign-objects ((addr '(:struct sockaddr-un))
			 (addr-len :int))
    (setf (mem-ref addr-len :int) (foreign-type-size '(:struct sockaddr-un)))
    ;; PASS ABOVE IN
    (let ((sock (c-accept4 sockfd addr addr-len flags)))
      (cond ((<= 0 sock)
	     ;; pass single item back?
	     (values sock addr))
	    ((or (= (sb-alien:get-errno) protoform.libc::+e-again+)
		 (= (sb-alien:get-errno) protoform.libc::+e-wouldblock+))
	     (values nil nil))
	    (t
	     (get-str-errno))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun recv-vector (sockfd buffer flags &key (start 0) (end (length buffer)))
;;   "Call recv until buffer is full"
;;   (declare (type (array (unsigned-byte 8)) buffer))
;;   (let ((len (- end start)))
;;     (with-foreign-objects ((buf :unsigned-char len)) ; create pointer to buf with size len
;;       (let ((r (c-recv sockfd buf len flags)))
;;         (dotimes (i len)
;;           (setf (aref buffer start) (mem-aref buf :unsigned-char i))
;;           (incf start))
;;         (when (< r 0)
;;           (error (get-str-errno)))
;;         r))))

(defun recv (sockfd buffer flags &key (start 0) (end (length buffer)))
  (declare (type (array (unsigned-byte 8)) buffer))
  (let ((len (- end start)))
    (with-foreign-objects ((buf :unsigned-char len)) ; create pointer to buf with size len
      ;; read into c array, then copy to buffer
      (let ((r (c-recv sockfd buf len flags)))
        (dotimes (i len)
          (setf (aref buffer start) (mem-aref buf :unsigned-char i))
          (incf start))
	(cond ((> r 0)
	       r)
	      ((and (= r -1)
		    (or (= (sb-alien:get-errno) protoform.libc::+e-again+)
			(= (sb-alien:get-errno) protoform.libc::+e-wouldblock+)))
	       r)
	      (t ; (= r 0) ; 0-byte buffer or other peer gracefully disconnected
	       r))))))

(defun recv2 (sockfd buffer-array buffer-vector flags &key (start 0) (end (length buffer-vector)))
  (declare (type (array (unsigned-byte 8)) buffer-vector))
  (let ((len (- end start)))
      ;; read into c array, then copy to buffer
    (let ((r (c-recv sockfd buffer-array len flags)))
      ;; (format t "len: ~a~%" len)
      (dotimes (i len)
	;; (format t "start: ~a~%" start)
	(setf (aref buffer-vector start)
	      (mem-aref buffer-array :unsigned-char i))
	(incf start))
      (cond ((> r 0)
	     r)
	    ((and (= r -1)
		  (or (= (sb-alien:get-errno) protoform.libc::+e-again+)
		      (= (sb-alien:get-errno) protoform.libc::+e-wouldblock+)))
	     r)
	    (t ; (= r 0) ; 0-byte buffer or other peer gracefully disconnected ECONNRESET or EINTR
	     r)))))

(defun recv-ptr (sockfd buffer-ptr flags &key (start 0) (end (length buffer-vector)))
  ;; increment into pointer for start?
  ;; mem-aref ptr :void 0
  (let* ((len-ptr (- end start))
	 (len-recv (c-recv sockfd buffer-ptr len-ptr flags)))
      (cond ((> len-recv 0)
	     len-recv)
	    ;; nonblocking
	    ((and (= len-recv -1)
		  (or (= (sb-alien:get-errno) protoform.libc::+e-again+)
		      (= (sb-alien:get-errno) protoform.libc::+e-wouldblock+)))
	     len-recv)
	    ;; Peer "gracefully" disconnected; ECONNRESET (hard close), EPIPE (normal close)
	    ;; ECONN = closed but more data remains in the buffer, and open side attemps op
	    ;; EPIPE = closed and buffer empty, and open side attempts op
	    ;; 0-byte buffer which normally does not happen unless we specify cpying 0 bytes...
	    (t ; (= r 0)
	     len-recv))))

(defun set-nonblock (sock flag)
  (when (not flag)
    (sb-posix:fcntl sock
		    sb-posix::f-setfl
		    (logand (sb-posix:fcntl sock sb-posix::f-getfl)
			    (lognot sb-posix::o-nonblock)))))

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
    
    ;; (format t "[init-socket] sun-path ~a~%" (foreign-string-to-lisp (foreign-slot-value sockaddr-server '(:struct sockaddr-un) 'sun-path)))
    ;; (format t "[init-socket] sockaddr-un size ~a~%" (foreign-type-size '(:struct sockaddr-un)))
    sockaddr))

(defun init-socket (nonblock path-server)
  
  (let ((sock (c-socket protoform.libc::+af-unix+
			(if nonblock
			    (logior protoform.libc::+sock-stream+ protoform.libc::+sock-nonblock+)
			    (logior protoform.libc::+sock-stream+))
			0)))
    (when (= sock -1)
      (error (get-str-errno)))
    
    ;; return sock and sockaddr
    ;; caller must free sockaddr
    (values sock
	    (init-sockaddr path-server))))


(defun init-socket-server (nonblock path-server)
  
  (multiple-value-bind (sock-server sockaddr-server)
      (init-socket nonblock path-server)
    
    (with-foreign-string (ptr path-server)
      (c-unlink ptr))
    
    (let ((ret (c-bind sock-server
		       sockaddr-server
		       (foreign-type-size '(:struct sockaddr-un)))))
      (when (= ret -1) (error (get-str-errno))))
    (format t "[init-socket-server] Bound socket~%")
    
    ;; Free the struct
    (foreign-free sockaddr-server)
        
    (let ((ret (c-listen sock-server 64)))
      (when (= ret -1) (error (get-str-errno))))
    (format t "[init-socket-server] Listening on socket for connections...~%")
    
    (return-from init-socket-server sock-server)))

;; reverse arg order
;; pass sleep in
;; refactor
(defun init-socket-client (path-server nonblock)
  
  (multiple-value-bind (sock-client sockaddr-server)
      (init-socket nonblock path-server)

    ;; Try to connect - wait for server
    (loop
       (when (zerop (c-connect sock-client
			       sockaddr-server
			       (foreign-type-size '(:struct sockaddr-un))))
	 (return))
       (format t "[init-socket-client] Trying connect again in 0.5 seconds...~%")
       (sleep 0.5))

    (format t "[init-socket-client] Connected socket~%")

    (foreign-free sockaddr-server)
    
    (return-from init-socket-client sock-client)))
