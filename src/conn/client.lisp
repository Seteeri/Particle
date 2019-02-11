(in-package :protoform.conn)

(defclass conn-client ()
  ((sock :accessor sock :initarg :sock :initform nil)
   (id :accessor id :initarg :id :initform nil)
   
   (buffer-ptr-recv :accessor buffer-ptr-recv :initarg :buffer-ptr-recv :initform (foreign-alloc :unsigned-char :count 4096))
   (buffer-arr-recv :accessor buffer-arr-recv :initarg :buffer-arr-recv :initform (make-array 4096
											      :adjustable nil
											      :fill-pointer nil
											      :element-type '(unsigned-byte 8)))

   (buffer-ptr-send :accessor buffer-ptr-send :initarg :buffer-ptr-send :initform (foreign-alloc :unsigned-char :count 4096))
   (buffer-arr-send :accessor buffer-arr-send :initarg :buffer-arr-send :initform (make-array 4096
											      :adjustable nil
											      :fill-pointer nil
											      :element-type '(unsigned-byte 8)))))

;; Switch to overloading initialize-instance
(defun init-conn-client (&optional (sock nil))
  (make-instance 'conn-client
		 :sock sock))

(defun communicate-request (conn-client message ret)
  (with-slots (sock
	       buffer-ptr-recv
	       buffer-arr-recv
	       buffer-ptr-send
	       buffer-arr-send
	       lock-recv
	       lock-send)
      conn-client

    ;; (bt:with-lock-held (lock-send)
    (send-message sock
		  buffer-ptr-send
		  message)
  
    (when ret
      ;; (bt:with-lock-held (lock-recv)
      (recv-message sock
		    buffer-ptr-send
		    buffer-arr-send))))
