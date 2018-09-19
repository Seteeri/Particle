(in-package :protoform.model)

;; These are sent to view

(defun request-exit (conn-client)
  (communicate-request conn-client
		       "(:exit)"
		       t))

(defun request-sync (conn-client)
  (communicate-request conn-client
		       "(:sync)"
		       t))

(defun request-query-buffer-objects (conn-client)
  (communicate-request conn-client
		       "(:query-buffer-objects)"
		       t))

(defun request-memcpy (conn-client
		       dest
		       src
		       size
		       ret
		       &key
			 (offset-dest 0)
			 (offset-src 0))
  
  (communicate-request conn-client
		       (format nil "(:memcpy \"~a\" \"~a\" ~a ~a ~a ~a)"
			       dest
			       src
			       offset-dest
			       offset-src
			       size
			       ret)
		       ret))
