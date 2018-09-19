(in-package :protoform.controller)

;; These are sent to model
;; Make these macros around communicate-request

(defun request-sync (conn-client)
  (communicate-request conn-client
		       "(:sync)"
		       t))

(defun request-exit (conn-client)
  (communicate-request conn-client
		       "(:exit)"
		       t))

(defun request-insert (conn-client
		       keysym
		       ret)
  (communicate-request conn-client
		       (format nil "(:insert ~a ~a)" keysym ret)
		       ret))

(defun request-delete (conn-client
		       pos
		       ret)
  (communicate-request conn-client
		       (format nil "(:delete ~a ~a)" pos ret)
		       ret))

(defun request-modify-rgba (conn-client
			    pos
			    rgba
			    ret)
  (communicate-request conn-client
		       (format nil "(:modify-rgba ~a ~a ~a)" pos rgba ret)
		       ret))

(defun request-view (conn-client)
  (communicate-request conn-client
		       "(:view)"
		       t))
