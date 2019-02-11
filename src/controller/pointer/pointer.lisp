(in-package :protoform.controller)

(defun handle-event-pointer-motion (event)
  (let* ((pointer-event (libinput:event-get-pointer-event event))
	 (dx (libinput:event-pointer-get-dx-unaccelerated pointer-event))
	 (dy (libinput:event-pointer-get-dy-unaccelerated pointer-event)))

    (sb-concurrency:enqueue (list nil
				  'translate-node
				  '()
				  (lambda ()
				    (funcall *translate-node-rel* nil dx dy)))
			    *queue-tasks-sync*)
    
    ;; (format t "dx: ~A, dy: ~A~%" dx dy)

    t))

