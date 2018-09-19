(in-package :protoform.libc)

(defcstruct pollfd
    (fd :int)
  (events :short)
  (revents :short))

(defcfun ("poll" c-poll) :int
  (fds (:pointer (:struct pollfd)))
  (nfds :uint)
  (timeout :int))

;; (let ((fds-poll (foreign-alloc '(:struct pollfd))))
;;   (setf (foreign-slot-value fds-poll '(:struct pollfd) 'fd) libinput-fd)
;;   (setf (foreign-slot-value fds-poll '(:struct pollfd) 'events) #x001)
;;   (setf (foreign-slot-value fds-poll '(:struct pollfd) 'revents) 0)
;;   (setf epoll-fd fds-poll))))
