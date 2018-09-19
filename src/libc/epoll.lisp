(in-package :protoform.libc)

;; (define-foreign-library libc
;;     (:unix (:or "/usr/lib64/libc.so.6" "/usr/lib64/libc.so.0" "/usr/lib64/libc.so" "/usr/lib/x86_64-linux-gnu/libc.so"))
;;   (t (:default "libc")))
;; (use-foreign-library libc)

(defcenum events
    (:in #x001)
  (:pri #x002)
  (:out #x004)
  (:rdnorm #x040)
  (:rdband #x080)
  (:wrnorm #x100)
  (:wrband #x200)
  (:msg #x400)
  (:err #x008)
  (:hup #x010)
  (:rdhup #x2000)
  (:exclusive 268435456) ;(ash 1 28)
  (:wakeup 536870912)    ;(ash 1 29)
  (:oneshot 1073741824)  ;(ash 1 30)
  (:et 2147483648))      ;(ash 1 31)

;; Valid opcodes ( "op" parameter ) to issue to epoll_ctl().
(defcenum ctl
    (:add 1) ; Add a file descriptor to the interface.
  (:del 2) ; Remove a file descriptor from the interface.
  (:mod 3)); Change file descriptor epoll_event structure.

(defcunion data
    (ptr :pointer)
  (fd :int32)
  (u32 :uint32)
  (u64 :uint64))

(defcstruct event
    (events :uint32)
  (data (:union data)))

(defcfun ("epoll_create" c-epoll-create) :int
  (size :int))

(defcfun ("epoll_create1" c-epoll-create1) :int
  (flags :int))

(defcfun ("epoll_ctl" c-epoll-ctl) :int
  (epfd :int)
  (op :int)
  (fd :int)
  (event (:pointer (:struct event))))

(defcfun ("epoll_wait" c-epoll-wait) :int
  (epfd :int)
  (event (:pointer (:struct event)))
  (maxevents :int)
  (timeout :int))

(defcfun ("epoll_pwait" c-epoll-pwait) :int
  (epfd :int)
  (event (:pointer (:struct event)))
  (maxevents :int)
  (timeout :int)
  (ss :pointer)) ; const __sigset_t *__ss

(defun epoll-ctl (epfd op fd event)
  (c-epoll-ctl epfd
	       (if (keywordp op)
		   (foreign-enum-value 'ctl op)
		   op)
	       fd
	       event))

(defun ctl-epoll (fd-epoll fd-event flags op)
  (with-foreign-objects ((ev-epoll '(:struct event)))
    (setf (foreign-slot-value ev-epoll '(:struct event) 'events)
	  flags)
    (setf (foreign-slot-value (foreign-slot-value ev-epoll '(:struct event) 'data)
			      '(:union data)
			      'fd)
	  fd-event)
    (epoll-ctl fd-epoll
	       op fd-event
	       ev-epoll)))
