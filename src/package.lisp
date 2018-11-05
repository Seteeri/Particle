(defpackage #:protoform.util
  (:use #:cl
        #:cffi)
  (:export #:align-size
	   #:normalize
	   #:rad-to-deg
	   #:deg-to-rad
	   #:read-file-string))

(defpackage #:protoform.libc
  (:use #:cl
        #:cffi)
  (:export #:+shut-rdwr+
	   #:c-bind
	   #:c-recv
	   #:c-send
	   #:c-listen
	   #:c-shutdown
	   #:c-close
	   #:c-socket
	   #:c-unlink
	   #:c-connect
	   #:c-accept
	   #:c-accept4
	   #:+sock-nonblock+
	   #:sun-family
	   #:sun-path
	   #:sockaddr-un

	   #:ctl-epoll
	   #:c-epoll-create
	   #:event
	   #:events
	   #:data
	   #:fd
	   #:c-epoll-wait
	   
	   ;; #:pollfd
	   ;; #:fd
	   ;; #:events
	   ;; #:revents
	   ;; #:c-poll

	   #:c-memmove
	   #:init-mmap
	   #:c-memcpy
	   #:name
	   #:path
	   #:size
	   #:mmap
	   #:mmap-file
	   #:ptr
	   #:cleanup-mmap))

(defpackage #:protoform.opengl
  (:use #:cl
        #:cffi
	#:3d-vectors
	#:3d-matrices
	#:protoform.util)	
  (:export #:init-gles
	   #:cad-shader
	   #:compile-shader
	   #:attach-shader
	   #:delete-shader
	   #:init-buffer-object
           #:init-vao
           #:init-boa-element
	   #:init-buffer-object
	   #:init-buffer-draw-indirect
	   #:set-bo-indirect
	   #:update-binding-buffer
	   #:make-orthographic-vector
	   #:make-perspective-vector
	   #:clean-up-buffer-object
	   #:get-gl-maxes
	   #:set-bo-element
	   #:set-bo-draw-indirect
	   #:copy-buffer
	   
	   #:target
	   #:binding-layout
	   #:fn-bind
	   #:buffers
	   #:count-buffers
	   #:size-buffer
	   #:mapped-persistent
	   #:ptrs-buffer

	   #:single
	   #:double
	   #:triple
	   #:quad
	   ))

(defpackage #:protoform.conn
  (:use #:cl
        #:cffi
	#:protoform.libc)
  (:export #:init-sock-server
	   #:init-sock-client
	   #:accept4
	   #:send-message
	   #:recv-message))

(defpackage #:protoform.drm
  (:use #:cl
        #:cffi
	#:protoform.libc))

(defpackage #:protoform.analyzer-dep
  (:use #:cl
        #:cffi
	#:protoform.util)
  (:export #:analyze-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:protoform.view
  (:use #:cl
        #:cffi
	#:protoform.libc
	#:protoform.opengl
	#:protoform.conn
	;; #:protoform.drm
	#:3d-vectors
	#:3d-matrices
	#:trivial-timers)
  (:export #:run-view))

(defpackage #:protoform.model
  (:use #:cl
        #:cffi
	#:protoform.libc
	#:protoform.opengl
	#:protoform.conn
	#:protoform.util
	#:3d-vectors
	#:3d-matrices
	#:trivial-timers
	#:lparallel
	#:lparallel.queue
	#:protoform.analyzer-dep)
  (:export #:run-model))


(defpackage #:protoform
  (:use #:cl
        #:cffi
	#:protoform.libc
	#:protoform.conn)
  (:export #:main))
