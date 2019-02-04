(defpackage #:protoform.aux
  (:use #:cl
        #:cffi
	#:3d-vectors
	#:3d-matrices)
  (:export #:align-size
	   #:normalize
	   #:rad-to-deg
	   #:deg-to-rad
	   #:read-file-string
	   #:model-matrix
	   #:matrix
	   #:translation
	   #:rotation
	   #:scale
	   #:set-matrix
	   #:update-transform))

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
	#:protoform.aux)	
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

;; (defpackage #:protoform.analyzer-dep
;;   (:use #:cl
;;         #:cffi
;; 	#:protoform.util)
;;   (:export #:analyze-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:protoform.controller
  (:use #:cl
        #:cffi
	#:protoform.libc
	#:protoform.aux
	#:3d-vectors
	#:3d-matrices
	#:lparallel
	#:lparallel.queue
	#:trivial-timers)
  (:export #:init-controller
	   #:run-controller
	   #:register-callback
	   #:unregister-callback
	   #:key-callbacks))

(defpackage #:protoform.font
  (:use #:cl
        #:cffi
	#:protoform.libc
	#:protoform.aux
	#:3d-vectors
	#:3d-matrices
	#:lparallel
	#:lparallel.queue)
  (:export #:init-metrics
	   #:advance
	   #:translate
	   #:range
	   #:bounds-origin
	   #:bounds-texture
	   #:scale
	   #:dims-glyph
	   #:ratio-aspect
	   #:scale-uv
	   #:uv))

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
	#:alexandria
	#:protoform.libc
	#:protoform.opengl
	#:protoform.conn
	#:protoform.aux
	#:protoform.font
	#:protoform.controller	
	#:3d-vectors
	#:3d-matrices
	#:lparallel
	#:lparallel.queue)
  (:export #:run-model))


(defpackage #:protoform
  (:use #:cl
        #:cffi
	#:protoform.libc
	#:protoform.conn)
  (:export #:main))
