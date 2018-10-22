;; https://common-lisp.net/~mmommer/asdf-howto.shtml

(asdf:defsystem #:protoform
    :description "A Common Lisp OpenGL UI-shell based on DRM-KMS and OpenGL ES"
    :author "Kevin Ednalino <kcednalino@gmail.com>"
    :license "Apache License 2.0"
    :depends-on (#:osicat
		 #:cl-drm
		 #:cl-gbm
		 #:cl-egl
		 #:cl-opengl
		 #:cl-wayland
		 #:cl-libinput
		 #:cl-xkb
		 #:cl-pango
		 #:cl-cairo2
		 #:cl-glfw3
		 #:3d-vectors
		 #:3d-matrices
		 #:cl-digraph
		 #:cl-digraph.dot
		 #:usocket
		 #:pango-markup
		 #:bordeaux-threads
		 #:inferior-shell
		 #:trivial-timers
		 #:str
		 #:easing
		 #:lparallel
		 #:skip-list)
		 ;; #:babel
		 ;; #:dlist

    :serial t
    :components ((:file "src/package")
		 (:file "src/util")
		 (:module libc
		 	  :pathname "src/libc"
		 	  :components ((:file "epoll")
				       (:file "memory")
				       (:file "socket")))
		 (:module opengl
			  :pathname "src/opengl"
			  :components ((:file "opengl")
				       (:file "buffer-object")))
		 (:module conn
		 	  :pathname "src/conn"
		 	  :components ((:file "client")
				       (:file "protocol")))
		 (:module drm
			  :pathname "src/drm"
			  :components ((:file "gbm")
				       (:file "egl")
				       (:file "drm")
				       (:file "fb")
				       (:file "plane")
				       (:file "crtc")
				       (:file "connector")))
		 (:module view
		 	  :pathname "src/view"
		 	  :components ((:file "view")
				       (:file "cache")
				       (:file "handle-shm")
				       (:file "compute")
				       (:file "raster")
				       (:file "memcpy")
				       (:file "glfw")))
		 (:module model
			  :pathname "src/model"
			  :components ((:file "model")
				       (:file "rpc")
				       (:file "handle-shm")				       
				       (:file "matrix-model")
				       (:file "projview")
				       (:file "node")
				       (:file "pango")
				       (:file "metrics")
				       (:file "pointer")
				       (:module controller
				       		:pathname "controller"
				       		:components ((:file "keysymdef")
							     (:file "controller")
				       			     (:file "xkb")
				       			     (:file "callback")
				       			     (:file "event-keyboard")))))
		 (:file "src/protoform")))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:set-floating-point-modes :traps nil))
