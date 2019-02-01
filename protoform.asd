;; https://common-lisp.net/~mmommer/asdf-howto.shtml

(asdf:defsystem #:protoform
    :description "A Common Lisp nodal UI platform based on DRM-KMS and OpenGL"
    :author "Kevin Ednalino <kcednalino@gmail.com>"
    :license "Apache License 2.0"
    :depends-on (#:alexandria
		 #:osicat
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
		 #:skip-list
		 #:sb-concurrency
		 #:pack
		 #:spatial-trees)
    ;; #:babel
    ;; #:dlist
    :serial t
    :around-compile (lambda (next)
                      (proclaim '(optimize
    				  (debug 0)
                                  (safety 0)
    				  (space 0)
                                  (speed 3)))
                      (funcall next))
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
				       (:file "shm")
				       (:file "compute")
				       (:file "raster")
				       (:file "memcpy")
				       (:file "glfw")))
		 (:module analyzer-dep
		 	  :pathname "src/analyzer-dep"
		 	  :components ((:file "analyzer-dep")))
		 (:module model
			  :pathname "src/model"
			  :components ((:module shm
				       		:pathname "shm"
				       		:components ((:file "shm")
							     (:file "atomic-counter")
							     (:file "draw-indirect")
							     (:file "element")
							     (:file "nodes")
							     (:file "projview")
							     (:file "texture")
				       			     (:file "vertices")))
				       (:module node
				       		:pathname "node"
				       		:components ((:file "node")
							     (:file "operators")
							     (:file "shm")
				       			     (:file "pointer")))
				       (:module projview
				       		:pathname "projview"
				       		:components ((:file "projview")))
				       (:module controller
				       		:pathname "controller"
				       		:components ((:file "keysymdef")
							     (:file "controller")
				       			     (:file "xkb")
				       			     (:file "callback")
							     (:file "dispatch")
				       			     (:module events
				       				      :pathname "events"
				       				      :components ((:file "keyboard")
										   (:file "touch")
										   (:file "tablet-tool")))))
				       (:module rpc
				       		:pathname "rpc"
				       		:components ((:file "rpc")
							     (:file "view")
							     (:file "memcpy")))
				       (:file "pango")
				       (:file "metrics")
				       (:file "animation")
				       (:file "variables")
				       (:file "callbacks")
				       (:file "matrix-model")
				       (:file "init")
				       (:file "main")))
		 (:file "src/protoform")))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:set-floating-point-modes :traps nil))
