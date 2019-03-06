;; https://common-lisp.net/~mmommer/asdf-howto.shtml

(asdf:defsystem #:protoform
    :description " A Common Lisp nodal REPL shell based on DRM-KMS, OpenGL and Wayland"
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
		 #:spatial-trees
		 #:qbase64
		 #:cl-redis
		 #:trivial-benchmark)
    ;; #:babel
    ;; #:dlist
    :serial t
    :around-compile (lambda (next)
                      (proclaim '(optimize
    				  (debug 1)
                                  (safety 1)
    				  (space 0)
                                  (speed 3)))
                      (funcall next))
    :components ((:file "src/package")
		 (:module aux
			  :pathname "src/aux"
		 	  :components ((:file "matrix-model")
				       (:file "util")))
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
		 (:module controller
			  :pathname "src/controller"
			  :components ((:file "keysymdef")
				       (:file "controller")
				       (:file "xkb")
				       (:file "callback")
				       (:module keyboard
				       		:pathname "keyboard"
				       		:components ((:file "keyboard")
							     (:file "dispatch")))
				       (:module touch
				       		:pathname "touch"
				       		:components ((:file "touch")))
				       (:module pointer
				       		:pathname "pointer"
				       		:components ((:file "pointer")))
				       (:module tablet-tool
				       		:pathname "tablet-tool"
				       		:components ((:file "tablet-tool")))))
		 (:module font
			  :pathname "src/font"
			  :components ((:file "metrics")))
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
				       (:module vcs
				       		:pathname "vcs"
				       		:components ((:file "vcs")))
				       (:module node
				       		:pathname "node"
				       		:components ((:file "node")
							     (:file "link")
							     (:file "degree")
							     (:file "methods")
							     (:file "translation")
							     (:file "traversal")
				       			     (:file "pointer")
							     (:file "shm")
							     (:module ops
				       				      :pathname "ops"
				       				      :components ((:file "callbacks")
										   (:file "operators")))))
				       (:module projview
				       		:pathname "projview"
				       		:components ((:file "projview")
							     (:file "shm")
							     (:module ops
				       				      :pathname "ops"
				       				      :components ((:file "callbacks")
										   (:file "operators")))))
				       (:module rpc
				       		:pathname "rpc"
				       		:components ((:file "conn")
							     (:module local
				       				      :pathname "local"
				       				      :components ((:file "view")))
							     (:module remote
				       				      :pathname "remote"
				       				      :components ((:file "send")
										   (:file "memcpy")))))
				       (:module task-manager
				       		:pathname "task-manager"
				       		:components ((:file "manager")
							     (:file "task")))
				       (:file "animation")
				       (:module main
						:pathname "main"
						:components ((:file "variables")
							     (:file "init")
							     (:file "main")))))
		 (:file "src/protoform")))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:set-floating-point-modes :traps nil))
