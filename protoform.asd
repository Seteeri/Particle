;; https://common-lisp.net/~mmommer/asdf-howto.shtml

(asdf:defsystem #:protoform
    :description "A Common Lisp nodal userland based on DRM-KMS, OpenGL and Wayland"
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
		 #:cl-glfw3
		 #:3d-vectors
		 #:3d-matrices
		 #:cl-digraph
		 #:cl-digraph.dot
		 #:usocket
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
		 #:trivial-benchmark)
    ;; #:cl-redis
    ;; #:pango-markup
    ;; #:cl-pango
    ;; #:cl-cairo2		 
    ;; #:pango-markup    
    ;; #:babel
    ;; #:dlist
    ;; #:skip-list    
    ;; #:sb-concurrency    
    ;; #:qbase64
    ;; #:cl-redis	
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
				       (:file "misc")))
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
		 (:module render
		 	  :pathname "src/render"
		 	  :components ((:file "init")
				       (:file "render")
				       (:file "step")
				       (:file "cache")
				       (:file "shm")
				       (:file "compute")
				       (:file "raster")
				       (:file "memcpy")
				       (:file "glfw")
				       (:file "server")))
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
							     (:file "pointer")
							     (:file "link")
							     (:file "degree")
							     (:file "translate")
							     (:file "traverse")
							     (:file "memcpy")))
				       (:module projview
				       		:pathname "projview"
				       		:components ((:file "projview")
							     (:file "shm")))
				       (:module client
				       		:pathname "client"
				       		:components ((:file "conn")
						             (:file "recv")
							     (:file "send")))
				       (:module task-manager
				       		:pathname "task-manager"
				       		:components ((:file "manager")
							     (:file "task")
							     (:file "animation")))
				       (:module user
				       		:pathname "user"
				       		:components ((:module node
				       				      :pathname "node"
				       				      :components ((:file "callbacks")
										   (:file "operators")))
							     (:module projview
				       				      :pathname "projview"
				       				      :components ((:file "callbacks")
										   (:file "operators")))
							     (:module file
				       				      :pathname "file"
				       				      :components ((:file "file")
										   (:file "character")))))
				       (:file "special")
				       (:file "init")
				       (:file "main")))
		 (:file "src/protoform")))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:set-floating-point-modes :traps nil))
