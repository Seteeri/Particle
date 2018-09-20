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
		 #:pango-markup
		 #:bordeaux-threads
		 #:trivial-timers
		 #:babel
		 #:str
		 #:easing
		 #:skip-list
		 #:lparallel)
    		 ;; #:swank
		 ;; #:dlist

    :serial t
    :components ((:file "src/package")
		 (:module libc
		 	  :pathname "src/libc"
		 	  :components ((:file "epoll")
				       (:file "memory")
				       (:file "socket")))
		 (:module opengl
			  :pathname "src/opengl"
			  :components ((:file "opengl")
				       (:file "buffer-object")
				       (:file "array-element")
				       (:file "array-indirect")))
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
		 	  :components ((:file "texture")
				       (:file "view")
				       (:file "compute")
				       (:file "raster")
				       (:file "conn-client")
				       (:file "mapping-base")
				       (:file "glfw")))
		 (:module controller
			  :pathname "src/controller"
			  :components ((:file "controller")
				       (:file "conn-client")
				       (:file "xkb")
				       (:file "callback")
				       (:file "state")
				       (:file "keysymdef")
				       (:file "cursor")
				       (:file "projview")))
		 (:module model
			  :pathname "src/model"
			  :components ((:file "model")
				       (:file "conn-client")
				       (:file "conn-server")
				       (:file "matrix-model")
				       (:file "mapping-base")				       
				       (:file "projview")
				       (:module layout-text
						:pathname "font"
						:components ((:file "metrics")
							     (:file "chr")
							     (:file "layout")))
				       (:module structures
						:pathname "structures"
						:components ((:file "dag")))))
		 ;; (:module lexer
		 ;; 	  :pathname "lexer"
		 ;; 	  :components ((:file "sharpsign")
		 ;; 		       (:file "lexer")))))
		 (:file "src/protoform")))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:set-floating-point-modes :traps nil))
