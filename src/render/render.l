#{
    Global variables start with an asterisk "*"
    Global constants may be written all-uppercase
    Functions and other global symbols start with a lower case letter
    Locally bound symbols start with an upper case letter
    Local functions start with an underscore "_"
    Classes start with a plus-sign "+", where the first letter
        is in lower case for abstract classes
        and in upper case for normal classes 
    Methods end with a right arrow ">"
    Class variables may be indicated by an upper case letter 
}#

#Create and change to namespace model
(symbols 'render 'pico)

# C library paths
(def 'PATH-MATHC "/home/user/quicklisp/local-projects/protoform/ext/mathc/libmathc.so")
(def 'PATH-GLYPHS "/home/user/quicklisp/local-projects/protoform/src/glyphs")
# UDS paths
(def 'PATH-UDS-MODEL  "/tmp/protoform-model.socket")
(def 'PATH-UDS-INPUT  "/tmp/protoform-input.socket")
(def 'PATH-UDS-RENDER "/tmp/protoform-render.socket")

# Set math precision to 6 decimals
(scl 6)

# Wrapper libraries
(load "/home/user/quicklisp/local-projects/protoform/src/c/mathc.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/posix/mman.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/socket.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/gl/es.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/glfw.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/epoll.l")
# Lisp libraries
#...
(load "/home/user/quicklisp/local-projects/protoform/src/ipc/ipc.l")
#(load "/home/user/quicklisp/local-projects/protoform/src/render/raster.l")
#(load "/home/user/quicklisp/local-projects/protoform/src/render/compute.l")


(def '*window)


(de init-render ()
    #{
    - Model can modify these during runtime through wire
    - Model will send (memcpy dest size offset)
    - Render will look up symbol/Mmap, get ptr, memcpy
    }#
    #(setq *mmap-atomic-counter (init-mmap-atomic-counter))
    #(setq *mmap-draw-indirect  (init-mmap-draw-indirect))
    #(setq *mmap-vertices       (init-mmap-vertices))
    #(setq *mmap-element        (init-mmap-element))
    #(setq *mmap-projview       (init-mmap-projview))
    #(setq *mmap-tex-glyphs     (init-mmap-tex-glyphs))

    (init-window)

    (init-gl)

    (when NIL
      (init-ipc))

    #Init buffers/resources
    #Glyphs will be loaded during texture buffer object creation
    #(init-buffer-objects)

    #(new '(gl~+Buffer-Object))
    #(bye)

    #Should not copy all data every frame - waste
    #Try single buffer + persistent mapping + coherent
    #Disable draw flag for node, before copying data, then enable after
    
    #{
    (defun setup-render (params-model)
  
      (fmt-render "setup-render" "Initializing shm handles~%")
      (init-handles-shm params-model)

      (fmt-render "setup-render" "Initializing buffer object caches~%")
      (init-bo-caches params-model)

      (fmt-render "setup-render" "Initializing buffer object steps~%")
      (init-bo-step params-model)

      (fmt-render "setup-render" "Initializing shader bindings~%")
      ;; Shader specific initialization
      (gl:use-program (prog-rast-msdf *render*))
      (init-buff-rast-msdf params-model)
      (gl:use-program (prog-compute *render*))
      (init-buff-compute params-model)

      ;; At this point, shm already has data loaded by model
      ;; so copy to OpenGL buffers
      (memcpy-shm-to-all)

      (fmt-render "setup-render" "Finished initializing shm handles~%"))
    }#
    

    #Init pipeline
    #(init-prog-rast)
    #(init-prog-comp)
    
    T)

(de init-window ()
    (glfw~init-2)

    #OpenGL ES only GL binding API that does not depend on GLX
    #Migrate to Vulkan later...
    (glfw~window-hint glfw~CONTEXT-VERSION-MAJOR 3)
    (glfw~window-hint glfw~CONTEXT-VERSION-MINOR 2)
    (glfw~window-hint glfw~CLIENT-API            glfw~OPENGL-ES-API)
    (glfw~window-hint glfw~CONTEXT-CREATION-API  glfw~NATIVE-CONTEXT-API)
    
    (let (Title (native "@" "malloc" 'N 10))
      (struct Title 'N (cons "Protoform" 32))
      (setq *window (glfw~create-window 640 480 Title 0 0))
      (when (= *window 0)
	(quit "(= *window 0)"))
      (native "@" "free" NIL Title))
    
    (glfw~make-context-current *window)
    
    (glfw~set-key-callback *window
			   (lisp 'key-callback
				 '((Window Key Scancode Action Mods)
				   (when (and (= (struct Key 'I)    glfw~KEY-ESCAPE)
					      (= (struct Action 'I) glfw~PRESS))
				     (glfw~set-window-should-close (struct Window 'N) glfw~TRUE))))))


(de init-gl ()
    #Get width height from args
    (gl~viewport 0 0 640 480)
    (gl~enable gl~CULL-FACE)
    (gl~enable gl~DEPTH-TEST)
    (gl~enable gl~BLEND)
    (gl~blend-func gl~SRC-ALPHA gl~ONE-MINUS-SRC-ALPHA))


(de init-ipc ()
    (setq *ep-events (native "@" "malloc" 'N (* 12 2)) #1 event
	  *ep-fd     (epoll~create-size 1)
	  *buf-sz    (socket~read-rmem-default)
	  *buf-rd    (native "@" "malloc" 'N *buf-sz))
    
    (setq *ipc (new '(ipc~+IPC) PATH-UDS-RENDER 'block
		    PATH-UDS-MODEL 'block))
    
    (epoll~ctl-epfd *ep-fd 
		    epoll~ADD
		    (get *ipc 'ipc~listener)
		    epoll~IN)
    
    (epoll~ctl-epfd *ep-fd 
		    epoll~ADD
		    (get *ipc 'ipc~connection)
		    epoll~IN))


(de run-render ()
    #After connecting, start processing messages
    #Render will send frame start and wait for frame end
    #Up to model to perform time management
    
    (while (= (glfw~window-should-close *window) 0)
      
      (println (usec))

      (gl~clear-color 1.0
		      1.0
		      1.0
		      0.0)
      (gl~clear-mask (| gl~COLOR-BUFFER-BIT gl~DEPTH-BUFFER-BIT))
      
      (glfw~swap-buffers *window)
      (glfw~poll-events))

    (glfw~destroy-window *window)
    (glfw~terminate))


(de main ()
    (init-render)
    (run-render)
    (println 'Exiting...)
    (bye))
