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
#Shader paths
(def '*path-structs-shader "/home/user/quicklisp/local-projects/protoform/glsl/structs.glsl")
(def '*paths-rast-vert-shader
    (list *path-structs-shader
	  "/home/user/quicklisp/local-projects/protoform/glsl/node.vs.glsl"))
(def '*paths-rast-frag-shader
    (list *path-structs-shader
	  "/home/user/quicklisp/local-projects/protoform/glsl/filter-bilinear.fs.glsl"
	  "/home/user/quicklisp/local-projects/protoform/glsl/msdf.fs.glsl"))
(def '*paths-comp-shader
    (list *path-structs-shader
	  "/home/user/quicklisp/local-projects/protoform/glsl/node.cs.glsl"))


# Set math precision to 6 decimals
(scl 6)

# Wrapper libraries
(load "/home/user/quicklisp/local-projects/protoform/src/c/mathc.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/posix/mman.l")
#(load "/home/user/quicklisp/local-projects/protoform/src/c/posix/stat.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/socket.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/gl/es.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/glfw.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/epoll.l")
# Lisp libraries
#...
(load "/home/user/quicklisp/local-projects/protoform/src/ipc/ipc.l")
(load "/home/user/quicklisp/local-projects/protoform/src/render/params-buffer.l")
(load "/home/user/quicklisp/local-projects/protoform/src/render/raster.l")
#(load "/home/user/quicklisp/local-projects/protoform/src/render/compute.l")


(def '*window)
(def '*buffer-objects)
(def '*prog-rast)
(def '*prog-comp)
(def '*vao)


(de init-render ()
    #{
    - Model can modify these during runtime through wire
    - Model will send (memcpy dest size offset)
    - Render will look up symbol/Mmap, get ptr, memcpy

    - Should not copy all data every frame - waste
    - Try single buffer + persistent mapping + coherent
    - Disable draw flag for node, before copying data, then enable after
    
    }#

    #Get number of nodes and window size from args instead of file
    (let (Argv (argv))
      (setq *width      (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "width invalid")))
	    *height     (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "height invalid")))
	    *verts-max   (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "verts invalid")))))
    
    #Init context
    (init-window)
    (init-gl)

    #Init OpenGL programs
    (init-rast)

    #Don't need yet...
    #To disable drawing an object, use 0.0 (float) that is multiplied in vertex shader
    #Change flags with a vec4
    #(init-comp)

    (init-ipc))


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
    (gl~blend-func gl~SRC-ALPHA gl~ONE-MINUS-SRC-ALPHA)

    #Get some function pointers
    (setq gl~*glBufferStorage (glfw~get-proc-address "glBufferStorage"))

    T)


(de init-ipc ()
    (setq *ep-events (native "@" "malloc" 'N (* 12 2)) #1 event
	  *ep-fd     (epoll~create-size 1)
	  *buf-sz    (socket~read-rmem-default)
	  *buf-rd    (native "@" "malloc" 'N *buf-sz))

    
    #PATH-UDS-RENDER 'block
    (when T
      (setq *ipc (new '(ipc~+IPC) ""             'ipc~nonblock
		      PATH-UDS-MODEL 'ipc~nonblock))
      
      (epoll~ctl-epfd *ep-fd 
		      epoll~ADD
		      (get *ipc 'ipc~connection)
		      epoll~IN)))


(de run-render ()
    #After connecting, start processing messages
    #Render will send frame start and wait for frame end
    #Up to model to perform time management
    
    (while (= (glfw~window-should-close *window) 0)
      
      #(println (usec))
      
      (gl~clear-color 1.0
		      1.0
		      1.0
		      0.0)
      (gl~clear-mask (| gl~COLOR-BUFFER-BIT gl~DEPTH-BUFFER-BIT))
      
      (glfw~swap-buffers *window)

      (glfw~poll-events)
      (epoll-wait 0)
      T)

    (glfw~destroy-window *window)
    (glfw~terminate))


(de epoll-wait (Timeout)
    #{
    Poll events ASAP and enqueue for processing by controller thread
    
    epoll-wait:
    * ep-events size should correspond to maxevents
    * -1 = timeout = block/infinite
    0 = return if nothing
    * returns event count
    }#

    #(prinl (usec) " | input | Starting epoll...")
    
    (let (Events     NIL
	  Fd         NIL
          Num-Events (epoll~wait-epfd *ep-fd *ep-events 2 Timeout))
	 
	 (for I Num-Events

	   (let (Ep-events (struct *ep-events '(I I)))
	     (setq Events (get Ep-events 1))
	     (setq Fd     (get Ep-events 2))
	     
	     #(prinl (usec) " FD: " Fd ", Events: " Events)

	     (cond
	       ((= Fd (get *ipc 'ipc~listener))
		(ipc~handle-sock-listen> *ipc Events Fd *ep-fd))
	       (T
		(ipc~handle-sock-conn> *ipc Events Fd *ep-fd)))))))


(de main ()
    (init-render)
    (run-render)
    (println 'Exiting...)
    (bye))
