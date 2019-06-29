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
  
    ############################################################################
    
    (init-window)
    (init-gl)
    (when NIL
      (init-ipc))
    
    #Init buffers -> programs
    (init-buffer-objects)
    #(init-tex-glyphs (get (get '*buffer-objects 'texture-glyphs) 'gl~ptr))        
    (init-progs)

    (gl~use-program *prog-rast)
    (setq *vao (car (gl~gen-vertex-arrays 1)))
    (gl~bind-vertex-array *vao)

    (bye)

    #(gl:use-program (prog-compute *render*))
    #(init-buff-compute params-model)
    
    #Use program raster

    #At this point, shm already has data loaded by model
    #so copy to OpenGL buffers
    #
    #(memcpy-shm-to-all)
    #
    #Or wait until socket handling?
    
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
    (gl~blend-func gl~SRC-ALPHA gl~ONE-MINUS-SRC-ALPHA)

    #Get some function pointers
    (setq gl~*glBufferStorage (glfw~get-proc-address "glBufferStorage"))

    T)


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


(de init-buffer-objects ()

    #{
    * Some buffers have a different bind layout per shader stage
    * Texture requires setting fmt after and other ops
    * Set initial data for buffers element and draw-indirect
    * glMapNamedBuffer is unavailable so to persistently map the buffer, it needs to be bound...
    * to bind a buffer, requires an appropriate program
    * What is the behavior when binding a buffer with no program bound?
    * Doesn't matter here...
    }#
    
    (for Params *params-buffer
	 
	 (with Params
	       
	       (let (Buffer (new '(gl~+Buffer-Object)
				   (: size-b)
				   (: target)
				   (if (> (: index-vs) -1)
				       (: index-vs)
				       (: index-cs))
				   T))
		 
		 #Use symbol properties
		 (put '*buffer-objects (: name) Buffer)
		 
		 #Poss more idiomatic way to do this?
		 (when (= (: target) gl~TEXTURE-BUFFER)
		   
		   #texturei max - GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
		   #already active?
		   (gl~active-texture gl~TEXTURE0)
		   
		   (gl~bind-b> Buffer)
		   
		   (gl~tex-buffer gl~TEXTURE-BUFFER
				  gl~RGBA8
				  (: buffer)))))))


(de init-tex-glyphs (Ptr)
    #{
    * To load faster, do bulk memcpy or convert to bytes
    * Possible Configurations:
      * all ppm, all metrics
      * per ppm+metrics
      * per ppm, per metrics (current)
      * Need separate ppm since data is copied to shm?
      * Or list of lists with each list starting with the name
   }#

   (let Offset 0
	(for Code 255
	     (let (Path-glyph (pack PATH-GLYPHS "/tex/" Code "-data.l"))
	       #(println "Reading " Path-glyph " ...")
	       (any (in Path-glyph (till NIL T)))
	       (struct (+ Ptr Offset) 'N
		       (0 . 4)
		       (2 . 4)
		       (1 . 4)
		       (0 . 4)
		       (3 . 4)
		       (2 . 4))
	       (inc 'Offset)))))


(de init-progs ()
    (setq *prog-rast (gl~init-prog-rast *paths-rast-vert-shader
					*paths-rast-frag-shader))
    (setq *prog-comp (gl~init-prog-comp *paths-comp-shader)))


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
