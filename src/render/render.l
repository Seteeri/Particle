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


# (scl 6)

(load (pack (pwd) "/src/misc.l"))
(mapc 'ld-cwd
      (list "/src/glm.l"
            "/src/c/posix/mman.l"
            "/src/c/posix/string.l"
            "/src/c/socket.l"
            "/src/c/gles/gles.l"
            "/src/c/glfw.l"
            "/src/c/epoll.l"
            "/src/ipc/ipc.l"
            "/src/prot/conn.l"
            "/src/prot/send.l"
            "/src/prot/recv.l"
            "/src/prot/obj.l"
            "/src/worker/vertex.l"
            "/src/render/params-buffer.l"
            "/src/render/raster.l"
            "/src/render/conn.l"))
            #"/src/render/compute.l"

(def 'NAME-PID "RENDER")

#Shader paths
(defcwdpath '*path-structs-shader "/glsl/structs.glsl")
(def '*paths-rast-vert-shader
      (list *path-structs-shader
            (pack *cwd "/glsl/node.vs.glsl")))
(def '*paths-rast-frag-shader
      (list *path-structs-shader
            (pack *cwd "/glsl/filter-bilinear.fs.glsl")
            (pack *cwd "/glsl/msdf.fs.glsl")))
(def '*paths-comp-shader
      (list *path-structs-shader
            (pack *cwd "/glsl/node.cs.glsl")))
(defcwdpath '*path-prog-rast "/bin/prog-rast.bin")

(def '*window)
(def '*buffer-objects)
(def '*prog-rast)
(def '*prog-comp)
(def '*vao)
(def '*fence)
(def 'TEST-GLYPH T)

(def 'CLEAR-MASK (| gl~COLOR-BUFFER-BIT gl~DEPTH-BUFFER-BIT))


(de init-window ()
  (glfw~init-2)

  #OpenGL ES only GL binding API that does not depend on GLX
  #Migrate to Vulkan later...
  (glfw~window-hint glfw~CONTEXT-VERSION-MAJOR 3)
  (glfw~window-hint glfw~CONTEXT-VERSION-MINOR 2)
  (glfw~window-hint glfw~CLIENT-API            glfw~OPENGL-ES-API)
  (glfw~window-hint glfw~CONTEXT-CREATION-API  glfw~NATIVE-CONTEXT-API)

  (let Title (native "@" "malloc" 'N 16)
    (struct Title 'N (cons "Particle" 16))
    (setq *window (glfw~create-window *width *height Title 0 0))
    (when (=0 *window) (quit "(= *window 0)"))
    (native "@" "free" NIL Title))

  (glfw~make-context-current *window)

  (glfw~set-framebuffer-size-callback *window (lisp 'framebuffer-callback serv-framebuffer-size))
  (glfw~set-key-callback *window (lisp 'key-callback serv-key))                                      

  # Crash
  #(glfw~set-window-focus-callback *window (lisp 'focus-callback serv-focus))
  #(glfw~set-window-size-callback *window (lisp 'size-callback serv-window-size))
  
  T)

  
(de serv-framebuffer-size (Window Width Height)
  # Must update projmatrix also

  # Add interface/refactor IPC
  (for Cli (get *ipc 'ipc~clients)
    (let (Sock (car Cli)
          ID (get (cdr Cli) 1))
      (when (= ID "CTRL")
        (send-msg *ipc
                  Sock
                  (pack "(fbsize " Width " " Height ")")))))

  (gl~viewport 0 0 Width Height)
  
  (setq *width Width
        *height Height))

        
(de serv-key (Window Key Scancode Action Mods)
  (when (and (= Key glfw~KEY-ESCAPE)
             (= Action glfw~PRESS))
    # Send also to ctrl
    (for Cli (get *ipc 'ipc~clients)
      (let (Sock (car Cli)
            ID (get (cdr Cli) 1))
        (when (= ID "CTRL")
          (send-msg *ipc
                    Sock
                    (pack "(bye)")))))
 
    # Cleanup GL?
    (glfw~destroy-window *window)
    (glfw~terminate)
    (ipc~end> *ipc)
    (println "Render says bye!")
    (bye)
                       
    (glfw~set-window-should-close Window glfw~TRUE)))

    
(de serv-window-size (Window Width Height))
(de serv-focus (Window Focused))
    

(de init-gl ()
  (gl~viewport 0 0 *width *height)

  (gl~enable gl~CULL-FACE)
  (gl~enable gl~DEPTH-TEST)
  (gl~enable gl~BLEND)
  
  (gl~blend-equation-separate gl~FUNC-ADD gl~FUNC-ADD)
  (gl~blend-func-separate gl~ONE gl~ONE-MINUS-SRC-ALPHA gl~ONE gl~ONE-MINUS-SRC-ALPHA)
  
  #(gl~blend-func gl~SRC-ALPHA gl~ONE-MINUS-SRC-ALPHA)
  
  # Get some function pointers
  (setq gl~*glBufferStorage (glfw~get-proc-address "glBufferStorage")))


(de init-render ()
  #{
  - Model can modify these during runtime through wire
  - Model will send (memcpy dest size offset)
  - Render will look up symbol/Mmap, get ptr, memcpy

  - Should not copy all data every frame - waste
  - Try single buffer + persistent mapping + coherent
  - Disable draw flag for node, before copying data, then enable after
  }#
  
  #Init context
  (init-window)
  
  (init-gl)

  (init-rast)

  #Don't need yet...
  #To disable drawing an object, use 0.0 (float) that is multiplied in vertex shader
  #Change flags with a vec4
  #(init-comp)
  
  (setq *ipc (new '(ipc~+IPC)
                    *uds-rend T recv-conn acpt-cli))  
  
  T)  

  
(de run-render ()
  #(gl~use-program *prog-rast)
  #(gl~bind-vertex-array *vao)

  (prinl-info "run-render" "Starting draw loop...")

  # Draws only on event?
  # With Wayland, clients will send damage events...
  (when NIL
    (test-projview)
    (test-vert))

  (gc 0)
    
  (while (=0 (glfw~window-should-close *window))

    (when *fence
      (gl~client-wait *fence)
      (gl~delete-sync *fence))

    #{
      0.16862745098
      0.211764705882
      0.0
    }#

    (gl~clear-color 0.0
                    0.0 
                    0.0 
                    0.0)
    (gl~clear CLEAR-MASK)

    (gl~draw-elements-indirect gl~TRIANGLES
                               gl~UNSIGNED-INT
                               0)

    (setq *fence (gl~fence-sync gl~SYNC-GPU-COMMANDS-COMPLETE 0))

    (glfw~swap-buffers *window)

    # Start next frame...

    (glfw~poll-events)

    # Broadcast frame time
    # Must handle if send fails, i.e. clients slow - in this case, ignore
    #
    # Poss only send if clients are ready
    #(send-msg-clients *ipc (pack "(frametime " (format (usec)) ")"))
    
    # Refactor this so it blocks until event
    (ipc~poll> *ipc 0)
    
    # Avg 7 ms max to GC
    #(bench (gc))
    
    #(println 'render "loop...")
    
    T)

  (glfw~destroy-window *window)
  (glfw~terminate))

  
(de recv-conn (Sock)
  (if (flush-recv *ipc Sock)
      (parse-msgs *ipc Sock proc-msg)
      (disc-cli *ipc Sock)))

(de main ()
  (bind-args)
  (init-render)
  (run-render)
  (println 'Exiting...)
  (bye))


########################################################################


(def '*data-node (list 143 7 180 62 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 119 42 178 62 0 0 0 0 0 0 0 0 0
                       0 0 0 0 0 0 0 111 18 3 60 0 0 0 0 152 242 44 61 86 98 183 61 0 0 0 0 0 0 128
                       63 182 181 53 63 138 137 9 63 0 0 0 0 0 0 128 63 182 181 53 63 138 137 9 63 0
                       0 0 0 0 0 128 63 182 181 53 63 138 137 9 63 0 0 0 0 0 0 128 63 182 181 53 63
                       138 137 9 63 0 0 0 0 0 0 128 63 117 154 58 63 40 255 57 63 0 0 0 0 0 0 0 0
                       117 154 58 63 176 1 140 62 0 0 0 0 0 0 0 0 20 203 138 62 176 1 140 62 0 0 0 0
                       0 0 0 0 20 203 138 62 40 255 57 63 0 0 0 0 0 0 0 0 0 196 5 0 96 0 0 0 96 0 0
                       0 1 0 0 0))


(def '*data-proj (list 171 170 42 61 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                       136 69 74 61 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                       32 64 128 187 0 0 0 0 0 0 0 128 0 0 0 128 64 128 128 191 0 0 128 63))


(def '*data-view (list 0 0 128 63 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                       128 63 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                       128 63 0 0 0 0 0 0 48 193 0 0 0 65 0 0 32 193 0 0 128 63))


(de test-projview ()
    #Write projview
    (let (I 0
            Ptr (get (get '*buffer-objects 'projview) 'gl~ptr))
      (for B *data-proj
           (struct (+ Ptr I) 'N (cons B 1))
           (inc 'I)))
    (let (I 64
            Ptr (get (get '*buffer-objects 'projview) 'gl~ptr))
      (for B *data-view
           (struct (+ Ptr I) 'N (cons B 1))
           (inc 'I))))

(de test-vert ()
    #Write test node
    (let (I 0
            Ptr (get (get '*buffer-objects 'nodes) 'gl~ptr))
      (for B *data-node
           (struct (+ Ptr I) 'N (cons B 1))
           (inc 'I))))
