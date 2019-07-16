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


(symbols 'render 'pico)

# Set math precision to 6 decimals
(scl 6)

# C library paths
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


# Wrapper libraries
(load "/home/user/quicklisp/local-projects/protoform/src/c/mathc.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/posix/mman.l")
#(load "/home/user/quicklisp/local-projects/protoform/src/c/posix/stat.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/posix/string.l")
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
(load "/home/user/quicklisp/local-projects/protoform/src/render/conn.l")
(load "/home/user/quicklisp/local-projects/protoform/src/prot/prot.l")


(def 'CLEAR-MASK (| gl~COLOR-BUFFER-BIT gl~DEPTH-BUFFER-BIT))


(def '*window)
(def '*buffer-objects)
(def '*prog-rast)
(def '*prog-comp)
(def '*vao)
(def '*fence)

(def 'TEST-GLYPH T)


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
    (setq *width     (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "width invalid")))
          *height    (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "height invalid")))
          *verts-max (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "verts invalid")))))

  (init-ipc)

  #Init context
  (init-window)
  (init-gl)

  #Init OpenGL programs
  (init-rast)

  #Don't need yet...
  #To disable drawing an object, use 0.0 (float) that is multiplied in vertex shader
  #Change flags with a vec4
  #(init-comp)

  T)


(de init-window ()
  (glfw~init-2)

  #OpenGL ES only GL binding API that does not depend on GLX
  #Migrate to Vulkan later...
  (glfw~window-hint glfw~CONTEXT-VERSION-MAJOR 3)
  (glfw~window-hint glfw~CONTEXT-VERSION-MINOR 2)
  (glfw~window-hint glfw~CLIENT-API            glfw~OPENGL-ES-API)
  (glfw~window-hint glfw~CONTEXT-CREATION-API  glfw~NATIVE-CONTEXT-API)

  (let (Title (native "@" "malloc" 'N 16))
    (struct Title 'N (cons "Protoform" 16))
    (setq *window (glfw~create-window *width *height Title 0 0))
    (when (= *window 0)
      (quit "(= *window 0)"))
    (native "@" "free" NIL Title))

  (glfw~make-context-current *window)

  (glfw~set-window-size-callback *window
                                 (lisp 'window-callback
                                       '((Window Width Height)
                                         (gl~viewport 0 0 (struct Width 'I) (struct Height 'I)))))

  (glfw~set-key-callback *window
                         (lisp 'key-callback
                               '((Window Key Scancode Action Mods)
                                 (when (and (= (struct Key 'I)    glfw~KEY-ESCAPE)
                                            (= (struct Action 'I) glfw~PRESS))
                                   (glfw~set-window-should-close (struct Window 'N) glfw~TRUE))))))


(de init-gl ()
  (gl~viewport 0 0 *width *height)

  (gl~enable gl~CULL-FACE)
  (gl~enable gl~DEPTH-TEST)
  (when TEST-GLYPH
    (gl~enable gl~BLEND)
    (gl~blend-func gl~SRC-ALPHA gl~ONE-MINUS-SRC-ALPHA))

  #Get some function pointers
  (setq gl~*glBufferStorage (glfw~get-proc-address "glBufferStorage"))

  T)


(de init-ipc ()
  (setq *ipc (new '(ipc~+IPC)
                  PATH-UDS-RENDER 'ipc~block recv-conn accept-client
                  NIL)))


(de run-render ()
  #(gl~use-program *prog-rast)
  #(gl~bind-vertex-array *vao)

  (prinl (usec) " | RENDER | Starting draw loop...")

  # Draws only on event?
  # With Wayland, clients will send damage events...
  (when NIL
    #(test-projview)
    (test-vert))

  (while (= (glfw~window-should-close *window) 0)

    (when *fence
      (gl~client-wait *fence)
      (gl~delete-sync *fence))

    (gl~clear-color 0.0
                    0.16862745098
                    0.211764705882
                    0.0)
    (gl~clear-mask CLEAR-MASK)

    (gl~draw-elements-indirect gl~TRIANGLES
                               gl~UNSIGNED-INT
                               0)

    (setq *fence (gl~fence-sync gl~SYNC-GPU-COMMANDS-COMPLETE 0))

    (glfw~swap-buffers *window)

    # Start next frame...

    (glfw~poll-events)

    # Broadcast frame time
    # Must handle if send fails, i.e. clients slow - in this case, ignore

    (send-msg-clients *ipc (format (usec)))

    # Read all msgs in queue (up to 64) at this point
    # Track amount of memcpy; break after so many bytes

    (ipc~poll> *ipc 0)

    # Force gc
    #(gc)

    T)

  (glfw~destroy-window *window)
  (glfw~terminate))


(de recv-conn (Sock Events)
  #{
  Problem: When sending memcpy + data, data is read on memcpy msg event, however
  epoll will still generate read event which may result in no data available

  Solution: Ignore Msg=NIL or use edge triggering for epoll
  }#

  # Must flush socket - aka read up to recv len until empty

  (let (Msg  (recv-msg *ipc Sock Events)
        Data (any Msg)
        Name-Fn (get Data 1))

    (prinl (usec) " | RENDER  | recv-conn | " Msg)

    # For now, assume memcpy
    (cond ((= Name-Fn "mc")
           (handle-memcpy Sock Data))

          (T
           (prinl (usec) " | RENDER  | recv-conn | UNKNOWN REQUEST! " Msg)))))


#{
(dm flush-msgs> (Sock Buf Sz)

  #For Buf-Sz: pass T to recv full length, else pass length
  (let Bytes-Read (socket~recv Sock
                               (if (num? Buf) Buf (: buf-rd))
                               (if (num? Sz)  Sz  (: sz-buf-rd))
                               0)

     #(println "recv-sock" (struct (: buf-rd) '(B B B B B B B B)))

     # Handle -1
     (if (=0 Bytes-Read)

         # Poss check for (= (& Events epoll~ERR) epoll~ERR) and so on...
         # For now, assume error and disconnect
         (disconnect-client> This Sock)

         # Read msgs until len
         T)

     Bytes-Read))
}#


(de main ()
  (init-render)
  (run-render)
  (println 'Exiting...)
  (bye))