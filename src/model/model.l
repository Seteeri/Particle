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
(symbols 'model 'pico)


# Set math precision to 6 decimals
(scl 6)


# C library paths
(def 'PATH-MATHC "/home/user/quicklisp/local-projects/protoform/ext/mathc/libmathc.so")
(def 'PATH-GLYPHS "/home/user/quicklisp/local-projects/protoform/src/glyphs")
# UDS paths
(def 'PATH-UDS-MODEL "/tmp/protoform-model.socket")
(def 'PATH-UDS-RENDER "/tmp/protoform-render.socket")


# Wrapper libraries
(load "/home/user/quicklisp/local-projects/protoform/src/c/mathc.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/posix/mman.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/posix/stdio.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/posix/string.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/posix/unistd.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/epoll.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/socket.l")
# Lisp libraries
(load "/home/user/quicklisp/local-projects/protoform/src/model/projview.l")
(load "/home/user/quicklisp/local-projects/protoform/src/model/metrics.l")
(load "/home/user/quicklisp/local-projects/protoform/src/model/render.l")
#(load "/home/user/quicklisp/local-projects/protoform/src/model/mmap.l")
(load "/home/user/quicklisp/local-projects/protoform/src/model/graph/vertex.l")
(load "/home/user/quicklisp/local-projects/protoform/src/ipc/ipc.l")


(de init-verts ()
    (let Verts ()
	 (for I *verts-max
	      (let Vertex (new '(+Vertex) T T T T T)
		   #(prinl "Created vertex " I " "  Vertex)
		   (push 'Verts Vertex)))
	 Verts))

(de init-verts-default ()
    #{
    Traverse all internal/protected symbols
    - PicoLisp symbols = 300+?
    - Pointer = cons pair

    - Track transient symbols
    }#

    #{
    (let Vertex (get *verts 1)
	 (println Vertex)
	 (println (view (getl Vertex))))
    }#
    T)


(de init-model ()

    #(*/ 1.0 2560.0 2.0)
    #1080.0
    #(** 2 19)
    
    #Get number of nodes and window size from args instead of file
    (let (Argv (argv))
      (setq *width      (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "width invalid")))
	    *height     (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "height invalid")))
	    *verts-max   (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "verts invalid")))
	    
	    *scale-glyph 5.8239365
	    *adv-glyph   9.375))
          
    #Setup projview
    (setq *projview (new '(+Projview) *width *height 'orthographic))
    #Simplify these - single function?
    (update-mat-proj> *projview)
    (update-mat-view> *projview)

    #Debug
    #(show *projview)
    #(view (getl *projview))

    #Load here, copy to shm, msg view, view copies to OpenGL buffers
    (when NIL
      (setq *metrics (load-metrics)))

    #{
    GC beforehand since static data is at the beginning,
    and allocations afterwards will go right after
    Nodes won't be deleted so they are considered static data
    which improves locality
    
    Increase heap size to node size
    208 bytes per node roughly
    ->Do test run to determine size

    2**19 = 524288
    }#
    
    (gc (+ 104 2))
    (prinl "Heap resized to 106 MB for " *verts-max " nodes")

    #{
     - Graphs = namespaces?
     - Generate nodes for builtin symbols?
    }#    
    (setq *verts (init-verts))
    (init-verts-default)

    (init-ipc))


(de init-ipc ()
    (setq *ep-events (native "@" "malloc" 'N (* 12 2)) #1 event
	  *ep-fd     (epoll~create-size 1)
	  *buf-sz    (socket~read-rmem-default)
	  *buf-rd    (native "@" "malloc" 'N *buf-sz))

    (setq *ipc (new '(ipc~+IPC)
		    PATH-UDS-MODEL 'ipc~block
		    ""             'ipc~block))

    (epoll~ctl-epfd *ep-fd 
		    epoll~ADD
		    (get *ipc 'ipc~listener)
		    epoll~IN)
    
    (epoll~ctl-epfd *ep-fd 
		    epoll~ADD
		    (get *ipc 'ipc~connection)
		    epoll~IN))


(de run-model ()
    #After connecting, start processing messages
    #Render will send frame start and wait for frame end
    #Up to model to perform time management

    #(prinl "run-model | " "Going to sleep...")
    #(wait 1000000)
    
    (let (Events NIL
	  Fd     NIL)
      (loop

	 (prinl "Epolling...")
	 
	 (setq num-events (epoll~wait-epfd *ep-fd *ep-events 2 -1))
	 
	 (when (> num-events 0)

	   (let (Ep-events (struct *ep-events '(I I)))
	     (setq Events (get Ep-events 1))
	     (setq Fd     (get Ep-events 2))
	     
	     (prinl (usec) " FD: " Fd ", Events: " Events)

	     (cond 
	       ((= Fd (get *ipc 'ipc~listener))
		(ipc~handle-sock-listen Events Fd *ep-fd))
	       (T
		(ipc~handle-sock-conn Events Fd *ep-fd))))))))


(de main ()
    (init-model)
    (run-model)
    (println 'Exiting...)
    (bye))

#Launch from script etc

#...Libinput proc
#.....Read events, send to controller
#...Controller proc
#.....Recv events, pass to model
#.....Events can be handled immediately or delayed so Ctrl+Alt+Del not blocked e.g.

#...I/O proc for disk (network also?)
#.....Load/cache data in shm until model requests it

#...Model/Wayland proc
#.....Respond to client events
#.....Respond to view events - or is view just another client?
#.......Server will share shm with each client and view
#.......View will copy into GL buffers
#.....Coroutines useful for spreading work across frames
#.......Could also do manually as we have
