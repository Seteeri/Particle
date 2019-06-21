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


# C library paths
(def 'PATH-MATHC "/home/user/quicklisp/local-projects/protoform/ext/mathc/libmathc.so")
(def 'PATH-GLYPHS "/home/user/quicklisp/local-projects/protoform/src/glyphs")
# UDS paths
(def 'PATH-UDS-RENDER "/tmp/protoform-render.socket")


# Set math precision to 6 decimals
(scl 6)


# Wrapper libraries
(load "/home/user/quicklisp/local-projects/protoform/src/c/mathc.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/os.l")
(load "/home/user/quicklisp/local-projects/protoform/src/c/sock.l")
# Lisp libraries
(load "/home/user/quicklisp/local-projects/protoform/src/model/projview.l")
(load "/home/user/quicklisp/local-projects/protoform/src/model/dag.l")
(load "/home/user/quicklisp/local-projects/protoform/src/model/metrics.l")
(load "/home/user/quicklisp/local-projects/protoform/src/model/render.l")
#(load "/home/user/quicklisp/local-projects/protoform/src/model/mmap.l")


(de print-model (Ctx
		 Str)
    (println "MODEL" *Pid Ctx Ctr))


(de init-nodes ()
    T)


(de init-model ()

    #(*/ 1.0 2560.0 2.0)
    #1080.0
    #(** 2 19)
    
    #Get number of nodes and window size from args instead of file
    (let (Argv (argv))
      (setq *width      (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "width invalid")))
	    *height     (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "height invalid")))
	    *inst-max   (let (A (format (cut 1 'Argv))) (if (num? A) A (quit "nodes invalid")))
	    
	    *scale-glyph 5.8239365
	    *adv-glyph   9.375))
          
    #Setup projview
    (with (setq *projview (new '(+Projview)))
	  (=: width *width)
	  (=: height *height)
	  (=: type-proj 'orthographic)
	  (=: fov (to-radians 45)))
    #Simplify these - single function?
    (update-mat-proj> *projview)
    (update-mat-view> *projview)

    #Load here, copy to shm, msg view, view copies to OpenGL buffers
    (when NIL
      (setq *metrics (load-metrics)))
    
    #GC before setting these so static data is before shm and any new data is after
    #Initialize symbols/objects instead
    (init-nodes)
    #{
    ;; Create DAG library 
    ;; Create graphs = namespaces
    ;; Create namespace for pico
    ;; (generate nodes for builtin symbols? -> follow picolisp tradition so yes since they are the same)
    }#
    (setq *dag-pico (new '(+DAG)))

    #Send node updates also
    (init-render)
    
    NIL)

(de run-model ()
    #After connecting, start processing messages
    #Render will send frame start and wait for frame end
    #Up to model to perform time management
    
    #{
    (loop
       (let (Msg (recv-msg))
	 (when Msg
	   (if (lst? (first message))
	       (for n message
		    (apply (symbol-function (find-symbol (string (first n)) :protoform.model))
			   (cdr n)))
	       (apply (symbol-function (find-symbol (string (first message)) :protoform.model))
		      (cdr message))))))
    }#
    
    T)

(symbols 'pico)
(de main ()
    (model~init-model)
    (model~run-model)
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

#{
#Use nng
(setq *queue-tasks-sync  NIL  #(sb-concurrency:make-queue) -> list
      *queue-tasks-async NIL  #(sb-concurrency:make-queue) -> list
      *mb-async          NIL  #(sb-concurrency:make-mailbox) -> list
      *mb-io             NIL  #(sb-concurrency:make-mailbox) -> list
      *tasks-inactive    NIL  #(make-hash-table :size 64) -> list
      *tasks-active      NIL  #(make-hash-table :size 64) -> list
      *ht-timing-fn      NIL) #(make-hash-table :size 64) -> list
}#  
