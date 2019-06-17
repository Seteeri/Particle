#Create and change to namespace model
(symbols 'model 'pico)

# Set math precision to 6 decimals
(scl 6)

# C library paths
(setq PATH-MATHC "/home/user/quicklisp/local-projects/protoform/ext/mathc/libmathc.so")

# Lisp libraries
(load "/home/user/quicklisp/local-projects/protoform/src/model/mathc.l")
(load "/home/user/quicklisp/local-projects/protoform/src/model/projview.l")
(load "/home/user/quicklisp/local-projects/protoform/src/model/dag.l")
(load "/home/user/quicklisp/local-projects/protoform/src/model/metrics.l")
(load "/home/user/quicklisp/local-projects/protoform/src/model/os.l")

(de fmt-model (ctx
	       str)
    (println "MODEL" *Pid ctx str))

(de init-model ()
    
    #Set global parameters
    #Make dims integers
    (setq *width      (*/ 1.0 2560.0 2.0)
	  *height     1080.0
	  *inst-max   (** 2 19)
	  *scale-glyph 5.8239365
	  *adv-glyph   9.375
	  *path-glyphs "/home/user/quicklisp/local-projects/protoform/src/glyphs/metrics/")

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
    (setq *metrics (load-metrics))
    
    #Require native calls
    #GC before setting these so static data is before shm and any new data is after
    (setq *shm-projview       NIL) #(init-shm-projview)
    (setq *shm-nodes          NIL) #(init-shm-nodes)
    (setq *shm-atomic-counter NIL) #(init-shm-atomic-counter)
    (setq *shm-vertices       NIL) #(init-shm-vertices)
    (setq *shm-element        NIL) #(init-shm-element)
    (setq *shm-draw-indirect  NIL) #(init-shm-draw-indirect)
    (setq *shm-texture-glyphs NIL) #(init-shm-texture-glyphs)

    (println "Bye!")
    (bye)
    
    #Use nng
    (setq *queue-tasks-sync  NIL  #(sb-concurrency:make-queue) -> list
	  *queue-tasks-async NIL  #(sb-concurrency:make-queue) -> list
	  *mb-async          NIL  #(sb-concurrency:make-mailbox) -> list
	  *mb-io             NIL  #(sb-concurrency:make-mailbox) -> list
	  *tasks-inactive    NIL  #(make-hash-table :size 64) -> list
	  *tasks-active      NIL  #(make-hash-table :size 64) -> list
	  *ht-timing-fn      NIL) #(make-hash-table :size 64) -> list
    
    #Use C sockets
    (setq *bs-ptr NIL) #(foreign-alloc :unsigned-char :count 212992)
    (setq *path-socket-view "/tmp/protoform-view.socket")
    (setq *sock-render NIL) #(init-sock-client *path-socket-view :block)
    #(init-conn-rpc-view)

    #Create DAG library 
    #Create graphs = namespaces
    #Create namespace for pico (generate nodes for builtin symbols? -> follow picolisp tradition so yes since they are the same)
    (setq *dag-pico (new '(+DAG)))

    #Create protoform namespace and symbols
    (setq *np-pico NIL) #(init-node-ptr-shm *dag-pico (0 0 0))

    #Update
    #(send-node *np-pico* nil)

    NIL)

#{
(de init-conn-rpc-view ()
  ;; Batch these  
  (send-setup-render)
  (loop
     :for (sym params) :on *params-shm* :by #'cddr
     :do (send-memcpy-shm-to-cache (second params)
				   (symbol-value sym)))
  (send-draw t)
  (send-serving nil))
}#

(symbols 'pico)
(de main ()
    (model~init-model)
    (println 'Exiting...)
    (bye))

#Launch from script etc

#...Libinput proc
#.....Read events, send to controller
#...Controller proc
#.....Recv events, pass to model
#.....Events can be handled immediately or delayed so Ctrl+Alt+Del not blocked e.g.

#...I/O proc
#.....Load/cache data in shm until model requests it

#...Model/Wayland proc
#.....Respond to client events
#.....Respond to view events - or is view just another client?
#.......Server will share shm with each client and view
#.......View will copy into GL buffers
#.....Coroutines useful for spreading work across frames
#.......Could also do manually as we have
