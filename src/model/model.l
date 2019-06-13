(symbols 'model 'pico)
#Create and change to namespace model

(load "/home/user/quicklisp/local-projects/protoform/src/aux/mathc.l")
(load "/home/user/quicklisp/local-projects/protoform/src/model/projview/projview.l")

(class +DAG)

# Set math precision to 4 decimals
(scl 4)

(de fmt-model (ctx
	       str)
    (println "MODEL" *Pid ctx str))

(de init-model ()

    #nng required for a bunch of these
    #use sockets for now; do later
    
    #Refactor to launch independent subprocesses from top-level script  
    (setq *proc-libinput   'poll-fd-li
	  *proc-controller 'run-controller)
    
    #async/io/execute-mb-tasks -> process
    #socket/serve-socket -> model responsiblity
    #later...wayland socket -> process/here - process wayland while waiting for drawing?
    #  clients wait on msg from server when to draw
    
    (setq *width    (*/ 2560 2)
	  *height   1080
	  *inst-max (** 2 19))

    (with (setq *projview (new '(+Projview)))
	  (=: width *width)
	  (=: height *height)
	  (=: type-proj 'orthographic))
    
    (let (mat (update-mat-proj> *projview)
	  mat-2 (struct mat '(-1.0 . 16)))
      (prinl)
      (println (cut 4 'mat-2))
      (println (cut 4 'mat-2))
      (println (cut 4 'mat-2))
      (println (cut 4 'mat-2)))
        
    #Simple port
    (setq *controller NIL) #(init-controller *queue-tasks-sync #'translate-node-rel)

    #Simple port
    (setq *metrics NIL) #(init-metrics (merge-pathnames #P"glyphs-msdf/" (asdf:system-source-directory :protoform))  +scale-msdf+)

    #Use nng
    (setq *queue-tasks-sync  NIL  #(sb-concurrency:make-queue) -> list
	  *queue-tasks-async NIL  #(sb-concurrency:make-queue) -> list
	  *mb-async          NIL  #(sb-concurrency:make-mailbox) -> list
	  *mb-io             NIL  #(sb-concurrency:make-mailbox) -> list
	  *tasks-inactive    NIL  #(make-hash-table :size 64) -> list
	  *tasks-active      NIL  #(make-hash-table :size 64) -> list
	  *ht-timing-fn      NIL) #(make-hash-table :size 64) -> list

    #Require native calls
    (setq *shm-projview       NIL) #(init-shm-projview)
    (setq *shm-nodes          NIL) #(init-shm-nodes)
    (setq *shm-atomic-counter NIL) #(init-shm-atomic-counter)
    (setq *shm-vertices       NIL) #(init-shm-vertices)
    (setq *shm-element        NIL) #(init-shm-element)
    (setq *shm-draw-indirect  NIL) #(init-shm-draw-indirect)
    (setq *shm-texture-glyphs NIL) #(init-shm-texture-glyphs)

    #Use nng
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
