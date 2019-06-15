(symbols 'model 'pico)
#Create and change to namespace model

# Set math precision to 6 decimals
(scl 6)

# Lisp libraries
(load "/home/user/quicklisp/local-projects/protoform/src/model/mathc.l")
(load "/home/user/quicklisp/local-projects/protoform/src/model/projview.l")

# C libraries
(setq PATH-MATHC "/home/user/quicklisp/local-projects/protoform/ext/mathc/libmathc.so")

(class +DAG)

(de fmt-model (ctx
	       str)
    (println "MODEL" *Pid ctx str))


#Use sockets, nanomsg later?

#Refactor to launch independent subprocesses from top-level script  
#(setq *proc-libinput   'poll-fd-li
#      *proc-controller 'run-controller)
#async/io/execute-mb-tasks -> process
#socket/serve-socket -> model responsiblity
#later...wayland socket -> process/here - process wayland while waiting for drawing?
#  clients wait on msg from server when to draw

(de init-model ()
    
    #Set global parameters
    (setq *width    (*/ 1.0 2560.0 2.0)
	  *height   1080.0
	  *inst-max (** 2 19))
    
    #Setup projview
    (with (setq *projview (new '(+Projview)))
	  (=: width *width)
	  (=: height *height)
	  (=: type-proj 'orthographic)
	  (=: fov (native PATH-MATHC "to_radians" -1.0 (cons 45 -1))))
        
    (update-mat-proj> *projview)

    (print-mat4 (get *projview 'mat-proj))
    (print-mat4-2 (get *projview 'mat-proj))
    
    (update-mat-view> *projview)
    
    #(print-mat4 (get *projview 'mat-view))
    
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
