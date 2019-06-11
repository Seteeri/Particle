#Create and change to namespace model
(symbols 'model 'pico)

(load "/home/user/quicklisp/local-projects/protoform/src/model/projview/projview.l")

(class +DAG)

(de fmt-model (ctx
	       str)
    (println "MODEL" *Pid ctx str))

(de init-model ()

  #Do last
  #Refactor to launch independent subprocesses from top-level script  
  (setq *proc-libinput   'poll-fd-li
	*proc-controller 'run-controller
	*proc-async      '(execute-mb-tasks *mb-async)
	*proc-io         '(execute-mb-tasks *mb-io)
	*proc-socket     'serve-socket)
	
  (setq *width    (/ 2560 2)
	*height   1080
	*inst-max (** 2 19))

  (with (setq *projview (new '(+Projview)))
	(=: width *width)
	(=: height *height)
	(=: type-proj 'orthographic))
  
  (setq *controller NIL) #(init-controller *queue-tasks-sync #'translate-node-rel))
  
  (setq *metrics NIL) #(init-metrics (merge-pathnames #P"glyphs-msdf/" (asdf:system-source-directory :protoform))  +scale-msdf+)
  
  (setq *queue-tasks-sync  NIL  #(sb-concurrency:make-queue) -> list
	*queue-tasks-async NIL  #(sb-concurrency:make-queue) -> list
	*mb-async          NIL  #(sb-concurrency:make-mailbox) -> list
	*mb-io             NIL  #(sb-concurrency:make-mailbox) -> list
	*tasks-inactive    NIL  #(make-hash-table :size 64) -> property list
	*tasks-active      NIL  #(make-hash-table :size 64) -> property list
	*ht-timing-fn      NIL) #(make-hash-table :size 64) -> property list

  #Require native calls
  (setq *shm-projview       NIL) #(init-shm-projview)
  (setq *shm-nodes          NIL) #(init-shm-nodes)
  (setq *shm-atomic-counter NIL) #(init-shm-atomic-counter)
  (setq *shm-vertices       NIL) #(init-shm-vertices)
  (setq *shm-element        NIL) #(init-shm-element)
  (setq *shm-draw-indirect  NIL) #(init-shm-draw-indirect)
  (setq *shm-texture-glyphs NIL) #(init-shm-texture-glyphs)

  #Require sockets
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

  (println "init-model done")

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
(de main () (model~init-model))
