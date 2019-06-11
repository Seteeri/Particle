(de main () (bye))

#Create and change to namespace model
(symbols 'model 'pico)

(load "@projview/projview.l")

(class +Projview)
(class +DAG)

(de fmt-model (ctx
	       str)
    (println "MODEL" *Pid ctx str))

(de init-model ()
    
  #Refactor to launch independent subprocesses from top-level script  
  (setq *proc-libinput   NIL  #poll-fd-li
	*proc-controller NIL  #run-controller
	*proc-async      NIL  #execute-mb-tasks *mb-async
	*proc-io         NIL  #execute-mb-tasks *mb-io
	*proc-socket     NIL) #serve-socket

  (setq *width    (/ 2560 2)
	*height   1080
	*inst-max (** 2 19)) #524288

  (setq *projview (new '(+Projview)
		       *width
		       *height
		       'orthographic)))
  
  (setq *controller NIL) #(init-controller *queue-tasks-sync #'translate-node-rel)
  
  (setq *metrics NIL) #(init-metrics (merge-pathnames #P"glyphs-msdf/" (asdf:system-source-directory :protoform))  +scale-msdf+)
  
  (setq *queue-tasks-sync  NIL  #(sb-concurrency:make-queue)
	*queue-tasks-async NIL  #(sb-concurrency:make-queue)
	*mb-async          NIL  #(sb-concurrency:make-mailbox)
	*mb-io             NIL  #(sb-concurrency:make-mailbox)
	*tasks-inactive    NIL  #(make-hash-table :size 64)
	*tasks-active      NIL  #(make-hash-table :size 64)
	*ht-timing-fn      NIL) #(make-hash-table :size 64)

  (setq *shm-projview       NIL) #(init-shm-projview)
  (setq *shm-nodes          NIL) #(init-shm-nodes)
  (setq *shm-atomic-counter NIL) #(init-shm-atomic-counter)
  (setq *shm-vertices       NIL) #(init-shm-vertices)
  (setq *shm-element        NIL) #(init-shm-element)
  (setq *shm-draw-indirect  NIL) #(init-shm-draw-indirect)
  (setq *shm-texture-glyphs NIL) #(init-shm-texture-glyphs)

  (setq *bs-ptr NIL) #(foreign-alloc :unsigned-char :count 212992)
  (setq *path-socket-view "/tmp/protoform-view.socket")
  (setq *sock-render NIL) #(init-sock-client *path-socket-view :block)
  #(init-conn-rpc-view)
  
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
