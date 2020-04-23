#{
DATAFLOW:

      Model <----+
        ^        |
        |        |
        |        V
Input/Ctrl --> Worker N
        |        ^
        |        |
        V        |
      Render <---+

Memcpy Trail
* Ctrl/Serialize, Ctrl/Socket -> Render/GL

* Input epolls for events and forwards them
  * Input not just physically attached devices/peripherals
  * Poss network I/O
  * Also connects to render and model?
    -> If ctrl goes down

* Ctrl
  * Applies functions to data
  * Pulls from Model/Update cache
  * Cache to reduce necessary copies and decrease latency
  * Pushes to Model & Render
  * HMM: Create boss and workers?
    * Or input is the boss?
    * Poss simply rotate through workers - find next available

* Model epolls for memcpy
  * Recvs data from Ctrl and updates memory
  * Broadcast updates
    -> Poss decentralize?
  * Poss use Redis or some other mechanism/library
  * Instead of pushing to model push to other nodes...P2P
    * All nodes would maintain entire copy of nodes?
    * Poss GC to clear unused nodes...or create custom mechanism to implement pruning

* Render epolls for memcpy
  * Pushes frame time to Ctrls -> Triggers anim frame generation
  * After pushing frame, Render can either wait or continue

* Supports at least quad-core...seems like a waste to have input/model/render on
own proc
  * Poss assign to same process at the expense of latency to increase bandwith
}#


(de fork-2 (Fn)
  (let Pid-1 (fork)
    (cond
      (Pid-1 (native "@" "waitpid" NIL Pid-1 0 0))
      ((not Pid-1) # Child Proc B
       (let Pid-2 (fork)
         (cond
           (Pid-2 (bye))
           ((not pid) # Child Proc C
            (Fn))
           (T (quit "fork=-1")))))
      (T (quit "fork=-1")))))


(de main ()
  
  (let Home (pack (pwd) "/src/")
        
      (fork-2 '(() (exec "pil" (pack Home "render/render.l") "-main" "+")))
      (fork-2 '(() (exec "pil" (pack Home "input/input.l")   "-main" "+")))
      (fork-2 '(() (exec "pil" (pack Home "ctrl/ctrl.l")     "-main" "+")))
      (fork-2 '(() (exec "pil" (pack Home "worker/worker.l") "-main" "+")))
      
      T)

  (bye))
  
#{
  (def '*proc-render)
  (def '*proc-model)
  (def '*proc-input)
  (def '*proc-ctrl)
  (def '*proc-work-1)
  (def '*proc-work-2)
  (def '*proc-work-3)
  (def '*proc-work-4)

  (pipe (call "taskset" "-pc" "0" (format *proc-work-1)) (read))
  #(prinl (pipe (call "taskset" "-pc" "1" (format *proc-work-2)) (read)))
  
  (pipe (call "taskset" "-pc" "1" (format *proc-model)) (read))
  
  (pipe (call "taskset" "-pc" "2" (format *proc-ctrl))  (read))
  (pipe (call "taskset" "-pc" "2" (format *proc-input)) (read))

  # Poss push all of CPU 2 to 3 with render?
  (pipe (call "taskset" "-pc" "3" (format *proc-render)) (read))))
}#
