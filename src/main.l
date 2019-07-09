#{
DATAFLOW:

      Model <---+
                |
                |
                V
Input/Ctrl -> Worker N
        ^       ^
        |       |
        V       |
      Render <--+

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


(def '*proc-render)
(def '*proc-model)
(def '*proc-input)
(def '*proc-ctrl)
(def '*proc-work-1)
(def '*proc-work-2)
(def '*proc-work-3)
(def '*proc-work-4)


(de main ()

    #2**19

    (let (Width  (format (/ 1920 2))
          Height (format (/ 1080 2))
          Verts  (format (** 2 4)))

      (when T
        # Takes the longest to start so run first
        (unless (setq *proc-render (fork))
          (exec "pil" "render/render.l" "-render~main" Width Height Verts)))

      (when T
        (unless (setq *proc-model (fork))
          (exec "pil" "model/model.l" "-model~main" Width Height Verts)))

      (when T
        (unless (setq *proc-input (fork))
          (exec "pil" "input/input.l" "-input~main" Width Height Verts)))

      (when T
        (unless (setq *proc-ctrl (fork))
          (exec "pil" "ctrl/ctrl.l" "-ctrl~main" Width Height Verts)))

      (when T
          (unless (setq *proc-work-1 (fork))
            (exec "pil" "ctrl/worker.l" "-worker~main" Width Height Verts 1))
          (unless (setq *proc-work-2 (fork))
            (exec "pil" "ctrl/worker.l" "-worker~main" Width Height Verts 2)))

      # Use taskset to assign processes here

      (println *proc-render *proc-model *proc-input *proc-ctrl *proc-work-1 *proc-work-2)
                  
      (prinl (pipe (call "taskset" "-pc" "0" (format *proc-work-1)) (read)))
      
      (prinl (pipe (call "taskset" "-pc" "1" (format *proc-work-2)) (read)))
      
      (prinl (pipe (call "taskset" "-pc" "2" (format *proc-model)) (read)))
      (prinl (pipe (call "taskset" "-pc" "2" (format *proc-ctrl)) (read)))
      (prinl (pipe (call "taskset" "-pc" "2" (format *proc-input)) (read)))
            
      (prinl (pipe (call "taskset" "-pc" "3" (format *proc-render)) (read)))
      
      T))
