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
    
    #{
    Pass:
    (def '*sca-vert 0.008)
    (def '*sca-glyph 5.8239365) # MSDF/tex parameter
    (def '*adv-glyph 9.375)       # MSDF/tex parameter
    }#

    (let (Width  (format (/ 2560 2))
          Height (format (/ 1080 2))
          Verts  (format (** 2 12)))

      (when T
        # Takes the longest to start so run first
        (unless (setq *proc-render (fork))
          (exec "pil" "src/render/render.l" "-render~main" Width Height Verts)))

      (when T
        (unless (setq *proc-model (fork))
          (exec "pil" "src/model/model.l" "-model~main" Width Height Verts)))

      (when T
        (unless (setq *proc-input (fork))
          (exec "pil" "src/input/input.l" "-input~main" Width Height Verts)))

      (when T
        (unless (setq *proc-ctrl (fork))
          (exec "pil" "src/ctrl/ctrl.l" "-ctrl~main" Width Height Verts)))

      (when T
          (unless (setq *proc-work-1 (fork))
            (exec "pil" "src/ctrl/worker/worker.l" "-worker~main" Width Height Verts 1)))

      # Use taskset to assign processes here

      (prinl "Render: " *proc-render)
      (prinl "Model: " *proc-model)
      (prinl "Input: " *proc-input)
      (prinl "Ctrl: " *proc-ctrl)
      (prinl "Worker: " *proc-work-1)
                  
      (pipe (call "taskset" "-pc" "0" (format *proc-work-1)) (read))
      #(prinl (pipe (call "taskset" "-pc" "1" (format *proc-work-2)) (read)))
      
      (pipe (call "taskset" "-pc" "1" (format *proc-model)) (read))
      
      (pipe (call "taskset" "-pc" "2" (format *proc-ctrl))  (read))
      (pipe (call "taskset" "-pc" "2" (format *proc-input)) (read))

      # Poss push all of CPU 2 to 3 with render?
      (pipe (call "taskset" "-pc" "3" (format *proc-render)) (read))))