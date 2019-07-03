#{
DATAFLOW:
Servers: Input, Model, Render
Client: Ctrl

I/O       CPU         I/O
Input --> Ctrl 1 <--> Model, Render
      --> Ctrl 2 <-->
      --> Ctrl 3 <-->
      --> Ctrl N <-->

Memcpy Trail
* Ctrl/Serialize, Ctrl/Socket -> Render/GL

* Input epolls for events and forwards them
  * Input not just physically attached devices/peripherals
  * Poss network I/O

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

* Render epolls for memcpy
  * Pushes frame time to Ctrls -> Triggers anim frame generation
  * After pushing frame, Render can either wait or continue

* Supports at least quad-core...seems like a waste to have input/model/render on
own proc
  * Poss assign to same process at the expense of latency to increase bandwith
}#

(de main ()

    #2**19

    (let (Width  (format (/ 1920 2))
          Height (format (/ 1080 2))
          Verts  (format (** 2 4)))

      (when T
        # Takes the longest to start so run first
        (unless (fork)
          (exec "pil" "render/render.l" "-render~main" Width Height Verts)))

      (when T
        (unless (fork)
          (exec "pil" "model/model.l" "-model~main" Width Height Verts)))

      (when T
        (unless (fork)
          (exec "pil" "input/input.l" "-input~main" Width Height Verts)))

      (wait 3000)

      (when T
        # These processes are dedicated a CPU each
        (for I 1
          (unless (fork)
            (exec "pil" "ctrl/ctrl.l" "-ctrl~main" Width Height Verts))))

      T)

    #(wait 1000000)
    (println "Forked processes"))
