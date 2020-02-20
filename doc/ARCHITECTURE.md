Architecture
============

* Focused on Wayland and modern OpenGL ES 3.2+ (Vulkan)
* PicoLisp due to simplicity, expressiveness and consistency
* Process-based system
  * Components: Input/Controller, Workers, Model, Render
  * IPC through message passing
  * Multiple processes provide fault-tolerance and scalability
* Rendering engine = Particle system
  * Compute shaders
  * AZDO Techniques
    * Persistent mapping
    * Instanced drawing
    * Indirect drawing
  * Fully programmable vertex pulling
    * Texture buffers/UBOs/SSBOs -> gl_VertexID+gl_InstanceID
* R-tree for spatial indexing

Reddit Thread: "Is There anything Emacs CAN't do?"
https://www.reddit.com/r/emacs/comments/3v19uj/is_there_anything_emacs_cant_do

* Threading
  * PicoLisp does not have threads; Particle uses processes and IPC with shared
  nothing concurrency to achieve parallelism.
  * Processes also provide the following benefits:
    * Fault-tolerance - a process crashing does not take the system down with it compared to a thread
    * Data-redundancy - data is distributed similar to the relationship between a CPU cache and main memory
    * Opporunities to increase GC performance - multiple processes = parallel and incremental GC
      * Assuming data is evenly distributed...
* General Responsiveness and Performance
  * Part of Emacs latency is Emacs Lisp but also its single-threaded architecture
  * PicoLisp is much faster than Emacs Lisp
* More Flexible Keybindings
* Advanced Graphical Capabilities
  * Particle is based on OpenGL and provides unfiltered access to the drawing API
  and OpenGL; of course, one of the fundamental design principles is to avoid
  overlapping windows.
  * This allows for effects like powermode and highlight-tail.
* Web Browser
  * This is planned.
* Be an OS
  * A goal of Particle is to create a Lisp userspace, and possibly one day,
  integrate with PilOS/PilMCU. For now, replacing the DE/WM is the first step by
  integrating Wayland/XWayland.
