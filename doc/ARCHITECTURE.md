Architecture/Infrastructure
===========================

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

Reddit Thread: "Is There anything Emacs CAN't do?"
https://www.reddit.com/r/emacs/comments/3v19uj/is_there_anything_emacs_cant_do

Data Structures
===============
