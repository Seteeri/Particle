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

## Principles

Particle maximizes the following principles:

* Expressiveness
  * The program should provide primitives that allow the user to express their thoughts in the most concise, succinct, and accurate way possible.
* Flexiblity
  * The program should not stand in the way of the user; it must balance structure with freedom.
* Minimalism
  * Make the whole system understandable by breaking information into digestible pieces.

These principles are built upon the underlying programming language, [PicoLisp](https://picolisp.com):

* Simple
  * The internal data structure should be as simple as possible. Only one single data structure is used to build all higher level constructs.
* Unlimited
  * There are no limits imposed upon the language due to limitations of the virtual machine architecture. That is, there is no upper bound in symbol name length, number digit counts, stack depth, or data structure and buffer sizes, except for the total memory size of the host machine.
* Dynamic
  * Behavior should be as dynamic as possible ("run"-time vs. "compile"-time). All decisions are delayed until runtime where possible. This involves matters like memory management, dynamic symbol binding, and late method binding.
* Practical
  * PicoLisp is not just a toy of theoretical value. It is in use since 1988 in actual application development, research and production.

...to create a consistent *discoverable* system with harmonious defaults.


Data Structures
===============
