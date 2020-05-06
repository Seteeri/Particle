![PARTICLE](https://github.com/Seteeri/Particle/blob/master/art/particle.png)

> The Grid. A digital frontier. I tried to picture clusters of
information as they moved through the computer. What did they look like?
 Ships? Motorcycles? Were the circuits like freeways? I kept dreaming of
  a world I thought I'd never see. And then one day . . .

---

Particle is a personal knowledge/information manager (aka PIM) implemented with a presentation-based lisp-structured UI.

The idea is to take the extensibility of Emacs combined with the dynamics and cohesion of past Lisp Machines, Oberon, Open Dylan, Intentional Programming and other models, and to evolve that consistent text interface to arbitrary objects, taking advantage of modern hardware such as GPUs and parallelism.

Plans are to initially support Linux and Android. Windows requires WSL or virtualization; Mac requires the latter. Once Pil21 is done (LLVM-based), a native solution can be provided. The Pinephone is also on the way ;)

The target audience consists of programmers, power users, information/knowledge workers.

## Demos

## Goals

* To get work done efficiently and effectively
* To remember everything, an extension of the human brain (Evernote/Stepan Pachikov)
* To structure a system in such a way that it can be described, explained, and understood as a whole (Oberon/Niklaus Wurth)
* To ameliorate the following sentiments: (Qix/Brad Beer)
  * "I'm tired of needing a new application for every different task."
  * "I'm tired of needing a new data format for every different task."
  * "I'm tired of not being able to connect and use my data in other applications."
  * "I'm tired of not being able to share my data with others."
  * "I'm tired of needing markup, native-code, JIT-Code, scripting languages, database languages, domain specific languages, etc..."
  * "I want to use the full power of the computer."
  * "I want the computer to meet me more than half way."

## UI

Ostensibly, it resembles an outliner, however, it integrates various computing concepts and UI designs from CLIs, shells, REPLs, notebooks, WMs/DEs, creative coding, mindmapping, wikis and note-taking programs into a single object-oriented (or symbolic, if you will) interface; *it is not a visual programming language*.

On MS and Apple systems, it serves as an outliner "app", however, with Linux and BSD systems it integrates the window manager, i.e. functions as the Wayland compositor. The ultimate goal of Particle is to create a Lispy userland, eventually replacing the init system and encompassing all layers above that.

*WIP*
* Local data first - no cloud dependency
* Distributable - synchronize across multiple devices
* Storage mechanisms
  * Plaintext (Lisp code)
  * Binary (Lisp data)
  * Database backend (Lisp data)
* Tag-based searching through trees
* Import/link any data
  * Binary files such as images, video, audio etc. are not imported into the db and left as separate files
* Export* to s-expr, XML, Orgmode, Markdown, HTML, PDF, ODT, SQL
* Orthographic view aka zooming interface
* Browser integration (aka web clipper)
* Wayland/Userspace* integration


Advanced:


* Fuzzy string matching
  * Tag suggestions
* Code-specific support
* Revision control
  * Tree diffs
* Encryption - text and block-based
* OCR
* Ink-pen input; drawn annotations
* Collaboration - separate related project


Interfaces:

* Touch/Mobile - power constraints and form factor need to be adressed
* AR/VR - I do have a design for AR...

## API

* PicoLisp VM
  * Purely cons-based
  * Threaded interpreter only
    * No compiler explicitly to maintain "formal equivalence of code and data"
  * Lisp-1 meaning single namespace for function and variable names
  * Dynamically scoped, dynamic/shallow symbol binding, late method binding
    * Transient symbols (includes strings) lexically scoped
  * Written in macro asm; No C code
  * ~3x faster than CPython
  * Built-in database using external symbols as first-class data type
    * memory based db file = emulation of single address space?
* Modern OpenGL ES 3.2, aka programmable pipeline
  * Vulkan planned
  * Uses glyph instancing aka "particle system"
    * Multi-channel signed distance fonts
      * Felzenszwalb/Huttenlocher distance transform*
      * Glyph hinting*
      * Subpixel antialiasing*
    * Compute shaders
    * AZDO techniques to minimize draw calls and reduce CPU<->GPU comms
      * Persistent mapping
      * Instanced drawing
      * Indirect drawing
      * Explicit synchronization with double/triple buffering (testing)
      * Programmable vertex pulling
        * Texture buffers/UBOs/SSBOs
        * gl_VertexID+gl_InstanceID
  * Transparency sorting currently avoided by preventing objects overlapping
    * Orthographic projection
    * Investigate further later
* Process-based
  * Renderer, Input, and Control/Workers separate processes
    * Number of workers matches core count approximately
  * IPC through sockets, pipes, and memory-backed DB with concurrent GC
  * Provides fault-tolerance and scalability
* Maintains responsive UI by cycling processes to allow parallel GC
  * Renderer primarily memcpys to minimize GC occurance and duration
* Parallelism*
  * Auto-parallelize dependency tree
  * Ported from CL library lparallel
* Wayland focused

## Inspirations

* Primary:

  * Transformers: Beast Wars, Digimon, Reboot, Tron - bridging the divide
  * The Humane Interface by Jeff Raskin
  * Emacs by RMS
  * Presentation Based User Interfaces by E.C. Ciccarelli at MIT
  * Open Dylan by Apple
  * OpenDoc by Apple
  * Lisp discovered by John McCarthy
  * Lisp Machines by Xerox PARC and MIT
  * Evernote by Stepan Pachikov
  * Oberon OS by Niklaus Wirth at ETH Zürich
  * Intentional Programming by Charles Simonyi at Microsoft
  
* Others:

  * Compiz 3D effects
  * Firefox Tree Style Tab addon
  * Uzbl/Conkeror numbered links for navigation
  * Unreal Blueprints, Blender nodal systems
  * EagleMode ZUI system
  * Sir Tim Berners-Lee
  * Paul Graham
  * Peter Norvig
  * Brett Victor
  * Robert Strandh
  * Chris Schafmeister

## FAQ

See [FAQ](https://github.com/Seteeri/Particle/tree/master/doc/FAQ.md)

## License

Apache License 2.0
