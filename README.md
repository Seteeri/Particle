![PARTICLE LOGO](https://github.com/Seteeri/Particle/blob/master/art/particle.png)

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

![PARTICLE](https://github.com/Seteeri/Particle/blob/master/art/types.png)

Types are color coded to remove the need for tokens to optimize drawing and information efficiency. Lines are double spaced to allow space for the pointer (cursor). 

Can you identify the types and the relationships between the objects?

Strings do not have double quotes; it is possible to remove other identifying tokens such as curly brackets and the dollar sign. Pairs do not have parentheses and terminate with the NIL symbol for proper lists (or arbitrary data for improper lists). Circular lists follow PicoLisp conventions and terminate with a dot.

In constrast to text/string editors, the data structure (AST) is transformed directly so the reader is not involved. The list is constructed as you type. Typing keys produce a single character string, and specific keyboard shortcuts/combinations perform fundamental Lisp operations such as packing strings and interning the string to produce a symbol. Numbers are input directly using ALT+"NUM" or similarly, converted from a string to a number or vice versa with format.

Particle separates the representation from the data ("presentaion-based"), allowing arbitrary representations beyond conventional strings. This aspect is not new. The crucial feature that enables Particle is rather than view text and images as distinct types on both the semantic and data level, text is considered equal to images. Another way of thinking about text is they are simply glyphs, icons - symbols, and likewise conventional GUIs use the same concept and people think about them the same way when interacting. 

At the data structure level, both text and images have the same data layout. This is enabled by the underlying Lisp structure that implements text, aka encoded numbers, in cons cells as symbols. Likewise, images are also implemented as cons cells. The power of storing data in cons cells over raw bytes like in a typical program, is cons cells have inherent properties due to their two-pointer structure which enables linking arbitrary data which can then be interpreted in different ways at a higher abstraction level. Ultimately, this allows operations that work on text to work on images also. The same way in a compiled Lisp, macros can create new DSLs or syntax aka glyphs, which can be used alongside native syntax, the same can be done with images - they can be mixed with text/code.

For example, a pixel data type consisting of a list of 4 numbers from 0-255, representing RGBA, can be directly represented as a square with the corresponding color. An image can then be defined as a list of pixels, or a list of list of 4 numbers, and that data type can be literally represented as all the pixels combined into a single image. All of this can be done with basic Lisp operations and without typing any explicit code. This idea can then be further extended to allow the user to create literal symbols to represent arbitrary data, and yet they retain the composability of the underlying Lisp structure. In addition, in the context of code, code can be represented by text, so for example, comments can represent code to more effectively indicate the programmer's intentions.

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

Ostensibly, it resembles an outliner, however, it integrates various computing concepts and UI designs from CLIs, shells, REPLs, notebooks, WMs/DEs, creative coding, mindmapping, wikis and note-taking programs into a single object-oriented (more operatively, symbolic) interface; *it is not a visual programming language*.

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
  * Microsoft Excel
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
