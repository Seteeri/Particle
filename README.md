Particle
========

*Image to Be Inserted*

> The Grid. A digital frontier. I tried to picture clusters of
information as they moved through the computer. What did they look like?
 Ships? Motorcycles? Were the circuits like freeways? I kept dreaming of
  a world I thought I'd never see. And then one day . . .

Inspired by:

* Transformers (Beast Wars), Digimon, Reboot
* Compiz
* Blender
* Tron
* Emacs
* Lisp
* Oberon OS

Particle is a 3D Lisp structured REPL/shell. It is the implementation of
my vision of a programmable UI to replace the desktop paradigm - a way to map
our thoughts into the computer. It integrates concepts from CLIs, shells, REPLs,
notebooks, WMs/DEs, creative coding, and mindmapping into a single interface.

The target audience consists of programmers, power users and the like, and
"busy" people.

The goal of Particle is to create a Lispy userspace through being the
init system and encompassing all layers above that. The first step is to
maintain backwards compatibility with the conventional desktop (Wayland) and
the C world while rewriting/replacing parts in Lisp.

This will allow a smoother transition to a proper Lisp OS. PilOS provides a minimal
starting point; however, there would be many hurdles to overcome, driver support
coming to mind. Initially, a SOC could be targeted as a starting point. 
It begs the question as to how useful Lisp at the OS level would be today
in contrast to past Lisp Machines.

I am planning to port PilOS to ARM64 or even RISC-V once devices become
more adequate.

My personal dream would be to have a consistent computing experience between
a desktop/workstation, laptop/tablet and smartphone/PDA where information or
objects can easily flow between devices, i.e. convergence. This is one step
towards that.

## The Principles

Particle maximizes the following principles:

* Programmability
* Expressivity
* Dynamism
* Simplicity/Minimalism

These principles are shared with the underlying programming language (PicoLisp)
to create a consistent *understandable* system.

## The Inspiration

* Primary Inspirations:
  * Compiz - 3D desktop effects
  * Blender - 3D editor, extensible UI, keyboard driven
  * Lisp - the programmable programming language
  * Emacs/Vim/StumpWM - consistency, extensibility, text and keyboard driven
  * Evernote - Stepan Pachikov wanted to remember everything
  * Firefox Tree Style Tab addon

* Secondary Inspirations:
  * McCLIM - "presentations" - (Convergent evolution I suppose ;))
  * Uzbl/Conkeror - numbered links for navigation
  * Unreal Blueprints - nodal system
  * EagleMode - ZUI system

* Personal Influences:
  * The Humane Interface by Jeff Raskin
    * Elimination of modes
    * Persistence
    * Unlimited undo
    * Incremental search
    * Elimintation of applications -> commands
    * Redesigned file hierarchies
    * ZUIs
  * Douglas Engelbart - "The Mother of All Demos"
  * Xerox PARC - pioneered many modern computing elements and paradigms
  * Sir Tim Berners-Lee - WWW
  * Paul Graham, Peter Norvig
  * Brett Victor - Inventing on Principle
  * Zach Bean - quicklisp
  * Robert Strandh - CLOSOS/LispOS
  * Chris Schafmeister - Molecular Metaprogramming

## The Interface

* Built around DAG/trees  - common pattern across domains:
  * HTML/DOM
  * Task management
  * Version control
  * Init dependencies
  * Garbage collection tracing
  * Gantt charts
  * Compiler internals
* 3D orthographic nodal environment - "turtles all the way down"
* Primarily keyboard driven interface
* Non-destructive editing; undo/redo capabilities
* Non-blocking UI - user always aware of computer status
* Wayland provides conventional desktop
* Solarized color theme for default

## The Architecture

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

## The Roadmap

Core:
1. Interactive Core (REPL)
2. Wayland Integration
3. Widget Toolkit (Vertex-based)
4. Desktop Functionality
   1. Media
      * FFMPEG for media - images, video, audio
      * Integrate GEGL? Image/graphicsmagick? etc...

Lispify:

1. Native Web Browser
   * Web remains accessible through Wayland/DE
   * WebKit Integration - Hmm, port to Lisp?
   * JavaScript Engines - Embed QuickJS, Ducktape or Jsish
2. Port Userspace Tools/Libraries
   * Media - FFmpeg huge so focus on commonly used *open* image/video/audio formats
      * BMP, GIF, JPEG/2000, PNG, WebP
      * FLAC, MP3, Vorbis, Opus
      * Theora, Dirac
      * Ogg, MKV, WebM
      * glTF, COLLADA
   * Toybox (Toolchain)

Personal Extensions
1. Per-object vector motion blur (personal favorite)
2. Power mode

Future Ideas
* Tiled forward rendering (Forward+)
  * Clustered -> volumetric forward shading
* Augmented reality through OpenCV
* Convergence...
* PilOS bootloader...

## The Requirements

* OpenGL ES 3.2+ (OpenGL 4.3+) (See [Mesamatrix](https://mesamatrix.net/))
* Vulkan/WSI (hopefully...)

## The Installation

1. Clone this repo
2. ...

## The License

Permissively licensed
