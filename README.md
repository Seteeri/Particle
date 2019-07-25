Particle
========

> The Grid. A digital frontier. I tried to picture clusters of
information as they moved through the computer. What did they look like?
 Ships? Motorcycles? Were the circuits like freeways? I kept dreaming of
  a world I thought I'd never see. And then one day . . .

Inspired by:
* Transformers (Beast Wars)
* Digimon
* Reboot
* Compiz
* Blender
* Tron
* Emacs
* Lisp

Particle is a 3D Lisp structured/projectional REPL. It is the implementation of my vision
of a programmable UI to replace the desktop paradigm - a way to map our thoughts
into the computer. It integrates concepts from CLIs, shells, REPLs, notebooks,
WMs/DEs, creative coding, and mindmapping into a single interface.

My personal dream would be to have a consistent computing experience between
a desktop/workstation, laptop/tablet and smartphone/PDA where information or
objects can easily flow between devices, i.e. convergence.

Particle maximizes the following principles:

* Programmability
* Expressivity
* Dynamism
* Simplicity/Minimalism

These principles are shared with the underlying programming language (PicoLisp)
and OS (BSD) to create a consistent harmonious system.

Particle provides the following interfaces for the user:

* A Lispy userland based on cons cells
  * Top-down approach rather than a bottom-up approach
* Backwards compatibility with the conventional desktop (X11)

Future plans include building the userland upon Dragonfly BSD, although
current development is occuring on Arch Linux/Sway.

PilOS provides a minimal starting point; however, there would be many hurdles to
overcome including writing a compatibility layer like LinuxKPI to reuse linux
drivers. Initially, specific hardware or a SOC with adequate performance could
be targeted to provide a suitable base. It begs the question as to how useful a
Lisp at the OS level would be (past Lisp Machines may provide an answer...).

The target audience consists of programmers, power users and the like,
particularly intelligence analysis.


## The Inspiration

* Primary Inspirations:
  * Compiz - 3D desktop effects
  * Blender - 3D editor, extensible UI, keyboard driven
  * Emacs - consistency, extensibility, text and keyboard driven
  * Lisp - the programmable programming language
  * Evernote - Stepan Pachikov wanted to remember everything

* Secondary Inspirations:
  * McCLIM - central concept of "presentation types"
  * Uzbl/Conkeror - numbered links for navigation
  * Unreal Blueprints - nodal system
  * EagleMode - ZUI system
  * Xerox Parc - not much more to say...

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
  * Sir Tim Berners-Lee - WWW
  * Paul Graham, Peter Norvig
  * Brett Victor - Inventing on Principle
  * Zach Bean - quicklisp
  * Robert Strandh - CLOSOS/LispOS
  * Chris Schafmeister - Molecular Metaprogramming

## The Interface

* Top-down approach
  * First step - UI/REPL/DE/WM
  * Second step - init integration
  * Third step - kernel integration
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
* Multi process system
  * Components: Controller, Workers, Model, Render
  * IPC through message passing
  * Multiple workers increases stability/robustness
* Rendering engine is essentially a particle system
  * Compute shaders
  * AZDO Techniques
    * Persistent mapping
    * Instanced drawing
    * Indirect drawing
  * Fully programmable vertex pulling
    * Texture buffers/UBOs/SSBOs -> gl_VertexID+gl_InstanceID
* R-tree for spatial indexing

## The Roadmap

Interactive Core
1. REPL
2. Wayland Integration
   * Web remains accessible through Wayland/DE -> browser
3. Widget toolkit (vertex-based)

Desktop Functionality
1. Media Functionality
   * FFMPEG for media - images, video, audio
   * Integrate GEGL? Image/graphicsmagick? etc...
2. WebKit Integration
   * Build around Webkit like Next

Personal Extensions
1. Per-object vector motion blur (personal favorite)
2. Power mode

Future Ideas
* Native Library Ports:
  * Commonly used *open* image/video/audio formats
    * BMP, GIF, JPEG/2000, PNG, WebP
    * FLAC, MP3, Vorbis, Opus
    * Theora, Dirac
    * Ogg, MKV, WebM
    * glTF, COLLADA, OpenGEX
* Tiled forward rendering (Forward+)
  * Clustered -> volumetric forward shading
* Augmented reality through OpenCV
* Convergence...

## The Requirements

* OpenGL ES 3.2+ (OpenGL 4.3+) (See [Mesamatrix](https://mesamatrix.net/))
* Vulkan/WSI (hopefully...)

## The Installation

1. Clone this repo
2. ...

*Will update when ready...*

## The License

Permissively licensed