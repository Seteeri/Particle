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

Particle is a 3D Lisp structure/projectional editor. It is the implementation of my vision 
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
and OS (FreeBSD) to create a consistent harmonious system.

Particle provides the following interfaces for the user:

* A Lispy userland based on cons cells
  * Top-down approach rather than a bottom-up approach
* Backwards compatibility with the conventional desktop (X11)

Future plans include building the userland upon FreeBSD, although development is
currently occuring on Linux also. 

Another route to consider is PilOS; however, there would be many hurdles to
overcome including writing drivers, which would not be feasible nor likely to be
completed in a timely manner. Initially, specific hardware or a SOC with 
adequate performance could be targeted to provide a starting point to gain 
momentum, but again, using an existing OS makes more sense to start with. It
begs the question as to how useful a Lisp at the OS level would be (past Lisp 
Machines may provide an answer...).

The target audience consists of programmers, power users and the like, 
particularly intelligence analysis.


## The Inspiration

* Primary Inspirations:
  * Compiz - 3D desktop effects
  * Blender - 3D, multi-domain dataset, Python UI
  * Emacs - consistency, extensibility, text and keyboard driven    
  * Lisp - the programmable programming language
        
* Secondary Inspirations:
  * McCLIM - central concept of "presentation types"
  * Uzbl/Conkeror - numbered links for navigation
  * Unreal Blueprints - nodal system
  * EagleMode - ZUI system
  * OpenDoc - early proprietary attempt by Apple to create compound documents
  * LightTable - drafting table metaphor
  * Minecraft - expressiveness
  * Xerox Parc - ...
  * Evernote
    
* Personal Influences:
  * The Humane Interface by Jeff Raskin
    * Elimination of modes
    * Persistence
    * Unlimited undo
    * Incremental search
    * Elmintation of applications -> commands
    * Redesigned file hierarchies
    * ZUIs as an alternative
  * Sir Tim Berners-Lee - WWW
  * Paul Graham, Peter Norvig - ...
  * Brett Victor - Inventing on Principle
  * Chris Schafmeister - Molecular Metaprogramming  
  * Zach Bean - quicklisp
  * Robert Strandh - CLOSOS/LispOS

## The Interface

* Top-down approach
  * First step - build the shell/DE
  * Second step - integrate the init system
  * Third step - ???
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

* PicoLisp due to simplicity and expressiveness
* Two process system - model/view
  * Model contains a task manager that spreads tasks across frames
    * Maintains low-latency through soft deadlines
  * View SRP: draw triangles and poll socket for commands
    * Separate process from model to minimize pressure on GC
* Rendering engine is essentially a particle system
  * Plans to pursue tiled forward shading engine (Forward+)
  * OpenGL ES 3.2+
  * Compute shaders
  * AZDO Techniques
    * Persistent mapping
    * Instanced drawing
    * Indirect drawing
      * Multi-indirect (not yet implemented in OpenGL ES API)
  * Fully programmable vertex pulling
    * Texture buffers/UBOs/SSBOs -> gl_VertexID+gl_InstanceID
  * Separate shader stages and programmable pipelines
* R-tree for spatial indexing
* Windows, MacOS, BSD, WASM support will come later

## The Roadmap

Core
1. Vertices/Cells
2. Wayland Integration 
   * Web remains accessible through Wayland/DE -> browser
3. Widget toolkit (vertex-based)
   
Extensions
1. Media Functionality
   * FFMPEG for media - images, video, audio
   * Integrate GEGL? Image/graphicsmagick? etc...   
2. WebKit Integration
   * Build around Webkit like Next

Personal Extensions
1. Per-object vector motion blur (personal favorite)
2. Tiled forward rendering (Forward+) including lights
   * Future: Implement clustered -> volumetric forward shading

Future Ideas
* Augmented reality through OpenCV
* Convergence...

## The Requirements

* Atomic modesetting/nuclear pagefliping
* OpenGL ES 3.2+ (OpenGL 4.3+) (See [Mesamatrix](https://mesamatrix.net/))
* Vulkan/WSI (hopefully...)

## The Installation

1. Clone this repo
2. ...

*Will update when ready...*