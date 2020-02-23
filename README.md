Particle
========

*Image to Be Inserted*

> The Grid. A digital frontier. I tried to picture clusters of
information as they moved through the computer. What did they look like?
 Ships? Motorcycles? Were the circuits like freeways? I kept dreaming of
  a world I thought I'd never see. And then one day . . .

Particle is a personal knowledge database or personal information manager
implemented through a 3D Lisp UI. It is the realization of my vision of a 
programmable UI to replace the desktop paradigm - a way to map our thoughts into
the computer.

It integrates various computing and UI concepts from CLIs, shells, REPLs, 
notebooks, WMs/DEs, creative coding, mindmapping, note-taking into a single 
interface that can provide convergence across multiple devices such as 
desktops/workstations, laptops/tablets and smartphones/devices where information
can easily and literally flow between nodes. This is one step towards that.

The goal of Particle is to create a Lispy userspace, eventually replacing the
init system and encompassing all layers above that. The first step is to
maintain backwards compatibility with the conventional desktop (Wayland) and
the C world while rewriting/replacing parts in Lisp, possibly into an actual
Lisp OS.

The computing landscape has changed significantly since the days of Lisp 
Machines so it begs the question as to how useful Lisp at the OS level would be
today in contrast to past Lisp Machines. I believe another attempt is warranted
albeit with a different approach taking advantage of today's computing power
and ubiquitiousness.

The target audience consists of programmers, power users, information workers
and "busy" people.

## The Principles

Particle maximizes the following principles:

* Programmability
* Expressivity
* Dynamism
* Simplicity/Minimalism

These principles are shared with the underlying programming language (PicoLisp)
to create a consistent *understandable* system.

## The Features (Planned)

* Native/local application; no cloud dependency
* Synchronized copies across multiple devices
* Tag-based searching
* Linkable data
* Store any data including images, videos, audio, etc.
* Exportable to s-expr, XML, Orgmode, Markdown, HTML, PDF, ODT, SQL
* Web clipper to import data from browsing

Others:
* Revision control
* Fuzzy search/auto-completion
* Ink-pen input including drawn annotations
* OCR scans/images
* Code-specific support
* Highlighting  

## The Infrastructure

*See ARCHITECTURE.md*

## The Inspiration

* Primary Inspiration:
  * Transformers (Beast Wars), Digimon, Reboot, Tron - bridging the divide
  * Compiz - 3D desktop effects
  * Blender - 3D editor, extensible UI, keyboard driven
  * Firefox Tree Style Tab addon  
  * Lisp - the programmable programming language
  * Emacs/Vim/StumpWM - consistency, extensibility, text and keyboard driven

* Secondary Inspiration:
  * McCLIM - "presentations" - (Convergent evolution I suppose ;))
  * Uzbl/Conkeror - numbered links for navigation
  * Unreal Blueprints - nodal system
  * EagleMode - ZUI system
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
  * Paul Graham, Peter Norvig - applied Lisp
  * Brett Victor - Inventing on Principle
  * Zach Bean - quicklisp
  * Robert Strandh - CLOSOS/LispOS
  * Chris Schafmeister - Molecular Metaprogramming
  * Evernote - Stepan Pachikov wanted to remember everything
  * Oberon OS

## The Roadmap

1. UI
2. Wayland Integration
3. Desktop Integration
  1. Open Formats
    * Native
      * BMP, GIF, JPEG/2000, PNG, WebP
      * FLAC, MP3, Vorbis, Opus
      * Theora, Dirac
      * Ogg, MKV, WebM
      * glTF, COLLADA
      * Default to FFMPEG
    2. Processing Libraries
      * CFFI first; later port GEGL, libmypaint
  2. Web Clipper
4. Userspace Integration
   * Port Tools/Libraries such as Toybox
5. Web Integration
   * Web remains accessible through Wayland/DE
   * WebKit/JS-Engine Integration
   * Poss to write HTML/CSS library
     * JS engine resource intensive

## The Requirements

* OpenGL ES 3.2+ (OpenGL 4.3+) (See [Mesamatrix](https://mesamatrix.net/))
* Vulkan/WSI (hopefully...)

## The Installation

1. Clone this repo
2. ...

## The License

Permissively licensed
