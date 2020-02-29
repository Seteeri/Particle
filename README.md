![PARTICLE](https://github.com/Seteeri/Particle/blob/master/particle.png)

> The Grid. A digital frontier. I tried to picture clusters of
information as they moved through the computer. What did they look like?
 Ships? Motorcycles? Were the circuits like freeways? I kept dreaming of
  a world I thought I'd never see. And then one day . . .

---

## Introduction

Particle is a personal knowledge base/manager (traditionally called a PIM) 
implemented through a 3D zoomable structured UI based on Lisp.

It is the realization of my vision to map our thoughts into the computer. 
The long-term technical goal of Particle is to create a Lispy userspace, 
eventually replacing the init system and encompassing all layers above that.

It integrates various computing concepts and UI deisgns from CLIs, shells, 
REPLs, notebooks, WMs/DEs, creative coding, mindmapping, note-taking into a 
single object-oriented interface that can allow convergence across multiple 
devices such as desktops/workstations, laptops/tablets and smartphones/devices 
where information can flow easily.

The computing landscape has changed significantly since the days of Lisp 
Machines so it begs the question as to how useful Lisp at the OS/kernel level 
would be today. I believe another attempt is warranted albeit with a different 
approach taking advantage of today's computing power and ubiquitiousness.

The target audience consists of programmers, power users, information workers
and "busy" people.

## Principles

Particle maximizes the following principles:

* Programmability
* Expressivity
* Dynamism
* Simplicity/Minimalism

These principles are shared with the underlying programming language (PicoLisp)
to create a consistent *discoverable* system with integrated defaults.

## Features (Planned)

* Native/local application first - no cloud dependency
* Synchronize across multiple devices
* Search based on tags
* Link/store any data including images, videos, audio, etc.
* Export to s-expr, XML, Orgmode, Markdown, HTML, PDF, ODT, SQL
* Import data from browsing through web clipper
* Database backend (object persistence) allows for replication, sharding, load balancing etc.

Possible:

* Revision control
* Fuzzy search/auto-completion
* Ink-pen input including drawn annotations
* OCR scans/images
* Code-specific support
* Highlighting

## Inspirations

* Primary Inspiration:

  * Transformers (Beast Wars), Digimon, Reboot, Tron - bridging the divide
  * Compiz - 3D desktop effects
  * Blender - 3D editor, extensible UI, keyboard driven
  * Firefox Tree Style Tab addon  
  * Lisp - the programmable programming language
  * Emacs/Org-mode - consistency, extensibility, text and keyboard driven

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

## Roadmap to PID 1

All Platforms:

* Input/Display (UI)
* Desktop Integration
  * Web Clipper
  * Integrate C libraries for various file formats (data types)
  * Default to FFMPEG for media
  * Implement Lisp encoder/decoder for open formats (low-priority)
  * Port processing libraries like GEGL, libmypaint (low-priority)

Linux/BSD:

* Web Integration
  * WebKit/JS* Integration
  * Migrate to native solution
    * Layout engine
    * JS engine - port QuickJS?
* Wayland Integration  
* Userspace Integration
  * Port unix utilities such as Toybox or provide Lisp analogues
* Init Integration

## Installation

Requirements:
* OpenGL ES 3.2+ (OpenGL 4.3+) (See [Mesamatrix](https://mesamatrix.net/))
* Vulkan/WSI (hopefully...)

1. Clone this repo
2. ...

## Architecture

*See ARCHITECTURE.md*

## License

Permissively licensed

## FAQ

**Why structured data? Why not use plain text?**

Plain text is convenient but does not scale. Extracting useful information
through repeatedly parsing plain text becomes redundant and inefficient on larger
scales (at tens or hundreds of thousands of pieces of information). In the case
of Particle, the structured data are s-expressions so it is a relatively simple 
model to understand and parse; the Lisp implementation is also open-source.

**Why not use Emacs, org-mode, or ParEdit/Parinfer/Smartparens etc.?**

Emacs is undoubtedly powerful but also difficult to evolve due to its aging
codebase. It could be rewritten, however, the goals for such a project remain 
undefined which hampers initial development from starting. IMHO, writing it 
purely in Lisp would be interesting...

Emacs is based around the idea of text buffers, however, editing text requires 
extracting data from the text into internal data structures and parsing back 
into text. For tools that work on parenthesis, they work on the syntactic level,
not the semantic level. If the data is structured, then its type is known 
providing additional information for the computer to assist the user. In 
addition, Emacs is centered around editing text, however, modern users have a 
need to incorporate other types of data.
