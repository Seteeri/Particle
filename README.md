![PARTICLE](https://github.com/Seteeri/Particle/blob/master/particle.png)

> The Grid. A digital frontier. I tried to picture clusters of
information as they moved through the computer. What did they look like?
 Ships? Motorcycles? Were the circuits like freeways? I kept dreaming of
  a world I thought I'd never see. And then one day . . .

---

Particle is a personal knowledge manager (also called a PIM/PKB)
implemented through a structured/projectional UI based on Lisp.

It is the realization of my vision to map our thoughts into the computer.
The long-term technical goal of Particle is to create a Lispy userspace,
eventually replacing the init system and encompassing all layers above that.

It integrates various computing concepts and UI designs from CLIs, shells,
REPLs, notebooks, WMs/DEs, creative coding, mindmapping, outliners, wikis and
note-taking into a single object-oriented interface that can allow convergence 
across multiple devices such as desktops/workstations, laptops/tablets and 
smartphones/devices where information can flow easily.

The computing landscape has changed significantly since the days of Lisp
Machines so it begs the question as to how useful Lisp at the OS/kernel level
would be today. I believe another attempt is warranted albeit with a different
approach taking advantage of today's computing power and ubiquitiousness.

The target audience consists of programmers, power users, information workers
and "busy" people.

## Features

* Local data first - no cloud dependency
* Distributable - synchronize across multiple devices
* Database backend (object persistence) - scalability and robustness
* Tag-based searching
* Import/link any data including images, videos, audio, etc.
* Export to s-expr, XML, Orgmode, Markdown, HTML, PDF, ODT, SQL
* Visualize data in different forms while maintaining interaction - extract patterns as ideas for new projects
* Encryption - text and block-based
* Web clipper

Possible:

* Revision control
* Fuzzy search/auto-completion
  * Tag suggestions
* Ink-pen input including drawn annotations
* OCR scans/images
* Code-specific support
* Highlighting
* Collaboration - outside scope of this project

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
* Vulkan/WSI (TODO)

1. Clone this repo
2. ...

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

**What about Evernote, OneNote, Notion etc.?**

[The Sad State of Personal Knowledgebases](https://marcusvorwaller.com/blog/2015/12/14/personal-knowledgebases/)

> Today most people don’t use a PK but they do, it’s almost certain to be Evernote or OneNote or something along those lines, basically a flat list of notes that’s easily searchable and taggable or folderable. Power users might use a personal wiki.
>
> To me, all of these seem comparable to using a roll of toilet paper to write a book. You can do it, but there are better ways. Some of better options exist now, but I think that we’re still far from having a great personal knowledgebase.
>
> In a perfect world a PK would have the following features:
>
>  * Good search.
>  * Unlimited size. Since it will be used to store just about everything you want to save for your whole life it needs to handle getting big well.
>  * Simple to use. It should have zero learning curve for someone who just wants to dump a bunch of notes in it and a fast learning curve for anyone wanting to use more powerful features.
>  * Convenient and fast. It should be available online or offline on your phone or tablet or laptop or wherever else you might want to use it. Adding content to it should be as close to effortless as possible and accessible from within other apps.
>  * Structured. It should work fine without any organization but should allow for very flexible relationships between notes and, now that basic AI is becoming more viable, it should suggest relationships intelligently.
>
> Surprisingly, no software with all those features exists yet. There are some interesting options though...

*See ANECDOTES.md*

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

## Inspirations

* Primary:

  * Transformers (Beast Wars), Digimon, Reboot, Tron - bridging the divide
  * Compiz - 3D desktop effects
  * Blender - 3D editor, extensible UI, keyboard driven
  * Firefox Tree Style Tab addon
  * Lisp - the programmable programming language
  * Emacs/Org-mode - consistency, extensibility, text and keyboard driven

* Secondary:

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

