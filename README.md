![PARTICLE](https://github.com/Seteeri/Particle/blob/master/art/particle.png)

> The Grid. A digital frontier. I tried to picture clusters of
information as they moved through the computer. What did they look like?
 Ships? Motorcycles? Were the circuits like freeways? I kept dreaming of
  a world I thought I'd never see. And then one day . . .

---

Particle is a personal knowledge manager (aka PIM) implemented as an outliner through a presentation-based lisp-structured UI.

It is closest in concept to an outliner, however, it integrates various computing concepts and UI designs from CLIs, shells, REPLs, notebooks, WMs/DEs, creative coding, mindmapping, wikis and note-taking programs into a single object-oriented interface, inspired by Lisp Machines of the past.

On Windows/Mac/iOS/Android, it serves as an outliner, however, with Linux and BSD systems it integrates the window manager, i.e. functions as the Wayland compositor. The ultimate goal of Particle is to create a Lispy userland, eventually replacing the init system and encompassing all layers above that.

The computing landscape has been reshaped significantly since the days of the Lisp Machines, and so too has our computer science knowledge base grown, so it begs the question as to how useful Lisp at the kernel level would be today in terms of cybersecurity, parallel computing, etc.

As much as a Lisp Machine from scratch would be intriguing, for it to be actual useful would be highly energy intensive and fundamentally require commercial support; even then, only with specialized hardware could it be remotely competitive with existing technology. So I believe a more pragmatic approach from the top-down by focusing on the UX through the userspace will allow us to get there.

Currently, plans are to initially support Linux and Android. Windows requires WSL or virtualization; Mac requires the latter. Once Pil21 is done, a native solution can be provided. Until then, a HTML/WebGL interface may be developed in conjunction.

I do have the Pinephone on the way so I will also be testing it on that device to hopefully develop a mobile interface and any other devices I can get my hands on...

The target audience consists of programmers, power users, information workers and "busy" people.

## Features (Planned)

* Local data first - no cloud dependency
* Distributable - synchronize across multiple devices
* Storage mechanism
  * Plain text (Lisp code)
    * Internal symbols
  * Binary (Lisp data)
    * Internal symbols -> external symbols 
    * Uses built-in database functionality of VM
    * PicoLisp has persistent objects built-in as a first class data type
* Tag-based searching
* Import/link any data including images, videos, audio, etc.
* Export to s-expr, XML, Orgmode, Markdown, HTML, PDF, ODT, SQL
* Browser integration (aka web clipper)
* Wayland/userspace integration


Advanced:


* Fuzzy string matching
  * Tag suggestions
* OCR
  * Integration of tesseract library
* Code-specific support
* Revision control
  * Tree diffs
* Encryption - text and block-based
* Ink-pen input; drawn annotations
* Collaboration - separate related project


Interfaces:

* Touch/Mobile - due to power/battery constraints, an HTML frontend might be more effective than OpenGL/WebGL...
* AR/VR - I do have a possible solution for this, moreso for AR...

  
## Roadmap to PID 1

All Platforms:

* Core UI
* Desktop Integration
  * Web Clipper
  * Integrate C libraries for various file formats (data types)
  * Default to FFMPEG for media
  * Implement Lisp encoder/decoder for open formats (low-priority)
  * Port processing libraries like GEGL, libmypaint (low-priority)

Linux/BSD:

* Web Integration
  * C
    * WebKit + JS*
  * Lisp
    * Write layout engine
    * JS engine - port QuickJS to Lisp?
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

**Why structured data? Why not plain text?**

Plain text is convenient but does not scale. Extracting useful information through repeatedly parsing plain text becomes redundant and inefficient on larger scales (at tens or hundreds of thousands of pieces of information). In the case of Particle, the structured data are s-expressions so it is a relatively simple model to understand and parse; the Lisp implementation is also open-source.

**Why not Emacs, org-mode, or ParEdit/Parinfer/Smartparens etc.?**

Emacs is undoubtedly powerful but also difficult to evolve due to its aging codebase. It could be rewritten, however, the goals for such a project remain undefined which hampers initial development from starting. IMHO, writing it purely in Lisp would be interesting...

Emacs is based around the idea of text buffers, however, editing text requires extracting data from the text into internal data structures and parsing back into text. For tools that work on parenthesis, they work on the syntactic level, not the semantic level. If the data is structured, then its type is known providing additional information for the computer to assist the user. In addition, Emacs is centered around editing text, however, modern users have a need to incorporate other types of data.

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

**There have been many attempts at visual languages, projectional/structured editors, presentation-based interfaces. Why yet another one?**

Originally, I started this project in Common Lisp and encountered CLIM, which is a descendant of the Symbolics Genera-based Lisp Machine's Dynamic Windows/Lisp Listener. Its central feature is the concept of presentation types, commands and transformers. 

https://www.reddit.com/r/lisp/comments/22lbpe/whatever_became_of_clim/

> The core of it is: there is a single mechanism in which associations between objects (value, etc.) represented in the user interface by widgets (view, component, graphic, element, etc.) are stored, so that commands and other interactions can be defined on the object type, once independently of the specific visual representation being used.

> This differs from "data binding" by exposing the domain objects, not just data, and therefore introducing the notion of commands operating on the objects.

> This differs from "naked objects" (generating UI from domain objects) by admitting different UI representations of the same object.

However, I wanted to approach the issue from a top-down or hollistic perspective by building a higher-level environment that more conceptually vertically integrated, analagous to modding a game versus using a game engine framework like Unity or Unreal etc. The integration of the interface maximizes the reuse of concepts and by extension commands, since everything is patterned in Lisp, short of the kernel itself (another discussion). It naturally provides discoverability and thus a gradual learning curve as one ventures deeper into the system by learning new types beyond the fundamental Lisp types. This isn't really new - this was one of the wonderful things about Lisp Machines, at least from what I've read in discussions and seen on YouTube.

https://groups.google.com/forum/#!topic/comp.lang.lisp/XpvUwF2xKbk%5B101-125%5D

> The value in Genera's Dynamic Windows is NOT about "completion" it's
> about object-oriented connected-ness of the entire system.  You aren't
> clicking on the "name" in dynamic windows.  You are clicking on the EQ
> object.  This means you can, for example, mouse an instance of
> something and get that instance as an object to inspect and
> manipulate.  Emacs has NO basis for equivalent stuff.  None.  If
> you've seen Genera only through screen shots, you could never imagine.
> It's like trying to explain someone whose only seen streams of text
> what an object pointer is.

The key insight is that Lisp data structures are based upon singly linked lists. "Lists" being the operative word. Lists are more common to the layperson than most people realize. For example, when one creates an outline on paper or uses bullets in a word processing program or an outliner itself, etc. they are creating a structure or an interface. The alphanumeric or pictorial icon, indentation and spaces indicate the structure. They indicate which words or data belong to which other words or data.

Effectively, Particle enforces a hierarchical (graph) structure to the UI which makes conventional widgets and toolkits obsolete. For the layperson, all they must know to use the program are lists and strings, and their basic operations like delete, newline, cut/copy/paste which are already conceptually familiar. Eventually other types are encountered, however, because the user already knows the basic commands, they can use the pre-existing knowledge to explore newer types and functionality.

In contrast, Emacs relies on the concept of (gap) buffers and strictly manipulating arrays of characters (arrays), which works well for solely manipulating text, *and other objects granted they have a textual representation*. The inability to express other data types and interact with them in other ways is ultimately what led me to consider other approaches; it's aging codebase being difficult to evolve another main issue.

In Particle, a list of windows is literally a list of windows and the same list operations apply (exchange windows for arbitrary data type of your choice). Of course, there can be more specialized commands also, however, that is moreso an issue of discoverability. The fundamental concern of the UI is a tradeoff between the developer having complete design freedom versus the user having to learn it.

It is important to note that the goal of Particle is not to teach the user programming but rather implicitly how the system works, i.e. data structures. Should the user want to learn programming, they will already have gained some of the fundamental concepts, at least to program in Lisp.

> I’m a huge proponent of designing your code around the data, rather than the other way around, and I think it’s one of the reasons git has been fairly successful… I will, in fact, claim that the difference between a bad programmer and a good one is whether he considers his code or his data structures more important. Bad programmers worry about the code. Good programmers worry about data structures and their relationships.

-- Linus Torvalds

**Why PicoLisp?**

To build on the previous section, PicoLisp was chosen for the reason (and for other beneficial reasons) that it is a pure Lisp where all data consists of cons cells, with only three basic types: Pairs, Numbers, and Symbols which all consist of cons cells; strings are also symbols.

This consistency makes it easy to reason about the system by connecting the linked-list structure of the UI directly to the underlying linked-list data structure (or more specifically linked cons cells), and because Lisp code can manipulate data through eval, the UI can be dynamically modified through itself. This means operating on the UI (or the data representation) is the same as operating on the underlying data which has the same representation.

At the end of the day, you could say it is using a list metaphor instead of a desktop metaphor since what's really important is that TODO list on your desk and not so much the desk itself ;)

## Inspirations

* Primary:

  * Transformers (Beast Wars), Digimon, Reboot, Tron - bridging the divide
  * Compiz - 3D desktop effects
  * Firefox Tree Style Tab addon  
  * Lisp/Lisp Machines - the programmable programming language
  * Emacs/Org-mode - consistency, extensibility, text and keyboard driven
  * CLIM - presentation-based interface
  * The Humane Interface by Jeff Raskin
    * Elimination of modes
    * Persistence
    * Unlimited undo
    * Incremental search
    * Elimintation of applications -> commands
    * Redesigned file hierarchies
    * ZUIs
  * Evernote - Stepan Pachikov wanted to remember everything
  
* Secondary:
  
  * Uzbl/Conkeror - numbered links for navigation
  * Unreal Blueprints - nodal system
  * EagleMode - ZUI system
  * Douglas Engelbart - "The Mother of All Demos"
  * Xerox PARC - pioneered many modern computing elements and paradigms
  * Sir Tim Berners-Lee - WWW
  * Paul Graham, Peter Norvig - applied Lisp
  * Brett Victor - Inventing on Principle
  * Zach Bean - quicklisp
  * Robert Strandh - CLOSOS/LispOS
  * Chris Schafmeister - Molecular Metaprogramming  
  * Oberon OS
