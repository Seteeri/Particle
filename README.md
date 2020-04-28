![PARTICLE](https://github.com/Seteeri/Particle/blob/master/art/particle.png)

> The Grid. A digital frontier. I tried to picture clusters of
information as they moved through the computer. What did they look like?
 Ships? Motorcycles? Were the circuits like freeways? I kept dreaming of
  a world I thought I'd never see. And then one day . . .

---

Particle is a personal knowledge manager (aka PIM) implemented as an outliner through a presentation-based lisp-structured UI.

It is closest in concept to an outliner, however, it integrates various computing concepts and UI designs from CLIs, shells, REPLs, notebooks, WMs/DEs, creative coding, mindmapping, wikis and note-taking programs into a single object-oriented (or symbolic, if you will) interface.

The idea is to take the extensibility of Emacs combined with the dynamics ahd cohesion of past Lisp Machines, Oberon and others, and instead of being based solely around text, evolve that concept to arbitrary objects, based upon a fundamental data concept and thus language, to create a uniform and consistent interface taking advantage of modern hardware features such as GPUs and parallelism.

On Windows/Mac/iOS/Android, it serves as an outliner, however, with Linux and BSD systems it integrates the window manager, i.e. functions as the Wayland compositor. The ultimate goal of Particle is to create a Lispy userland, eventually replacing the init system and encompassing all layers above that.

Currently, plans are to initially support Linux and Android. Windows requires WSL or virtualization; Mac requires the latter. Once Pil21 is done (LLVM based), a native solution can be provided. The Pinephone is also on the way ;)

The target audience consists of programmers, power users, information workers and "busy" people.

## Goals

* To get work done in an efficient and productive manner
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

## Features (Planned)

* Local data first - no cloud dependency
* Distributable - synchronize across multiple devices
* Storage mechanism
  * Plain text (Lisp code)
    * Internal symbols
  * Binary (Lisp data)
    * External symbols 
    * PicoLisp has built-in database with external symbols as first-class data type
      * Hmm...memory based db file = ~single memory address space across proccesses?
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

## Future

The computing landscape has been reshaped significantly since the days of the Lisp Machines, and so too has our computer science knowledge base grown, so it begs the question as to how useful Lisp at the kernel level would be today in terms of cybersecurity, parallel computing, etc.

As much as a Lisp Machine from scratch would be intriguing, for it to be actual useful would be highly energy intensive and fundamentally require commercial support; even then, only with specialized hardware could it be remotely competitive with existing technology. So I believe a more pragmatic approach from the top-down by focusing on the UX through the userspace will allow us to get there.

## License

Apache License 2.0

## Roadmap to PID 1

See [Roadmap](https://github.com/Seteeri/Particle/tree/master/doc/ROADMAP.md)

## Installation

See [Installation](https://github.com/Seteeri/Particle/tree/master/doc/ROADMAP.md)

## FAQ

See [FAQ](https://github.com/Seteeri/Particle/tree/master/doc/FAQ.md)

## Demos
