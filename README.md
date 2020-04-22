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

## License

Apache License 2.0

## Roadmap to PID 1

See [Roadmap](https://github.com/Seteeri/Particle/tree/master/doc/ROADMAP.md)

## Installation

See [Installation](https://github.com/Seteeri/Particle/tree/master/doc/ROADMAP.md)

## FAQ

See [FAQ](https://github.com/Seteeri/Particle/tree/master/doc/FAQ.md)
