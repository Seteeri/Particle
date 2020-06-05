![PARTICLE LOGO](https://github.com/Seteeri/Particle/blob/master/art/particle.png)

> The Grid. A digital frontier. I tried to picture clusters of
information as they moved through the computer. What did they look like?
 Ships? Motorcycles? Were the circuits like freeways? I kept dreaming of
  a world I thought I'd never see. And then one day . . .

---

Particle is a PicoLisp presentation binary tree (AST) UI.

Particle is the realization of my vision of the human computer interaction model. I want to take the extensibility, dynamicness, and cohesion of past Lisp Machines, Smalltalk-like enivronments, and others, and evolve the text interface to empower as many users as possible to take advantage of the increasing integration of the digital world, the ever-growing computing power, and ubiquitousness of the internet.

I believe the way for a future FOSS system does not lie solely with the desktop - that has been settled - but with the computing needs of tomorrow.

Originally, this project was attempted in Python and Common Lisp but became only possible with PicoLisp, because *all* data is based on cons cell structures retained during runtime, which allows the interface and data to be homoiconic. For example, a C array would break this principle (for interop, they can be symbolized through a number, i.e. pointer).

One interesting implication is when characters (string data) are input via keystrokes, they exist as cons cells so there is no reading or parsing. Consequently, there is no conventional GUI as all data exists in the same domain; it is not separate from the underlying data like in conventional programs (it is possible to build a conventional GUI).

Currently, the core REPL/UI is being developed. Afterwards, the window manager and PIM will be developed, which will make it more generally useful.

Plans are to initially support Linux and Android. Windows requires WSL or virtualization; Mac requires the latter. Once Pil21 is done (LLVM-based), a native solution can be provided. The Pinephone is also on the way ;)

The ultimate goal of Particle is to create a Lispy userland, eventually replacing the init system and encompassing all layers above that, through a more pragmatic and gradual approach.

The target audience consists of information/knowledge workers, power users, programmers.

* To ameliorate the following sentiments:
  * "I'm tired of needing a new application for every different task."
  * "I'm tired of needing a new data format for every different task."
  * "I'm tired of not being able to connect and use my data in other applications."
  * "I'm tired of not being able to share my data with others."
  * "I'm tired of needing markup, native-code, JIT-Code, scripting languages, database languages, domain specific languages, etc..."
  * "I want to use the full power of the computer."
  * "I want the computer to meet me more than half way."

# Preview

![PARTICLE SCREENSHOT](https://github.com/Seteeri/Particle/blob/master/art/screenshot.png)

Can you identify the s-expression structure?

These colored text are representations for the fundamental types; the use of color allows the removal of some character tokens to optimize drawing and information efficiency:
* Strings do not have double quotes; it is possible to remove other identifying tokens such as curly brackets and the dollar sign. 
* Pairs use dot notation and terminate with the `NIL` symbol for proper lists, or arbitrary data for improper lists
  * Parentheses are possible, for the more traditional types ;)
* Lines are double-spaced to vertically delimit pairs and to allow room for the pointer name/mode (rather than in-between pairs like conventional text cursors)
* The pointer is also a symbol!

The hierarchial structure is a direct consequence of the underlying Lisp data, which is a tree; manifesting in a natural UI.

Particle separates the representation from the data ("presentation-based"), allowing arbitrary representations beyond conventional strings - this is an old idea.

The enabling feature is simple: rather than view text and images as distinct types on both the semantic and data level, text is rendered isomorphic to images. 

Another way of thinking about text is they are already images, aka glyphs, icons, or more operatively - symbols; conventional GUIs use the same concept but with real objects, and users similarly think about them the same way during interaction. For text editors, they can be thought of as one-dimensional serial pictographs.

This idea is then further extended to create arbitrary representations for arbitrary data while maintaining the same underlying operations, or interface if you will, no matter the level of abstractions composed.

# Goals

* To complete user tasks, efficiently and effectively ("getting $hit done")
  * In terms of programatic solutions, priority is on lowering human development and response time through adaptability/flexibility over raw performance
  * C exists where it is needed
* To optimize the planning and direction; collection; processing and exploitation; analysis and production; and dissemination of fused multi-domain information in a time-sensitive environment (author's professional requirements ;))

# Inspirations

* Primary:

  * Transformers: Beast Wars, Digimon, Reboot, Tron - bridging the divide between the digital world and reality, users and programs
  * Evernote by Stepan Pachikov
    * To remember everything, an extension of the human brain
  * Oberon OS by Niklaus Wirth at ETH Zürich
    * To structure a system in such a way that it can be described, explained, and understood as a whole
  * The Humane Interface by Jeff Raskin
  * LISP discovered by John McCarthy
  * Lisp Machines by Xerox PARC and MIT
  * Presentation Based User Interfaces by E.C. Ciccarelli at MIT
  * Apple Inc.
    * HyperCard by Bill Atkinson
    * Open Dylan
    * OpenDoc
  * Emacs by RMS at MIT
  * Firefox Tree Style Tab addon


* Programs (in no particular order):

  * Compiz 3D effects
  * Uzbl numbered links for navigation
  * Unreal Blueprints
  * Blender Nodes
  * EagleMode ZUI
  * Microsoft Excel by MS
  * Tetris by Alexey Pajitnov
  * Intentional Programming by Charles Simonyi at Microsoft

* Users (in no particular order):
  
  * Alan Kay
  * Sir Tim Berners-Lee
  * Paul Graham
  * Peter Norvig
  * Brett Victor
  * Robert Strandh
  * Chris Schafmeister
  * Randy Pausch

# UI

On MS and Apple systems, it serves as an outliner "app" or personal knowledge manager, however, with Linux and BSD systems it goes further absorbing the Wayland compositor, creating an encompassing Lisp environment.

*WIP*
* Keyboard-driven first
  * Mouse and touch support
  * Conventional or Vim style (modal) shortcuts
* Local/offline data first - no cloud dependency
* Storage mechanisms
  * Plaintext (Lisp code)
  * Binary (Lisp data)
  * Database (Lisp data)
    * Persistent symbols, called external symbols, first-class data type
    * NoSQL
    * Full ACID, replication, journaling
    * [PicoLisp DB Vs ORM](https://picolisp.com/wiki/?pilvsorm)
* Hierarchial/tree structure focused around lists/symbols
  * Defaults:
    * Start - main workspace
    * Log - undo/cmd history
    * Pointer-Mode - "mouse pointer" and modes; contains key bindings
    * Selection - aka buffers/registers for cut/copy/paste of objects etc.
    * Property-List - property list for object exploration
    * Search-Replace - search/replace symbols/properties
    * Files - file navigator
    * Help - command palette; provides context-dependent suggestions
* Filesystem Interface
  * Interfaced through symbols
  * Filepaths are translated to tags
  * "/this/is/some/path/fora/file.ext" = (file ext this is some path fora)
  * Each directory/word is a tag
  * Order determines priority when searching but is otherwise equivalent

# Manual

* [Introduction](https://github.com/Seteeri/Particle/tree/master/doc/INTRO.md)

* [Premise](https://github.com/Seteeri/Particle/tree/master/doc/PREMISE.md)

* [Roadmap to PID 1](https://github.com/Seteeri/Particle/tree/master/doc/INTRO.md)

* [The Road Beyond](https://github.com/Seteeri/Particle/tree/master/doc/INTRO.md)

* [Installation](https://github.com/Seteeri/Particle/tree/master/doc/INTRO.md)

* [Architecture](https://github.com/Seteeri/Particle/tree/master/doc/INTRO.md)

  * [UI](https://github.com/Seteeri/Particle/tree/master/doc/UI.md)

  * [Toolkit](https://github.com/Seteeri/Particle/tree/master/doc/TK.md)

  * [API](https://github.com/Seteeri/Particle/tree/master/doc/API.md)

* [FAQ](https://github.com/Seteeri/Particle/tree/master/doc/FAQ.md)

  * Plain text is universal.

  * Pictographs are a poor solution; only text can represent code.
  
  * Smalltalk VMs never became mainstream.

  * Attempts at structured/projectional editors have largely failed and the ones that have succeeded are niche.

  * Does this roughly offer the same benefits that the old lisp machines provided?

* [Relevant Anecdotes](https://github.com/Seteeri/Particle/tree/master/doc/INTRO.md)

  * [LISP](https://github.com/Seteeri/Particle/tree/master/doc/ANECDOTES-LISP.md)

  * [CLIM](https://github.com/Seteeri/Particle/tree/master/doc/ANECDOTES-CLIM.md)

  * [EMACS](https://github.com/Seteeri/Particle/tree/master/doc/ANECDOTES-EMACS.md)

  * [WASM](https://github.com/Seteeri/Particle/tree/master/doc/ANECDOTES-WASM.md)

  * [MISC](https://github.com/Seteeri/Particle/tree/master/doc/ANECDOTES-MISC.md)

# License

Apache License 2.0

