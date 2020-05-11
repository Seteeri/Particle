![PARTICLE LOGO](https://github.com/Seteeri/Particle/blob/master/art/particle.png)

> The Grid. A digital frontier. I tried to picture clusters of
information as they moved through the computer. What did they look like?
 Ships? Motorcycles? Were the circuits like freeways? I kept dreaming of
  a world I thought I'd never see. And then one day . . .

---

Particle is a PicoLisp presentation binary tree (AST) UI.

The idea is to take the extensibility of Emacs combined with the dynamics and cohesion of past Lisp Machines, Oberon, Open Dylan, Intentional Programming and other models, and to evolve that consistent text interface to arbitrary objects, taking advantage of modern hardware such as GPUs and parallelism.

Originally, this project was attempted in other languages but became only possible with PicoLisp, because *all* data structures are based on cons cells and this structure is retained during runtime, which allows the GUI and data to be homoiconic (overloaded term these days). When characters (strings) are input via keystrokes, they exist as cons cells so there is no reading or parsing, which has implications later explained. In addition, there is no GUI in the traditional sense as the data itself becomes the GUI and is no longer separate from the system like in conventional programs (it is possible to build a conventional GUI in Particle).

On MS and Apple systems, it serves as an outliner "app" or personal knowledge manager, however, with Linux and BSD systems it goes further absorbing the Wayland compositor, creating an encompassing Lisp environment. The ultimate goal of Particle is to create a Lispy userland, eventually replacing the init system and encompassing all layers above that, through a more pragmatic and gradual approach.

Plans are to initially support Linux and Android. Windows requires WSL or virtualization; Mac requires the latter. Once Pil21 is done (LLVM-based), a native solution can be provided. The Pinephone is also on the way ;)

The target audience consists of programmers, power users, information/knowledge workers.

# Preview

![PARTICLE](https://github.com/Seteeri/Particle/blob/master/art/types.png)

These colored text are representations for the fundamental types; the use of color allows the removal of some character tokens to optimize drawing and information efficiency. Lines are double-spaced to allow room for the pointer (cursor).

Strings do not have double quotes; it is possible to remove other identifying tokens such as curly brackets and the dollar sign. Pairs use dot notation and terminate with the `NIL` symbol for proper lists (or arbitrary data for improper lists); parentheses are possible. Circular lists follow PicoLisp conventions and terminate with a dot.

Can you identify the s-expression structure?

This idea is then further extended to create arbitrary representations for arbitrary data while maintaining the same underlying operations, or interface if you will, no matter the level of abstractions composed.

# Goals

* To get work done efficiently and effectively
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

# Inspirations

* Transformers: Beast Wars, Digimon, Reboot, Tron - bridging the divide
* The Humane Interface by Jeff Raskin
* Lisp discovered by John McCarthy
* Emacs by RMS
* Presentation Based User Interfaces by E.C. Ciccarelli at MIT
* Open Dylan by Apple
* OpenDoc by Apple
* Lisp Machines by Xerox PARC and MIT
* Evernote by Stepan Pachikov
* Oberon OS by Niklaus Wirth at ETH Zürich
* Intentional Programming by Charles Simonyi at Microsoft
  
* Others:

  * Compiz 3D effects
  * Firefox Tree Style Tab addon
  * Uzbl/Conkeror numbered links for navigation
  * Unreal Blueprints, Blender nodal systems
  * EagleMode ZUI system
  * Microsoft Excel
  * Alan Kay
  * Sir Tim Berners-Lee
  * Paul Graham
  * Peter Norvig
  * Brett Victor
  * Robert Strandh
  * Chris Schafmeister
  * Randy Pausch
  
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

  * Why structured data? Why not plain text?

  * Why not Emacs, org-mode, or ParEdit/Parinfer/Smartparens etc.?

  * What about Evernote, OneNote, Notion etc.?

  * Why PicoLisp? Dynamic scope is evil...

  * Does this roughly offer the same benefits that the old lisp machines provided?

  * Is this the same as Microsoft's OLE or Apple's OpenDoc systems which both failed?

  * Is this intentional programming?

* [Relevant Anecdotes](https://github.com/Seteeri/Particle/tree/master/doc/INTRO.md)

  * [LISP](https://github.com/Seteeri/Particle/tree/master/doc/ANECDOTES-LISP.md)

  * [CLIM](https://github.com/Seteeri/Particle/tree/master/doc/ANECDOTES-CLIM.md)

  * [EMACS](https://github.com/Seteeri/Particle/tree/master/doc/ANECDOTES-EMACS.md)

  * [WASM](https://github.com/Seteeri/Particle/tree/master/doc/ANECDOTES-WASM.md)

  * [MISC](https://github.com/Seteeri/Particle/tree/master/doc/ANECDOTES-MISC.md)

# License

Apache License 2.0

