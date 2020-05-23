Table of Contents
=================

![PARTICLE](https://github.com/Seteeri/Particle/blob/master/art/screenshot.png)

Particle separates the representation from the data ("presentation-based"), allowing arbitrary representations beyond conventional strings - this aspect is not new. The crucial feature that enables Particle is rather than view text and images as distinct types on both the semantic and data level, text is considered equal to images. Another way of thinking about text is they are simply glyphs, icons - symbols, and likewise conventional GUIs use the same concept and people think about them the same way when interacting. 

At the data structure level, both text and images have the same data layout. This is enabled by the underlying Lisp structure that implements text, aka encoded numbers, in cons cells as symbols. Likewise, images are also implemented as cons cells. The power of storing data in cons cells over raw bytes like in a typical program, is cons cells have inherent properties due to their two-pointer structure which enables linking arbitrary data which can then be interpreted in different ways at a higher abstraction level. Ultimately, this allows operations that work on text to work on images also. The same way in a compiled Lisp, macros can create new DSLs or syntax aka glyphs, which can be used alongside native syntax, the same can be done with images - they can be mixed with text/code.

For example, a pixel data type consisting of a list of 4 numbers from 0-255, representing RGBA, can be directly represented as a square with the corresponding color. An image can then be defined as a list of pixels, or a list of list of 4 numbers, and that data type can be literally represented as all the pixels combined into a single image. All of this can be done with basic Lisp operations and without typing any explicit code. This idea can then be further extended to allow the user to create literal symbols to represent arbitrary data, and yet they retain the composability of the underlying Lisp structure. In addition, in the context of code, code can be represented by text, so for example, comments can represent code to more effectively indicate the programmer's intentions.

# Goals

* To achieve goals and objectives, efficiently and effectively ("getting $hit done")
  * Focus: development time > performance
  * C always exists for maximum speed/performance
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
* Lisp Machines by Xerox PARC and MIT
* Presentation Based User Interfaces by E.C. Ciccarelli at MIT
* Apple
  * HyperCard
  * Open Dylan
  * OpenDoc
* Evernote by Stepan Pachikov
* Oberon OS by Niklaus Wirth at ETH Zürich
* Intentional Programming by Charles Simonyi at Microsoft
* Emacs by RMS
  
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
  
# Premise

[Premise](https://github.com/Seteeri/Particle/tree/master/doc/PREMISE.md)

# Roadmap to PID 1

All Platforms:

* Core UI
* Desktop Functionality
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
* Userspace/Inir Integration
  * Port unix utilities such as Toybox -> Provide Lisp analogues


# The Road Beyond

The computing landscape has been reshaped significantly since the days of the Lisp Machines, and so too has our computer science knowledge base grown, so it begs the question as to how useful Lisp at the kernel level would be today in terms of cybersecurity, parallel computing, etc.

As much as a Lisp Machine from scratch would be intriguing, for it to be actual useful would be highly energy intensive and fundamentally require commercial support; even then, only with specialized hardware could it be remotely competitive with existing technology. So I believe a more pragmatic approach from the top-down by focusing on the UX through the userspace will allow us to get there.


# Quick Start

## Installation

Requirements:
* OpenGL ES 3.2+ (OpenGL 4.3+) (See [Mesamatrix](https://mesamatrix.net/))
* Vulkan/WSI (TODO)

# Architecture

[UI](https://github.com/Seteeri/Particle/tree/master/doc/UI.md)

[TK](https://github.com/Seteeri/Particle/tree/master/doc/TK.md)

[API](https://github.com/Seteeri/Particle/tree/master/doc/API.md)

[FAQ](https://github.com/Seteeri/Particle/tree/master/doc/FAQ.md)

# Relevant Anecdotes

[LISP](https://github.com/Seteeri/Particle/tree/master/doc/ANECDOTES-LISP.md)

[CLIM](https://github.com/Seteeri/Particle/tree/master/doc/ANECDOTES-CLIM.md)

[EMACS](https://github.com/Seteeri/Particle/tree/master/doc/ANECDOTES-EMACS.md)

[WASM](https://github.com/Seteeri/Particle/tree/master/doc/ANECDOTES-WASM.md)

[MISC](https://github.com/Seteeri/Particle/tree/master/doc/ANECDOTES-MISC.md)
