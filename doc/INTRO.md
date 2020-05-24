Table of Contents
=================

![PARTICLE](https://github.com/Seteeri/Particle/blob/master/art/screenshot.png)

Particle separates the representation from the data ("presentation-based"), allowing arbitrary representations beyond conventional strings - this is not new. The enabling feature is rather than view text and images as distinct types on both the semantic and data level, text is considered equal to images. Another way of thinking about text is they are already images, aka glyphs, icons, or more operatively - symbols; conventional GUIs use the same concept and people think about them the same way when interacting. This means text editors can be thought of as one-dimensional serial pictographs.

At the data structure level, both text and images have the same data layout. This is enabled by the underlying Lisp structure that implements text, aka encoded numbers, in cons cells as symbols. Likewise, images are also implemented as cons cells. The power of storing data in cons cells over raw bytes like in a typical program, is cons cells have inherent properties due to their two-pointer structure which enables linking arbitrary data which can then be interpreted in different ways at a higher abstraction level. Ultimately, this allows operations that work on text to work on images also. The same way in a compiled Lisp, macros can create new DSLs or syntax aka glyphs, which can be used alongside native syntax, the same can be done with images - they can be mixed with text/code.

For example, a pixel data type consisting of a list of 4 numbers from 0-255, representing RGBA, can be directly represented as a square with the corresponding color. An image can then be defined as a list of pixels, or a list of list of 4 numbers, and that data type can be literally represented as all the pixels combined into a single image. All of this can be done with basic Lisp operations and without typing any explicit code. This idea can then be further extended to allow the user to create literal symbols to represent arbitrary data, and yet they retain the composability of the underlying Lisp structure. In addition, in the context of code, code can be represented by text, so for example, comments can represent code to more effectively indicate the programmer's intentions.
  
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

# License

Apache License 2.0
