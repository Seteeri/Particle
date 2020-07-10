Table of Contents
=================

![PARTICLE](https://github.com/Seteeri/Particle/blob/master/art/screenshot.png)
  
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
