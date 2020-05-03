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


## The Road Beyond

The computing landscape has been reshaped significantly since the days of the Lisp Machines, and so too has our computer science knowledge base grown, so it begs the question as to how useful Lisp at the kernel level would be today in terms of cybersecurity, parallel computing, etc.

As much as a Lisp Machine from scratch would be intriguing, for it to be actual useful would be highly energy intensive and fundamentally require commercial support; even then, only with specialized hardware could it be remotely competitive with existing technology. So I believe a more pragmatic approach from the top-down by focusing on the UX through the userspace will allow us to get there.
