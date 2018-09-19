Protoform
=========

[![Alt text](https://img.youtube.com/vi/BemmZaOyzbk/0.jpg)](https://www.youtube.com/watch?v=BemmZaOyzbk)

Protoform is a Common Lisp 3D nodal/graph IDE based on DRM-KMS and 
Open GL ES designed around working in a freeform/prototyping 
envirornment with file data from various domains, analagous to a desktop
environment providing default programs for common tasks.

It is the implementation of my vision of a more consistent programmable
user-interface to replace the desktop. It aims to integrate the REPL, 
CLI, WM/DE, and common data/file types into a single 3D interface.

Traditionally computers have worked by having different applications and
UIs working on both different and similar data with overlapping 
operations and functionality. For example, a word processor might include
some image editing operations, and an image editor might provide the
ability to rasterize text.

The idea of Protoform is to invert that model so users have a consistent
interface with less redundancy which is faster to learn. Data today is 
not simply numbers and text but much more, and users need an interface
that can cut across all domains.

It aims to integrate Wayland/X which would provide windows as planes in 
space, effectively making Protoform a meta-manager around the desktop. 
This would allow grouping windows into a single plane with the plane 
representing the conventional desktop, e.g. Weston running on 
Weston/etc., or have windows existing in the environment in free space.

The target audience consists of programmers, developers, system
administrators, power users and the like. It is designed for workstations 
and creators, or any computing situation dealing with integrating 
information across various domains.

Eventually, I would like to implement a different interface for the less
programmatically inclined users, starting with people that are familiar
with the MS Office suite.

## The Inspiration

It was designed to address limitations I've personally encountered in
the quest for a more efficient desktop; the main limitation being, 
confinement to the desktop/window metaphor and the staticness of widgets

Rather, I imagine developing a system the same way a Lisp program can be
developed incrementally and molded to the abstraction of the user, or 
even similar to building environments in the sandbox game, Minecraft.

With the growth of open-source, the new direction of Wayland, 
fragmentation of the desktop, and increasing computing resources, I 
believe this presents an opportunity to redefine the computing 
environment. Protoform is not an entirely new idea, but a different 
attempt to build upon the success and failures of those before.

A major inspiration was to [build](https://www.reddit.com/r/emacs/comments/3v19uj/is_there_anything_emacs_cant_do/)
on Emacs - take it to another level:
* Adopt Common Lisp
* Adopt modern rendering techniques shifting towards GPU computing
* Adopt a more gradual learning curve  - enhance discoverability of features

The goal is not to create virtual realities for HMDs etc., although that
naturally remains an orthogonal possibility. Protoform was designed to 
be more fundamental and to provide basic primitives and building blocks, 
to allow it to be adapated for those scenarios, the same way Emacs has 
been extended to do almost everything.

* Primary Inspirations:
    * Blender - 3D, flexible UI
    * Emacs - consistency, extensibility, keyboard-driven
    
* Secondary Inspirations:
    * Unreal Blueprints - nodal system
    * Uzbl/Conkeror - numbered links for navigation
    * OpenDoc - early proprietary Apple attempt to create compound documents
    * Compiz - 3D desktop effects
    * EagleMode - example of a ZUI
    * Minecraft - expressiveness
    
* Personal Influences:
    * Gypsy by Larry Tesler
    * The Humane Interface by Jeff Raskin - commands/transformers    

## The Interface

* 3D orthographic non-windowing environment
* Multi-paradigm nodal environment
* Undo/revision control/non-destructive editing
* Wayland - provides conventional desktop

## The Features

* Atomic modesetting/nuclear pageflipping
* Muti-channel signed distance glyphs
  * Generated offline however can be generated on-the-fly
  * Anti-aliasing applied through shader
* OpenGL ES 3.2+
  * Compute shaders:
    * Computes drawable instances
    * TODO: Frustum culling using oriented bounding boxes, i.e. the transformed planes
    * TODO: Occlusion culling (low-priority)
  * Single/double/triple buffered buffer objects
    * Persistent mapping
    * Synchronization optional with triple buffering
  * Instanced - combines multiple draw calls into a single draw call for the same object
  * Indirect - allows parameters for drawing, including instancing, to be provided from a buffer object
    * TODO: Multi-indirect - combines multiple indirect draw calls into a single draw call (not yet implemented in OpenGL ES API)
  * Programmable vertex pulling - texture/uniform/shader-storage buffers; retrieve data through gl_VertexID+gl_InstanceID
  * Separate shader stages and programmable pipelines

## The Architecture

TODO

## The Requirements

* Atomic modesetting/nuclear pagefliping
* OpenGL ES 3.2+ (OpenGL 4.3+) (See [Mesamatrix](https://mesamatrix.net/))
* Vulkan/WSI (hopefully...)

```lisp
    :depends-on (#:osicat
                 #:cl-drm
                 #:cl-gbm
                 #:cl-egl
                 #:cl-opengl
                 #:cl-wayland
                 #:cl-libinput
                 #:cl-epoll
                 #:cl-xkb
                 #:cl-freetype2
                 #:cl-glfw3
                 #:3d-vectors
                 #:3d-matrices
                 #:dlist
                 #:bordeaux-threads
                 #:lparallel
                 #:trivial-timers
                 #:babel
                 #:swank
                 #:str
                 #:easing
                 #:skip-list)
```


## The Roadmap

TODO

### The Testing Configuration

Lenovo Miix 720:

```
GL Vendor: Intel Open Source Technology Center
GL Renderer: Mesa DRI Intel(R) HD Graphics 620 (Kaby Lake GT2)
GL Version: OpenGL ES 3.2 Mesa 17.3.3
GLSL Version: OpenGL ES GLSL ES 3.20
```

`dmesg | grep drm`
```
[drm] Memory usable by graphics device = 4096M
```

`lspci -v`
```
00:02.0 VGA compatible controller: Intel Corporation HD Graphics 620 (rev 02) (prog-if 00 [VGA controller])
    Subsystem: Lenovo HD Graphics 620
    Flags: bus master, fast devsel, latency 0, IRQ 123
    Memory at d7000000 (64-bit, non-prefetchable) [size=16M]
    Memory at b0000000 (64-bit, prefetchable) [size=256M]
```

`lshw`
```
 *-memory
      description: System Memory
      physical id: 2
      slot: System board or motherboard
      size: 16GiB
    *-bank:0
         description: SODIMM DDR4 Synchronous 2133 MHz (0.5 ns)
         product: M471A1K43BB1-CRC
         vendor: Samsung
         physical id: 0
         serial: 00000000
         slot: ChannelA-DIMM0
         size: 8GiB
         width: 64 bits
         clock: 2133MHz (0.5ns)
    *-bank:1
         description: SODIMM DDR4 Synchronous 2133 MHz (0.5 ns)
         product: M471A1K43BB1-CRC
         vendor: Samsung
         physical id: 1
         serial: 00000000
         slot: ChannelB-DIMM0
         size: 8GiB
         width: 64 bits
         clock: 2133MHz (0.5ns)
 *-cache:0
      description: L1 cache
      physical id: 6
      slot: L1 Cache
      size: 128KiB
      capacity: 128KiB
      capabilities: synchronous internal write-back unified
      configuration: level=1
 *-cache:1
      description: L2 cache
      physical id: 7
      slot: L2 Cache
      size: 512KiB
      capacity: 512KiB
      capabilities: synchronous internal write-back unified
      configuration: level=2
 *-cache:2
      description: L3 cache
      physical id: 8
      slot: L3 Cache
      size: 4MiB
      capacity: 4MiB
      capabilities: synchronous internal write-back unified
      configuration: level=3
 *-cpu
      description: CPU
      product: Intel(R) Core(TM) i7-7500U CPU @ 2.70GHz
      vendor: Intel Corp.
      physical id: 9
      bus info: cpu@0
      version: Intel(R) Core(TM) i7-7500U CPU @ 2.70GHz
      serial: To Be Filled By O.E.M.
      slot: U3E1
      size: 2356MHz
      capacity: 3500MHz
      width: 64 bits
      clock: 100MHz
      capabilities: x86-64 fpu fpu_exception wp vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush dts acpi mmx fxsr sse sse2 ss ht tm pbe syscall nx pdpe1gb rdtscp constant_tsc art arch_perfmon pebs bts rep_good nopl xtopology nonstop_tsc cpuid aperfmperf tsc_known_freq pni pclmulqdq dtes64 monitor ds_cpl vmx est tm2 ssse3 sdbg fma cx16 xtpr pdcm pcid sse4_1 sse4_2 x2apic movbe popcnt tsc_deadline_timer aes xsave avx f16c rdrand lahf_lm abm 3dnowprefetch cpuid_fault epb invpcid_single pti tpr_shadow vnmi flexpriority ept vpid fsgsbase tsc_adjust bmi1 avx2 smep bmi2 erms invpcid mpx rdseed adx smap clflushopt intel_pt xsaveopt xsavec xgetbv1 xsaves dtherm ida arat pln pts hwp hwp_notify hwp_act_window hwp_epp cpufreq
      configuration: cores=2 enabledcores=2 threads=4
```
