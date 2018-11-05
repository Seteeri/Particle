Protoform
=========

[![Alt text](https://img.youtube.com/vi/BemmZaOyzbk/0.jpg)](https://www.youtube.com/watch?v=BemmZaOyzbk)

> The Grid. A digital frontier. I tried to picture clusters of 
information as they moved through the computer. What did they look like?
 Ships? Motorcycles? Were the circuits like freeways? I kept dreaming of
  a world I thought I'd never see. And then one day . . .

Protoform is a Common Lisp 3D nodal/graph IDE based on DRM-KMS and 
Open GL ES designed around working in a freeform/prototyping 
envirornment with data across different domains, analagous to a desktop
environment providing default programs for common tasks.

It is the implementation of my vision of a more consistent programmable
user-interface to replace the desktop. It aims to integrate concepts 
from the REPL, CLI, WM/DE, and common data/file types into a single 3D 
interface.

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
space, effectively making Protoform a "meta-manager" around the desktop.
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

* Primary Inspirations:
    * Blender - 3D, multi-domain dataset, flexible UI
    * Emacs - consistency, extensibility, text and keyboard driven
    * Compiz - 3D desktop effects
    
* Secondary Inspirations:
    * Uzbl/Conkeror - numbered links for navigation
    * Unreal Blueprints - nodal system
    * Minecraft - expressiveness
    * EagleMode - example of a ZUI
    * OpenDoc - early proprietary attempt by Apple to create compound documents
    
* Personal Influences:
    * Gypsy by Larry Tesler
    * The Humane Interface by Jeff Raskin - commands/transformers    

It was designed to address limitations I've personally encountered in
the quest for a more efficient workflow; the main limitation being, 
confinement to the desktop/window metaphor and the staticness of widgets.

With the growth of open-source, the new direction of Wayland, 
fragmentation of the desktop, and increasing computing resources, I 
believe this presents an opportunity to redefine the computing 
environment. Protoform is not an entirely new idea, but a different 
attempt to build upon the success and failures of those before.

The initial inspiration came from Blender's multi-domain toolset and 
later from Emacs's text buffer oriented system. The idea was to fuse the
ideas of Blender, Emacs, and other programs to abstract a consistent,
extensible, and ultimately efficient UI that could incorporate 
tried-and-true concepts like the command-line interface but also 
visualize the different types of data today. 

> Successful programming is all about managing complexity.

It is a fusion between expressive creative coding through frameworks 
like Cinder, openFrameworks or Processing, and more convergent tools such
as Blender and Emacs, and sandbox type games such as Minecraft.

One major anti-pattern frequently encountered is the limitations created
by programs extending functionality through a scripting language and
embedding an interpreter such as Lua or Python, which as more 
functionality is developed, leads to interoperability issues, where data
and code must be managed in two different domains leading to a
"desert of complexity and duplication".

The goal is not to create virtual realities for HMDs, although that
naturally remains an orthogonal possibility.

## The Interface

* 3D orthographic non-windowing environment
* Consistent nodal environment - "turtles all the way down"
* Primarily keyboard driven interface
* Wayland extension provides conventional desktop
* Undo/revision control/non-destructive editing

## The Architecture

* Two process system
* Task-based parallelism
* Tiled forward shading engine (Forward+)
  * OpenGL ES 3.2+
  * Compute shaders
  * AZDO Techniques
    * Persistent mapping
    * Instanced drawing
    * Indirect drawing
      * Multi-indirect - not yet implemented in OpenGL ES API
  * Fully programmable vertex pulling
    * Texture buffers/UBOs/SSBOs -> gl_VertexID+gl_InstanceID
  * Separate shader stages and programmable pipelines

## The Roadmap

Core Functionality - Version 0.1
1. OpenGL infrastructure - DONE
   1. Framebuffers, render to texture, etc. - WIP
2. Pango text rendering - DONE
3. MSDF text rendering - DONE
4. Drawing nodes, lines - DONE
5. Task-basked parallelism - WIP

Core Extensions - Version 0.2
1. Numbered links for navigations
2. Wayland compositing
3. Undo system

Personal Extensions - Version 0.3
1. FFMPEG for media - images, video, audio
   * GEGL another option
2. Per-object vector motion blur (personal favorite)
3. Portals

Future Extensions
1. Tiled forward rendering (Forward+) including lights
   * Future: Implement clustered then volumetric forward shading
2. Native web browser engine

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
		 #:cl-xkb
		 #:cl-pango
		 #:cl-cairo2
		 #:cl-glfw3
		 #:3d-vectors
		 #:3d-matrices
		 #:cl-digraph
		 #:cl-digraph.dot
		 #:usocket
		 #:pango-markup
		 #:bordeaux-threads
		 #:inferior-shell
		 #:trivial-timers
		 #:str
		 #:easing
		 #:lparallel
		 #:skip-list)
```

## The Installation

...

## The Hardware

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
