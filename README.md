Protoform
=========

[![Alt text](https://img.youtube.com/vi/BemmZaOyzbk/0.jpg)](https://www.youtube.com/watch?v=BemmZaOyzbk)

> The Grid. A digital frontier. I tried to picture clusters of 
information as they moved through the computer. What did they look like?
 Ships? Motorcycles? Were the circuits like freeways? I kept dreaming of
  a world I thought I'd never see. And then one day . . .

Protoform is a Common Lisp 3D nodal shell based on DRM-KMS and Open GL 
ES. It is the implementation of my vision of a more consistent 
programmable UI to replace the desktop/application paradigm - a way to
map our thoughts into the computer. It integrates concepts from 
CLIs/shells/REPLs/notebooks, WMs/DEs, creative coding, and mindmapping 
into a single 3D interface.

User-aspect Goals:

1. Create a Common Lisp userland short of creating a pure Lisp OS/kernel
(Protoform may be considered a top-down approach rather than a bottom-up 
approach).
2. Integrate Wayland by providing the desktop as a node (plane) 
in 3D space (It is possible to allow windows to exist in 3D space 
allowing for convergence, however memcpy performance limits the number 
of windows...).

It is **not** an attempt to create a visual programming language; it is
more akin to an IDE. It is designed to edit Lisp code/data, which are 
effectively binary trees. It makes Lisp code/data first-class objects
just as conventional windows are. It takes a more pragmatic approach
to a Lisp environment by building on top of the Unix model to allow 
integrating Lisp into existing interfaces and leveraging the existing 
FOSS ecosystem and infrastructure, thus allowing more seamless adoption, 
rather than taking a more adamant fundamental approach, which would 
require a high initial investment.

The target audience consists of programmers, developers, power users and
the like.

## The Inspiration

* Primary Inspirations:
    * Compiz - 3D desktop effects
    * Blender - 3D, multi-domain dataset, flexible UI
    * Emacs - consistency, extensibility, text and keyboard driven    
    * The Humane Interface by Jeff Raskin
      * Elimination of modes
      * Persistence
      * Unlimited undo
      * Incremental search
      * Elmintation of applications -> commands
      * Redesigned file hierarchies
      * ZUIs as an alternative
    * Robert Strandh - CLOSOS/LispOS
        
* Secondary Inspirations:
    * McCLIM - central concept of "presentation types"
    * Uzbl/Conkeror - numbered links for navigation
    * Unreal Blueprints - nodal system
    * EagleMode - ZUI system
    * OpenDoc - early proprietary attempt by Apple to create compound documents
    * LightTable - drafting table metaphor
    * Minecraft - expressiveness
    * Xerox Parc - ...
    
* Personal Influences:
    * Sir Tim Berners-Lee - WWW
    * Brett Victor - Inventing on Principle
    * Chris Schafmeister - Molecular Metaprogramming
    * Paul Graham, Peter Norvig - ...
    * Zach Bean - quicklisp

It was designed to address limitations I've personally encountered in
the quest for a more efficient workflow; the main limitation being, 
confinement to the desktop/window metaphor and the staticness of widgets.

The chances of a Linux *desktop* are virtually nil. Existing Windows and
MacOS desktops are performant enough that there is no compelling reason
to switch to a Linux desktop for the average user - that market is done
and settled. The price comparison is a non-issue since any consumer 
computer will come with the OS already installed. Essentially, the 
headache of learning a new system and new programs (among other FOSS 
issues) is not worth it as it provides no significant technological 
advantage to existing commercial desktops. 

The previous statement with more obvious wording: Windows and MacOS 
users will not adopt a desktop that functions exactly like their current 
desktop (see book "Diffusion of Innovations" by Everett Rogers). GNOME,
KDE, Cinnamon, MATE, Xfce, etc. provide productivity enhancements but 
at the end of the day, it's just another desktop and thus the same 
underlying model for interacting with a computer.

**The way for a future FOSS system does not lie with the desktop but with
the computing needs of tomorrow.**

If the theory that FOSS development grants it advantages over proprietary 
development, then its innovative nature should allow it to create an 
even better system; sadly, the wheel keeps being reinvented. I believe 
current desktops are on-par in terms of functionality with Windows and 
MacOS, that it is time to move forward.

With the growth of open-source in the last decade, Wayland, 
fragmentation of the desktop, systemd debate, Moore's Law, data breaches
and privacy issues, and finally emerging AR/VR technologies, I believe
this presents an opportunity to redefine the computing environment. 
Protoform is not an entirely new idea, but a different attempt to build
upon the success and failures of those before.

There have been numerous implementations and attemps at structured or 
projectional editors going all the way back to the days of Interlisp-D 
and to the new projects being crowdfunded. However, several issues
have yet to be addressed:

* Integration with existing interfaces (the desktop)
* Homoiconic GUI
  * Requires meta-circular evaluator of language implementation + 
  homoiconicity of language design
  * The nodes must be able to edit nodes (itself)
  * The editor must be able to edit itself

The initial inspiration came from Blender and later from the robustness
of Emacs, and its strengths and weaknesses.

[**What if we could take Emacs to another level?**](https://www.google.com/search?q=what+can%27t+emacs+do+site:www.reddit.com)
* Elisp -> Common Lisp
  * Single-thread -> Multithreading
* Software rendering -> OpenGL
* Help system/manual -> Better discoverability

[A comment made by lispm, an old time lisper, on the concept of
a Lisp OS](https://news.ycombinator.com/item?id=15466124):

> I expect this will be a fairly controversial comment, so I want to 
> preface this by saying that I'm a big Lisp fan (just look at my 
> handle). Lisp is my favorite programming language. I've been using it 
> for nearly forty years. My first Lisp was P-Lisp on an Apple II in 
> 1980. And I worked on Symbolics Lisp machines in the 1990s. They were 
> very cool, but there's a reason they failed: general-purpose computing
> is infrastructure, and the economics of infrastructure are such that 
> having a single standard is the most economical solution, even if that
> standard is sub-optimal. For better or worse, the standard for 
> general-purpose computing is the C machine.
>
> Because it's general-purpose you certainly can run Lisp on a C machine
> (just as you could run C on a Lisp machine). You can even do this at 
> the system level. But Lisp will always be at a disadvantage because 
> the hardware is optimized for C. Because of this, C will always win at
> the system level because at that level performance matters.
>
> But that in and of itself is not the determining factor. The 
> determining factor is the infrastructure that has grown up around the 
> C machine in the last few decades. There is an enormous amount of work
> that has gone into building compilers, network stacks, data 
> interchange formats, libraries, etc. etc. and they are all optimized 
> for C. For Lisp to be competitive at the system level, nearly all of 
> this infrastructure would have to be re-created, and that is not going
> to happen. Even with the enormous productivity advantages that Lisp 
> has over C (and they really are enormous) this is not enough to 
> overcome the economic advantages that C has by virtue of being the 
> entrenched standard.
>
> The way Lisp can still win in today's world is not by trying to 
> replace C on the system level, but by "embracing and extending" C at 
> the application level. I use Clozure Common Lisp. It has an 
> Objective-C bridge, so I can call ObjC functions as if they were Lisp 
> functions. There is no reason for me to know or care that these 
> functions are actually written in C (except insofar as I have to be a 
> little bit careful about memory management when I call C functions 
> from Lisp) and so using Lisp in this way still gives me a huge lever 
> that is economically viable even in today's world. I have web servers 
> in production running in CCL on Linux, and it's a huge win. I can spin
> up a new web app on AWS in just a few minutes from a standing start. 
> It's a Lisp machine, but at the application level, not the system 
> level. My kernel (Linux) and web front end (nginx) are written in C, 
> but that doesn't impact me at all because they are written by someone 
> else. I just treat them as black boxes.
>
> ...But cool is not enough to win in the real world.

I imagine - what if there were something like Emacs for the non-programmer
user? Considering the way technology and computers are becoming 
increasingly integrated into our everyday lives, the bottleneck to 
leveraging computing most effectively and driving innovation will be the
connection between man and machine.

The average user is familiar with GUI widgets, but what if those were 
completely dynamic and an intrinsic property, similar to how Emacs 
allows everything to be customized through elisp. Teaching a lay user
how to program is not a trivial task as it is inherently based upon the 
user's ability to think, a reflection of the mind if you will (the 
history and usefulness of VPLs are a good study). Having observed 
Blender's nodal workflow and Unreal's Blueprint system, I attempted to 
combine those models of interaction with the analogy of LEGOs, while at 
the same time allowing for the entire system to remain hackable to 
programmers to maintain a gradual learning curve.

I do not believe it is about dumbing down programming to make it easier
to learn, but creating the right tools around programming to drive
motivation and learning.

*See writings*

## The Interface

* Top-down approach
  * First step - build the shell/DE
  * Second step - integrate the init system
  * Third step - ???
* Built around DAGs, as they are a common pattern:
  * Task management
  * Version control
  * Init dependencies
  * Garbage collection
* 3D orthographic nodal environment - "turtles all the way down"  
* Primarily keyboard driven interface
* Non-destructive editing; undo/redo capabilities
* Non-blocking UI - user always aware of computer status
* Wayland provides conventional desktop
* Solarized color theme for default

## The Architecture

* Focusing on SBCL due to best performance
* Two process system - model/view
  * Model contains a task manager that spreads tasks across frames
    * Maintains low-latency through soft deadlines
  * View's sole purpose is to draw and poll socket for commands
    * Separate process from model to minimize pressure on GC
  * Force GC when idle: model GCs after executing frame tasks, while
    view draws; view GCs after drawing frame, while waiting for model 
    tasks
    * See Roadmap for GC plans
* Rendering engine is essentially a particle system
  * Plans to pursue tiled forward shading engine (Forward+)
  * OpenGL ES 3.2+
  * Compute shaders
  * AZDO Techniques
    * Persistent mapping
    * Instanced drawing
    * Indirect drawing
      * Multi-indirect (not yet implemented in OpenGL ES API)
  * Fully programmable vertex pulling
    * Texture buffers/UBOs/SSBOs -> gl_VertexID+gl_InstanceID
  * Separate shader stages and programmable pipelines
* R-tree for spatial indexing
* Windows, MacOS, BSD support will come later

## The Roadmap

I plan on working on a concurrent/incremental GC for SBCL to reduce or
eliminate the stop-the-world pauses. I'm not very familiar with garbage 
collectors, however, I am currently studying them... ;)

Core - nodal/repl/undo functionality
1. Nodes
2. REPL
3. Version control system for nodes/objects
4. Wayland Integration 
   * Web remains accessible through Wayland/DE -> browser
5. Widget toolkit (nodal-based)   
   
Extensions (equivalent to OpenGL ARB...)
1. Media Functionality
   * FFMPEG for media - images, video, audio
   * Integrate GEGL? Image/graphicsmagick? etc...   
2. WebKit Integration
   * Build around Webkit like Next

Personal Extensions
1. Per-object vector motion blur (personal favorite)
2. Tiled forward rendering (Forward+) including lights
   * Future: Implement clustered -> volumetric forward shading

Future Extensions
* Integrate EEVEE for rendering
* Common Lisp to WebAssembly 
  * WebAssembly describes an AST ;)
* Augmented reality through OpenCV
* Convergence...

## The Requirements

* Atomic modesetting/nuclear pagefliping
* OpenGL ES 3.2+ (OpenGL 4.3+) (See [Mesamatrix](https://mesamatrix.net/))
* Vulkan/WSI (hopefully...)

```lisp
   :depends-on (#:alexandria
		 #:osicat
		 #:cl-drm
		 #:cl-gbm
		 #:cl-egl
		 #:cl-opengl
		 #:cl-wayland
		 #:cl-libinput
		 #:cl-xkb
		 #:cl-glfw3
		 #:3d-vectors
		 #:3d-matrices
		 #:cl-digraph
		 #:cl-digraph.dot
		 #:usocket
		 #:bordeaux-threads
		 #:inferior-shell
		 #:trivial-timers
		 #:str
		 #:easing
		 #:lparallel
		 #:pack
		 #:spatial-trees
		 #:trivial-benchmark)
```

## The Installation

*Will update when ready...*

## The Hardware

I will acquire better hardware in the future...

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
