Protoform
=========

### Moving to PicoLisp

[![Alt text](https://img.youtube.com/vi/BemmZaOyzbk/0.jpg)](https://www.youtube.com/watch?v=BemmZaOyzbk)

> The Grid. A digital frontier. I tried to picture clusters of 
information as they moved through the computer. What did they look like?
 Ships? Motorcycles? Were the circuits like freeways? I kept dreaming of
  a world I thought I'd never see. And then one day . . .

Inspired by:
* Beast Wars CGI-cartoon (Transformers)
* Digimon
* Compiz
* Blender
* Tron
* Emacs
* Lisp

Protoform is a Lisp 3D nodal shell based on DRM-KMS and Open GL ES. It is the 
implementation of my vision of a more consistent programmable UI to replace the 
desktop/application paradigm - a way to map our thoughts into the computer. It 
integrates concepts from CLIs/shells/REPLs/notebooks, WMs/DEs, creative coding, 
and mindmapping into a single 3D interface.

User-aspect Goals:

1. Create a Lisp userland short of creating a pure Lisp OS/kernel (Protoform may
be considered a top-down approach rather than a bottom-up approach).

2. Integrate Wayland by providing the desktop as a node (plane) in 3D space (It
is possible to allow windows existing in 3D space allowing for convergence, 
however memcpy performance limits the number of windows...).

It is **not** an attempt to create a visual programming language; it is more 
akin to an IDE. It is designed to edit Lisp code/data, which are effectively 
binary trees. It puts Lisp code/data on the same level as conventional windows 
are. It takes a more pragmatic approach to a Lisp environment by integrating 
into existing interfaces and leveraging the existing FOSS 
ecosystem/infrastructure, thus allowing more seamless adoption, rather than 
taking a more adamant fundamental approach, which would require a high initial 
investment.

The target audience consists of programmers, developers, power users and the 
like.

## The Inspiration

* Primary Inspirations:
  * Compiz - 3D desktop effects
  * Blender - 3D, multi-domain dataset, Python UI
  * Emacs - consistency, extensibility, text and keyboard driven    
  * Lisp - the programmable programming language
        
* Secondary Inspirations:
  * McCLIM - central concept of "presentation types"
  * Uzbl/Conkeror - numbered links for navigation
  * Unreal Blueprints - nodal system
  * EagleMode - ZUI system
  * OpenDoc - early proprietary attempt by Apple to create compound documents
  * LightTable - drafting table metaphor
  * Minecraft - expressiveness
  * Xerox Parc - ...
  * Evernote
    
* Personal Influences:
  * The Humane Interface by Jeff Raskin
    * Elimination of modes
    * Persistence
    * Unlimited undo
    * Incremental search
    * Elmintation of applications -> commands
    * Redesigned file hierarchies
    * ZUIs as an alternative
  * Sir Tim Berners-Lee - WWW
  * Paul Graham, Peter Norvig - ...
  * Brett Victor - Inventing on Principle
  * Chris Schafmeister - Molecular Metaprogramming  
  * Zach Bean - quicklisp
  * Robert Strandh - CLOSOS/LispOS
    
It was designed to address limitations I've personally encountered in the quest 
for a more efficient workflow; the main limitation being, confinement to the 
desktop/window metaphor and the staticness of widgets.

The chances of a Linux *desktop* are virtually nil:

1. Ecosystem/infrastructure - most popular programs are developed for Windows 
and MacOS first

2. Mobile computing - desktop segment is less important to the average user as
computing shifts to mobile devices and the cloud

3. Feature cost - open-source versions may not have feature parity with 
proprietary solutions, and thus financial cost will not outweight features; not 
to mention, predictable guranteed support for paid versions, whereas open source
depends on when developers have time to respond; maybe this can be offset when
more reliable crowdfunding platforms

4. Learning cost - learning a new system may not be worth it unless it provides
a significant technological advantage, which again FOSS programs tend to lack
feature parity compared to their proprietary counterparts; they may have a few 
significant features but it may or may not be enough.

5. Social cost - programming languages follow trends, and so do programs; FOSS 
tends to lack marketing or advertising, although social media has alleviated
some of this

6. Peformance - as long as programs run fast enough on similar platforms, 
performance is a minor issue

* Vendor lock-in/subscriptions are another possible reason to switch to FOSS (like
CERN).
* Privacy issues and data breaches (security) is another possible reason to
switch, although still not relevant enough for the average user

In other words, Windows and MacOS users will not adopt a desktop and relearn
a different set of procedures that results in the same functionality that their
current desktop already provides (see book "Diffusion of Innovations" by 
Everett Rogers). GNOME, KDE, Cinnamon, MATE, Xfce, etc. have some unique 
features, but at the end of the day, it's just another desktop and thus the same 
underlying model for interacting with a computer.

**The way for a future FOSS system does not lie with the desktop but with the 
computing needs of tomorrow.**

The question is whether FOSS development provides an advantage over proprietary
development - possibly we are starting to see the tipping point where large
companies are releasing internally developed projects to the coummunity, whether
for reasons of convenience or prestige, or actual altruism.

With everything just stated, Wayland, systemd debate, Moore's Law, and finally 
emerging AR/VR technologies, I believe this presents an opportunity to redefine
the computing environment. Protoform is not an entirely new idea, but a 
different attempt to build upon the success and failures of those before.

## The Delta

There have been numerous implementations and attemps at structured or 
projectional editors going all the way back to the days of Interlisp-D and to 
the new projects being crowdfunded. However, several issues have yet to be 
addressed:

* Integration with existing interfaces (the desktop)
* Homoiconic GUI
  * Requires meta-circular evaluator of language implementation + 
  homoiconicity of language design
  * The nodes must be able to edit nodes (itself)
  * The editor must be able to edit itself

Considering computing technology becoming increasingly integrated into our
everyday lives, the bottleneck to leveraging computing most effectively and 
driving innovation will be the connection between man and machine, and just as
important, managing the information overload occuring today. What good is having
enormous amounts of information available at your fingertips if you cannot parse
it?

The average user is familiar with GUI widgets, but what if those were completely
dynamic and an intrinsic property, similar to how Emacs allows everything to be 
customized through elisp. Teaching a lay user how to program is not a trivial 
task as it is inherently based upon the user's cognitive ability, a reflection 
of the mind if you will (the history and usefulness of VPLs are a good study). 
Having observed Blender's nodal workflow and Unreal's Blueprint system, I am
attempting to combine those models of interaction with the analogy of LEGOs, 
while at the same time allowing for the entire system to remain hackable to 
programmers to maintain a gradual learning curve.

I do not believe it is about dumbing down computers and technology to make it 
easier to learn, but creating the right tools around programming to drive 
motivation and learning.

*See writings*

## The Interface

* Top-down approach
  * First step - build the shell/DE
  * Second step - integrate the init system
  * Third step - ???
* Built around DAG/trees  - common pattern across domains:
  * HTML/DOM
  * Task management
  * Version control
  * Init dependencies
  * Garbage collection tracing
  * Gantt charts
  * Compiler internals
* 3D orthographic nodal environment - "turtles all the way down"  
* Primarily keyboard driven interface
* Non-destructive editing; undo/redo capabilities
* Non-blocking UI - user always aware of computer status
* Wayland provides conventional desktop
* Solarized color theme for default

## The Architecture

* PicoLisp due to simplicity and expressiveness
* Two process system - model/view
  * Model contains a task manager that spreads tasks across frames
    * Maintains low-latency through soft deadlines
  * View SRP: draw triangles and poll socket for commands
    * Separate process from model to minimize pressure on GC
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
* Windows, MacOS, BSD, WASM support will come later

## The Roadmap

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
* Lisp to WebAssembly 
  * WebAssembly describes an AST ;)
* Augmented reality through OpenCV
* Convergence...

## The Requirements

* Atomic modesetting/nuclear pagefliping
* OpenGL ES 3.2+ (OpenGL 4.3+) (See [Mesamatrix](https://mesamatrix.net/))
* Vulkan/WSI (hopefully...)

## The Installation

1. Clone this repo
2. ...

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
