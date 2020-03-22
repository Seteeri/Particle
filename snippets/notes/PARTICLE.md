Particle
========

The name comes from the idea that in Lisp there exists two types of data: atoms
and lists. The word atom means "indivisible" and was chosen to state their
fundamental nature. However, in our system, we breakdown atoms into their
individual bytes and bits. Using a loose analogy of physics, the name particle
is derived as we break down the Lisp atoms into their subcomponents.

It also comes from the way the cons cells are rendered, like a particle engine
uses the same model/instancing.

Particle's goals:
* expressiveness
* dynamicness
* interactiveness

Combination:
* Mindmap - VYM
* Notes - Orgmode
* TODO Lists - Evernote
* Project Management - Trello
* Tags - Tagspaces
* Messaging - Gitter
* Non-hierarchial FS - WinFS
* CMS - Wiki


Long Term Goals:

* Core dumping - support image-based editing like traditional Lisps
  * PicoLisp has external symbols
* Mini PicoLisp -> WASM (32-bit)
* PicoLisp (ASM) + JIT
  * Write PicoLisp interpreter in PicoLisp and then write JIT in PicoLisp
  * Poss port to RPython?

Tentative Ideas:

* PyPy Project Ideas - http://doc.pypy.org/en/latest/project-ideas.html
  * Lua SSA-IR: http://wiki.luajit.org/SSA-IR-2.0
  * "Sea of Nodes": https://darksi.de/d.sea-of-nodes/
    * LuaJIT uses alternative structure

## Why

Initially, Common Lisp was used but more control was needed over the GC. I
Modifying SBCL's GC was one possiblity, but would require a significant
investment in understanding the codebase and the runtime/compiler implementation

This also lead to the realization of the definition of a Lisp. In Lisp, there
exists only one fundamental data structure that everything else is built upon -
the cons cell. The cons cell is further composed of two pointers, also known
as CAR and CDR. With this fundamental principle, the entire system can be
visualized.

Last but not least, permissively licensed code.

### LISP Defined

* Lisp is based on s-expressions which is used for code and data (homoiconicity).
* S-expressions consists of an atom or a cons cell.
* Atoms are either symbols or numeric literals.
* Nested cons cells form lists.
* All data is either an atom or a cons cell.

# Particle Defined

Starting with hardware memory cells

C/ASM: pointers/memory addresses

LISP: s-expressions = two pointers/memory addresses

Thus it only makes sense that the GUI is made of s-expressions = lists = binary trees -
this is the definition of Protoform.

*Note to self: research relationship between binary trees, general trees,
directed acyclic graphs*
https://stackoverflow.com/questions/16860566/s-expression-for-directed-acyclic-graph

Store DAGs as binary trees?

## TODO

   * Check heap size every frame, warn if large...
     * Or after every message, if it starts to reach capacity, GC, resize
       do next free

   * Refactor serialization [WIP]
     * Be able to send raw bytes [Done]
     * (mc src dest sz)+bytes [Done]
   * Synchronize Render with Model [Done]
     * Render must wait for Model - otherwise anims may skip frames
     * Double buffer render
       * GPU might be rendering while we are waiting
   * Test vertex serialization [Done]
     * Move vertices to separate process to reduce pressure on GC
     * Model can push/pull from it...or model holds data and controller processes it?
   * Add task manager to spread work across frames [Later]
   * Test projview + input handling [Done]
     * Use directional keys to move camera
   * Test vertex serialization [Done]
     -> Create diagrams for metrics calculations so easier to understand/visualize
     -> Define mathc path in top level or in wrapper?
   * Refactor out init parse args [...]
   * Refactor IPC connection management [Mon]
     * Remove direct epoll usage in process - isolate in IPC [Done]
       * Add epoll method to reuse across processes [Done]
       * Move epoll data to IPC [Done]
     * Refactor IPC init [Done]
       * Pass in list of server pairs [No]
       * Integrate server sockets into conns [Done]
       * Simplify ID/FD lookup [???]
     * Dispatch on both server and outgoing sockets [Done]
   * Implement workers [Done]
     * Ctrl connects input/render/model?
       * Handles synchronous mode (delay)
     * Workers only connect to ctrl/render/model
       * Recvs work from ctrl
       * Recvs frame from render and sends data to render (async mode/drop)
       * Gets data from model and sends data to model (updates/sync)
   * Test multiple controllers with multiple vertices mutating simultaneously [Done]
     * N process will move N vert x dist
   * Refactor [Done]
     * Move xkb to ctrl and pass events to worker [Done]
     * Respond to window resizing [Done]
     * Implement proper exit [Done]
       * Render send exit key also

   * Rewrite IPC
     * Abstract common functionality among worker/model/render conns [Done]
     * Serialize entire object for all [Done]
     * Model needs to write data [Done]
     * Make epoll event like timerfd [Done]
     * Rewrite flush-msgs [Done]
       * Use doubly linked list
       * On EPOLLIN, append new data to tail
       * Read as many as msgs from head until insufficient data
     * Use single header [Done...necessary?]
       * Must refactor flush?
     * Re-use head list [Skip]
     * Fix caller error handling for send/recv msg [Done]
       * If looping, do "while (setq msg (recv-msg))"
     * Fix data sending integrity; warn and block [Done]
       * Add loop to send until empty - add to ipc
       * If blocked, then render or model is lagging
     * Add ability to push/pull range and/or batch [Sun]
       * Batch: process msgs as list [Done]
       * Rx: pass adtl args
         * Add handlers to processes
       * For initial loading, could use fork or read default values from file
     * Render should only send msg if client rdy otherwise send will block
       * Worker can choose to have late msgs dropped - send function to render
     * Start tracking memcpy performance (bytes/millisecond)

   * Loading [Done]
     * Have model save symbols to file
     * Worker will then load file

   * Bindings [Done]
     * Ensure mods not pressed also
     * Sort time
     * Repeat keys

  * Primary Representation [Done]
    * Implement drawing s-exp [Done]
      * Draw space (" ") by itself draw box or draw with quotes
        * For now draw quotes on all strings
      * Create draw interface where a matrix can be specified?
      * Follow pp style?
        * We follow indent, poss use rule len list > 12?
    * Integrate with particle [Done]
    * Handle modifying lists and layout
      * Append item [Done]
      * Remove item [Done]
        * Skip removing NIL at end of list since it would still point to NIL
       * When deleting list, skip deleting NIL [Done]

  * Pointer Foundation [Done]
    * Refactor pointer/selector to be a particle/symbol whose CAR is the tgt [WIP]
      * Implement p* - master ptr [Done]
      * Refactor cmd-char etc. to update p* [Done]
      * Replace current marker with p* symbol
        * Refactor initial particle creation
  
  * Pointer Selection [WIP]
    * Implement prev/next, up/down when list creation implemented
    * As user moves, ptr changes are stored in command history [WIP]
      * Store in command history
      * To move around, get last input
    * History can be purged or saved to disk
  
  * Poss to mix drawing lists horizontal/vertical if there are arrows
  or very few expections like with pointers
  
  * Misc [Done]
    * Refactor symbol particles [Done]
      * Data stores symbol
      * A/CAR = NIL
      * B/CDR = VAL  
    * Implement master particle list [Done]
      * Need this to store particles of globals
        * This is the exception, like internal table
      * Store as assoc list...
        * Data is the key?
        * Value is the particle
      * Or solely so particles don't get deleted
        * Poss move vertices into it?
    * Rewrite cmd-del [Done]

  * Basic List Handling
    * Add cmd for NIL [Done]
      * Lift fn from cmd-make-char and cmd-make-nil -> app-data
    * To enter NIL list, move pointer beneath it [Done]      
    * Fix:
      * Not moving to top list automatically when deleting sublist
        * Or option?
      * Creating nested empty lists
    * UI
      * Alt+Backspace = Exit list
      * Alt+Enter = Enter list
      OR!
      * Alt+WASD = for items
        * Or Q/E to enter/exit list
      * Alt+IJKL = for lists
        * Hor - next/prev nearest list
        * Vert - enter/exit nearest list
              
  * List Layout
    * Implement basic lay property [WIP]
      * Lay horizontal [Done]
      * Lay vertical [Done]
      * Lay mixed [Done]
        * Y to X [Done]
        * X to Y [Done]
    * Interpret lay structure [Later]
  
  * Separate layout from generating particles [Done]
    
  * Integrate layout with generating particles (cmds)
    * Create function to calculate bounds of a pair [Done]
    * Position new particle based on the ref's layout [Done]
      * Must calculate previous item [Done]
    * Move pointer semantically fwd and back [Done]
    * Implement doubly-linked particles [Sun]
    * Fix sublist handling [Done]
    * Test nested lists + NILs [Done]
    * Refactor del [Done]
    * Support y layout
    * Support mixed layout
    
    * Fix lay-atom function - why checking b?
    * Traversal follows layout  [Sun]
    * Handle random edits [Sun]
      * Update subsequent items in list
      * Update superlist
        * This can result in slow updates, so lists should be done outside 
          the list then merged in
    * Refactor pack cmd [Sun]
      
    GOAL: Able to build test tree

    * Refactor del> to pass ignore flags during recurse [???]    
    * Refactor layout and mov functions [???]
      * Restrict mov> functions to be used by layout
    * Refactor pointer to point to last (NIL)
      * More consistent with entering a list    
    * Add cmd to swap/toggle layouts X/Y
      * Must relayout car/cdr
      * Lists of lists are Y
      * Nums and Strs are X
      * Other syms...default to Y also, but should be X also
      * Refactor list UI

    * Refactor other cmds
      * Implement split space
    
    * Add skip car/cdr for layout fn
    * Cur/part relative position functions
    * Refactor gen layout use -> move it
    * Make fn: mov-cur X/adv Y/nl          
    * Handle missing bounds in metrics for space and ctrl chars
    
  * Refactor [Mon]
    
  * Optimize ipc to batch messages, flush etc. [Tues]
    * Instead of directly sending msgs, put into list
    * Call flush to send all
    * Requires rewriting protocol to read multiple messages from single
    string
   
  * Move to external symbols
    * Move verts.bin -> db file
    * Can test multiple workers pull/push database
          
  * Use special printing for control characters like enter etc.
    * newline ("^J"), return ("^M") or TAB ("^I")
    
  =-----------------------------------------------------------------------------

  * Instead of drawing lines to connect nodes, draw generic grid in bg
  to guide user

  * Implement/Draw Primary Lists:
    1. Main etc.
    2. Pointer
    3. Binds
    4. Command/History

  * Supporting Structures
    * Implement AABB
    * Implement treemap as alternative to graph?
      * Sunburst, conetree, etc.
      * See 'A Visual Survey of Tree Visualization'

  * Pointer
    * Instead of calling p#, use the fn+#
      * Default is append-0
    * Implement semantic move
    * Insert inbetween
      * Redraw cdr
    * Create pointer list at top
      * p0 is special or maybe call it T
        * Separate controls to move it but same functionality
      * To switch between pointers, move p0
               
               p0
      lst-ptrs p1 p2 p3 p4 p5

      p1   p2    p3 etc
      qwerasdfzxcvr
      
      * p0 -> symbol = mov one
      * p0 -> list   = mov all (relative) 
                       or apply fn to all ptrs
      * use rot list, only use first

  * Timestamp optional
    * GPS optional
    * Really just tags...except expected/defined tags on all items

  * Notation
    * Circular lists -> dot instead of ] or Left-arrow
    * For X layout: Down-arrow Dot Right-arrow
    * For Y layout: Right-arrow Dot Down-arrow

  * Design different views/presentations
    * Use familiar setups from Office and other note-taking apps etc.
    * Ex: Tags
      * For class, use tag property = list of tags
    * Ex: Outline
      * Nested lists of objects
      * or create class, properties defined as a hierarchy or nested lists
        * (=: l1 (I))
        * (=: l2 (1))
        * (=: l3 (A))
        * (=: l4 (i))
    * Ex: Table
      * Create class, user defines properties

  * Auto-pack strings [Later]
    * Upon char, make string, until non-char
      * Do NIL to create new list
    * When user marks part of string, break it down by spaces into words/chars
    * Mark word/chars with tags

  * Lists
    * Differentiate colors for tagged and untagged data?
      * Show number of tags?
    * Store origin in Particle
      * Update on vertex update -> fn called on particle
    * Don't draw NIL at end of list
      * Pointer points to last item
    * Newline only applies if current/prev is a string
    * Add specific cmd to replace any with list/NIL
      * Default is to append/insert

  * Refactor gen-particle to use globals instead of passing

  * Create special command to create pointer symbols [Later]

  * FPS - do in render
  
  * Load own code as data
    * Really draw relevant internal symbols from table

  * Swap part
    * (con (nth n) (new)) to change CDR
    * (con (new) (nth n+1))

  * Draw lines
    * One node per line segment
    * Scale node to fit line

  * Split screen
    * Add commands to do this

  * Map keys to grid on screen
    * 26 squares if using plain alphabet
    * User can use keys to manuever

  * Rename namespace gl -> gles

  * Error Handling
    * Set * Err to (quit)
    * On error, print msg and return to top-level

  * Explore file browsing using built in functions
    * For example, get a list of files in directory -> return list of strings of
    files

  * Test multiple workers
    * Need data sync on model side to broadcast updates

  POST DEMO:

  * Implement Wayland - BASIC!
    * Setup tiles for 6 windows = 3 col, 2 row
    * Proof of concept working with eval already

   * Implement drawing cons cells/box-ptr diagrams [Later]

   * Test compute shaders
     * Rotate all vertices
     * Could use shader then pull data back?
       * Syncing becomes an issue -> at that point, keep transforms on GPU side
       * Vertex data simply contains an offset to the struct

   * Show commands
     * Add disassemble functionality
     * Render frame time?

  https://stackoverflow.com/questions/287871/how-to-print-colored-text-in-terminal-in-python

  * Implement framebuffers
    * Render to target
    * Allows us to create screens/viewports
    * Create viewport from current view

  * Write docs

  * Basic animations
    * Easing functions
    * Fades
    * Use compute shader on large amount
      * For demo max verts

  * Use touch to modify lists
    * Ability to pull items out
    * Requires spatial index, unproject
      * Port r-tree from CL

  * Screenshot
    * glReadPixels

  * Implement DRM backend

  * Implement Reingold-Tilford Drawing Algorithm (stratified/hierarchial/pyramidial) [Later]
    * Radial also
    * Hyperbolic

  * Unscheduled Stuff

   * Replace serialization with pr/rd/wr [Later]
     * Use pr/rd/wr/bytes
       * (call 'mkfifo "a" "b") + (open "a"/"b"...) + (in/out "a"...)
         * /proc/sys/fs/pipe-max-size
         * Main -> Pipe -> Socket -> Socket -> Pipe -> Main
           * 6 copies total
         * Sender
           * Use lisp to pr data to pipe
           * Use C to open named pipe fd and read into socket
         * Recver
           * Use C to open named pipe fd and write from socket
           * Use lisp rd to get objects
       * Later use plio as library
     * Format - create sep msgs for model/render : objcpy/memcpy
       * sz-msg, sz-sexpr bin-sexpr sz-dat bin-dat
       * Worker/Model: Obj - Obj
         * Serialize to obj (sexpr:msg + dat)
           * Lisp object
       * Worker/Render: Obj - Ptr
         * Serialize to ptr (sexpr:msg + dat)
           * C struct
         * Write bytes from list to gl ptr through for+bytes
           * Memcpy is must faster
     * Optimizations
       * If modifying large numbers of objects, particularly numerical calcs,
         use compute shader, else use lisp functions, or C lib
         * Eventually this would be the bottleneck
       * Send deltas only instead of entire object
         -> Need not serialize everything - add ability to serialize only certain members
         * Must define protocol/msg
       * Use LZO or LZ4 data compression

  * CTRL MUST SEND MSGS TO ALL CLIENTS
    * Maintain queue per client
    * Add to all clients

  * C wrappers
       * Move xkb class into wrapper
       * Use c-<fn> for native wrappers
       * Use <fn-lisp> for lispy wrappers
       * Check link status
       * Refactor namespaces
         * posix
         * mathc and more...
       * Refactor li into subfolders [?]
       * Remove print msgs from gl

   * Try proportional fonts with kerning
     * Change blend mode?

   * Is crashing due to window resizing? or msg 'C vs 'S...
     -> Seems to have fixed it

   * Tasks uses DAG/ptrees [Later]
     * Built on top of cons cells...
     * To make task wait for another task, it needs a task ID
     * If dependency graph used, need a way to continously modify it
     * When a DAG is executed, store the output of the entire graph
   * Tasks can be submitted as parallel or serial (waits for current task to complete)
     * ?For parallel tasks, can cache results, so that if after one of the data
       is modified while the task was running, it can run it again up to the point
       the data was modified?
     * Otherwise, have to lock data while it is being used

   * Optimize GL struct [Later]
     * 3 programs to render 3 vertex types
       * Glyphs
         - Index/uniform UVs (-64 bytes)
         - Index/uniform RGBAs?
         - Also scaling/rotation
         - Pass Vec3s; calc matrices on GPU
       * Pixel (RGBA/no-texture)
       * Arbitrary Texture
     * Min struct size = Vec4(X, Y, Z), Short/Texel-Offset, Short/Glyph-Index = 16 bytes
       * + glyph-index, can be derived from texel-offset but seems unecessary work
       * @ 1 GB/16 Bytes = 67 108 864 chars
         * For 208 Bytes = 5 162 220 chars
       * Position can be calculated from a single point + offset
         * Reduce size to 8 bytes

  * Port dependencies; pull code from rosetta code as baseline
    * nanomsg - IPC; use existing PicoLisp library
     3d-vectors/3d-matrices - use C, either gcc or so; native?
     https://github.com/recp/cglm [arch linux package]
     https://github.com/felselva/mathc
     https://github.com/Kazade/kazmath
     https://github.com/datenwolf/linmath.h
     https://github.com/HandmadeMath/Handmade-Math
     https://github.com/scoopr/vectorial
     https://sleef.org/
     easing - (with mathc)
     spatial-trees - r-tree (port this)
     lparallel - ptree specifically (port this)

  * Note 32 MB = 8 ms to mark/sweep
    * = 2,000,000 cons cells -> Test this with loop/cons/heap
    
## OTHER STUFF

* Benchmark/Profile
  * Tests
    * Sorting Algorithms
    * String Manipulation

* Overall Strategy
  * Build prototype in PicoLisp
  * Port PicoLisp ASM to RPython -> Free JIT!
    * BIG ISSUE! Python VM is stack-based...
      * Or not an issue? As long as cons cell can be defined...
      * Would be interesting to see stack based vs register based
    * Port miniPicoLisp? 32-bit
      * Could port to WASM
    * Port 64-bit C code
      * Build emulator...see questions
    * Review Build Your Own Lisp before reading PicoLisp code
  * Create JIT for PicoLisp
    * Use C version, optimize, work on JIT
    * See Emacs JIT, Pixie JIT, CLISP JIT for examples
    * Register based so use DynASM might work
  * If support discontinued, compile to C, optimize, start from there

* Mailing list:
  * WASM backend/implementation
  * Image dumping -> RD/WR/PR + STR/ANY
    * Default DB provide this?
    * Dump heap and reload heap
  * Struct does not have 'H' for shorts?
  * Suggestion to docs with more figures of symbols cons trees in diff scenarios
  * (= (cons 0 NIL) (box)) returns NIL when it should be true
    * Ptr or struct equality?

OLD

Particle
========

*Image to Be Inserted*

> The Grid. A digital frontier. I tried to picture clusters of
information as they moved through the computer. What did they look like?
 Ships? Motorcycles? Were the circuits like freeways? I kept dreaming of
  a world I thought I'd never see. And then one day . . .

Particle is a personal knowledge database or personal information manager
implemented through a 3D Lisp UI. It is the realization of my vision of a 
programmable UI to replace the desktop paradigm - a way to map our thoughts into
the computer.

It integrates various computing and UI concepts from CLIs, shells, REPLs, 
notebooks, WMs/DEs, creative coding, mindmapping, note-taking into a single 
interface that can provide convergence across multiple devices such as 
desktops/workstations, laptops/tablets and smartphones/devices where information
can easily and literally flow between nodes. This is one step towards that.

The goal of Particle is to create a Lispy userspace, eventually replacing the
init system and encompassing all layers above that. The first step is to
maintain backwards compatibility with the conventional desktop (Wayland) and
the C world while rewriting/replacing parts in Lisp, possibly into an actual
Lisp OS.

PilOS provides a minimal starting point; however, there would be many hurdles to
overcome. Initially, a SOC could be targeted as a starting point. 

The computing landscape has changed significantly since the days of Lisp 
Machines so it begs the question as to how useful Lisp at the OS level would be
today in contrast to past Lisp Machines. I believe another attempt is warranted
albeit with a different approach taking advantage of today's computing power
and ubiquitiousness.

The target audience consists of programmers, power users and the like, and
"busy" people.

## The Principles

Particle maximizes the following principles:

* Programmability
* Expressivity
* Dynamism
* Simplicity/Minimalism

These principles are shared with the underlying programming language (PicoLisp)
to create a consistent *understandable* system.

## The Inspiration

* Primary Inspiration:
  * Transformers (Beast Wars), Digimon, Reboot, Tron - bridging the divide
  * Compiz - 3D desktop effects
  * Blender - 3D editor, extensible UI, keyboard driven
  * Firefox Tree Style Tab addon  
  * Lisp - the programmable programming language
  * Emacs/Vim/StumpWM - consistency, extensibility, text and keyboard driven

* Secondary Inspiration:
  * McCLIM - "presentations" - (Convergent evolution I suppose ;))
  * Uzbl/Conkeror - numbered links for navigation
  * Unreal Blueprints - nodal system
  * EagleMode - ZUI system
  * The Humane Interface by Jeff Raskin
    * Elimination of modes
    * Persistence
    * Unlimited undo
    * Incremental search
    * Elimintation of applications -> commands
    * Redesigned file hierarchies
    * ZUIs
  * Douglas Engelbart - "The Mother of All Demos"
  * Xerox PARC - pioneered many modern computing elements and paradigms
  * Sir Tim Berners-Lee - WWW
  * Paul Graham, Peter Norvig - applied Lisp
  * Brett Victor - Inventing on Principle
  * Zach Bean - quicklisp
  * Robert Strandh - CLOSOS/LispOS
  * Chris Schafmeister - Molecular Metaprogramming
  * Evernote - Stepan Pachikov wanted to remember everything
  * Oberon OS
  
## The Interface

* Built on lisp data structures - lists/cons, symbols, numbers
* 3D orthographic "nodal" environment - "turtles all the way down"
* Primarily keyboard driven interface
* Non-destructive; undo/redo capabilities
* Non-blocking UI - user always aware of computer status
  * User can choose take risk to block
* Wayland provides conventional desktop
* Solarized color theme as default

## The Infrastructure

*See ARCHITECTURE.md*

## The Roadmap

Core:
1. Interactive Core (REPL)
2. Wayland Integration
3. Widget Toolkit (Vertex-based)
4. Data Types (Desktop Functionality)
   1. Open Formats
      * Native
        * BMP, GIF, JPEG/2000, PNG, WebP
        * FLAC, MP3, Vorbis, Opus
        * Theora, Dirac
        * Ogg, MKV, WebM
        * glTF, COLLADA
      * Other formats use FFMPEG
    2. Processing Libraries
       * Port GEGL, libmypaint
      
Lispify:

1. Native Web Browser
   * Web remains accessible through Wayland/DE
   * WebKit Integration - Hmm, port to Lisp?
   * JavaScript Engines - Embed QuickJS, Ducktape or Jsish
2. Port Userspace Tools/Libraries
   * Toybox

Personal Extensions
1. Per-object vector motion blur (personal favorite)
2. Power mode

Future Ideas
* Tiled forward rendering (Forward+)
  * Clustered -> volumetric forward shading
* Augmented reality through OpenCV
* Convergence...
* PilOS bootloader...

## The Requirements

* OpenGL ES 3.2+ (OpenGL 4.3+) (See [Mesamatrix](https://mesamatrix.net/))
* Vulkan/WSI (hopefully...)

## The Installation

1. Clone this repo
2. ...

## The License

Permissively licensed
