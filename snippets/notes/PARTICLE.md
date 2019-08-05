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

   * Implement bindings in worker [Done]

   * Implement string/atom/list, eval functionality [Mon...]
     * Create particle for pointer
     * Atoms
       * Pack (Chr->Str)
       * Intern (Str->I-Sym)
       * Name+Quote or Sym+Quote (I-Sym->T-Sym or get sym name)
         * (= (sym 'a) (any 'a) 'a "a") -> T
       * ALT+NUM to produce numbers and NUM to produce chars and Format to convert
     * Lists
       * Cons - create cons from last two objects from timeline
     * Eval
       * Test functions on data
       * Only need pack and backspace/delete (pop off/GC)
     * Timeline
       * Undo
       * Redo
       * If undo occurs and there is CDR, branch
       * Dumping the heap - make external symbols?

   * Create default environment
      * Draw pico namespace
      * Draw pico symbols
      * Draw timeline
      * Draw lists downward

   * Load source code
     * Turn into strings
     * Then test symbols
       * Symbols that already exist - get existing

   * Test compute shaders
     * Rotate all vertices
     * Could use shader then pull data back?
       * Syncing becomes an issue -> at that point, keep transforms on GPU side
       * Vertex data simply contains an offset to the struct

   * File browser - most basic functionality
     * Wrap call
     * Use OS commands: dir, dirname, cd, info, path
     * Functionality must exist for manipulating output
       * Create file objects?

   * Implement Wayland - BASIC! [Thurs/Fri]
     * Setup tiles for 6 windows = 3 col, 2 row
     * Proof of concept working with eval already

   * Show commands
     * Add disassemble functionality
     * Render frame time?
     
   * Test multiple workers
     * Need data sync on model side to broadcast updates

   * PRE-RELEASE - REPL - SEPT 1
     * CLEAN UP
     * RECORD DEMO
     * ANNOUNCE ON MAILING LIST, LATER REDDIT

   POST DEMO:

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

* Benchmark/Profile
  * JIT Melee:
    * Python (PyPy)
    * Ruby (Truffle?)
    * Erlang
    * Lisp (PicoLisp)
    * Lua (LuaRaptorJIT)
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
  * Does the emulator slowdown apply to 32-bit only? Is there speed penalty on x86-64?
  * WASM backend/implementation
  * Something like disassemble function? -> No bytecode
  * Image dumping -> RD/WR/PR + STR/ANY
    * Default DB provide this?
    * Dump heap and reload heap
  * How to delete symbol?
  * Struct does not have 'H' for shorts?
  * Suggestion to docs with more figures of symbols cons trees in diff scenarios
  * (= (cons 0 NIL) (box)) returns NIL when it should be true
