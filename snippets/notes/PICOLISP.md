PicoLisp
=======

The purpose of this branch is to support the development of Protoform by 
adding additional necessary features while still adhering to the 
minimal/KISS philosophy of PicoLisp.

This is a branch of PicoLisp, not an independent fork, so it will remain
compatible with upstream.

Protoform's goals are to maximize:
* expressiveness
* dynamicness
* interactiveness

The last goal of interactiveness implies minimal input latency which is
the inverse of throughput. Currently, the main road block to achieving
this is the garbage collector as memory allocation is fundamental to all
aspects of a program. 

Throughput is still a concern, however, to the user, throughput is not 
directly visible and often perceived through latency. For example, a 
non-responsive computer tends to indicate it is busy (which hopefully
multi-core can address in the future). In other words, maintaining 
visual/interactive fidelity is a higher priority than CPU throughput. 

In addition, higher throughput can be addressed through C extensions, as
the Python ecosystem has shown, among other languages. As JIT compiler 
technology develops, that should improve both interactiveness and 
throughput simultaneously.

Protoform Requirements:

* Semi-automatic memory management for soft realtime
* Ability to compile to WASM
  * https://www.reddit.com/r/lisp/comments/7z7wuq/has_anyone_considered_or_started_a_project_to/

Short Term Goals:

* Semi-automatic memory management
* NNG for IPC
  * Multiple processing would implicitly give concurrent/parallel GC?

Long Term Goals:

* Core dumping - support image-based editing like traditional Lisps
  * Or is this possible through homoiconicity already?
  * Faster to bypass reader
  * Dump stack/heap
    * Reload C libraries
* Mini PicoLisp -> WASM (32-bit)
* PicoLisp (C/ASM) + JIT
  * Poss port to RYPthon

Tentative Ideas:

* PyPy Project Ideas - http://doc.pypy.org/en/latest/project-ideas.html
  * Lua SSA-IR: http://wiki.luajit.org/SSA-IR-2.0
  * "Sea of Nodes": https://darksi.de/d.sea-of-nodes/

## Why

Initially, I used Common Lisp but I needed more control over the GC. I
considered modifying SBCL's GC, but for someone who is not so familiar
with writing compilers nor language development, the codebase is 
relatively large so I needed a simpler approach (possibly I will 
reattempt modifying SBCL's GC at a later time).

Next, I considered ECL which uses Boehm GC which has incremental mode 
(and parallel collecting), however, I ran into issues enabling it 
(evalmacros kept segfaulting). In addition, after doing further research
on Boehm, more control over the GC might still be needed so it may not
have actually been the proper solution. A generational GC would still do
a full collection at some point resulting in a large spike so we'd need 
to control that behavior also.

Last but not least, permissively licensed code.

### LISP Defined

There were a few other Lisps I considered but the specifics made them
unviable, until I came across PicoLisp which seems to fit the precise 
definition of a Lisp:

* Lisp is based on s-expressions which is used for code and data (homoiconicity).
* S-expressions consists of an atom or a cons cell.
* Atoms are either symbols or numeric literals.
* Nested cons cells form lists.
* All data is either an atom or a cons cell.

# Protoform Defined

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

* Migrate Protoform to PicoLisp

  - Each process needs to run server/client
    - Can plugin in and eval to any of them

  * TODO
    * Integrate everything to last working state [WIP]
        IPC:
        -> Send messages with length first [Done]
        -> Server, handle name conflicts -> disconnect [Done]
        -> Pass epoll FD to IPC [Done]
        -> Pass name to constructor and use name for both server/client [?]
        -> Fix socket retry loop [???]

        * Check heap size every frame, warn if large...
          * Or after every message, if it starts to reach capacity, GC, resize
            do next free
            
        PIPELINE:
        * Integrate model into render process
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
        * Refactor out init parse args [Mon]
        * Refactor IPC connection management [Mon]
          * Remove direct epoll usage in process - isolate in IPC
            * Add epoll method to reuse across processes
            * Move epoll data to IPC
          * Refactor IPC init
            * Pass in list of server pairs
            * Integrate server sockets into conns
            * Simplify ID/FD lookup
          * Dispatch on both server and outgoing sockets
        * Implement workers [Mon/Tues]
          * Ctrl connects input/render/model? and spawns workers
            * Handles synchronous mode (delay)
          * Workers only connect to ctrl/render/model
            * Recvs work from ctrl
            * Recvs frame from render and sends data to render (async mode/drop)
            * Gets data from model and sends data to model (updates/sync)
        * Test multiple controllers with multiple vertices mutating simultaneously [Mon/Tues]
        * Implement event dispatching/event handling [Tues/Wed]
        * SHM?
          * Both render/ctrl/workers mmap segment
          * Ctrl sends (mc dst src sz off) to Render
          * Instead of reading data after, it will read from mem
          * Will synchronize by having ctrl wait for render to cpy and send msg
          * Issue is how to synchronize many workers
  
    * Implement Wayland [Weekend]
      * Setup tiles for 6 windows = 3 col, 2 row
      * Proof of concept working with eval already
      
    * DAG System [Later]
      * Create ptree system
      * Input/Worker, Output, Worker, Worker
      * Input loop: BFS->task, recv sock, send sock
        * If worker not available, block
      * Output will set/get data for workers
        * If integrating this functionality with input:
          * wait on sock for worker
            * if msg ready: traverse node (yield), send task
            * if msg get: find data, send data
            * if msg set: recv data, set data
      * Optimization: if node has 1 pred, traverse until end or many,
      send all to same worker
        * a->b->c->d,  worker=(a,b,c,d)

  
* Refactor/Fix

   * Refactor li into subfolders
   * Socket change block/nonblock to T/NIL for convenience
   * Ensure defs are defined for globals

    * IPC
      * IPC can take in list of connections
      * Pass option to create epoll instance
      * Pass handler functions in

    * Refactor bindings
      * Direct C calls use original C name

    * Refactor namespaces 
      * posix
      * mathc
      
    * Fix native calls, ensure backtick before library symbol
    * Prefix constants with lib name
    * Render - make sure no TCP_NODELAY
  
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
    
## GC Strategies

Three primary algorithms:
* Mark...
  * Sweep
  * Region
  * Compact
* Stop/Copy (scavenging) - Cheney semi-space
* Noncopying Implicit Collection - Baker
  * Has advantages of both Mark + Stop/Copy
  * Main weakness is fragmentation...but not an issue for PicoLisp?

Mark/sweep is faster when low mortality/high liveliness since less
sweeping is done.

Stop/copy is faster when high mortality/low liveliness due to copying 
all live objects. 

Mark/sweep uses less space than stop/copy since stop/copy always
reserves half of the space.

Larger objects favor mark/sweep than stop/copying.

Stop/copy defragments by compacting data through copying.

Stop/copy running time proportional to amount of live objects, not the
size of the heap.

Stop/copy provides better worst-case space bounds than noncopying.

In PicoLisp, since everything is a cons cell, the GC can be optimized
around that, which favors stop/copy.

https://www.quora.com/Which-type-of-garbage-collection-mechanism-is-more-efficient-mark-sweep-or-stop-copy
https://www.hboehm.info/gc/complexity.html

Give user option:
- Expand heap
  - Can only expand until run out of memory
- Collect heap
- Fork+Collect heap
  - User must do side-effect free operations until

Scenarios

* Side effects involving resources, such as I/O, are an issue
  * Need custom syntax/fn to specify function is not pure
 
* Fork + Mark/Sweep
 * Fork on GC
   * Needs size of heap so mem capacity limited to half of total ram
   * Speed depends on heap size
   * Twin procs would amortize deltas at the expense of space vs fork
 * Mark-Sweep
 * Replay eval
 * Switch process
 * Misc
   * To maximize CPU throughput, do parallel mark/sweep/compact
     * Each heap/1 MB chunk
   * Partition heap into N CPU segments, join after all done

* Short Term (feasibility/suitablility, greatest->least):
  * GC not feasible/suitable -> Need semi-auto memory management
    * Per-frame Allocation
      * Runtime
        * Init/manage heap
        * Provide allocation functions - pointer bumping
          * Some allocations have to be automatic
      * On frame start:
        * avail-start = cons (or avail cons)
      * At frame end:
        * avail = avail-start
        * Any data created between those points, will be overwritten
        * For static data, memcpy to next module/frame before ptr reset
          * Copying GC basically...
      * Almost generational...
      * Create heap large enough that GC need not be triggered
        * Copy to another process while previous GC's
    -->> Modfy heap function to take a number that will set the avail pointer
    * For now, make heap large enough to prevent GC
      * Can track usage (+/- 1 MB granularity) after every eval
      * Inform user, ask to GC or resize heap
    * Hooks
      * Runtime allocates enough for default/protected symbols
        * Namespaces are like areas?
      * Alloc functions need heap pointer to allocate/link cons cells
  * TODO
    * Remove checks from cons* fn's in GC to disable GC mark/sweep
      * Maybe have function that swaps pointer functions
      -> Need it as backup if memory is full
    * Add function to get/set Avail ptr
      -> Modfy heap function to take a number that will set the avail pointer
    
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

Current Solution:
- For now limit to 32 MB, so we can call GC each frame
  - Lisp environment really to control graphics
- Track heap size after each eval
- When heap reaches threshold, prompt user
  - To calculate size, determine how much data can be processed in n ms
- Default is to perform GC
- Alternative is to expand heap
  - Can expand until run out of memory, at that point force GC or crash
- Fork+Collect heap
  - User must do side-effect free operations until

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

## RESEARCH

DL
https://www.youtube.com/watch?v=R7EEoWg6Ekk

Write PicoLisp ("A") interpreter in PicoLisp ("B") -> AKA meta-circular interpreter
Have A do optimizations:
- AST rewriting
  - Replace optimized nodes with machine code