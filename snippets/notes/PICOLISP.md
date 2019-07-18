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

   * Refactor IPC [Done]
     * Abstract common functionality among worker/model/render conns [Done]
     * Serialize entire object for all [Done]
       -> Need not serialize everything - add ability to serialize only certain members [Later]
     * Model needs to write data [Done]
       * To sync, worker can pull every update or have model push updates out
     * Implement message flushing [Done]
       * Lag with camera update and pointer update at same time
              
   * Implement string/atom/list, eval functionality [Wed...]
     * ASCII key creates a string with vertex in property list
      
   * Refactor
   
   * Implement event dispatching/event handling in ctrl [Tues/Wed/Later?]
     * Store in Ctrl or Worker?
       * Or store in model and worker pulls from model
         -> Cache would eventually result in a copy per worker
     * Use assoc list
     * Store combinations as keys      
   
   * Implement Wayland [Wed/Thurs]
     * Setup tiles for 6 windows = 3 col, 2 row
     * Proof of concept working with eval already   
   
   * RECORD DEMO? 
   
 
* Refactor/Fix

   * https://www.khronos.org/registry/OpenGL-Refpages/es3.0/html/glGetProgramBinary.xhtml

   * Refactor li into subfolders
   * Socket change block/nonblock to T/NIL for convenience
   * Ensure defs are defined for globals

   * Refactor bindings
     * Direct C calls use original C name

   * Refactor namespaces 
     * posix
     * mathc
     
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
       * Pixel (RGBA/no-texture)
       * Arbitrary Texture
       * Text
     * Reduce struct with removing excessive UV allocation
     * Reduce struct to 128 bytes with uniform UV
     * Options for max-chars/min-size
       * Uniform UV (more likely)
         * Move UVs to uniform buffer object
       * Uniform rotation (more likely)
       * Uniform scaling (less likely)
       * Uniform rgba (less likely)
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

https://academia.stackexchange.com/questions/109/is-there-any-efficient-non-linear-note-taking-software

Write PicoLisp ("A") interpreter in PicoLisp ("B") -> AKA meta-circular interpreter
Have A do optimizations:
- AST rewriting
  - Replace optimized nodes with machine code
  
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
  