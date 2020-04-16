Particle
========

The name comes from the idea that in Lisp there exists two types of data: atoms
and lists. The word atom means "indivisible" and was chosen to state their
fundamental nature. However, in our system, we breakdown atoms into their
individual bytes and bits. Using a loose analogy of physics, the name particle
is derived as we break down the Lisp atoms into their subcomponents.

It also comes from the way the cons cells are rendered, like a particle engine
uses the same model/instancing.

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


## TODO

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
* Draw symbols with cons cell [Done]
* Make NIL type [Done]
* Draw str cdr with dot [Done]
* Refactor pointer to point to last (NIL) [Done]
  * More consistent with entering a list    
* Rewrite list handling [WIP]
  * When enter-list, change ptr symbol to reflect it instead of pos      
* Need pointer/new fns to change layout, new row/col [Done]
  * Change pointer symbol to reflect layout: p0^ or p0> [TODO]
  * Should be consistent with outline format
  * Default layout is oriented towards lists of text/num/syms similar to
  alphanumerical outlines
  
* Refactor methods: generation, bounds, layout (PART I)
  * Refactor calculate bounds [Done]
  * Refactor gen [Done]
    * Should calc be last resort if not there? -> That is an error then
  * Then layout to get dims [Done]
    * And use skip flags
  * Rename skip flags to symbols to be more clear [Done]
  * Gen should not upd verts - only pos them [Done]
  * Refactor cmd-make-char [Done]
    * Calculate x max from prev item
    * For single chars, the bounds is the same          
  * Refactor cmd-make-nl [Done]
    * Calculate y min from last NL (or list start) until that item
  * Refactor cmd-del [Done]

* Refactor sublist handling (PART II) [WIP]
  * Refactor swap-layout [Done]
  * Refactor layout Y [Done]      
  * Ptr mov is with regard to layout - inverses [Done]    
  * Refactor cmd-make-car to create new list when on car [Done]
    * Swap-layout
    * Newline flag
    * Update CDR
  * Test for sublists in ptr cmds [Done]              
  * Rename dims -> dims [Done]
    * Dims = l w h
    * Extent = min bnd'g rect: xmin ymin xmax ymax
    * Bounds = aka bounding box/rect; 
    * So extents == bounds

    +-------+--------------------------------------------------------+
    | CMD   |                       Pointer                          |
    +-------+-------------------+--------------------+---------------+
    |       | Pair/Atom         | Car/Atom           | NIL           |
    +-------+-------------------+--------------------+---------------+
    | ASCII | Ins-back, Mov-Cdr | Write-car, Mov-Car | Same as left  |
    +-------+-------------------+--------------------+---------------+
    | NIL   | Same as above     | Same as above      | Ins...        |
    +-------+-------------------+--------------------+---------------+
    | NL    | List w. Pair      | Mov nl             | Empty list    |
    +-------+-------------------+--------------------+---------------+
    
  * Refactor
    * Refactor ops [Thr]
      * Support sublist [Done]
      * Refactor repl-list-car: reuse pair
      * Refactor repl-car: reuse pair, merge with above fn
      * Refactor ins-list-car: reuse pair
    * Refactor ops into methods [Fri]
    * Fix space glyph [Sat]
    * Handle multi-line strings  [Sat]
    
    * Refactor bindings [Sat]
      * Improve discoverability - user sees it immediately
        * Once they are adv enough, they can remove it from default setup...
      * Either show *bindinds-key or *binds directly
      * Store key syms and fn syms
        * Get val when called
      * Mmm, might need to group commands
        * Majority is ascii/str keys
    
    * Why is xkb in worker? Should be in ctrl [Sun]
    
    * Replace font with terminus? [Sun]
      
  * List/Atomic Ops [Thr?]
    * cmd-del, cmd-backspace...
    * Replace input with output
    
  * Str Ops
    * Core
    * Basic search/replace
      * Search outputs list
      * List can be searched again or replaced, results in output again
      * Output can then be merged
    * Pattern matching      
  
  * Logging System
    * Log commands  
  
  * Eval
    * Quick demo
  
  ----------------
    
  * Pointer System
    * Use master pointer to select main pointer
    * Master ptr has dedicated binds/cmds
      * Ctrl+WASD: Mstr
      * Alt+WASD: Main
    * System
      * -> list = all
        * list of pairs will select those ranges
      * -> pair = between car cdr
        * (p . NIL) = until EOL
        * (p . T) = until Ptr
      * -> atom = single
    * or show props on single line (nested pairs)

  * Buffer System (aka Cut/Copy/Paste)
    * Buffers have ptr prop
      * p1 -> b1, p2 -> b2, etc.
    OR
    * Pointers have buf prop (most logical)
      * n-ptrs : b1
      * Similar to ptr list, use a buffer list which follows same pattern
      * Show first and last items
    * CUA
        
  * CLI
    * Plain list  
  
  * Refactor [Nxt Week]
    * Camera needs to move with content like when entering a newline
      * Requires unproject to test if coord is in the viewport
      * When a new item is entered, check its bnds against the view bounds  
    * Technically '*list and '*main can be different, i.e. sublist can be on
    same line as parent list
      * It can't unless layout swapped...
    * For single chars, the bounds is the same everytime
      * Avoid calc      
    * Fix magic numbers
    * Refeactor "*0" mov-part-abv> into cursor fn  
    * Should swap-layout recurse or not?
      * Maintain substructure
      * Do first level or immediate
      * Shortcut with Ctrl, Alt, Shift Tab
      * Really only useful when setting up sublists  
    * Opt: buffer is a circular list
    * Draw num in car [?]
      * Handle decimals
    * Utilize double shift binds
      * Search menu?
      * Double ctrl
      * Double alt
      
    * Replace dot with arrow indicating layout (Right/Down)
    * Make fn: mov-cur X/adv Y/nl
    * Cache last item for lists
    * Refactor other items to use skip flags like mov> etc.
    * Make columns for Y layout?            
    * Limit x length of items like line wrap
    * Store ref to last item in list for faster bnds calc
    * Is there a way to map modifier keys to Car or Pair/Cdr?
      * Use mod keys to decide whether to keep input or replace it
      
  * Implement external symbols
    * Move verts.bin -> verts.db
    * Refactor Particle into subclass of Entity
    * Can test multiple workers pull/push database      
      
  * Optimize
    * IPC
      * Utilize multiple workers - see notes
      * Batch messages, flush etc.
      * Instead of directly sending msgs, put into list
      * Call flush to send all
      * Requires rewriting protocol to read multiple messages from single str
          
  * Use special printing for control characters like enter etc.
    * newline ("^J"), return ("^M") or TAB ("^I")
    
  * Directory Nav
      
  * Tag/Note System
    * Need "template"
    * Search functions
    
  --------

  * Test multiple workers
    * Need data sync on model side to broadcast updates

  * OpenGL
    * Implement viewports
      * Allows split screen
      * Create viewport from current view
    * Implement framebuffers
      * Render to target
      * Allows us to create screens/viewports
    * Implement screenshot
      * glReadPixels
      
  * Basic animations
    * Easing functions
    * Fades
    * Use compute shader on large amount
      * For demo max verts

  * Touch interface
    * Refactor math library; implement glunproject
    * Ability to pull items out
    * Requires spatial index, unproject
      * Port r-tree from CL      
      
  * Instead of drawing lines to connect nodes, draw generic grid in bg
  to guide user

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

  * Lists
    * Differentiate colors for tagged and untagged data?
      * Show number of tags?
    * Don't draw NIL at end of list
      * Pointer points to last item
    
  * Map keys to grid on screen
    * 26 squares if using plain alphabet
    * User can use keys to manuever

  * Rename namespace gl -> gles

  * Error Handling
    * Set * Err to (quit)
    * On error, print msg and return to top-level

----------

  * Demos:
    * Core
      * Lisp
        * Strs
        * Lists
        * Eval
      * Systems
        * Help - need "quick escape"
          * Draw help, bind to move ptr while held
          * Sublime
          * Ext of tags?
        * Cmds
          * AKA Timeline/History/Undo
        * Ptrs
        * Buffers
        * Views (Viewports)
        * State
      * CLI
    * Key Concept
      * Pixels as list of pixels or lists of lists
        * Implement screenshot
      * Table as list of lists
      * Widget Deprecation
    * Application (Concrete)
      * OS/Dir
      * Outliner/PIM
        * Tags/Search
          * Internals
      * Spreadsheet
      * Window Manager
    * Advanced UI
      * Touch Interface    
    

LATER:

  * Implement Wayland - BASIC!
    * Setup tiles for each list
      * Multi
        * Buffers - Left
        * Cmds - Right
        * Ptrs - Top
        * Main - Bottom
      * Dual (L-R)
        * Major - Main
        * Minor - Buffers/Cmds/Ptrs
    * Proof of concept working with eval already

   * Test compute shaders
     * Rotate all vertices
     * Could use shader then pull data back?
       * Syncing becomes an issue -> at that point, keep transforms on GPU side
       * Vertex data simply contains an offset to the struct

  https://stackoverflow.com/questions/287871/how-to-print-colored-text-in-terminal-in-python

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
