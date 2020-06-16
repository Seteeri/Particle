TODO
====

  Embrace. Embody. Empower

  The cohesiveness of Lisp Machines
  The extensibility of Emacs
  The polish of Apple
  The power of open source

  GOAL is build structures more fluidly
  * Create list, enter strings, convert some to symbols, eval
  
  REPL:
  
  * Rearchitect
    * Gen/Lay
      * Reuse existing particles
        * Pass flag to gen -> idx
      * New will gen verts
      * Pass sym exp to gen
    * Create unit tests [Fri]
      * UI ops
      * Ptr
    
  * Use prog1
  
  * Root Lists (side/main)
    LEFT:
      * Mov from simple to advance, downward
      1. Help
        * show recommendations based on sel obj(s)
      2. ptr-mode
        * essentially the modes
          * keymaps
            * Ability to modify, add, update binds
            * Modify by eval'g a cmd
        * shows property list for ptr
      3. sel
        * aka buffers or registers
        * ptr will push sel to car
        * Single ptr
          * Ptr is yellow
          * Selected is red
          * Orange used for condensed
      4. prop
        * show property list for selected obj(s)
      5. search & replace
        * show recommendations based on sel obj(s)
      6. files
        * show recommendations based on sel obj(s)
        * where to put this?
      7. Undo-log/timeline
        * circular list
        * save old list to file/db
    BOTTOM
      * Status
  
  * Str Ops before demo
  
  * Lists can be rotated to simulate scrolling
    * Requires redrawing entire list...
    * Frustrum culling
  * Make camera mov
    * Create cmds to center view, fit view etc
    * Mov to item - def is align to left side of screen
    * Either zoom out or move newline
  
  * Modes/ptrs
    * Str (Default)
      * Produces single char strings
      * L-Alt + Sp = to convert str-int-num
        * Use double mod
      * All other modes cmds accessible with mod keys
        * When in that mode, same shortcut without mod keys
    * Int
      * Map keys to sym/oop fns
      * put/get
      * getl/putl
      * type
    * Ext
      * Map keys to ext/db fns
    * Num
      * Hex, Bin, Dec, Fl
      * Enter sequence, then on space/enter, convert
      * a/s/m/d/e/q = add/subtract/multiply/divide/exp/sqrt
    * Pair
      * Map keys to pair/nil cmds
      * Includes glue/chop/pack/split
    * Bind to function keys

  * Create unit tests
    * Verify position
    
  * External Symbols?
    * Save code to binary and database
      * This will output both data with markup
      * To make "runable" version, strip comments and markup
      * Think of it as pre-parser step, but part of it
        * Either separate program or modify reader
  * Multiple workers
  * Min UI latency
    * Swap proc before cmd since don't know if GC will happen
  * Implement cmds Q/E : start/end of line/list
  
  * Track data:particles in binary tree *particles
    * Unlike a CLI, we hold references to old data
      * Which if future commands change old data, it has to be updated
        * E.g. zap
      * Which means data/particles must be tracked
    * Given multi particles repr same data
      * To gc data, delete all particles ref data
      * Assumes non-visible particles are not ref data
    * Data : List of Particles
      * handle 'zap - isyms replaced with name (tsym) - invalidates particles
    * Draw all symbols will access all data?
    * External symbols are more explicit      
    * Without this, to del all +Point of sym, must search all data
      * Or link all points
            
  * Pointer
    * Mov pointer to different list
      * Need cmd that we can type
    * Make Pointer class [later]
    * When ptr points to another ptr...input changes?
      * If user wants to use ptr as a marker, 
      explicit cmd to create a ptr like *cdr-1
  * Soft wrap list
    * Track pos
    * When limit reached, mov nl
    * Each list will have a length
    
  * Optimize display updates
    * Decouple upd-tree from command
    * Update only if in view
    * update backwards until non-y list
    * Relayout should use multiple workers
      * Scout pushes work into a queue
        * On finish scan, become worker
        * Batch nodes
      * Ctrl distributes tasks to workers
        * Workers send rdy msg to get work
      * Workers update and send serialized data to Render
      * Or go further, and use a timeout
        * Start handling longer tasks
          * Rotate process
          * Or...
            * Deploy task
            * Set timeout to rotate
            * Fork
  
  * ?
  
    * Fix eval output
      * Print system out
        * Where to put it?
          * Separate *out list
            * Part of process...
            * Visualize process?
          * Sublist of original cmd
          * Two lines
  
    * a s d m for arithmetic?
    * Named pipe + rd/pr  
  
    * Draw background grid
      
  * ?
  
    * https://github.com/mapbox/tiny-sdf
      * Felzenszwalb/Huttenlocher distance transform
    * https://github.com/astiopin/webgl_fonts
      * glyph hinting
      * subpixel antialiasing

  ---
                    
  * Cam
    * Cap zooming
    * Camera needs to move with content like when entering a newline
      * Requires unproject to test if coord is in the viewport
      * When a new item is entered, check its bnds against the view bounds      
        
  * Implement database - need for PIM
    * Load db files into memory/tmpfs
    * Convert +Particle/+Vertex into db
      * This will allow linking particles
        * The Ext sym name is like universal id/ptr
      * Refactor Particle into subclass of Entity
      * Can test multiple workers pull/push database
      * Workers send serialized data directly to raster
        * ...instead of writing to DB and having raster read it
    * Store classes in separate files?
      * verts.db
      * parts.db    

  * Pointer System
    * System
      * -> list = all
        * list of pairs will select those ranges
      * -> pair = between car cdr
        * (p . NIL) = until EOL
        * (p . T) = until Ptr
      * -> atom = single
  
  * Proportional fonts
  
  ---
    
  * Improve testing environment
    * Have processes run independently    
   
  * Basic search/replace (strs)
    * Essentially, searching symbol props
    * Conventional users expect strings, aka prop data when str
    * Also can search tg, dat, tim, ori (spatial)
    * Output list of syms/particles + finds
      * For str, context
      * Other props, whatever it is
    * Lists can be composed with search/repl fn
    * Lists can be sorted
    * Output can then be merged
  * Pattern matching  
          
  * Misc
    * Draw num in car [?]
      * Handle decimals
    * Draw grid in bg
    * Make fn: mov-cur X/adv Y/nl
    * Refactor other items to use skip flags like mov> etc.
    * Make columns for Y layout?
    * Why is xkb in worker? Should be in ctrl...
    * Draw ctrl characters: ^M, ^J, ^I
      * Draw newline when by itself
      * When packed do not draw it - make opt?
    * Generate undefined glyph - 0
   
  * Optimize
    * Math
      * Pass vecs to GLSL [later]
        * Use quats?
        * Pass 48 bytes instead of 64  
      * GL structs
    * IPC
      * Utilize multiple workers - see notes
      * Batch messages, flush etc.
      * Instead of directly sending msgs, put into list
      * Call flush to send all
      * Requires rewriting protocol to read multiple messages from single str  
    * Lazy load glyphs
      * Do later when msgs are refactored
      * Render loads tex
      * Gly loads metrics
      * Need worker to tell render to load
      * Load ASCII initially
      * Convert glyphs into db   
  
  * Directory Nav    
  
  ---

  * SEARCH IS THE KEY TO DISCOVERABILITY
    * Google's interface
    * Make as easy and intuitive to access help
      * Bind F1
      * (eval "help") (eval 'help)
    * Fuzzy-search  
  
  * Tag/Note System
    * Create +Note class
    * Need "template"
    * Search property list functions  
    
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
      
  * Instead of drawing lines to connect nodes, draw generic grid in bg
  to guide user

  * Timestamp optional
    * GPS optional
    * Really just tags...except expected/defined tags on all items

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
    
  * Map keys to grid on screen
    * 26 squares if using plain alphabet
    * User can use keys to manuever

  * Rename namespace gl -> gles

  * Error Handling
    * Set * Err to (quit)
    * On error, print msg and return to top-level

  * Symbols Mouse UI
    * all: left click = select/point
    * all: 2x left click = eval
    * Value = double right click
    * Props = right click          

  * Touch interface
    * Refactor math library; implement glunproject
    * Ability to pull items out
    * Requires spatial index, unproject
      * Port r-tree from CL          

DEMOS:

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
  
## The Interface

* Built on lisp data structures - lists/cons, symbols, numbers
* 3D orthographic "nodal" environment - "turtles all the way down"
* Primarily keyboard driven interface
* Non-destructive; undo/redo capabilities
* Non-blocking UI - user always aware of computer status
  * User can choose take risk to block
* Wayland provides conventional desktop
* Solarized color theme as default

Personal Extensions
1. Per-object vector motion blur (personal favorite)
2. Power mode

Future Ideas
* Tiled forward rendering (Forward+)
  * Clustered -> volumetric forward shading
* Augmented reality through OpenCV
* Convergence...
* PilOS bootloader...
