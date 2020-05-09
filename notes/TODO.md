TODO
====

  The cohesiveness of Lisp Machines
  The extensibility of Emacs
  The polish of Apple
  The power of open source

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
  * Make vert-scale a class variable of Vertex
  * Refactor layout
    * *binds-ops needs mix of x/y
    * Traverse parts and change lists to y etc
      * Either do in gen or lay phase
  * Refactor socket - set err instead of propogating
  * Refactor messages [Later]
    * Queue and flush
  * Refactor math
  
  * Eval
    * Fix eval output - need refactor layout
    * Print system out
      * Where to put it?
        * Separate *out list
          * Part of process...
          * Visualize process?
        * Sublist of original cmd
        * Two lines
  
  * Visual Hierarchy
    * Process -> Namespace -> Symbols...
    * Process assumed to be the same...so indicate with ptr?
      * Namespace is the root...  
  * Need to be able to mov pointer to another list
    * Either modify master ptr
    * Or mov current ptr
  * Draw spine or use grid to provide visual line guide
  
  * Draw *binds-ops
    * Need custom layout
    * Instead of double space, use ptr arrow
      * Arrow should be colored diff
      * Use two symbols: <up>0 <dn>0
      * So up/dn is really changing symbols and moving them into cur pos
    * Either show input or output
      * Code or data form
      * After change, eval it
        * For "button", create a list somewhere: (update bindings to *bindings-key)
        * User clicks it by eval'ing it
      * Ideal is to show symbols, instead of their val (num)  
    
  * Log/Undo System
    * Log commands
    * This cannot be unlimited - use a circular list
    * Push old commands to file
    
  * Cam
    * Cap zooming
    * Camera needs to move with content like when entering a newline
      * Requires unproject to test if coord is in the viewport
      * When a new item is entered, check its bnds against the view bounds
    * Refactor math class
    
  * Improve testing environment
    * Have processes run independently
              
  * Push all data into binary tree
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
  
  * Implement database
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

  * Font Rendering
    * https://github.com/mapbox/tiny-sdf
      * Felzenszwalb/Huttenlocher distance transform
    * https://github.com/astiopin/webgl_fonts
      * glyph hinting
      * subpixel antialiasing      
  
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
  
  * SEARCH IS THE KEY TO DISCOVERABILITY
    * Google's interface
    * Make as easy and intuitive to access help
      * Bind F1
      * (eval "help") (eval 'help)
    * Fuzzy-search
  
  * Adv Str Ops
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
  
  * Refactor
    * Refactor "*0" mov-part-abv> into cursor fn  
    * Opt: buffer is a circular list
      * Use dot by itself to indicate circular list
      * Remember, two dots means it refers to itself
    * Draw num in car [?]
      * Handle decimals
    * Utilize double shift binds
      * Search menu?
      * Double ctrl
      * Double alt
    * Draw grid in bg
    * Make Dot YELLOW!
      * Small so use most visible color to offset
      * Make NIL grey since its so common but not as useful
    
    * Replace dot with arrow indicating layout (Right/Down)
    * Make fn: mov-cur X/adv Y/nl
    * Cache last item for lists
    * Refactor other items to use skip flags like mov> etc.
    * Make columns for Y layout?            
    * Limit x length of items like line wrap
    * Store ref to last item in list for faster bnds calc
    * Is there a way to map modifier keys to Car or Pair/Cdr?
      * Use mod keys to decide whether to keep input or replace it
          
    * Symbols
      * all: left click = select/point
      * all: 2x left click = eval
      * Value = double right click
      * Props = right click      
        
    * Why is xkb in worker? Should be in ctrl []
    * Use mouse cursor for pointer symbol?
      * Or greek symbols
    * Colors follow HTML links
      * Symbols are blue
        * Unvisited is blue
        * Visited is purple aka eval'd
      * Strings are just grey/white/black (since bg is black)
    * Worker
      * Refactor point
      * Refactor ops into methods
      * Refactor line fns
      * Implement cmds Q/E : start/end of line/list
      * Draw ctrl characters: ^M, ^J, ^I
        * Draw newline when by itself
        * When packed do not draw it - make opt?
      * Lazy load glyphs [Later]
        * Do later when msgs are refactored
        * Render loads tex
        * Gly loads metrics
        * Need worker to tell render to load
        * Load ASCII initially
        * Convert glyphs into db    
    * Generate undefined glyph - 0                
          
  * Optimize
    * IPC
      * Utilize multiple workers - see notes
      * Batch messages, flush etc.
      * Instead of directly sending msgs, put into list
      * Call flush to send all
      * Requires rewriting protocol to read multiple messages from single str
              
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
