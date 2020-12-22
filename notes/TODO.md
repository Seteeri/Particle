TODO
====

  Embrace. Embolden. Empower
            Embody?
            
  Action . Feeling  . Thought = Behavior
            
  The cohesiveness of Lisp Machines
  The extensibility of Emacs
  The polish of Apple
  The power of open source

  GOAL is build structures more fluidly
  * Create list, enter strings, convert some to symbols, eval
  * +Pixel example to demonstrate power
  
  REPL:

  * Rewrite pointer to store any/reg
    * remove uncol from pt>
    * Mov trace to point
    * base cmds
      * str: y
      * list: y
      * bsp: 
      * del:
      * alt:
      * line: skip...
    * other cmds like repl
      
  * Rewrite tests
    * Dynamic tests
    * Randomize ops: permutations + double
      * Permutations dependent on num of cmds
    * Check that each Point is > x or < y

  * Camera Tracking
    * Create cmds to center view, fit view etc
    * Mov to item - def is align to left side of screen
    * Either zoom out or move newline  
  
  ---
  
  * Lists can be rotated to simulate scrolling
    * Requires redrawing entire list...
    * Frustrum culling
            
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

  ---
                    
  * Cam
    * Cap zooming
    * Camera needs to move with content like when entering a newline
      * Requires unproject to test if coord is in the viewport
      * When a new item is entered, check its bnds against the view bounds      

  * Pointer System
    * System
      * -> list = all
        * list of pairs will select those ranges
      * -> pair = between car cdr
        * (p . NIL) = until EOL
        * (p . T) = until Ptr
      * -> atom = single
  
  * Proportional fonts
   
  * Lazy load glyphs
    * Do later when msgs are refactored
    * Render loads tex
    * Gly loads metrics
    * Need worker to tell render to load
    * Load ASCII initially
    * Convert glyphs into db
    * https://github.com/mapbox/tiny-sdf
      * Felzenszwalb/Huttenlocher distance transform
    * https://github.com/astiopin/webgl_fonts
      * glyph hinting
      * subpixel antialiasing
      * less necessary with high dpi screens
    
    
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
    
  * Map keys to grid on screen
    * 26 squares if using plain alphabet
    * User can use keys to manuever

  * Rename namespace gl -> gles

  * Error Handling
    * Set * Err to (quit)
    * On error, print msg and return to top-level
  

LATER:


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

Personal Extensions
1. Per-object vector motion blur (personal favorite)
2. Power mode

Future Ideas
* Tiled forward rendering (Forward+)
  * Clustered -> volumetric forward shading
* Augmented reality through OpenCV
* Convergence...
