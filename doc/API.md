API
===

* PicoLisp VM
  * Purely cons-based
  * Threaded interpreter only
    * No compiler explicitly to maintain "formal equivalence of code and data"
  * Lisp-1 meaning single namespace for function and variable names
  * Dynamically scoped, dynamic/shallow symbol binding, late method binding
    * Transient symbols (includes strings) lexically scoped
  * Written in macro asm; No C code
  * ~3x faster than CPython
  * Built-in database using external symbols as first-class data type
    * memory based db file = emulation of single address space?
* Modern OpenGL ES 3.2, aka programmable pipeline
  * Vulkan planned
  * Uses glyph instancing aka "particle system"
    * Multi-channel signed distance fonts
      * Glyph hinting
      * Subpixel antialiasing
  * Compute shaders
  * AZDO techniques to minimize draw calls and reduce CPU<->GPU comms
    * Persistent mapping
    * Instanced drawing
    * Indirect drawing
    * Explicit synchronization with double/triple buffering (testing)
    * Programmable vertex pulling
      * Texture buffers/UBOs/SSBOs
      * gl_VertexID+gl_InstanceID
* Transparency sorting currently avoided by preventing objects overlapping
  * Orthographic projection
  * Investigate further later
* Process-based
  * Renderer, Input, and Control/Workers separate processes
    * Number of workers matches core count approximately
  * IPC through sockets, pipes, and memory-backed DB with concurrent GC
  * Provides fault-tolerance and scalability
* Maintains responsive UI by cycling processes to allow parallel GC
  * Renderer primarily memcpys to minimize GC occurance and duration
* Parallelism*
  * Auto-parallelize dependency tree
  * Ported from CL library lparallel
* Wayland focused

# Initialization Sequence    
    
# Process Data Flow

    Input  ->    Ctrl
              * Worker N  ------+
              ...         ------+ Render

    +---------------------------------+
    |          File DBs in RAM        |
    +---------------------------------+

## Input
* Polls for events and forwards to ctrl

## Ctrl
* Contains xkb and bindings
* Send jobs to workers
* Sends exit to everyone

### Workers
* Send rdy msg to Ctrl
  * After job done, sends rdy msg to ctrl, and cycle repeats
* Workers save Particles to DB in parallel
  * Process heap acts as cache to DB in RAM
* Any drawable output is serialized to bytes and sent directly to Render
  * Vertex class is representation of OpenGL buffer struct
* Any heavy numerical calculations are offloaded to shaders
  * Model matrix calculations for each vertex is done in shaders
  * Worker copies pos/rot/sca

## Render
* Draws using OpenGL
* Every frame, reads msgs
* Sole purpose is to memcpy from socket
  * It should not do any processing to prevent drawing delays

# DB
* Accessible to all processes
  * Acts like shared heap
  * "Pointers" or symbol names can be passed to different processes
    * Simulates single-address memory
* Background concurrent GC in process

Notes:
Merge input with control?

# Data Structures

## Lisp

    +Point
      +Pair
      +Atom
        +Num
        +Sym
          +Int
          +Ext
          +Str
          +Box
          +Nil
      +Pointer

    +Vertex

    +Projview

    +Font
    +Metrics
    
    +Pointer

## OpenGL

# SSBO

    (def '*params-buffer 
        (list (new '(+Params-Buffer)
                    'atomic-counter
                    gl~ATOMIC-COUNTER-BUFFER
                    (* 4 6)  #6 ints/params
                    4 -1 #-1 means same as cs
                    NIL)
                    
              (new '(+Params-Buffer)
                    'draw-indirect
                    gl~DRAW-INDIRECT-BUFFER
                    (* 4 6)  #6 ints/params
                    -1 -1
                    NIL)

              (new '(+Params-Buffer)
                    'element
                    gl~ELEMENT-ARRAY-BUFFER
                    (* 4 6)  #4 bytes/int * 6 ints or indices
                    -1 -1
                    NIL)

              (new '(+Params-Buffer)
                    'texture-glyphs
                    gl~TEXTURE-BUFFER 
                    (* 128 1024 1024)
                    -1 -1
                    'rgba8)
              
              (new '(+Params-Buffer)
                    'projview
                    gl~UNIFORM-BUFFER
                    (* (+ 16 16) 4)
                    0 0  #cs-in (cache), vs-in (raster)
                    NIL)

              (new '(+Params-Buffer)
                    'vertices
                    gl~UNIFORM-BUFFER
                    (* 16 4)
                    1 1
                    NIL)

              (new '(+Params-Buffer)
                    'nodes
                    gl~SHADER-STORAGE-BUFFER
                    (* *verts-max (meta '(+Vertex) 'sz-gl))
                    3 3
                    NIL)))

## Nodes Instance

    // stpq
    // TODO: use vec4 -> simpler
    struct uv_t {
        float u;
        float v;
        float s;
        float t;
    };

    // TODO: use vec4 -> simpler
    struct rgba_t {
        float r;
        float g;
        float b;
        float a;
    };

    struct instance_t {
        mat4 model;      // * 16 4 = 64 
        rgba_t rgbas[4]; // * 16 4 = 64
        uv_t uvs[4];     // * 16 4 = 64
        ivec4 w_flags;   // *  4 4 = 16
                        //        = 208 bytes
    };


    // Reduce size

    struct instance_t_2 {
        mat4 model;      // * 16 4 = 64 
        rgba_t rgbas;    // * 4  4 = 16
        uv_t uvs[4];     // * 8  4 = 32
        int w_flags;     // * 4  1 = 4
                        //        = 116 bytes
    };
                    
# UBO

    layout (std140, binding = 0) uniform projview
    {
        mat4 proj;
        mat4 view;
    };
    layout (std140, binding = 1) uniform vertices
    {
        vec4 vertex[4];
    };

# Design Decisions

## Pointers

Basic Symbol/Object Diagram:

            Symbol
            |
            V
      +-----+-----+                                +----------+---------+
      |  |  | VAL |                                |'hgfedcba'|'onmlkji'|
      +--+--+-----+                                +----------+---------+
         | tail                                       ^
         |                                            |
         V                                            | name
         +-----+-----+     +-----+-----+     +-----+--+--+
         |  |  |  ---+---> | KEY |  ---+---> |  |  |  |  |
         +--+--+-----+     +-----+-----+     +--+--+-----+
            |                                   |
            V                                   V
            +-----+-----+                       +-----+-----+
            | VAL | KEY |                       | VAL | KEY |
            +-----+-----+                       +-----+-----+
               
    
S-Expr: (((k1 (v1)) (k2) (k1 (v1)) . s) . VAL)

Pointer:

[N] = nested cell aka traditional diagram
. = condensed cell, dot with diff color - orange

!!! Use symbol to represent pointer symbol (inherent to system)
* Pointer symbol properties must be shown elsewhere since they cannot expand within the space except through shrinkage
* Inline symbols will be expanded like any data

Goals:
* Maintain consistency

Poss:

1. "Shrink" entire structure (minimal/low visual presence)
   -> Condense structure to two lines to avoid excessively small text
      -> Requires reversing
      -> Helps make it visually distinct even when smaller
   -> Not too different from normal UI elements having a different appearance
   like menu items or buttons
   -> Avoid zooming as a necessity
      -> Mitigate "desert fog"
      -> Ptr always near content/focus so minor issue
      -> User doesn't typically manipulate pointer directly...
   -> Allows main content to have focus
   -> Can still use condensed cells to minimize distractions
 
2. Embed Pointer
  -> No...
  -> It becomes part of the structure and so now it must be pulled out
 
Unfeasible:
 
* Nested/s-expr structure (medium visual presence)
   -> Difficult to read without resorting to parenthesis
      -> Want to avoid parenthesis...
   -> Defeats whole purpose of Particle...
   
* Double space (high visual presence)
   -> Major problem - distracts from main content
   -> Single space necessary to visually delimit pairs vertically
      -> Seems excessive when dealing with strings but is consistent
   -> Technically, must triple space so still fails
 
## GC

* Coroutine same for all
* Build test program?
* No way to detect GC...
  * Check heap size after each call
  * If within threshold, switch proc
  * If user does an op, GC will seen as part of op
  * Or call GC always after command
* Possible to detect if GC will occur beforehand
  * If var determined during fn call, calc data that will be used when var known

Proc 1
  Co
    loop inf
      Do [broadcast any data used, so Procs cache from db]
      Yield [after every command?]
  Switch to Proc 2
  GC
  Co
  
Proc 2
Proc 3
Proc 4


## Tree Layout

## Dependency Graph Parallelism
