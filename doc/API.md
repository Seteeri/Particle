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

+Particle

+Vertex

+Metrics

## OpenGL

SSBO

UBO

Various Buffers

# Design Concepts

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

## Search, Tags and Trees
