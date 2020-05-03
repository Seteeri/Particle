Architecture/Infrastructure
===========================

# Overview

* Focused on Wayland and modern OpenGL ES 3.2+ (Vulkan)
* PicoLisp due to simplicity, expressiveness and consistency
* Process-based system
  * Components: Input/Controller, Workers, Model, Render
  * IPC through message passing
  * Multiple processes provide fault-tolerance and scalability
* Rendering engine = Particle system
  * Compute shaders
  * AZDO Techniques
    * Persistent mapping
    * Instanced drawing
    * Indirect drawing
  * Fully programmable vertex pulling
    * Texture buffers/UBOs/SSBOs -> gl_VertexID+gl_InstanceID

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

# Parallel Model

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


# Data Structures

+Particle

+Vertex

+Metrics

## OpenGL Buffers
