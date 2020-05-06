API
===

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
