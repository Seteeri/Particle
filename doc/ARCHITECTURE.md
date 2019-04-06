Architecture
===========

Protoform utilizes two processes - one for the rendering and one for 
the model (data). They communicate through unix domain sockets through a
simple lispy RPC (similar to the Swank/SLIME protocol) and through 
shared memory.

The processes are separated since they are independent and also to
reduce pressure on the garbage collector (alternative solutions exist
such as performing more manual memory management in the model process
through libraries such as ManarDB).

How it works:
The render process runs independently continuously drawing. Between 
issuing draw calls and swapping etc., it will send a message to the 
model process to check if there is any data that needs to be copied from
the shared memory to the OpenGL buffers. This is a synchronous process
because if the model process writes to the shared memory while the 
render process is writing to it, it would result in unspecified
consequences, i.e. glitches or visual artifacts.

When the render process receives this message, it will then perform any
synchronous tasks such as animations and serializing any data necessary
to the shared memory, and then finally it will send a message back to
the render process to continue drawing (and read from the shared memory
if needed), and the cycle repeats.

# Model Process

This process contains lparallel kernel for the thread pool for parallelized
tasks. 

In addition, it initializes 4 threads:

Input:
* Epolls libinput file descriptor to receive events; passes to controller thread

Controller:
* Receives input events and dispatches callbacks; passes to model or view thread depending on the context

Model:
* Executes asynchronous tasks, e.g. I/O or tasks not requiring frame time or do not need to complete every frame

View:
* Handles the socket connection to the render process
* Receives frame events from render process; once event received, it will execute any synchronous tasks including serializing data and writing to shared memory
* This is required to synchronize access to shared memory between the processes
* Render process blocks on reply from view so it is important to complete tasks as quickly as possible

The input-controller-model pass messages between each other through 
lock-free queues (SBCL's mailbox primitive).

The model process (view thread) uses the shared memory to transfer data
to the render process to memcpy to the OpenGL buffer; thus the shared
memory are the same size as the OpenGL buffers.

# Render Process

This process contains the OpenGL context.

It has the single responsibility of managing buffer objects, including 
creation/destruction, memcpy from shm to buffer objects, and of course,
drawing.

The reason is to minimize drawing latency to maintain visual fidelity 
as much as possible.

# Progam Operation

``protoform.lisp`` will fork two processes, then exit:
* Model
* Render

``model.lisp`` initialization:
1. Setup lparallel kernel and queues
2. Initialization data (through ptree)
   * Shared memory
     * Owned by model so responsible for cleanup
   * Glyphs
   * Connect to view (server)
     * View will memcpy initial shm
3. Initialize threads

``view.lisp`` initialization:
1. Setup GLFW window
2. Setup OpenGL context
3. Create OpenGL resources like buffers etc.

Once both process have initialized, view will start sending frame
messages to model, and then wait for a response from model to continue.

The reason for this is, certain events can only occur during the frame
time such as animations.

Models primary responsibility is to process data and then ultimately
serialize to shm for view to copy. As much work as possible is shifted 
to the model process to minimize view latency when drawing. View consing
and GC'ing should be kept to a minimum to avoid any frame jerks. 
Currently most of the garbage will come from RPC reading objects in 
(this can later be optimized further).

For model, the RPC thread is the entry point for any action to occur.
The controller thread will read events and dispatch handlers, which will
enqueue ptrees. When the RPC thread receives a frame message, the ptrees
will be executed during a frame message, and finally a response sent 
back for view to continue.

Model ``view.lisp`` frame procedures:
1. Execute ptrees for frame tasks
2. Execute ptrees for animation tasks
3. Execute shm tasks

To exit, model will send an exit message to view, and then both processes
will shutdown simultaneously.

## Animations

1. Controller receives input event which triggers callback function
   * Callbacks are called in serial, however, validation is done in
   parallel - this allows callbacks to build the ptree in controller thread
   * For each input frame, controller creates a ptree and queue for IDs
   which is passed to callback functions
2. Callback creates tree node and places node ID in queue which will be
   dequeued by model frame.
   * For animations, initial trigger is done in input ptree and continued
   animations are placed in animation queue, where ptree is built during
   frame

NOTES:
1. Should controller execute ptrees for non-frame tasks? Where should
this occur? This creates concurrency issues because now have to make
sure frame (RPC thread) is modfying data used by controller thread (or
whatever thread executes it)
2. Unable to reuse input ptree since it is only used once per frame
and then free'd
   * Solution is to build ptree in frame for both input and anim tasks