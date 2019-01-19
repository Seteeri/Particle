Architecture
============

The basic architecture of Protoform is based on two components similar
to the MVC/PAC pattern for UIs.

In concrete terms, each component is a separate process. They
communicate with each other through a basic lispy RPC protocol which
consists of sending lisp code as strings which are then read into data
and executes the specified function. Data is shared through mapped 
shared memory.

# Main Concepts

## The Model

This process contains the raw data that the user ultimately cares about.
It is responsible for any processing of data up to serializing data to
shm for the view.

### Input

This process is basically a wrapper around libinput. 

Model runs an infinite loop around this that receives input events and
dispatches handlers

## The View

This process contains the OpenGL context.

It has the single responsibility of managing buffer objects, including 
creation/destruction, memcpy from shm to buffer objects, and of course,
drawing.

The reason is to minimize drawing latency to maintain visual fidelity 
as much as possible.

# UI Concepts

The interface is based on a directed graph (acyclic?)

There are 3 basic objects:
1. Vertices - data nodes - the data itself; represents the various lisp 
data types
2. Edges - essentially scaled nodes at its core; solely used for visual
purposes; minimal user interaction; edges must still be represented as 
distinct objects for consistency
3. Pointers (type of vertex) - used for selection and acts as the 3D
cursor; can have unlimited number

In the underlying code, they are all represented as nodes. The node 
contains all the required parameters for drawing as well as the data
it represents.

## Relationship to Widgets

All widgets are based on an underlying tree structure and can be broken
down into nodes.

**Explain core widgets and corresponding nodal structure**

Use icons for L,T,B, etc. instead of letters but for now use letters

------

Labels - cannot modify - check in backspace etc - user can manually change it ofc
         call it god mode???

[L]-[]...

Text Field/Box

[F]-[]...

[A]-[]...

Cycle Button

[B]-[U]-[P]
 |
[F]-[]...
 |
[B]-[D]-[N]

Drop-down List - similar to Radio Buttons but condensed; text fields + button
               - redundant?

[B]-[]...
 |
[D]-[] Label 1

Hide until needed to be seen
[] Label 2
[] Label 3
[] Label 4

List Box - same as checkbox essentially

[I]

Combo Box - combo of text field + drop-down/list box

-----------

Push Button - user adds node which will change state
              repeated adding = repeated pushing which creates a chain - natural undo
[B]-[]...

Radio Button -  user adds node to one of the labels
[R]
 |
-[] Label 1
 |
-[] Label 2
 | 
-[] Label 3
 | 
-[] Label 4

Check Box - user adds node to any of the labels
[C]
 |
-[] Label 1
 |
-[] Label 2
 |
-[] Label 3
 |
-[] Label 4

---------

Scrollers

Scrollbar

[S]----[<>]----[S]

Slider (use preexisting edges to cue user?)

[S]--[1]--[2]--[3]--[4]--[S]
      |     |    |     |
     [^]

Progress Bar

[P]--[%%]--[P]

------------

Grid View

link right-down
[G]--[]--[]--[]
|    |   |   |
[]->[]--[]--[]
|    |   |   |
[]->[]--[]--[]
|    |   |   |
[]->[]--[]--[]
|    |   |   |
[]->[]--[]--[]

Tree View

[]--[]--[]
     |
    []--[]--[]
     |   |
    []  []--[]--[]
     |       |
    []      []--[]

## Pointer Nodes

A pointer node is as the name states - it points to a node. However, it
is a node like any other node and thus can be operated on likewise. All
nodal functions have an option to ignore the pointer which is set to t 
by default.

**ALL FUNCTIONS ARE PASSED POINTERS, UNDO OPTION**
1. input pointer  - what node to use for input
2. output pointer - what node to link output to; default to input; nil means free node
2. undo - whether to move input data to output with fn; provide more options...

The following constructs are provided:

* Point  - attach 1 node to pointer; 1st degree only
* Radial - attach n nodes to ptr; 1st degree only
* Range  - attach 2 pointers to single pointer to mark range; any
nodes linked between the endpoint nodes will be selected; redundant
pointers enclosed are ignored

Pointers can be attached to other pointers and so on, thus the above
can be combined. For example, to select multiple ranges, the radial
and range methods can be combined.

Options can be attached to pointer to modify functionality. For example,
user can specify degree to say how deep to go or use a custom function
to filter.

## Data Nodes

These include any lisp data types including function objects, numbers 
etc. ASCII keys produce char nodes. To produce a string
node, user must convert a linear segment of nodes using the predefined
function, and then the string can be read as data using the read 
function. Numbers must also be converted to be used as numbers since
they are considered ASCII keys and thus pressing 1 on the keyboard will
produce a char type instead of a number type 
(consider an option for this?).

## ID Nodes

All nodes are given a string ID (or symbol?). These are nodes that are 
attached to whatever node they are identifying. They can be reattached
to another node to change its ID. A single node can have multiple IDs
which all reference it.

Users can reference the ID node itself by using a single quotation mark.

All nodes have IDs, including pointers

Pointers - p0...
Nodes    - n1...
Edges    - e2...

Camera   - c3...

# Progam Operation

``protoform.lisp`` will fork two processes, then exit:
* Model
* View

``model.lisp`` initialization:
1. Setup lparallel kernel and queues
2. Run initialization functions in parallel
   * Shared memory - owned by model so responsible for cleanup
   * Load glyphs
   * Libinput
   * Input callbacks
   * Connect to view (server)
     * View will memcpy initial shm
3. Run threads
   * RPC client which handles connection to view
   * Input dispatcher which polls libinput for events

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

NOTES:
1. Should controller execute ptrees for non-frame tasks? Where should
this occur? This creates concurrency issues because now have to make
sure frame (RPC thread) is modfying data used by controller thread (or
whatever thread executes it)

# User Operation

Three graphs are presented to the user:
1. Timeline symbols - undo graph; when functions are executed they are
moved here including input data
2. Protoform/default/implicit symbols - default program options etc.
3. User/runtime/explicit symbols - symbols created by user through eval/REPL

Primitive Ops
- Add/Remove nodes (default is to link automatically)
  - ascii -> edge+node
  - backspace/delete -> edge+node
- Add/Remove edges
  - must type command after dangling edge
- Eval node(s)
  - shift + enter: eval node from string-chars-cursor backwards -> newline node
  - alt   + enter: eval node
    - skips read-from-string
    - usually data will be quoted or in list already so simply eval
  - option: undo
    - t: save
    - nil: do not save

Base Ops
- Link/unlink nodes (implicitly add/remove edges)
  - This does not delete the node - could make option with unlink
- Swap node(s) locations
- Sawp ID->node (and node->ID?)
  - User types ID, then types swap, and magic happens
- Toggle node IDs
  - Assign to TAB?
- Center camera on node

Selections are performed through pointer + primitive ops