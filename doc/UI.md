User-interface
============

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

## Pointer Nodes

A pointer node is as the name states - it points to a node. However, it
is a node like any other node and thus can be operated on likewise. All
nodal functions have an option to ignore the pointer which is set to t 
by default.

The keyboard can only manipulate one pointer, however, that pointer
can in turn manipulate other poitners. A touchscreen interface should
allow for manipulating as many pointers as there are touch events.

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

# User Operation

The underlying data model is the directed graph. On a fundamental level,
there exists only vertices and edges, i.e. any objects can be linked to
other objects.

Planes are used for their simplicity, efficiency, and convenience. Many
objects are modeled as quads such as windows, pixels are typically
square, glyph rendering and metrics, etc. Other shapes can be used
however, there are tradeoffs such as using more advanced models as nodes
will lower the amount of instances that can be rendered.

Three graphs are presented to the user:
1. Timeline symbols - undo graph; when functions are executed they are
moved here including input data
2. Protoform/default/implicit symbols - default program options etc.
3. User/runtime/explicit symbols - symbols created by user through eval/REPL

## Primitive Ops

These have bindings associated with them due to their primitiveness

- Add/Remove nodes (default is to link automatically)
  - ASCII -> edge+node (default for char objects?)
  - Backspace/Delete -> edge+node (default for char objects?)
- Add/Remove edges
  - Option: dangling edges
  - [lk/uk] Link/unlink pointed node from pred
    - unlinks all by default
  - [lkp/ukp] Link/unlink pointer from pointee
    - unlinks all by default
- Eval node(s)
  - shift + enter: eval node from string-chars-cursor backwards -> newline node
  - alt   + enter: eval node
    - skips read-from-string
    - usually data will be quoted or in list already so simply eval
  - option: undo
    - t: save
    - nil: do not save
- Cut/Copy/Paste (Ctrl-X/C/V)
 - Cut: move node to right of pointer (alt+right)
 - Copy: copy and move node right of pointer (alt+up)
 - Paste: move node from right side to left side of pointer if possible (alt+left)
   - Nothing on the left - creates a free node
 - Use arrow keys to move objects also? -> Use alt + arrow?
 - After cutting a bunch of stuff, user can seperate into a graph or
   perform other basic operations...
- Help: F1 or Ctrl H or 911? or type it - 4 strokes
  - Move node to clear area (or dimension) and display satellite nodes

## Base Ops

These have alternative shortened names. Fast way to execute these
functions is to unlink pointer -> type command -> eval -> link pointer.
Or newline before above so eval has marker to stop.

- Undo/Redo? (Ctrl-Z/Y) - exception
- [nn] Swap node(s) locations
- [in/ni] Swap ID->node (and node->ID?)
  - User types ID, then types swap, and magic happens
- [tn][shift+tab] Toggle node IDs
- [cc] Center camera on node
- [bi/ubi] Bind/unbind keys

Selections are performed through pointer + primitive ops

## Default Controls

* Functions
 * Attachments: In(put), Out(put), Opt(ions)
* Pointer
  * Type+eval: (make-node-pointer)
  * Default is to select all children
    * Universal - apply to all/future
    * Global - apply to all/present
    * Local - apply to single
      
* Escape = exit
* Meta = toggle Wayland
* ASCII/Enter/Tab = create child node of selected node with char
  * Optimize spaces by not drawing them?
  * Or create separate buffer for them?
  * Can at least save shader time
* Shift + Tab = toggle node IDs
* Shift + Enter = eval ptr
* Backspace/Delete = destroy pred/suc
* Arrows = movement; default is pointer
  * Use zoom to control distance? so screen distance is consistent
  * Ptr - default/shift:
    * XY (nearest node)
      - or when attached to node, keep hopping nodes; if user breaks
      link, then go free mode until reattach
      - (spatial:intergraph;semantic:intragraph)
    * +Shift = Z?
      - use to move between layers?
    * +Shift+Alt = ?
    * +Shift+Alt+Ctrl = ? 
  * Camera - ctrl:
    * Ctrl = XY (nearest node)
      - follow pointer
    * Ctrl+Shift = Z
      - fit to next nearest node on screen bounds?
    * Ctrl+Shift+Alt = ?
  * Graph (semantic) - +Alt with above
    * Up/Down = move up/down level
    * Left/Right = move sideways on same level

* Alt+Dir = help/associated nodes (toggle or hold?)
* Chunk into fours
* Show transformers for data type
* How to show???
  1. Show on another layer - interrupts UI?
  2. Teleport the node
  3. Move/push other nodes out
  4. Show on another dimension