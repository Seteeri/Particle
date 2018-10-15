Architecture
============

The basic architecture of Protoform is based on two components similar
to the MVC/PAC pattern for UIs.

In concrete terms, each component is a separate process. They all 
communicate with each other through the Swank server/protocol in a P2P 
fashion. Data is shared through mapped shared memory.

It is based on the principle of data and transforming data, similar to
flow based programming.

# Main Concepts

## The Model

Input:  Multiple
Output: Multiple

This process contains the raw data that the user ultimately cares about.
It is responsible for any processing of data up to serializing data to
shm for the view.

It acts as the primary server/node that view and controller connects to.

It the functions to render text through Pango/Cairo, and it also 
contains functions that hook into other libraries.

### Input

This process is basically a wrapper around libinput. 

Model runs an infinite loop around this that receives input events and
dispatches handlers

The model/input converts user physical interactions into model data
transformations ending up as the view.

## The View

Input:  Multiple
Output: Single (display)

This process contains the OpenGL functionality and objects. 

It has the single responsibility of managing buffer objects, including 
creation/destruction, memcpy from shm to buffer objects, and of course,
drawing. 

The reason is to minimize drawing latency to maintain visual fidelity 
as much as possible.

This also allows for multiple views to connect to model, and at the same
time it is possible for multiple models to connect to one view.

# Core UI Concepts

The nodal interface is based on a directed graph (acyclic?) and consists
of vertices/nodes and edges which connect the vertices.

Underneath, nodes and edges are both visually rendered with the same
model, except for edges being scaled between the nodes they connect.

## Relationship to Widgets

All widgets are based on an underlying tree structure and can be broken
down into nodes.

**Explain core widgets and corresponding nodal structure**

## Nodes

* Universal - apply fn to all; set setting for future pointers
* Global - apply fn to all; set setting to all present pointers
* Local - apply fn to single; set setting to present pointer

## Pointer Nodes

A pointer node is as the name states - it points to a node. However, it
is a node like any other node and thus can be operated on just the same.
All nodal functions have an option to ignore the pointer which is set
to t by default.

A pointer node selects a single node. In order to select multiple nodes,
users can use the following constructs:

* Point Select - straight-forward; attach any number of nodes to ptr
* Range Select - attach ptr to ptr to mark end to define a range

Selection/Filter:
* Extents (all)
* Specific n level (default, n=0)
* Specific filter (fn)

To select the entire graph, user can point to the root node; multiple
graphs can be attached to pointer node.

* Implement point select for now...range opens up another set of problems
involving topology and traversal

## Function Nodes

Three components:
* In(put)
* Out(put)
* Opt(ionals)

## Satellite Nodes

Satellite nodes provide guidance on the pointee by displaying possible
operators available.

## Controls

