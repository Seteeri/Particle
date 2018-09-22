Architecture
============

The basic architecture of Protoform is based on two components similar
to the MVC/PAC pattern for UIs.

In conrete terms, each component is a separate process. They all 
communicate with each other through Unix domain sockets in a P2P fashion.
They pass data through mapped shared memory.

They are capable of using Swank to eval LISP code directly and they also
contain protocols for minimal latency.

It is based on the principle of data and transforming data, similar to
flow based programming.

## The Model

Input:  Multiple
Output: Multiple

This process contains the raw data that the user ultimately cares about.
It is responsible for any processing of data up to serializing data to
shm for the view.

It acts as the primary server/node that view and controller connects to.

It the functions to render text through Pango/Cairo, and it also 
contains functions that hook into other libraries.

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

## The Controller

Input:  Single?
Output: Single

The controller is essentially the view since the user is able to edit 
both the model and view through the same interface.

The user never sees the model directly but ultimately operates on the 
data through the view. User sees the nodes that represent the model data
and manipulates them directly.

The idea is consistency between the components although they are 
separated.

## Input

This process is basically a wrapper around libinput. 

It runs an infinite loop that receives input events and forwards them to
model to respond to them.

The model/input converts user physical interactions into model data
transformations ending up as the view.

Should just make this a thread of model?