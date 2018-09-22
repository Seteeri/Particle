Architecture
============

The basic architecture of Protoform is based on three components similar
to the MVC pattern for UIs.

In conrete terms, each component is a separate process. They all 
communicate with each other through Unix domain sockets in a P2P fashion.
They pass data through mapped shared memory.

They are capable of using Swank to eval LISP code directly and they also
contain protocols for minimal latency.

It is based on the principle of data and transforming data, similar to
flow based programming.

However, the user is able to edit both the model and view through the
same interface.

## The Model

This process contains the raw data that the user ultimately cares about.
It is responsible for any processing of data up to serializing data to
shm for the view.

It acts as the primary server/node that view and controller connects to.

It the functions to render text through Pango/Cairo, and it also 
contains functions that hook into other libraries.

## The View

This process contains the OpenGL functionality and objects. 

It has the single responsibility of managing buffer objects, including 
creation/destruction, memcpy from shm to buffer objects, and of course,
drawing. 

The reason is to minimize drawing latency to maintain visual fidelity 
as much as possible.

This also allows for multiple views to connect to model, and at the same
time it is possible for multiple models to connect to one view.

## The Controller

This process is basically a wrapper around libinput. 

It runs an infinite loop that receives input events and forwards them to
model to respond to them.

Should just make this a thread of model?