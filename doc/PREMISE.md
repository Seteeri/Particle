## The Premise

Most programs have different modes and corresponding interfaces (aka functions, operators, methods)
We define a mode as a set of interfaces for a type of object.

Examples:
* Mode A can exist for objects of type A; mode B can exist for objects of type B
* Objects of type A can have different modes, or different types of objects can use mode A
  
This last example is important to note for this discussion.

### The Spectrum

For example, in a code editor such as Emacs, there may be various objects that 
contain text such as text buffers and GUI elements, however, the text cannot be 
manipulated with the same tools in the same way consistently. Of course, there 
are valid reasons for why GUI text cannot be modified etc. however, for the 
purposes of this discussion, we assume the goal is to have a fully open 
modifiable dynamic environment.

On the other end of the spectrum, taking to an extreme 
*The Single Responsibility Principle* or *Separation of Concerns*, ends up 
overwhelming the user by reintroducing complexity by creating too many concepts,
albeit simple, that need to be managed. In the context, of programming, defining
different functions with the same arguments that do the same thing (assuming
argument signature irrelevant to function identity). This would also increase
complexity for the containing class/module/package and thus the program overall.

### The Learning Curve and Complexity

The more types there are and the more interfaces there are, the more contexts the
user has to keep track of, such as they have object of type x so what interface
applies to type x?

As the complexity increases, so does the learning curve. One could say, 
programming is creating/learning simultaneously. Functions are identified by 
their functionality and/or arguments, and objects are identified by their 
members and/or methods. Once familiarized, choosing what function for what
object or vice versa, induces heavy context switching in the user's mind 
between two domains - the solution (tool) and the problem.

It's easier for most people to see things physically built up as objects upon
objects, rather than as functions upon functions like a process. For example, an
object in a factory could be seen as an object evolving through an assembly line
or as the various functions on the assembly line, taking that object through a 
process.

If one were to be able to use a single interface for any object, that would
simplify that aspect of complexity. The inverse is to have one object and
various functions that all accept that object as an argument, which would be a
functional perspective; however, this has not proven to be the most popular 
method for reasons not discussed here. Like most things, the answer lies 
somewhere in the middle. 

The key to making it feasible is objects/functions need to be flexible and 
dynamic enough to be created/destroyed/transformed/viewed dynamically - this is 
provided by the Lisp environment and the OpenGL environment.

### The GUIs and DSLs

Programming concepts apply the same to UIs! Using a GUI *is* visual programming
in a limited manner.

This concept ties into DSLs and APIs; both provide external functionality, however,
APIs are static and DSLs are dynamic, more specifically, DSLs have their own syntax.
What LISP provides is the ability to create languages (DSLs) within languages
ad infinitum. DSLs can inherit this ability from their parent language.

What if GUIs could modify themselves dynamically? This is similar to WYSIWYG GUI builders
except reflectively. This is not a new idea, however, AFAIK it has not been done in this
manner.

```
	 ...
	  ^
     / \
   DSL  \
	/    \
 GPL     GPL(LISP)

GPL = General Purpose Language
```

### The Key Points

* The language must be able to directly represent the underlying language
	* If something is hidden or inaccessible, the DSL is incomplete and will 
	  always be crippled (different than Turing completeness).
* The language must be able to extend the underlying language
	* The language must not just be able to modify itself like normal, but to
	  also modify its modification process.
	* This is done inherently through the language.
	   
## The Idea

The core idea of Protoform is to use text as the primary UI and to provide the
ability to extend itself (the UI) the same way LISP users extend the language.

Through the text UI, objects can be manipulated to build more advanced constructs.

### Emacs Example

In Emacs, there is the mainbuffer and the minibuffer for commands; to go further,
text also exists in the menus and status bar. In order to execute commands, the 
user has to shift their focus to the minibuffer and its contextual properties 
such as limitations compared to the main buffer. Conventionally, the user 
highlights text, and then switches context to type/execute commands. 

In Protoform, both code and text/data (i.e. command) exist in the same "buffer".
Basically, code can be placed *arbitrarily* and executed *arbitrarily*. If Emacs
literally did this, the code would look jumbled because of the mixture of
commands/output. The tree/nodal structure allows it to be inherently structured
(as a branch of text).

To visualize this, the new commands/output is a branch of the current tree.
This allows unlimited versatility. After execution, the branch can remain like 
a tag or be moved after execution to another tree specifically designed for it
(akin to a mini buffer). The user can also create a new branch beforehand and 
attach commands/output there. In our model, Emacs buffer = s-expression/tree.

To summarize, all code and data is visualized in the same context - the user can
then explicitly change the context seamlessly across domains.

### Blender Example

As a DSL grows and expands (without implementing further DSLs), it begins to 
turn into a GPL.

In many programs, scripting languages are implemented to allow the user to extend
the program. In open source software, editing the source/patches also remain an
option. However, for proprietary software, this is the most viable solution to 
allow the user to extend the code in isolation from the proprietary code.

In Blender, the Python language is used to extend the program, including the UI.
In the Blender Game Engine, logic bricks exist as an alternative VPL to Python.
For simple programs, they work well. However, in practice, as the number of 
blocks grows, so does the complexity, which results in spaghetti code (quite 
literally). One way to resolve this is to give users better facilities to 
visually manage the logic bricks. 

At some point, the user may want to introduce customized or advanced logic, 
however, there are only so many capable bricks and the only way to create new 
bricks is to go into the source code to implement one. This requires an
understanding of the C language and the internals of the game engine. Other 
major issues with scripting languages include performance and interoperability.