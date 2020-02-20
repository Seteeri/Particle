## The Premise

[A comment made by lispm, an old time lisper, on the concept of
a Lisp OS](https://news.ycombinator.com/item?id=15466124):

> I expect this will be a fairly controversial comment, so I want to 
> preface this by saying that I'm a big Lisp fan (just look at my 
> handle). Lisp is my favorite programming language. I've been using it 
> for nearly forty years. My first Lisp was P-Lisp on an Apple II in 
> 1980. And I worked on Symbolics Lisp machines in the 1990s. They were 
> very cool, but there's a reason they failed: general-purpose computing
> is infrastructure, and the economics of infrastructure are such that 
> having a single standard is the most economical solution, even if that
> standard is sub-optimal. For better or worse, the standard for 
> general-purpose computing is the C machine.
>
> Because it's general-purpose you certainly can run Lisp on a C machine
> (just as you could run C on a Lisp machine). You can even do this at 
> the system level. But Lisp will always be at a disadvantage because 
> the hardware is optimized for C. Because of this, C will always win at
> the system level because at that level performance matters.
>
> But that in and of itself is not the determining factor. The 
> determining factor is the infrastructure that has grown up around the 
> C machine in the last few decades. There is an enormous amount of work
> that has gone into building compilers, network stacks, data 
> interchange formats, libraries, etc. etc. and they are all optimized 
> for C. For Lisp to be competitive at the system level, nearly all of 
> this infrastructure would have to be re-created, and that is not going
> to happen. Even with the enormous productivity advantages that Lisp 
> has over C (and they really are enormous) this is not enough to 
> overcome the economic advantages that C has by virtue of being the 
> entrenched standard.
>
> The way Lisp can still win in today's world is not by trying to 
> replace C on the system level, but by "embracing and extending" C at 
> the application level. I use Clozure Lisp. It has an 
> Objective-C bridge, so I can call ObjC functions as if they were Lisp 
> functions. There is no reason for me to know or care that these 
> functions are actually written in C (except insofar as I have to be a 
> little bit careful about memory management when I call C functions 
> from Lisp) and so using Lisp in this way still gives me a huge lever 
> that is economically viable even in today's world. I have web servers 
> in production running in CCL on Linux, and it's a huge win. I can spin
> up a new web app on AWS in just a few minutes from a standing start. 
> It's a Lisp machine, but at the application level, not the system 
> level. My kernel (Linux) and web front end (nginx) are written in C, 
> but that doesn't impact me at all because they are written by someone 
> else. I just treat them as black boxes.
>
> ...But cool is not enough to win in the real world.

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


### The Linux Desktop

The chances of a Linux *desktop* are virtually nil:

1. Ecosystem/infrastructure - most popular programs are developed for Windows 
and MacOS first

2. Mobile computing - desktop segment is less important to the average user as
computing shifts to mobile devices and the cloud

3. Feature cost - open-source versions may not have feature parity with 
proprietary solutions, and thus financial cost will not outweight features; not 
to mention, predictable guranteed support for paid versions, whereas open source
depends on when developers have time to respond; maybe this can be offset when
more reliable crowdfunding platforms

4. Learning cost - learning a new system may not be worth it unless it provides
a significant technological advantage, which again FOSS programs tend to lack
feature parity compared to their proprietary counterparts; they may have a few 
significant features but it may or may not be enough.

5. Social cost - programming languages follow trends, and so do programs; FOSS 
tends to lack marketing or advertising, although social media has alleviated
some of this

6. Peformance - as long as programs run fast enough on similar platforms, 
performance is a minor issue

* Vendor lock-in/subscriptions are another possible reason to switch to FOSS (like
CERN).
* Privacy issues and data breaches (security) is another possible reason to
switch, although still not relevant enough for the average user

In other words, Windows and MacOS users will not adopt a desktop and relearn
a different set of procedures that results in the same functionality that their
current desktop already provides (see book "Diffusion of Innovations" by 
Everett Rogers). GNOME, KDE, Cinnamon, MATE, Xfce, etc. have some unique 
features, but at the end of the day, it's just another desktop and thus the same 
underlying model for interacting with a computer.

**The way for a future FOSS system does not lie with the desktop but with the 
computing needs of tomorrow.**

With everything just stated, Wayland, systemd debate, Moore's Law, and finally 
emerging AR/VR technologies, I believe this presents an opportunity to redefine
the computing environment. Particle is not an entirely new idea, but a 
different attempt to build upon the success and failures of those before.

## The Delta

There have been numerous implementations and attemps at structured or 
projectional editors going all the way back to the days of Interlisp-D and to 
the new projects being crowdfunded. However, several issues have yet to be 
addressed:

* Integration with existing interfaces (the desktop)
* Homoiconic GUI
  * Requires meta-circular evaluator of language implementation + 
  homoiconicity of language design
  * The nodes must be able to edit nodes (itself)
  * The editor must be able to edit itself

Considering computing technology becoming increasingly integrated into our
everyday lives, the bottleneck to leveraging computing most effectively and 
driving innovation will be the connection between man and machine, and just as
important, managing the information overload occuring today. What good is having
enormous amounts of information available at your fingertips if you cannot parse
it?

The average user is familiar with GUI widgets, but what if those were completely
dynamic and an intrinsic property, similar to how Emacs allows everything to be 
customized through elisp. Teaching a lay user how to program is not a trivial 
task as it is inherently based upon the user's cognitive ability, a reflection 
of the mind if you will (the history and usefulness of VPLs are a good study). 
Having observed Blender's nodal workflow and Unreal's Blueprint system, I am
attempting to combine those models of interaction with the analogy of LEGOs, 
while at the same time allowing for the entire system to remain hackable to 
programmers to maintain a gradual learning curve.

I do not believe it is about dumbing down computers and technology to make it 
easier to learn, but creating the right tools around programming to drive 
motivation and learning.
