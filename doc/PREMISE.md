PREMISE
=======

# Inspirations

* Primary:

  * Transformers (Beast Wars), Digimon, Reboot, Tron - bridging the divide
  * Compiz - 3D desktop effects
  * Firefox Tree Style Tab addon  
  * Lisp/Lisp Machines - the programmable programming language
  * Emacs/Org-mode - consistency, extensibility, text and keyboard driven
  * CLIM - presentation-based interface
  * The Humane Interface by Jeff Raskin
    * Elimination of modes
    * Persistence
    * Unlimited undo
    * Incremental search
    * Elimintation of applications -> commands
    * Redesigned file hierarchies
    * ZUIs
  * Evernote - Stepan Pachikov wanted to remember everything
  
* Secondary:
  
  * Uzbl/Conkeror - numbered links for navigation
  * Unreal Blueprints - nodal system
  * EagleMode - ZUI system
  * Douglas Engelbart - "The Mother of All Demos"
  * Xerox PARC - pioneered many modern computing elements and paradigms
  * Sir Tim Berners-Lee - WWW
  * Paul Graham, Peter Norvig - applied Lisp
  * Brett Victor - Inventing on Principle
  * Zach Bean - quicklisp
  * Robert Strandh - CLOSOS/LispOS
  * Chris Schafmeister - Molecular Metaprogramming  
  * Oberon OS
  
# The Problem

Most programs have different modes and corresponding interfaces (aka functions, operators, methods) We define a mode as a set of interfaces for a type of object.

Examples:
* Mode A can exist for objects of type A; mode B can exist for objects of type B
* Objects of type A can have different modes, or different types of objects can use mode A
  
This last example is important to note for this discussion.

## The Spectrum

For example, in a code editor such as Emacs, there may be various objects that contain text such as text buffers and GUI elements, however, the text cannot be manipulated with the same tools in the same way consistently. Of course, there are valid reasons for why GUI text cannot be modified etc. however, for the purposes of this discussion, we assume the goal is to have a fully open modifiable dynamic environment.

On the other end of the spectrum, taking to an extreme *The Single Responsibility Principle* or *Separation of Concerns*, ends up overwhelming the user by reintroducing complexity by creating too many concepts, albeit simple, that need to be managed. In the context, of programming, defining different functions with the same arguments that do the same thing (assuming argument signature irrelevant to function identity). This would also increase complexity for the containing class/module/package and thus the program overall.

## The Learning Curve and Complexity

The more types there are and the more interfaces there are, the more contexts the user has to keep track of, such as they have object of type x so what interface applies to type x?

As the complexity increases, so does the learning curve. One could say, programming is creating/learning simultaneously. Functions are identified by their functionality and/or arguments, and objects are identified by their members and/or methods. Once familiarized, choosing what function for what object or vice versa, induces heavy context switching in the user's mind between two domains - the solution (tool) and the problem.

It's easier for most people to see things physically built up as objects upon objects, rather than as functions upon functions like a process. For example, an object in a factory could be seen as an object evolving through an assembly line or as the various functions on the assembly line, taking that object through a process.

If one were to be able to use a single interface for any object, that would simplify that aspect of complexity. The inverse is to have one object and various functions that all accept that object as an argument, which would be a functional perspective; however, this has not proven to be the most popular method for reasons not discussed here. Like most things, the answer lies somewhere in the middle. 

The key to making it feasible is objects/functions need to be flexible and dynamic enough to be created/destroyed/transformed/viewed dynamically - this is provided by the Lisp environment and the OpenGL environment.

## Emacs Example

In Emacs, there is the mainbuffer and the minibuffer for commands; to go further, text also exists in the menus and status bar. In order to execute commands, the user has to shift their focus to the minibuffer and its contextual properties such as limitations compared to the main buffer. Conventionally, the user highlights text, and then switches context to type/execute commands. 

In Protoform, both code and text/data (i.e. command) exist in the same "buffer". Basically, code can be placed *arbitrarily* and executed *arbitrarily*. If Emacs literally did this, the code would look jumbled because of the mixture of commands/output. The tree/nodal structure allows it to be inherently structured (as a branch of text).

To summarize, all code and data is visualized in the same context - the user can then explicitly change the context seamlessly across domains.

## Blender Example

As a DSL grows and expands (without implementing further DSLs), it begins to turn into a GPL.

In many programs, scripting languages are implemented to allow the user to extend the program. In open source software, editing the source/patches also remain an option. However, for proprietary software, this is the most viable solution to allow the user to extend the code in isolation from the proprietary code.

In Blender, the Python language is used to extend the program, including the UI. In the Blender Game Engine, logic bricks exist as an alternative VPL to Python. For simple programs, they work well. However, in practice, as the number of blocks grows, so does the complexity, which results in spaghetti code (quite literally). One way to resolve this is to give users better facilities to visually manage the logic bricks. At some point, the user may want to introduce customized or advanced logic, however, there are only so many capable bricks and the only way to create new bricks is to go into the source code to implement one. This requires an understanding of the C language and the internals of the game engine. Other major issues with scripting languages include performance and interoperability.

Unreal Engine's blueprints face similar issues of spaghetti code.

#### BRAINSTORM

visual DSL = symbols?

literally combine alphanumeric glyphs/symbols to form compound symbols which can be represented with an arbitrary glyph (icon in conventional terms)

still requires the user to have a basic understanding of the computing process similar to functions and visual nodal systems

# The Lightbulb

*draw diagram*

The consistency of PicoLisp (LISP) makes it easy to reason about the system by connecting the linked-list structure of the UI directly to the underlying linked-list data structure (or more specifically linked cons cells), and because Lisp code can manipulate data through eval, the UI can be dynamically modified through itself. This means operating on the UI (or the data representation) is the same as operating on the underlying data which has the same representation.

The relationship to other objects at the presentation level is the same as objects to the underlying data model, and the data model's relationship to other objects is the same at the presentation level.

The other issue is how to draw graphs and trees (which are mathemtically speaking, a subset of), which is an NP-complete problem. Node editors have shown that once a graph or tree gets to a certain size it can become unweildly and unmanageable (aka sphagehtti code). The same issue has arisen with graphing programs. 

With Particle, it restricts the layout of trees and automatically lays out the branches. It is a balance between giving the user complete freedom to the point they easily manage to shoot themselves in the foot versus taking the gun completely away from them. However, should the user want to be able to arbitrarily layout objects, it can be done programmatically.

## Origins

Originally, Particle began in Python, then moved to Common Lisp, at which point I encountered CLIM, which is a descendant of the Symbolics Genera-based Lisp Machine's Dynamic Windows/Lisp Listener. Its central feature is the concept of presentation types, commands and transformers. 

I think one of the issues it failed to become largely popular was, first, it did not appeal to developers outside the Lisp ecosystem as it was an interface manager and not just a toolkit (similar to what Qt has evolved into today), and secondly, it did not interact well with the established paradigms of fixed widgets. In some ways it reminds me of a display manager in that similar to the X windowing system, it too provided similar drawing primitives (which are today deprecated in favor of manipulating the buffer directly by toolkits etc.).

https://www.reddit.com/r/lisp/comments/22lbpe/whatever_became_of_clim/

> The core of it is: there is a single mechanism in which associations between objects (value, etc.) represented in the user interface by widgets (view, component, graphic, element, etc.) are stored, so that commands and other interactions can be defined on the object type, once independently of the specific visual representation being used.

> This differs from "data binding" by exposing the domain objects, not just data, and therefore introducing the notion of commands operating on the objects.

> This differs from "naked objects" (generating UI from domain objects) by admitting different UI representations of the same object.

However, I wanted to approach the issue from a top-down or hollistic perspective by building a higher-level environment that more conceptually vertically integrated, analagous to modding a game versus using a game engine framework like Unity or Unreal etc. The integration of the interface maximizes the reuse of concepts and by extension commands, since everything is patterned in Lisp, short of the kernel itself (another discussion). It naturally provides discoverability and thus a gradual learning curve as one ventures deeper into the system by learning new types beyond the fundamental Lisp types. This isn't really new - this was one of the wonderful things about Lisp Machines, at least from what I've researched.

This could be viewed as violating the Unix philosophy by integrating everything, however, I believe it still maintains the core tenets. The Unix philosophy is centered around the C model style of programming where processes communicate through piping text implemented with Bash scripts. Arguably, Lisp has a more dynamic approach but with an underlying model of lists and atoms based on cons cells.

Looking at the current ecosystem, the alternative is what has been witnessed over the last several decades -fragmentation, which depending on how it is viewed is also the freedom to change components. However, for some users, an integrated approach is needed for a more productive experience rather than an exploratory one currently favored.

https://groups.google.com/forum/#!topic/comp.lang.lisp/XpvUwF2xKbk%5B101-125%5D

> The value in Genera's Dynamic Windows is NOT about "completion" it's
> about object-oriented connected-ness of the entire system.  You aren't
> clicking on the "name" in dynamic windows.  You are clicking on the EQ
> object.  This means you can, for example, mouse an instance of
> something and get that instance as an object to inspect and
> manipulate.  Emacs has NO basis for equivalent stuff.  None.  If
> you've seen Genera only through screen shots, you could never imagine.
> It's like trying to explain someone whose only seen streams of text
> what an object pointer is.

The key insight is that Lisp data structures are based upon singly linked lists. "Lists" being the operative word. Lists are more common to the layperson than most people realize. For example, when one creates an outline on paper or uses bullets in a word processing program or an outliner itself, etc. they are creating a structure or an interface. The alphanumeric or pictorial icon, indentation and spaces indicate the structure. They indicate which words or data belong to which other words or data.

Effectively, Particle enforces a hierarchical (graph) structure to the UI which makes conventional widgets and toolkits obsolete. For the layperson, all they must know to use the program are lists and strings, and their basic operations like delete, newline, cut/copy/paste which are already conceptually familiar. Eventually other types are encountered, however, because the user already knows the basic commands, they can use the pre-existing knowledge to explore newer types and functionality.

In contrast, Emacs relies on the concept of (gap) buffers and strictly manipulating arrays of characters (arrays), which works well for solely manipulating text, *and other objects granted they have a textual representation*. The inability to express other data types and interact with them in other ways is ultimately what led me to consider other approaches; it's aging codebase being difficult to evolve another main issue.

In Particle, a list of windows is literally a list of windows and the same list operations apply (exchange windows for arbitrary data type of your choice). Of course, there can be more specialized commands also, however, that is moreso an issue of discoverability. The fundamental concern of the UI is a tradeoff between the developer having complete design freedom versus the user having to learn it.

The goal of Particle is not to be a pedagogical platform for programming nor a visual programming language in itself, however, the hope is the user can implicitly understand how the system works through using it, i.e. manipulating its data structures. Then, should the user want to learn programming, they will already have gained some of the fundamental concepts, at least to program in Lisp.

That being said, I believe learning programming initially through concepts rather than initially through code may be an easier route for the less abstract-minded people. Concrete code is meant to represent abstract concepts. For some, it is difficult to learn both systems at once and so it is easier to learn one system at a time.

> I’m a huge proponent of designing your code around the data, rather than the other way around, and I think it’s one of the reasons git has been fairly successful… I will, in fact, claim that the difference between a bad programmer and a good one is whether he considers his code or his data structures more important. Bad programmers worry about the code. Good programmers worry about data structures and their relationships.

-- Linus Torvalds

# Epilogue - Year of The Linux Desktop

The chances of a Linux *desktop* are virtually nil (from a commercial perspective):

1. Loss of ecosystem/infrastructure - most popular programs are developed for mobile, Windows and MacOS first; Linux is an afterthought; in addition, there are more developers and tools already existing

2. Mobile computing - desktop segment is less important to the average user as computing shifts to mobile devices and the cloud

3. Feature cost - FOSS may not have feature parity with proprietary solutions so financial cost will not outweigh functionality; commercial software has predictable guranteed support, whereas effective support for FOSS depends on a large enough community existing and ultimately when developers have time to respond

4. Learning cost - learning a new system may not be worth it unless it provides a significant technological advantage, which combined with the former point, the user is less likely to switch; some FOSS programs may have a few significant features but it may or may not be enough

5. Performance benefit - as long as programs run fast enough on similar platforms, performance is no longer the central issue as the rise of dynamic and scripting languages have shown including the growth of Python and the infamous Javascript; security and stability are just as important. FOSS programs are mixed in this regard, exception being the Linux kernel which is heavily developed with commercial resources from both software and hardware vendors.

6. Vendor lock-in benefit - another possible reason to switch to FOSS (like CERN); privacy and security issues are another possible reason to switch, although still not quite relevant enough for the average user

With regards to FOSS desktops, each has some unique features, but at the end of the day, it's just another desktop and the same underlying model for interacting with a computer. In other words, Windows and MacOS users will not adopt a desktop and relearn a different set of procedures that results in the same functionality that their current desktop already provides (see book "Diffusion of Innovations" by Everett Rogers).

However, replacing the desktop with a FOSS desktop is not the key point towards a better computing future. Considering computers have become integrated into our everyday lives, the bottleneck to leveraging computing most effectively and driving innovation, will be the connection between man and machine and managing the information overload occuring today; the dominance of Google and its search engine exist for a reason.

Learning to program, effectively and efficiently, is not a trivial task. It requires a great deal of abstract thinking similar to mathematics, which is notorious for its difficulty beyond basic arithmetic. However, I do not believe all hope is lost, and similar to the traits of a good teacher, a program should have similar traits; it is not solely about dumbing down computers and technology to make it easier to learn, but creating the right tools to drive motivation and gradual learning.

**The way for a future FOSS system does not lie with the desktop but with the computing needs of tomorrow.**
