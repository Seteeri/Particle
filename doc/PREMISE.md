PREMISE
=======

These articles will help provide a basis for understanding concepts presented here:

[Martin Fowler's Projectional Editing](https://martinfowler.com/bliki/ProjectionalEditing.html) 

[Understanding Compilers — For Humans (Version 2)](https://towardsdatascience.com/understanding-compilers-for-humans-version-2-157f0edb02dd)

# Two Interfaces, One Model

It's helpful to think about a program having two interfaces:

* machine (developer) - interface through APIs
* human (end user) - interface through GUIs/CLIs

Typically developers are also end users, however the inverse relationship does not hold true, end users are not always developers. Both use a series of representations to model the system, which will be described next.

## Multiple Representations

![Representations](https://github.com/Seteeri/Particle/blob/master/art/workbench.gif)

*Source: Martin Fowler's Projectional Editing*

    +---------------------+----------------------------------------+
    | Type                | Format                                 |
    +---------------------+----------------------------------------+
    | editable/projection | Text, Visual, GUI                      | 
    +---------------------+----------------------------------------+
    | storage/persistence | Text                                   |
    +---------------------+----------------------------------------+
    | abstract            | ADT: AST, e.g. LLVM IR SSA, GNU GIMPLE |
    |                     | (generally not immediately accessible) |
    +---------------------+----------------------------------------+
    | executable          | machine code,  e.g. x86, ARM, WASM     |
    +---------------------+----------------------------------------+

For example, in a typical IDE, the developer interacts with a projection represented with text/GUI such as GTK or Qt. The code will also be stored as text with UTF-8 encoding (the GUI configuration is usually saved elsewhere in an arbitrary binary format). Embeded documentation may also have its own markup language. The compiler will then take the code as input and generate abstract data structures to produce a final executable file. 

Text acts as the lingua franca or LCD in this ecosystem in order to accomodate multiple programming languages and their paradigms (arguably PLs are developed to solve a specific problem). Ultimately, this is geared towards the goal of producing the most optimized fastest executables for each language.

This advantage is at the detriment of the user and developer. In terms of code, there is much repetition, duplication, and redundancy, such as reparsing or reanalyzing text/code, which increases the chances of bugs. It is also difficult to understand the entire process and system due to the complexity steming from highly differentiated abstractions all being tightly coupled to each other. This then extends further and has implications for the end user resulting in a convoluted interface creating a confusing and frustrating experience.

What would happen, if each of these abstractions had the same format -> interfaces making them understandable to each other, and that format was both data and code?

In other words, what if the code was stored/persisted as Lisp, the abstraction (AST) was Lisp, the executable representation was Lisp, and the projection was Lisp?

This is what long ago Lisp Machines did. The PicoLisp language encodes Lisp for the AST, persistence, and executable abstractions. The last part missing is the projection abstraction, which I believe is where many attempts have failed at creating alternative interfaces - finding a way to make the projection abstraction the same as the others.

Particle explores that concept, particularly the projection aspect, and then hopes to take it one step further with today's technology.

Originally, this project was attempted in Python and Common Lisp but became only possible with PicoLisp, because *all* data is based on cons cell structures retained during runtime, which allows the interface and data to be homoiconic. For example, a C array would break this principle (for interop, they can be symbolized through a number, i.e. pointer). One interesting implication is when characters (`str`) are input via keystrokes, they exist as cons cells so there is no reading or parsing.

The primary advantage is to the user and developer - understandability and composability - leading to lower development time and increased productivity.

The primary disadvantage is runtime speed - the only way to achieve faster interpreting is at the hardware level - a Lisp CPU with firmware/microcode to interpret the Lisp data directly, where the Lisp primitives are the instruction set = assembly.

# Unification
  
Particle implements an old idea from Lisp Machines, which lives on today in McCLIM, where the representation is seperate from the underlying data ("presentation-based"). 

*Why it is no longer popular, is another discussion...too open? disallows vendor lock-in and thus software monetization?*

Most programs treat text and images differently and so users think and interact with them differently. Conventional GUIs use the same concept in the form of widgets. This means text editors can be thought of as one-dimensional serial pictographs.

## Projection Representation

In the projection representation, rather than view text and images as distinct types semantically, they are considered equals. In other words, glyphs *are* images, which can also be called icons, or more operatively - symbols. Images are Lisp data are code, which means they can be freely mixed interchangably with any data, thus an image can be directly passed to a function or itself can also be a function. 

Particle enforces a hierarchical (tree/graph) structure to the UI which makes conventional toolkits/widgets superfluous. For the layperson, in the context of text, they need only know the concepts of lists and strings, and their basic operations like delete, newline, cut/copy/paste which are already conceptually familiar. Eventually other types are encountered, however, because the user already knows the basic commands, they can use the pre-existing knowledge to explore newer types and functionality to build an effective mental model.

To better visualize this, the concept of a Pixel data type (visual representation = `[]`):

* Represents a list of 4 numbers from 0-255 (RGBA)
* Represents a square with the corresponding color

...then take the concept of an Image data type

* Represents a list of Pixel


What happens if two pixels are added? It could blend the colors into a single Pixel

    (+ [] []) -> []

What happens if two lists of Pixel are added? It could create a list aka new Image

    (+ ([]) ([])) -> [[]]

At this point, it is possible to perform operations without typing any explicit code, or simply done with drag-drop. However, the textual interface still exists! The Pixel can be directly referenced by its pointer representation `$177334050465605` or assigned to a normal symbol `(def *p0 [])`.

Let's take another example, a Window with a live interactive image representing a running GUI, and put some into a list:

    ([] [] [] [])

This Window can then be used to grab screenshots or process properties from `ps` etc.

    # The screenshot symbol could also be an icon for example
    (screenshot [])
    
    (ps [])

This idea can be used to create literal symbols to represent arbitrary data, while retaining the functionality of the underlying Lisp structure all within the same program.

A user would build up to an Image and then transfer that representation to another type/class all in realtime. It would be as if you were able to create native nodes/abstractions in a nodal type interface such as Blender's Node Editor or Unreal's Blueprints.


## Storage/Persistence Representations

Because all representations have the same underlying abstraction, they only differ with regards to their domain usage.

The fundamental class is the `+Point` class which references the program data in its `any` property and the display data used for rendering by OpenGL in its `verts` property. These objects are persisted to disk.

The `+Point` and `verts` data is considered the projectional representation and the `any` property is considered the abstract/persisted/executable representations.

This means to run the program, the executable representation can be "pulled" out of the projectional representation to remove unessential features or provide a main +Point referencing a function that can be evaluated.

Comments are also considered data, thus are stored with the projections. The comments themselves can be considered projections of the abstract data (see Intentional Programming). In other words, there exists a `+Comment` class, subclass of +Point, whose `verts` representation is arbitrary and not a literal representation of `any` data like normal.

# The Origins

After reviewing much of the existing software and attempts around building new interface paradigms, I noticed a common theme among all of them. They all used the traditional GUI like windows, widgets etc, resulting in the same inevitable ending; the GUIs were static and they were all separated from their underlying data - the data was designed around the UIs rather than the reverse. In the end, this made the project simply another visual theme but no different than conventional systems, effectively defeating the whole purpose of the project in the first place. 

This led me to start with thinking about the most popular interface - text - and to trace its history and re-evaluate its functionality. There are numerous attempts to create a more expressive command line or REPLs.

## The Lisp Paradox

[The Evolution of Lisp](https://wiki.c2.com/?TheEvolutionOfLisp)

> Lisp-based computers developed at MIT and Xerox. The MIT line went commercial with SymbolicsMachine, LispMachinesIncorporated (LMI), and later TexasInstrumentsExplorer which subsumed LMI. Commodity hardware (MooresLaw) and better compiler techniques erased the cost/benefit ratio for a special purpose LispMachine compared to stock hardware for general purpose computing. LMI, one of the last companies developing Lisp hardware, went bankrupt before it could bring a new RISC-based Lisp Machine to market.

[The Scheme Machine (1994) [pdf]](https://news.ycombinator.com/item?id=17706589)

> Quick question: why did we stop producing lisp machines, or any other machine more closely related to Lambda calculus model of computation than a Turing machine? Is it merely cultural, or are there technical reasons as to why we stopped producing them competitively?

> The target markets were too small (or even were shrinking) to justify the investments necessary to keep the architectures going. Keep in mind that all in all only around 10k machines were produced over the technology lifetime (75-92). That's not a large number. Lisp machines started with hand-made CPUs and ended with special micro-processors (TI, Symbolics, ...). The jump to Lisp-supporting newer RISC (or similar) architectures did not happen, because the main sponsors (DARPA, Military, etc.) did no longer saw them as interesting - applications could run on workstations and high-end PCs. Next-gen CPUs were in the making in end 80s / early 90s (Symbolics, Xerox, LMI, SPUR...) but they did not reach the market - money was running out. The whole thing had its last incarnation in commercial emulators: Medley (the Interlisp-D from Xerox) for Intel+SPARC and Open Genera (Symbolics) for DEC Alpha.

Another response:

> In short Moore's Law.

> Between 1970 and 2010 if you could design special purpose hardware that ran 10 times faster than the state of the art you would need to get it to market in volume in under 3 years.

> If you took any longer the general purpose CPUs from Intel would by that point be within spitting distance of your superior architecture, at a fraction of the cost.

> That's what happened to Symbolics, general purpose PC's could run their software faster than the dedicated machines they designed. 

These articles explain the demise of Symbolics:

https://danluu.com/symbolics-lisp-machines/

http://web.mit.edu/6.933/www/Symbolics.pdf

Despite its tragic history, I believe Lisp's homoiconic nature, arguably the only defining feature of Lisp left, is the key to a better interface. Not to mention, the Lisp ecosystem never strayed far from its original academic/scientific niche, so maybe its worth a shot to apply its principles to a general audience (not counting Apple's NewtonOS and Dylan). Last but not least, I don't think Lisp was ever meant to be operated on with a text editor and has always held its true power back...

Originally, Particle began in Python, then moved to Common Lisp, at which point I encountered CLIM, which is a descendant of the Symbolics Genera-based Lisp Machine's Dynamic Windows/Lisp Listener. Its central feature is the concept of presentation types, commands and transformers. 

I think one of the issues it failed to become largely popular was, first, it did not appeal to developers outside the Lisp ecosystem as it was an interface manager and not just a toolkit (similar to what Qt has evolved into today), and secondly, it did not interact well with the established paradigms of fixed widgets. In some ways it reminds me of a display manager in that similar to the X windowing system, it too provided similar drawing primitives (which today are deprecated in favor of manipulating the buffer directly by toolkits etc.).

https://www.reddit.com/r/lisp/comments/22lbpe/whatever_became_of_clim/

> The core of it is: there is a single mechanism in which associations between objects (value, etc.) represented in the user interface by widgets (view, component, graphic, element, etc.) are stored, so that commands and other interactions can be defined on the object type, once independently of the specific visual representation being used.

> This differs from "data binding" by exposing the domain objects, not just data, and therefore introducing the notion of commands operating on the objects.

> This differs from "naked objects" (generating UI from domain objects) by admitting different UI representations of the same object.

However, I wanted to approach the issue from a top-down or hollistic perspective by building a higher-level environment that more conceptually vertically integrated, analagous to modding a game versus using a game engine framework like Unity or Unreal etc. The integration of the interface maximizes the reuse of concepts and by extension commands, since everything is patterned in Lisp, short of the kernel itself (another discussion). It naturally provides discoverability and thus a gradual learning curve as one ventures deeper into the system by learning new types beyond the fundamental Lisp types. This isn't really new - this was one of the wonderful things about Lisp Machines, at least from what I've researched.

This could be viewed as violating the Unix philosophy by integrating everything, however, I believe it still maintains the core tenets. The Unix philosophy is centered around the C model style of programming where processes or programs, which do one thing well, communicate through piping text implemented with Bash scripts. Arguably, Lisp has a more dynamic approach but with an underlying model of lists and atoms based on cons cells.

I find open-source users fall into two camps, pragmatists and explorers, or should I say two user modes. The former being more convergent with software being a means to an end, and the latter being more divergent with software configuration being a means in itself. Open source is equally attractive for both groups as it allows one to optimize their workflow to accomplish their goals by modify the systems appropriately, and for the latter it grants users the freedom to experiment with their software stacks like LEGOs.

However, for some users, an integrated approach is needed for a more productive experience rather than an exploratory one currently favored. Looking at the current ecosystem, the alternative is what has been witnessed over the last several decades -fragmentation, which depending on how it is viewed is also the freedom to change components.

## The Lisp Lightbulb

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

In contrast, Emacs relies on the concept of gap buffers and strictly manipulating arrays of characters (arrays), which works well for text, *and other objects granted they have a textual representation*. The inability to express other data types and interact with them in other ways is ultimately what led me to consider other approaches; it's aging codebase being difficult to evolve another main issue.

The goal of Particle is not to be a pedagogical platform for programming nor a visual programming language in itself, however, the hope is the user can implicitly understand how the system works through using it, i.e. manipulating its data structures. Then, should the user want to learn programming, they will already have gained some of the fundamental concepts, at least to program in Lisp.

That being said, I believe learning programming initially through concepts rather than initially through code may be an easier route for the less abstract-minded people. Concrete code is meant to represent abstract concepts. For some, it is difficult to learn both systems at once and so it is easier to learn one system at a time.

> I’m a huge proponent of designing your code around the data, rather than the other way around, and I think it’s one of the reasons git has been fairly successful… I will, in fact, claim that the difference between a bad programmer and a good one is whether he considers his code or his data structures more important. Bad programmers worry about the code. Good programmers worry about data structures and their relationships.

-- Linus Torvalds

## The PicoLisp

The consistency of PicoLisp (LISP) makes it easy to reason about the system by connecting the linked-list structure of the UI directly to the underlying linked-list data structure (or more specifically linked cons cells), and because Lisp code can manipulate data through eval, the UI can be dynamically modified through itself. This means operating on the UI (or the data representation) is the same as operating on the underlying data which has the same representation.

The relationship to other objects at the presentation level is the same as objects to the underlying data model, and the data model's relationship to other objects is the same at the presentation level.

The other issue is how to draw graphs and trees (which mathematically speaking, the latter a subset of), which is an NP-complete problem. Node editors have shown that once a graph or tree gets to a certain size it can become unweildly and unmanageable (aka sphagehtti code). The same issue has arisen with graphing programs.

With Particle, it restricts the layout of trees and automatically lays out the branches similar to writing Lisp. It is a balance between giving the user complete freedom to the point they easily manage to shoot themselves in the foot versus taking the gun completely away from them. Too much restriction results in a tabular UI, and too little restriction results in sphagehtti nodes. However, should the user want to be able to arbitrarily layout objects, it can be done programmatically, possibly to demonstrate spatial relationships.

*external symbols, OS, database and how it ties together, lispos*

Unix is based on files, and files are based on b-trees so its trees all the way down anyways!

http://okmij.org/ftp/papers/DreamOSPaper.html

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

# Context Information

## Compilers

Compiler Pipeline:

![Compiler Pipeline](https://github.com/Seteeri/Particle/blob/master/art/compiler-flow.jpeg)

*Source: Understanding Compilers — For Humans (Version 2)*

Compiler Pipeline for LLVM:

![Compiler Pipeline for LLVM](https://github.com/Seteeri/Particle/blob/master/art/bitcode.png)

*Source: https://lowlevelbits.org/pdfs/bitcode.pdf*

[Why does LLVM have an assembly-like IR rather than a tree-like IR? Or: why do projects target LLVM IR instead of clang's AST?](https://softwareengineering.stackexchange.com/questions/355759/why-does-llvm-have-an-assembly-like-ir-rather-than-a-tree-like-ir-or-why-do-pr)

## Interpreters

AST Interpreter Pipeline:

![AST Interpreter Pipeline](https://github.com/Seteeri/Particle/blob/master/art/inter-flow.jpeg)

*Source: Understanding Compilers — For Humans (Version 2)*

Lisp Example:

![Lisp Pipeline](https://github.com/Seteeri/Particle/blob/master/art/reader.png)

*Source: https://kanaka.github.io/lambdaconf/#/*
