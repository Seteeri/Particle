PREMISE
=======
  
# The Problem

A program has different operators that apply to different objects. We define this set of operators that work on a specific object as a mode. So when a user is working on an object of type A, they can only use operators belonging to a set, called mode A. Modes need not be mutually exclusive and can overlap.

Examples:
* Mode A can exist for objects of type A; mode B can exist for objects of type B
* Objects of type A can have different modes, or different types of objects can use mode A
  
This last example is important to note for this discussion.

## The Spectrum

For example, a text editor typically has a text area, a menu, buttons and various widgets that all contain text objects, however, the text cannot be manipulated the same way consistently across objects. Text cannot be selected in a button the same way it can in a text area (Whether it is meant to be static or dynamic in a hypothetical system). Taken to an extreme, such as in a 3D creation program, every object has its own mode with minimal overlap, which the user must become familiar with to be effective and productive.

On the other end of the spectrum, taking to an extreme *The Single Responsibility Principle* or *Separation of Concerns*, the numerous modes end up overwhelming the user by reintroducing complexity through absolute simplicity effectively removing the concept abstractions which are necessary to manage complexity.

## The Learning Curve and Complexity

It should be obvious that the more types there are and the more type-specific operators there are, the more contexts the user has to keep track of, which increases the mental burden on the user and directs their attention away from the problem they are trying to solve.

As the complexity increases, so does the learning curve. Functions are identified by their functionality and arguments, and objects are identified by their type. Once familiarized, choosing what function for what object or vice versa, induces heavy context switching in the user's mind between two domains - the solution and the problem.

In the first step, the user identifies the problem, secondly, conjures the abstract solution in their mind, and thirdly, then implements said solution using elements of the system, or more specifically the objects, operators, and modes of the program (and programming language). Finally, the process is repeated. The less mental gymnastics required to implement said solution, i.e. the third step, the more efficient and productive the user becomes in solving problems until they deem there are virtually no problems left to solve and the system is deemed complete.

A system can be seen as objects built upon objects like parts of a car or functions taking in functions like a process. For example, an object in a factory could be seen as an object evolving through an assembly line or as the various functions on the assembly line apply functions to that object.

The current desktop has various applications with specific functionality that operate on different types of data implemented in different programming languages resulting in a user needing to learn each program's modes to be effective. One of the reasons for this is for proprietary and commercial reasons. Another reason is for the subjective reasons a programming language is chosen typically for infrastructure/social, financial cost, optimization/performance, or size reasons.

However, a few basic commands typically persistent between programs that most laypersons are familiar with both as a function and as a keyboard shortcut; often referred to as common user actions (CUA), such as New, Open, Save, Exit, Cut, Copy, Paste, Delete, Find.

This begs the question, why can programs not be made to reduce this burden on the users? In other words, is it possible to create a system where users can use those CUA across all programs on all objects?

## The Unix Legacy

The design of Unix has has a significant influence on the modern OS's. The Unix philosophy prefers to treat all data as text or better yet streams of bytes, which goes hand-in-hand with the C language and arrays. While text is still important and those methods useful, the computer medium has evolved to support data types beyond text such as multimedia and objects. However, in order to operate on those non-text objects, requires them to be represented as raw bytes.

Using a C struct as an inter-program format or IPC format, requires both programs to have the exact same memory layout and the same type sizes. However, programs can use different libraries which results in different layouts and different compilers that result in different type sizes, so it cannot be guaranteed. Trying to guarantee memory layout between programs would require all programs involved using the same compiler and compiler settings, same libraries built the same way and so on, which would create a lot of maintenance overhead, and cooperation and agreement between parties. Additionally, in a proprietary or commercial context, this is not feasible. FOSS can solve the sharing of data problem, however, it by itself is unlikely to solve the social aspects. In the FOSS ecosystem, rarely is there a unanimous concensus - DE/WMs/widget fragmentation, and the never-ending creation of new standards are evidence of this.

As a result, various serialization formats have emerged, popular text-based ones being JSON, XML, YAML, and popular binary ones being MsgPack and ProtocolBuffers. On top of that, many programming languages have language-specific protocols. Again, most of them exist for the fact that these formats are operated on by different programming languages which have different data types which may or may not map to the types supported by the format.

As the FOSS ecosystem has shown, having an open format is not enough for it to be successful, the language used need also be successful, which we'll define as widely used in the professional industry. For a language to be successful, it usually has to capitalize on an emerging frontier in computing, which typically leads to commercialization. Lisp had Lisp Machines, but the ecosystem did not transfer over to the newer architecture, and the infamous AI winter occured. C was developed to make assembly programming easier at Bell Labs with Unix. Java had Sun Microsystems. C++ was an alternative to Java. Javascript was the language of the first popular web browser, Netscape. As for Python, my best guess is it focused mostly on the humanistic aspect: readability, extensibility, and maintainability, which made it easy to learn, at the cost of performance when computers became fast enough to run interpreters at a relatively decent speed. The list goes on.

As abstractions build on abstractions, when a problem is identified and unable to be fixed at the current level, the user then attempts to fix the problem at a more fundamental level. If it is unable to be fixed, then a new layer of abstraction is built on top to offset that, the current state of the x86 architecture and commodity platforms good examples. Interestingly, Lisp allows the user to extend the language to fix the problem instead of complicating the system with more abstractions. Hence, when the entire system is made of Lisp down to the metal, the problems in the software domain are capable of being resolved thoroughly.

That begs the question, if that computing paradigm is better, why is it not popular today? 

Maybe for reasons outlined by Peter Gabriel - "worse is better" verus "the right thing". Maybe because of the monopoly MS had on the PC market. Maybe it was technology that was too early for its time.

However, the ecosystem and infrastructure that has been established around the C and Unix model is difficult to overcome. However, just as technology continues marching forward, new opportunities will present themselves. With the rise of mobile computing, multi-core processors, and the increasing connectivity of the world, abstractions that we have built are no longer flexible enough to take advantage of multi-core and parallel computing. The solution remains unknown.

## The Lisp Rebirth

Lisp was originally commercialized around the single programmer managing their entire stack down to the hardware. 

## Why did Lisp Fail?

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



#### BRAINSTORM

visual DSL = symbols?

literally combine alphanumeric glyphs/symbols to form compound symbols which can be represented with an arbitrary glyph (icon in conventional terms)

still requires the user to have a basic understanding of the computing process similar to functions and visual nodal systems

# The Lightbulb

*draw diagram*

The consistency of PicoLisp (LISP) makes it easy to reason about the system by connecting the linked-list structure of the UI directly to the underlying linked-list data structure (or more specifically linked cons cells), and because Lisp code can manipulate data through eval, the UI can be dynamically modified through itself. This means operating on the UI (or the data representation) is the same as operating on the underlying data which has the same representation.

The relationship to other objects at the presentation level is the same as objects to the underlying data model, and the data model's relationship to other objects is the same at the presentation level.

The other issue is how to draw graphs and trees (which are mathemtically speaking, a subset of), which is an NP-complete problem. Node editors have shown that once a graph or tree gets to a certain size it can become unweildly and unmanageable (aka sphagehtti code). The same issue has arisen with graphing programs.

With Particle, it restricts the layout of trees and automatically lays out the branches. It is a balance between giving the user complete freedom to the point they easily manage to shoot themselves in the foot versus taking the gun completely away from them. Too much restriction results in a tabular UI, and too little restriction results in sphagehtti nodes. However, should the user want to be able to arbitrarily layout objects, it can be done programmatically.

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
