## FAQ

**Why structured data? Why not plain text?**

Plain text is convenient but does not scale. Extracting useful information through repeatedly parsing plain text becomes redundant and inefficient on larger scales (at tens or hundreds of thousands of pieces of information). In the case of Particle, the structured data are s-expressions so it is a relatively simple model to understand and parse; the Lisp implementation is also open-source.

**Why not Emacs, org-mode, or ParEdit/Parinfer/Smartparens etc.?**

Emacs is undoubtedly powerful but also difficult to evolve due to its aging codebase. It could be rewritten, however, the goals for such a project remain undefined which hampers initial development from starting. In addition, Emacs is centered around editing buffers of text, however, modern users have a need to incorporate other data types with non-linear structures.

**What about Evernote, OneNote, Notion etc.?**

[The Sad State of Personal Knowledgebases](https://marcusvorwaller.com/blog/2015/12/14/personal-knowledgebases/)

> Today most people don’t use a PK but they do, it’s almost certain to be Evernote or OneNote or something along those lines, basically a flat list of notes that’s easily searchable and taggable or folderable. Power users might use a personal wiki.
>
> To me, all of these seem comparable to using a roll of toilet paper to write a book. You can do it, but there are better ways. Some of better options exist now, but I think that we’re still far from having a great personal knowledgebase.
>
> In a perfect world a PK would have the following features:
>
>  * Good search.
>  * Unlimited size. Since it will be used to store just about everything you want to save for your whole life it needs to handle getting big well.
>  * Simple to use. It should have zero learning curve for someone who just wants to dump a bunch of notes in it and a fast learning curve for anyone wanting to use more powerful features.
>  * Convenient and fast. It should be available online or offline on your phone or tablet or laptop or wherever else you might want to use it. Adding content to it should be as close to effortless as possible and accessible from within other apps.
>  * Structured. It should work fine without any organization but should allow for very flexible relationships between notes and, now that basic AI is becoming more viable, it should suggest relationships intelligently.
>
> Surprisingly, no software with all those features exists yet. There are some interesting options though...

**There have been many attempts at projectional/structured editors, presentation-based interfaces. Why yet another one? Why not use McCLIM?**

Originally, I started this project in Common Lisp and encountered CLIM, which is a descendant of the Symbolics Genera-based Lisp Machine's Dynamic Windows/Lisp Listener. Its central feature is the concept of presentation types, commands and transformers. 

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

**Is this another visual language like Eve etc.?**

The goal of Particle is not to be a pedagogical platform for programming nor a visual programming language in itself, however, the hope is the user can implicitly understand how the system works through using it, i.e. manipulating its data structures. Then, should the user want to learn programming, they will already have gained some of the fundamental concepts, at least to program in Lisp.

That being said, I believe learning programming initially through concepts rather than initially through code may be an easier route for the less abstract-minded people. Concrete code is meant to represent abstract concepts. For some, it is difficult to learn both systems at once and so it is easier to learn one system at a time.

> I’m a huge proponent of designing your code around the data, rather than the other way around, and I think it’s one of the reasons git has been fairly successful… I will, in fact, claim that the difference between a bad programmer and a good one is whether he considers his code or his data structures more important. Bad programmers worry about the code. Good programmers worry about data structures and their relationships.

-- Linus Torvalds

**Why PicoLisp?**

To build on the previous section, PicoLisp was chosen for the reason (and for other beneficial reasons) that it is a pure Lisp where all data consists of cons cells, with only three basic types: Pairs, Numbers, and Symbols which all consist of cons cells; strings are also symbols.

This consistency makes it easy to reason about the system by connecting the linked-list structure of the UI directly to the underlying linked-list data structure (or more specifically linked cons cells), and because Lisp code can manipulate data through eval, the UI can be dynamically modified through itself. This means operating on the UI (or the data representation) is the same as operating on the underlying data which has the same representation.

At the end of the day, you could say it is using a list metaphor instead of a desktop metaphor since what's really important is that TODO list on your desk and not so much the desk itself ;)

**Does this roughly offer the same benefits that the old lisp machines provided?**

I found this response by lispm to ChyrsaLisp to be relevant in regards to attempts at recreating Lisp Machines:

https://news.ycombinator.com/item?id=15466124

> No.

> I expect this will be a fairly controversial comment, so I want to preface this by saying that I'm a big Lisp fan (just look at my handle). Lisp is my favorite programming language. I've been using it for nearly forty years. My first Lisp was P-Lisp on an Apple II in 1980. And I worked on Symbolics Lisp machines in the 1990s. They were very cool, but there's a reason they failed: general-purpose computing is infrastructure, and the economics of infrastructure are such that having a single standard is the most economical solution, even if that standard is sub-optimal. For better or worse, the standard for general-purpose computing is the C machine.

> Because it's general-purpose you certainly can run Lisp on a C machine (just as you could run C on a Lisp machine). You can even do this at the system level. But Lisp will always be at a disadvantage because the hardware is optimized for C. Because of this, C will always win at the system level because at that level performance matters.

> But that in and of itself is not the determining factor. The determining factor is the infrastructure that has grown up around the C machine in the last few decades. There is an enormous amount of work that has gone into building compilers, network stacks, data interchange formats, libraries, etc. etc. and they are all optimized for C. For Lisp to be competitive at the system level, nearly all of this infrastructure would have to be re-created, and that is not going to happen. Even with the enormous productivity advantages that Lisp has over C (and they really are enormous) this is not enough to overcome the economic advantages that C has by virtue of being the entrenched standard.

> The way Lisp can still win in today's world is not by trying to replace C on the system level, but by "embracing and extending" C at the application level. I use Clozure Common Lisp. It has an Objective-C bridge, so I can call ObjC functions as if they were Lisp functions. There is no reason for me to know or care that these functions are actually written in C (except insofar as I have to be a little bit careful about memory management when I call C functions from Lisp) and so using Lisp in this way still gives me a huge lever that is economically viable even in today's world. I have web servers in production running in CCL on Linux, and it's a huge win. I can spin up a new web app on AWS in just a few minutes from a standing start. It's a Lisp machine, but at the application level, not the system level. My kernel (Linux) and web front end (nginx) are written in C, but that doesn't impact me at all because they are written by someone else. I just treat them as black boxes.

> I don't want to denigrate ChrysaLisp in any way. It's tremendously cool. But cool is not enough to win in the real world.


**Is this the same as Microsoft's OLE or Apple's OpenDoc systems which both failed?**

Not quite. OLE was a closed proprietary standard, in which OpenDoc was developed as an open standard. Both failed for various reasons. Data formats/standards exist because different programs written in different languages structure their data in different ways which makes for a complex ecosystem of formats. Additionally, some programs may be closed-source to be made unavailable for other programs to use which requires reverse-engineering. Unfortunately, this is all at the expense of the user, as data from one program cannot be used as data in another program unless a proper converter or importer is used, which leads to a waste of programming efforts that could be used towards more novel purposes.

Fortunately, the world has been moving towards open source. Some of the most widespread formats include XML, associated with Ant and office programs, and JSON associated with Javascript. However, S-Expressions are arguably simpler and can serve both roles as a generic serialization format and syntax for a programming language. Thus, it can represent both data and code simultaneously.
