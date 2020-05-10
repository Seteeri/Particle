## FAQ

**Why structured data? Why not plain text?**

Plain text is convenient but does not scale. Extracting useful information through repeatedly parsing plain text becomes redundant and inefficient on larger scales (at tens or hundreds of thousands of pieces of information). In the case of Particle, the structured data are s-expressions so it is a relatively simple model to understand and parse; the Lisp implementation is also open-source.

**Why not Emacs, org-mode, or ParEdit et. al.?**

Emacs is undoubtedly powerful but also difficult to evolve due to its aging codebase. It could be rewritten, however, the goals for such a project remain undefined which hampers initial development from starting. In addition, Emacs is centered around editing buffers of text, however, modern users have a need to incorporate other data types.

To delve a little deeper into Emacs:

Emacs development lacks thought into other user interfaces since text/CLI is believed to be the one and only solution to everything. Text has its own inflexibilities as indicated by the existence of other software. Unfortunately, it is assumed users coming to Emacs share the Unix mindset, which is become less and less the case, as the digital world continues to evolve. This results in the current OOB experience: defaults use unfamiliar terminology and unfamiliar key chords instead of shortcuts, and the learning curve is a vertical wall, which when combined, is an excellent way to ward off new users.
  
Ultimately, users' learning is guided by their needs and their way into deeper features. The emacs default setup is akin to notepad, however, if users wanted a notepad application they would have already used one, which means the user will then attempt to enable additional features, which leads them to the vertical learning wall they must scale with no equipment and minimal visibility.

For example, most text editors and IDEs use panes, often with a navigator of sorts on the left, a utility pane at the bottom, and a main content pane with tabs. If you take the initial Emacs experience and attempt to have the user setup a similar configuration, it requires learning an entire system at once due to the way it was designed. Most users think go to the settings, and enable plugins. However, in order to do this in Emacs, before doing anything, the user must navigate using key chords, which they are expected to have already memorized.
  
**What about note-taking apps like Evernote, OneNote, Notion etc.?**

All existing applications build their data structure around the interface, which ultimately runs into limitations. Particle uses the opposite approach and builds the UI around the data structure. In addition, they are all lacking some fundamental features.

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

**Is this the same as Microsoft's OLE or Apple's OpenDoc systems which both failed?**

Not quite. OLE was a closed proprietary standard, in which OpenDoc was developed as an open standard. Both failed for various reasons. Data formats/standards exist because different programs written in different languages structure their data in different ways which makes for a complex ecosystem of formats. Additionally, some programs may be closed-source to be made unavailable for other programs to use which requires reverse-engineering. Unfortunately, this is all at the expense of the user, as data from one program cannot be used as data in another program unless a proper converter or importer is used, which leads to a waste of programming efforts that could be used towards more novel purposes.

Fortunately, the world has been moving towards open source. Some of the most widespread formats include XML, associated with Ant and office programs, and JSON associated with Javascript. However, S-Expressions are arguably simpler and can serve both roles as a generic serialization format and syntax for a programming language. Thus, it can represent both data and code simultaneously.

**There already exists Jetbrains MPS...**

...

**Pictographs are a poor solution; only text can represent code.**

The idea is not to replace text but to include the use of images, side-by-side and intertwined with the former (I believe Light Table and Mathematica had/have similar ideas). Most programs will keep these domains separate which discourages and impedes the user from exploring the system. Plain text remains the most robust universal medium for communication, however, it is not the only medium, and it is not the mindset that laypersons generally approach a computer with. Without providing a method for users not naturally inclined to programming to properly utilize ever-growing computing power, the benefits of technology will not be fully reaped. Most textbooks, including scientific and mathematical ones, typically include pictures to demonstrate certain concepts, so text and images need not be mutually exclusive. At best, one can hope that maybe the system will encourage people to develop computational understanding. DSLs provide a gradual curve into that.

**Why PicoLisp?**

To build on the previous section, PicoLisp was chosen for the reason (and for other beneficial reasons) that it is a pure Lisp where all data consists of cons cells, with only three basic types: Pairs, Numbers, and Symbols which all consist of cons cells; strings are also symbols.

This consistency makes it easy to reason about the system by connecting the linked-list structure of the UI directly to the underlying linked-list data structure (or more specifically linked cons cells), and because Lisp code can manipulate data through eval, the UI can be dynamically modified through itself. This means operating on the UI (or the data representation) is the same as operating on the underlying data which has the same representation.

At the end of the day, you could say it is using a list metaphor instead of a desktop metaphor since what's really important is that TODO list on your desk and not so much the desk itself ;)

**Does this roughly offer the same benefits that the old lisp machines provided?**

This response by lispm to ChyrsaLisp might be relevant to attempts at recreating Lisp Machines:

https://news.ycombinator.com/item?id=15466124

> No.

> I expect this will be a fairly controversial comment, so I want to preface this by saying that I'm a big Lisp fan (just look at my handle). Lisp is my favorite programming language. I've been using it for nearly forty years. My first Lisp was P-Lisp on an Apple II in 1980. And I worked on Symbolics Lisp machines in the 1990s. They were very cool, but there's a reason they failed: general-purpose computing is infrastructure, and the economics of infrastructure are such that having a single standard is the most economical solution, even if that standard is sub-optimal. For better or worse, the standard for general-purpose computing is the C machine.

> Because it's general-purpose you certainly can run Lisp on a C machine (just as you could run C on a Lisp machine). You can even do this at the system level. But Lisp will always be at a disadvantage because the hardware is optimized for C. Because of this, C will always win at the system level because at that level performance matters.

> But that in and of itself is not the determining factor. The determining factor is the infrastructure that has grown up around the C machine in the last few decades. There is an enormous amount of work that has gone into building compilers, network stacks, data interchange formats, libraries, etc. etc. and they are all optimized for C. For Lisp to be competitive at the system level, nearly all of this infrastructure would have to be re-created, and that is not going to happen. Even with the enormous productivity advantages that Lisp has over C (and they really are enormous) this is not enough to overcome the economic advantages that C has by virtue of being the entrenched standard.

> The way Lisp can still win in today's world is not by trying to replace C on the system level, but by "embracing and extending" C at the application level. I use Clozure Common Lisp. It has an Objective-C bridge, so I can call ObjC functions as if they were Lisp functions. There is no reason for me to know or care that these functions are actually written in C (except insofar as I have to be a little bit careful about memory management when I call C functions from Lisp) and so using Lisp in this way still gives me a huge lever that is economically viable even in today's world. I have web servers in production running in CCL on Linux, and it's a huge win. I can spin up a new web app on AWS in just a few minutes from a standing start. It's a Lisp machine, but at the application level, not the system level. My kernel (Linux) and web front end (nginx) are written in C, but that doesn't impact me at all because they are written by someone else. I just treat them as black boxes.

> I don't want to denigrate ChrysaLisp in any way. It's tremendously cool. But cool is not enough to win in the real world.
