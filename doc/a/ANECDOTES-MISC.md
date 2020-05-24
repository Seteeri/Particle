Anecdotes: MISC
===============

https://datenwolf.net/bl20110930-0001/


My dream graphics system was completely abstract. Creating a window didn't involve selecting visual formats, framebuffer configurations. It was just "a window". Only when actual content is involved I want to tell the rendering subsystem, which color space I use. Ideally all applications worked in a contact color space (e.g. CIE XYZ or Lab), but sending images in some arbitrary color space, together with color profile information. Fonts/Glyphs would be rendered by some layer close to the hardware, to carefully adjust the rasterizing to the output devices properties. And last but not least the whole system should be distributed. Being able to "push" some window from one machine's display, to another machine's (and this action triggering a process migration) would be pinnacle. Imagine you begin writing an email on your smartphone, but you realize you'd prefer using a "usable" keyboard. Instead of saving a draft, closing the mail editor on the phone, transferring the draft to the PC, opening it, editing it there. Imaging you'd simply hold your smartphone besides your PC's monitor a NFC (near field communication) system in phone and monitor detects the relative position, and flick the email editor over to the PC allowing you to continue your edit there. Now imagine that this happens absolutely transparent to the programs involved, that this is something managed by the operating system.


# 

Oberon
https://news.ycombinator.com/item?id=21383016


Oberon is a gift to the computing world. It is one of the few examples of a top-to-bottom design of a full computing system (The Smalltalk Blue Book is also like this, sans the hardware).

I think the world is ready, or almost ready, for a comeback in this kind of design. Given that we have so many open, compatible standards (JSON, Office Documents/XML, hell even TCP/IP, etc, etc), the "traditional" problems of esoteric systems and their incompatibilities with each other, like we had back in the late 80s and 90s, seem to melt away. We are at a point where we can and should be experimenting with completely new systems from the ground up. Otherwise we will never get anything new, and everything will just be some iteration of Unix for the rest of time -- like medieval scholastics endlessly debating Aristotle and not discovering anything truly novel. 


---


I liked your comment on Oberon as an example of what non-C/UNIX might look like. Your statement on language-integrated OS's implies there is a technical reason they didn't make it. It's a common mistake technologists make where they think of everything in technical aspects. Business (i.e. Marketing) and social effects are far, far, more prevalent a reason for success or failure of technologies in terms of adoption. Here's four factors that will answer lots of your questions by themselves:

1. Is it free or reduces costs in some way? UNIX on minicomputers is automatically going to get adopted if its design is useful just because of massive reductions in equipment costs.

2. Is it backwards compatible and/or does it easily integrate with the established ecosystem? IBM, Microsoft, and Intel used backwards compatible to grab markets followed by lock-in. You could say Linux/BSD eventually did with them piling more stuff on the old code. OpenBSD was the exception with them ripping stuff out. Easy example of second is languages compiling to and/or using libraries from established ecosystems like C, Java, .NET, and Javascript.

3. Is it familiar? Do they understand the concepts? And do the developers, picky about syntax, see a similar syntax? This boosted C++, Java and C# over the likes of Lisp, Prolog, or Haskell.

4. Big, dominant company pushes for X to be the language for their platform. That explains most of the big ones that are compiled and Javascript. The other factors helped, though.

These four factors are enough to absolutely kill Lisp and Forth machines in both commercial market and FOSS adoption. They don't make them impossible. They're just an uphill battle with a lot more work to do. We see communities such as Erlang and Haskell getting stronger despite being so different from established languages without being able to easily integrate with their ecosystems. Clojure [wisely] cheated by building the weird language on top of a massive ecosystem (Java platform). There's also projects aimed at Javascript trying to re-create the advantages of Lisp and Smalltalk. So, there's hope the integrated systems can make it either with lots of effort or (more wisely) just working with those factors instead of against them.

Also, it's not a big loss anyway given only a few of these integrated OS's were attempted in a big way. Over 90% of efforts fail to go anywhere. Only a handful of attempts were made. If anything, they might still be ahead of the odds in the long run. They just gotta stop creating unnecessary obstacles for themselves. 


---
---


https://news.ycombinator.com/item?id=9847955


Wow, this is excellent. The Oberon and Bluebottle OS materials have always been quite scattered, so someone putting them in a central index is quite convenient.

For those unaware, Oberon's main qualities are the fact that it's a full operating system written in a garbage collected Pascal-like language (actually made by the same person who initially wrote Pascal) which uses said language's module system to provide reusable/chainable interfaces throughout the whole OS, support for orthogonal persistence and most notably, its highly unconventional user interface that bridges the power of the CLI and the GUI together in this vaguely hypertext-like workspace where you can dynamically live program the UI itself through on-screen text that can serve as an entry point or continuation to perform all sorts of computations, things you'd normally write hacky scripts for. Closest analogue is Xerox's Cedar.

You should consider trying it and stealing a few ideas from it for the greater good. 


---


> on-screen text that can serve as an entry point or continuation to perform all sorts of computations, things you'd normally write hacky scripts for.

The Acme editor famously works this way. So, in a rudimentary way, does the Emacs scratch buffer. It's a weird amalgam of text editor and REPL: you evaluate snippets of text and get the output in-place, and then edit the text to get what you want next. This live-text-as-code way of working is not how I'm used to interacting with a programming system and always slightly rewires my brain. But even in that simple form, you can grasp in it the beginnings of a whole computational paradigm, all the way up to UI. (Computational models don't usually imply a UI—this is an exception, as are spreadsheets.) Oberon must be the most systematic realization of this. I'd like to try it. 


---
---


https://news.ycombinator.com/item?id=20037962


 I'm with you that pure text can be limiting as a medium for thinking and communication, and that using a wider range of visual expression like diagrams and illustrations can dramatically enhance understanding.

No visual programming environment I've seen compares to the expressivity of writing code - but I also consider myself a visual thinker, and I believe there's still a lot of potential for imaginative rethinking of what it means to code. Text is just a subset of visual communication, and there's no reason why we need to limit ourselves to what can be typed - maybe we could include dynamic interactive symbols as new "words", or program by visually constructing diagrams that include code..

The sweetspot seems to me, visual languages that also let you use code.



The ideal would be a language which has a non-ambiguous mapping between both textual and visual, so that you can freely switch between them as you wish, but I’ve never seen such a thing that was satisfactory and haven’t been able to come up with something myself. Having a visual AST or flowchart version of an otherwise textual language just doesn’t do it for me.

Failing that, maybe a hybrid thing that lets me do mathematical code and pure algorithmic work textually, but to do all of the higher level architecture and coordination, asynchronous code and stream processing visually.

Personally, I really enjoyed my experience with Max/MSP, I found it quite liberating in many ways and much (but definitely not all) of it suited my way of thinking closely enough that I could bypass the pen-and-paper to figure out complex things. I also really liked not having to name things (until I wanted to, at least) which I found made experimenting with ideas, before they were solid enough to put names to things, was also quite interesting. 


---
---


https://news.ycombinator.com/item?id=16227130


A couple of points:

1. Making programming accessible probably means making software more accessible.

I remember a study of what mental capacity was most correlated with being able to learn foreign language. It was empathy. So a large part is wanting to communicate. If you want to communicate, you will find a way, and I think we see the same thing in programming. So called "non-programmers" learn the most absurd programming languages and systems if they are motivated. Also see Minecraft. And amazing/horrifying Excel spreadsheets.

One of the issues I see with "novice environments" is that they tend to be very separate from everything else on the machine. I would find that very demotivating.

What I would love to see is "open source as if we meant it", meaning programs that do something we want to do, that we would use, written in such a way (probably also: in such a language) that tinkering/adapting is a reasonable proposition. Yes, that means I don't think that currently is the case: except for the core-dev team, is it a reasonable proposition for people who want to adapt GNUmeric to download the source code and start tinkering? For novices?

2. I am not convinced by the low-end vs. high-end distinction

I think a lot of the same things that make programming awful for beginners also make it awful for advanced programmers. We have just gotten used to the pain and accept it, though I am not sure why.

3. I am not convinced by starting over from scratch

There are reasons why we have what we have, and not all of them are bad.

4. I am not convinced by not starting over from scratch

Of the reasons why we have what we have, a lot of them are bad. A lot needs to be reexamined and rethought.

Resolving that contradiction (thesis, antithesis?) is difficult, it requires looking at what we have in a lot of detail, including the history of how we got here, where we need apply tweaks and where we can interact properly with the rich computing tapestry we have.

Just extending what we have is probably not a solution, because one of the problems is too much cruft, but just starting over from scratch is likely to lead to cool but ultimately superficial projects.


---
---


https://news.ycombinator.com/item?id=7488554


Memory is linear and so is execution( at a basic level ) yet our programming and ideas are not. Imagine programming as a needle with one continues thread stringing boxed functions and values together, sometimes threading something that has been threaded before, and ultimately creating a crisscross of wire that's hard to understand. We cannot organize something that is linear with a non-linear representation. We try to make the crossed wire simpler to understand with programming languages but it is inherently unsolvable as a problem. There will never be a authoritative programming language.


---
---


https://news.ycombinator.com/item?id=20754592


 	
cryptica 9 months ago [–]

This sounds like my nightmare operating system. Reusing the same command to operate on very different kinds of objects is a bad idea. A file is very different from a process. A database table is very different from a file. Whenever you try to homogenize operations on different kinds of objects under a single operation name, you inevitably lose flexibility or you have to add a lot of if-then-else statements inside the implementation code which increases cyclomatic complexity and leads to bugs.

Also I don't think it makes it easier. What if I want to delete a process but I accidentally end up deleting a file because I mistyped the name of the process (or the file has the same name as the process; which one should be deleted?)? I think we need separate commands because the user needs to be in a different mindset when doing these operations.

I'm actually not a huge fan of the Unix philosophy for that reason; you end up with a lot of general purpose commands which work together in theory and you can combine them in an infinite number of ways... But in practice, they are too general and this means that combining them becomes too slow for a lot of scenarios... If commands are too small and too general, you will always end up having to write long sequences of multi-lined commands chained together in order to do anything useful and performance will be bad; you might as well just write C/C++ code.

	
	
taffer 9 months ago [–]

I would make a distinction between querying and mutating the operating system state.

For querying information, I think the idea of representing everything as the same data structure really makes sense. The Unix philosophy or a relational view is perfect for this. However, for more sematically complex things that have possibly surprising side effects, such as starting a service, setting up a new user account, connecting a drive, installing a printer, I think we need separate commands or procedures to encapsulate all the complex behavior.

	
	
fouc 9 months ago [–]

He probably meant having the same internal interface, it would still be doable to have various aliased commands that call the same interface.

As for UNIX philosophy of stringing together lots of general purpose commands to get the desired result - the nice thing about that is you don't need to know programming, and it's very convenient for one off scripting.

	
	
nerdponx 9 months ago [–]

As for UNIX philosophy of stringing together lots of general purpose commands to get the desired result - the nice thing about that is you don't need to know programming, and it's very convenient for one off scripting.

I beg to differ. It's a workflow by and for programmers, or at least people who think like programmers. 


---


 dgellow 9 months ago [–]

Just a general question: Do you think there is a market for a new operating system? The cost of building something better or equal to mainstream systems seem too high to even consider challenging the status quo.

	
	
repolfx 9 months ago [–]

I think there is, but it'd need to be very different to existing operating systems to justify itself, and in ways that other OS's can't simply adopt for themselves. In practice that means radical architectural change, and even then you'd re-use a lot of code.

For such a project you don't really want to just adopt existing ideas and implement them. I don't really understand why Google is doing Fuschia for this reason: architecturally it's nothing special. Is the GPLd Linux so bad? It took them this far.

If I were to do a new OS I'd explore ideas like all software running on a single language-level VM with a unified compiler (e.g. a JVM), a new take on software distribution ... maybe fully P2P, rethinking filesystems and the shell, making it radically simpler to administer than Linux, etc. You'd need to pick a whole lot of fights and make a lot of controversial decisions to justify such a lot of work: you'd need to be right about things other people are wrong about, not just once but multiple times. Then maybe if people see it working well they'd join you and port existing software, albeit, the porting process would inevitably involve some rewriting or even deeper changes, as if you can just run existing software and get all the benefits you probably didn't change anything important.

	
	
skissane 9 months ago [–]

Build a new operating system, or build an operating environment that runs on top of an existing operating system, like how e.g. Windows used to run on top of DOS? Or User Mode Linux on top of Linux?

An operating environment means you don't have to worry about stuff like device drivers. It can run inside a Docker container. (At most companies, try suggesting "let's run a brand-new OS that nobody has heard of", and you'll get an emphatic "no"... say "here's this app we want to run, it is packaged as a Docker container", and often nobody will even ask what is inside that Docker container, even if it contains your operating environment with the app running on top.)

You can start out using facilities of the host OS like the filesystem and networking stack. Later, if you want to, you can implement your own filesystem (create a 100GB file on the host filesystem, pretend that is a block device and your custom filesystem exists within it). Or your own networking stack (which can run in user space using facilities like Linux tun/tap drivers.)

An operating environment can always evolve into a standalone operating system at a later date. 


---
---


A Little Clojure
https://news.ycombinator.com/item?id=22797858


Maybe this lisp syntax intro is a good place to ask veterans a related question.

I find s-expressions quite beautiful and simplistic way to express both logic and data as a tree. On the other hand, top-level evaluation rules are often glossed over, and seem to mess up this elegance. I feel like there is a set of braces missing at top level, for s-expression concept to be complete. I don't really know enough about specific rules or reasoning why they need to be introduced. My mental model of top-level execution is an implicit `(do ...)` block. In rest of clojure code the `do` is a special form that is frowned upon because it is used only when side-effects are necessary.

As it is, the 'module' concept is strictly tied to a file, which feels unnecessary. If top-level was enclosed in braces and evaluated with same rules as the rest of s-expressions, maybe we could move away from concept of files and, for example, store our code in a graph database. This would allow for IDEs that can collapse and expand code tree as needed.

Rich Hickey uses expression 'place oriented programming' as derogatory term (PLOP) for relying on exact memory addresses as places for our data. It would seem to me that same term would apply to our coding practices - we tie code implementation to specific locations inside files on disk. This seems like accidental complexity that introduces mental burden. If location of implementation could be divorced from specific location inside specific source file, we could concentrate on implementation that is always in context where code is actually used.

Is there any decent discussion of such concepts, or some other language that explores such direction? I'm lacking both terminology and in-depth knowledge of code evaluation to express this in more clear way. 


---


Yes, much of this space is fairly well trod, though completely agree the threads are hard to find. I'll try to point some out:

1. Clojure, top level forms, trees, and evaluation

Agree in part about top level evaluation rules in Clojure. It does seem, for instance, like the ns form should enclose the rest of the forms that comprise that namespace, rather than essentially doing something that is unusual for Clojure- silently changing what seems to be a global context.

When one digs a little deeper, however, there is a logic to those semantics. The main reason comes from the problem that the compiler faces in having to reconcile the use of a program thing- a symbol or name or variable or whatever-one-calls-the-named-elements that are used in a program- with the defining of that thing.

There are basically two approaches to this problem. The compiler can read all the source code, find all the definitions, then reread all the source code, and match the uses to those definitions- and only then inform the programmer if there is some problem where a use doesn't match or doesn't have a definition. Even with modern computers, for large programs, this is too expensive and time consuming.

What many compilers do instead is to require that any used names are defined "first". Clojure does this- it reads files from top to bottom, and it requires that any used names are defined earlier in the file.

This notion of earlier- this notion that things defined in a program have an ordering to them, not just in their execution but also in their composition- this is deep and pervasive, and puts the lie in the idea that a program is just a big tree.

One branch off this tree, so to speak, where the ordering in a file doesn't correspond to the compositional or execution ordering, is in the functional programming concept of monads.

2. Alternatives to file storage for program modules

It is a pretty old idea that files are a poor way of storing source code (sorry). There is a long train of work that Wikipedia summarizes poorly with almost no references under Source Code In Database: https://en.wikipedia.org/wiki/Source_Code_in_Database. The idea here is that persisting code in a data structure and providing more "intuitive" tools for editing is better than requiring humans to work in files.

(Microsoft even tried in the 1990s to roll out a version of Windows that used a database for pervasive structured storage, rather than a file system. This was a failure, and a lot has been written about it- google WinFS).

Plain text files have a lot of underappreciated ergonomic properties. Their use doesn't keep tools from utilizing clever data structures to assist in the management and authoring of code in files. The SCID work has ultimately found its way into the cool incremental helpers and structural editors that most IDEs use now (Cursive/Paredit for Clojure: https://cursive-ide.com/userguide/paredit.html)

Forgoing the text editing paradigm altogether takes you into the world of visual programming editors, which also have a long history. An influential player in the space from the early rise of personal computers was a product called ProGraph. This technique is also now pervasive in tools like Scratch, but also in big data where flow graphs for processing immense streams of data are often constructed using visual tools, for instance, Nifi.

3. Literate programming

Another thread is Literate Programming, originally invented by Don Knuth. The idea is that the most important consumers of a program are other humans, not the computer, so one should author using tools that create both an artifact that a human can read, with both prose and code interspersed- as well as the code itself for a compiler to consume. But the combined prose/code artifact is a better way for communicating to other humans about the semantics of a program, than just the code.

This is a particular endearing thread, and the tool called Marginalia in the Clojure world provides something of the experience that Knuth intended.

4. Version control and program semantics

Yet another relevant thread is in version control, where the inability of files to keep the history of a programming authoring process is addressed. Early in Clojure's life Rich Hickey created a tool called Codeq: https://github.com/Datomic/codeq that loaded a git repo- essentially a graph of changes to program files- into a Datomic database- where Datomic can be seen as a graph db.

There has been some more recent work to be able to run semantic queries on those graphs. This is immensely useful for looking for patterns of code that may have security problems. A company doing a lot of work in this space is called Source(d).

Another set of tools for mining version control comes from a company called Empear, started by a programmer named Adam Tornhill. His work- originally in Clojure- looks at things like patterns of paired changes across files. Cases where the same sections of code in the same files are changed in the same commits demonstrate high "coupling" and are a "code smell."

==

All of this is really about building and maintaining a semantic model from the syntactic artifacts, which is what I read you as being ultimately interested in. There's a lot more, but that's all I have time for now. Hope that's helpful. 


---
--- 


WinFS

https://hal2020.com/tag/winfs/

https://www.reddit.com/r/programming/comments/1svf88/why_winfs_died_long/


---
---


Programmer's critique of missing structure of operating systems 
https://news.ycombinator.com/item?id=22357184


 I would like to point out, that having structured information as "bytes" or "text" is general and universal, but pretty much no one uses it as such. Maybe for simple append, or maybe for tasks like "count number of bytes". But every time you want to do something with the actual structure, you end up using ad-hoc parsers.

Unix utilities sound like a great thing, because how universal they are. Want to find something in text? Just use grep, or maybe with regexp. But what this really say is "use a parser made from simple condition", or "create parser using single character pattern matching language". And ok, this would be fine, if it really worked, but it can't really handle the structure. It may be great for admin, who just wants to quickly find something, but it is horrible and broken by design for anyone who really want to make something more permanent. So you end up with writing better parser / using library. And you are not working with text anymore, but with typed graphs / trees. And this happens every single time you actually do something even slightly complicated with "just text".

	
	
ratboy666 3 months ago [–]

Text is structured: character, word, line, possibly field. Even "bytes" (octets) have some structure. On top of text, I usually layer, these days, usually JSON. Bytes? usually sqlite3

Yes, creating a parser with grep isn't usually desired. But, "plain text" is quite useful. And, with JSON and sqlite3 in easy reach, I don't see the massive issue.

Please -- I would like examples where you had issues with doing something even slightly complicated with "just text", and had difficulty. I really want to examine this. Either the text toolkits are not adequate, or I will be convinced that I am wrong, and will investigate structured CLI and OS interfaces.

FredW 


---
---


WinFS, Integrated/Unified Storage, and Microsoft 
https://news.ycombinator.com/item?id=6905633


Few data points from someone who actually worked in the latest iteration of unified storage, WinFS (2004-2006).

The first problem was the lack of clear vision, contradictory requirements (AKA solving all world problems). For example, the data model had to be changed very late in the game. The previous data model was incredibly complex. It had entities of different types, links between them (of different types IIRC), it allowed multiple entities to be a parent of another entity, and from security stand point this was not solvable in any meaningful way. Shame that hundreds of people written tons of code for the flawed model without even realizing that it won't work, despite all high level architects involved into the project. At the end they realized that they couldn't possibly ship that, and decided to do a micro reset of WinFS' data model (around mid of 2005).

Second problem was SQL Server (actually a fork of what became SQL Server 2005) which was optimized to work as a dedicated service on a server machine and required lots of effort to massage it to work relatively well with other resource consumers on an average consumer grade machine.

Third, WinFS APIs were all managed and Longhorn Explorer was written in managed code as well. The whole thing was slow as hell and extremely unstable.

By the end the teams working on WinFS and related projects were fairly motivated though, we could see the light and the decision to kill WinFS (and do Longhorn reset) was simply a matter of time.

Ironically, the news of cancelling the project came the next day after the team declared WinFS Beta 2 ready. Now I think it was the right decision. 


---
---


Why HyperCard Had to Die (2011)
https://news.ycombinator.com/item?id=20549685


Author of linked piece speaking. Seems like many readers continue to miss the essential point, just as they did in 2011:

Hypercard wasn't a gem of graphic design (1-bit colour, plain line graphics) or of programming language design (one of the many laughable attempts at "natural language programming") or of high-performance number crunching... but it was simple. I.e., the entire system was fully covered by ~100 pages of printed manual. It fit in one's head. (And did not take 20 years to fit-in-head, either, an intelligent child could become "master of all he surveys" in a week or two.)

Where is the printed manual for the current MS VB, or the WWW's HTML/JS/CSS/etc stack (yes including all browser warts), or for any of the other proposed "replacements" ? How many trees would have to be killed to print such a manual, and would it fit in your house? Could you read it cover to cover, or would die of old age first? 


---
---


https://www.reddit.com/r/programming/comments/szsie/an_ide_is_not_enough/


    Any directed-graph data structure you invent for storing code is basically an AST of some kind.

Indeed, Lispers have known this since the publication of recursive functions of symbolic expressions and their computation by machine in 1958. With that paper John McCarthy discovered that all functional code can be represented as directed graphs with out degree at most one. Internally such directed graphs where represented as cons cells with car and cdr pointers in the machine. In that article John McCarthy mentioned that "circular list structures would have some advantages but difficulties in printing them, and in certain other operations, make it seem advisable not to use them for the present." As such, despite their advantages, the use of cyclic directed graphs was pushed indefinitely into the future, until the problems with printing them could be addressed.


---


    In the short term (i.e., 10+ years) this is all outweighed by the massive tooling support we have in place for text-based programming languages, but I do think that text is probably a local maximum, not the best possible representation for programs.

I came here to write exactly those words. The tragedy of the current situation is that with text source files and all the related tools, we might not have a good solution to the problem of how to represent our programming ideas, but we usually have a “good enough” solution. We demonstrably can make useful software using this approach.

Perhaps if we used something more specialised we could make software 5x faster for 50% of the cost and eliminating 90% of bugs, by learning lessons both from the programming community’s experience and from database admins, graphic designers, usability researchers, and all those other fields that we tend to ignore right now. Personally, I don’t find those figures implausible nor even unreasonably optimistic: it’s not as if software development is an efficient process today or reliably produces fast and bug-free code, after all.

But we can’t get there incrementally, because any radical new approach to representing code will mean giving up the supporting ecosystem that comes with familiar text-based languages and tools. To succeed, any new approach would need to provide an entire tools ecosystem, not just a way to edit programs, and it would need to attract a critical mass of developers, and the programming model would still need to be able to call to and from C code because no-one is going to reinvent the entire world’s code archive. This is classic deadlock, and I suspect the only way we’ll break it is for someone with so much money that cost effectively doesn’t matter to decide it’s important, and to fund enough smart people to actually build a credible suite of tools as well as the new big idea, so that other developers could realistically start to work within the whole ecosystem and produce meaningful results. That’s a pretty hard sell, but if text-based coding is a local maximum as we’re proposing then we would have to go down before we could go up again to a level greater than before.


# Others

The "No Code" Delusion
https://news.ycombinator.com/item?id=22033827

Newton OS: http://lispm.de/lisp-based-newton-os

https://news.ycombinator.com/item?id=6498878

http://web.archive.org/web/20090416033922/http://stevenf.tumblr.com/post/94591835/warning-a-long-rambly-exploration-of-the-state


Smalltalk
https://news.ycombinator.com/item?id=14333157

Maybe Visual Programming is The Answer. Maybe Not 
https://news.ycombinator.com/item?id=22978454

Ask HN: Which people and groups are researching new approaches to programming?
https://news.ycombinator.com/item?id=10926038
