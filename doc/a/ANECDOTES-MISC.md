Anecdotes: MISC
===============

https://datenwolf.net/bl20110930-0001/


My dream graphics system was completely abstract. Creating a window didn't involve selecting visual formats, framebuffer configurations. It was just "a window". Only when actual content is involved I want to tell the rendering subsystem, which color space I use. Ideally all applications worked in a contact color space (e.g. CIE XYZ or Lab), but sending images in some arbitrary color space, together with color profile information. Fonts/Glyphs would be rendered by some layer close to the hardware, to carefully adjust the rasterizing to the output devices properties. And last but not least the whole system should be distributed. Being able to "push" some window from one machine's display, to another machine's (and this action triggering a process migration) would be pinnacle. Imagine you begin writing an email on your smartphone, but you realize you'd prefer using a "usable" keyboard. Instead of saving a draft, closing the mail editor on the phone, transferring the draft to the PC, opening it, editing it there. Imaging you'd simply hold your smartphone besides your PC's monitor a NFC (near field communication) system in phone and monitor detects the relative position, and flick the email editor over to the PC allowing you to continue your edit there. Now imagine that this happens absolutely transparent to the programs involved, that this is something managed by the operating system.


# Oberon


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

# Others

Newton OS: http://lispm.de/lisp-based-newton-os

https://news.ycombinator.com/item?id=6498878

http://web.archive.org/web/20090416033922/http://stevenf.tumblr.com/post/94591835/warning-a-long-rambly-exploration-of-the-state


Smalltalk
https://news.ycombinator.com/item?id=14333157

Maybe Visual Programming is The Answer. Maybe Not 
https://news.ycombinator.com/item?id=22978454
