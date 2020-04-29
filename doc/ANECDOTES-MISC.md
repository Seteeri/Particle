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


https://news.ycombinator.com/item?id=6498878


http://web.archive.org/web/20090416033922/http://stevenf.tumblr.com/post/94591835/warning-a-long-rambly-exploration-of-the-state


Smalltalk
https://news.ycombinator.com/item?id=14333157
