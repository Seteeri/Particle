Anecdotes: LISP PAST
====================


https://news.ycombinator.com/item?id=1878220


Lisp Machines are something that you think is really cool when you first learn about them, then you come to the realization that pining for them is a waste of time.

I've had a flash of inspiration recently and have been thinking about Lisp Machines a lot in the past three weeks.

But first, a digression. There's an important lesson to be learned about why Symbolics failed. I think Richard Gabriel came to the completely wrong conclusion with "Worse is Better" (http://www.dreamsongs.com/WorseIsBetter.html). There are two reasons why:

1. Out of all the LispM-era Lisp hackers, only RMS understood the value of what's now known as Free Software. (If you haven't read it yet, read Steven Levy's Hackers - it describes the MIT/LMI/Symbolics split and how RMS came to start FSF and GNU).

2. Portability is really important.

The key lesson to draw from Unix isn't that "Worse is Better," it's that survivable software is Free and portable. Free because getting software to someone's harddrive is 80% of success, and portable because you don't know where people will want to use your software (there are some really weird places).

Symbolics was neither. If Genera had been Free Software, it would by definition still be around today. If Genera had been portable, it's likely Symbolics would never have gone out of business (the Alpha virtual machine would have been done sooner, with less resources, and for more systems).

Being released as Free Software today wouldn't help. Genera's predecessor, MIT CADR, was made available under an MIT-style license in 2004 (http://www.heeltoe.com/retro/mit/mit_cadr_lmss.html). There's a VM emulator which runs the code. The whole system is pretty useless.

Now on to the inspiration part:

It's possible to make a very high-performance, portable Lisp operating system on modern hardware. This has been a possibility ever since the Pentium came out. The main bottleneck to conventional Lisp runtime performance is the way operating systems manage memory allocation and virtual memory.

A type-safe runtime that has control over memory layout, virtual memory, and is aware of DMA can provide extremely high throughput for allocation and GC (this has been shown by Azure's Linux patches for their JVM), true zero-copy I/O, almost optimal levels of fragmentation, and excellent locality properties. If you go single address space (and there's no reason not to) and move paging into software (object faulting and specialized array access), you've also eliminated TLB misses.

Throw in the fact that it now becomes trivial to do exokernel-type stuff like for example caching pre-formatted IP packets, and it should be possible to build network servers that have throughput many times that of anything that kernel/user-space split OSes like Linux or FreeBSD are capable of for dynamic content (ie - not just issuing DMA requests from one device to another).

The only problem is device drivers. Lisp doesn't make writing device drivers any more fun, or reduce the number of devices you have to support.

What to do?

The reason I've been thinking about this is that I came across this: http://www.cliki.net/Zeta-C

I've heard of Zeta-C multiple times before, but for some reason this time I made the connection - "why not use Zeta-C to compile an OS kernel?"

I explored the idea further, and it seems to me that it wouldn't be an unreasonable amount of work to take the NetBSD device subsystem and have it running on top of a Lisp runtime with the necessary emulation of those parts of the NetBSD kernel that the drivers depend on. If you don't know, NetBSD's device drivers are modular - they're written on top of bus abstraction layers, which are written on top of other abstraction layers (for example, memory-mapped vs port I/O is abstracted). So the actual system twiddling bits can be neatly encapsulated (which isn't necessarily true for Linux drivers, for example).

I'm aware of Movitz (http://common-lisp.net/project/movitz/) and LoperOS (http://www.loper-os.org/). Movitz makes the mistake of trying not to be portable, but there's useful things there. I haven't spoken to Slava about this yet so I don't know what's going on with LoperOS. I am also aware of TUNES, and think it was an interesting waste of time.

The main thing is to get Zeta-C to work on Common Lisp. Then it's to build a new portable, boot-strappable runtime (I think the Portable Standard Lisp approach of having a SYSLISP layered on top of VOPs is the right way to go for this), and either build a compiler targeting that runtime, or adapt the IR-generating parts of one of SBCL, CMUCL or Clozure. Further bootstrapping can be done with SWANK and X11 once a basic networking stack is in place. I think such a system would be quite fun to hack on.

If you've gotten this far, let me know what you think about this idea. I also have some preliminary thoughts about how this can be worked into the base of a new high-performance/scalability transactional database startup, if you want to hear about that email me: vsedach@gmail.com 


---
---


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
