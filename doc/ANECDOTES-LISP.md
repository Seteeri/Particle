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

