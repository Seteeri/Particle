# https://www.reddit.com/r/lisp/comments/22lbpe/whatever_became_of_clim/

Independent of CLIM or McCLIM per se, I think that the paradigm of presentations is something worth keeping around.

The core of it is: there is a single mechanism in which associations between objects (value, etc.) represented in the user interface by widgets (view, component, graphic, element, etc.) are stored, so that commands and other interactions can be defined on the object type, once independently of the specific visual representation being used.

This differs from "data binding" by exposing the domain objects, not just data, and therefore introducing the notion of commands operating on the objects.

This differs from "naked objects" (generating UI from domain objects) by admitting different UI representations of the same object.

##


CLIM probably is a dead end at this point, but not for any of the reasons you mention, most of which are not true.

CLIM wasn't an attempt to reinvent anything. It was an attempt to specify a portable version of DynamicWindows, the later presentation framework on Symbolics Lisp Machines. DynamicWindows is an evolution of earlier presentation-based windowing systems on Lisp Machines, like the one described in Eugene Ciccaralli's 1984 paper on the subject (MIT AITR 794). These are presentation systems from the dawn of bitmapped graphical UI. There were not a lot of predecessors for them to reinvent.

It probably is a dead end. It's just too different from what people are used to, with no compelling reason for them to adapt to the differences.

CLIM is worth learning about though, and even working with for educational purposes, if you can make that happen. It does have some cool features that will be unfamiliar to anyone who hasn't worked with a LispM.

It's getting harder to find a way to work with it, though. The latest version of Lispworks on my Mac claims it no longer knows how to load CLIM, and the MCCLIM project seems pretty much stalled. Too bad; the MCCLIM listener was a decent way to get a feel for what CLIM was like.

################################################################

https://www.reddit.com/r/lisp/comments/3i36rn/status_of_mcclim/

Output records are much like a browser DOM, plus a type system, but minus any reasonable way of reflowing text or recomputing layout efficiently. I don't think incremental redisplay counts as an answer, but that's a part of CLIM I don't know well. General designs (basically Porter/Duff compositing operators plus arbitrary transformations) might have been an experimental feature in the early 90s, but the design is mostly reasonable and could be implemented efficiently today using GPU acceleration. Given modern browser engines do much of this so well, it's silly not to leverage them, except for the joy of doing things yourself in 100% Lisp (which was always a big part of what made hacking on McCLIM fun).

CLIM's interaction model of presentations + commands + translators + gestures is theoretically compelling but not fully baked. There's an unbridged gap where, if the higher level facilities don't do what you want, there aren't sufficient protocols by which to customize its behavior, short of dropping down to toolkit-style programming with events and gadgets. Also, gadgets were never well integrated with the command framework. This is still an interesting design space to explore, were someone to design a successor to CLIM.

McCLIM was never going to satisfy the people who want conventional Gtk or Cocoa applications and shouldn't have aspired to do so. It could've evolved into something uniquely cool had it had broken away from the CLIM 2 spec and gone in its own direction. Instead, developer enthusiasm waned and it fizzled out.