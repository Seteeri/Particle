Anecdotes
=========

https://news.ycombinator.com/item?id=21961070

I think it’s wonderful to think McCLIM but it’s important to note that so much of the value of the Lisp machine’s interface is derived from (1) the OS level integration of the dynamic language, (2) the single-process aspect (everything lives in the same memory space, all objects can interact without serialization, and (3) the fact that developers thought that documentation and interaction were a true part—a first class citizen—of the development of their software.

I guess this blurs into the user experience. The interface, which McCLIM is based on, gives a lot of the widgets and interaction modes you need for something like a Lisp machine experience, but you really need your entire development and end-user environment to be built up with the above points as well.

---

This is what happens when very smart computer scientists and engineers sit down and build a pure developer's computer from scratch, from the hardware up. It's the most elegant IDE ever invented. It takes a while to get used to it, but once you do, the separation between programmer and computer simply vanishes and you "become" the computer. Describing the experience to a programmer who has never used it is like describing an acid trip to someone who has never done acid: It's always going to sound exaggerated or scary and thus not very interesting.

The best IDEs today have incorporated bits and pieces of these ideas but nobody has put it all together in one seamless box the way the Lisp Machines did. I really miss that experience. 

---

The web browser has been trending this way ever since JS was added to it. It's an active environment controlled by a dynamic language, it simply isn't done as well, and it isn't done entirely consistently across web browser implementations. The shortcomings and flaws don't negate the fact it is the same kind of thing.

The Original Java Utopia, the world dreamed up by Sun where Java Applets would abolish the Windows desktop and the JVM would obsolete platform dependence, was a shadow of this idea with a fundamentally more static language, and one more like Smalltalk than Lisp.

But with Java we've already lost the thread of an active environment, one which is responsive to on-the-fly reprogrammability and deep inspection. Maybe it could be done in terms of the JVM, but Java never seems to be used that way. 

---
---

https://news.ycombinator.com/item?id=22317371

I’ve always pictured the opposite—rather than taking a GUI and bolting on “fake” command-line like features, why not have a base-class for GUI “main” windows that’s essentially already a true, fully-featured terminal emulator showing the stdin/stdout of the executing process?

Then, as you would add graphical controls to the main window in the layout builder, the terminal-emulator-interface part of the window would get squished down further until it just looks like a status-bar. (Which is essentially what status bars are: a very underpowered terminal-emulator only capable of showing the last line of stdout.)

But click [or tab into, or Ctrl-` to activate] the thing, and it’d “pop down”, adding more height to the window, if there’s room; or “pop up”, resizing the rest of the window narrower; or just overlay translucently on top, like in games with consoles.

Opening this wouldn’t give you a login shell, exactly—it’d be the stdin of the program. But the toolkit would hook the stdin and turn it into a REPL, based on the CORBA/DCOM/DBUS service-objects available in the program; and the GUI toolkit would then enhance this REPL, by introspecting these service-objects, to enable both textual autocomplete/auto-validate of commands, and the drawing of these sorts of graphical command-palettes. (But you’d be able to “shell out” to process these objects with commands, like you could in scripting languages.)

Or, to put that another way: Plan9’s Acme editor is a cool idea, making the whole editor into interface where you can just write magic runes on the walls and then activate them. But there’s no real understanding or support for those runes, because they’re fundamentally opaque Plan9 executable binaries. What would this look like if we translated it into the object-oriented world that GUIs live in? What would Smalltalk’s Acme be like? And then, what if all applications in the OS were built on top of it, the way that you can build applications as major modes in Emacs or as plugins in Blender?

Or, to put than another other way: if Automator in macOS can already latch onto a running program’s service API and tell it to do things as a batch script, why can’t I interactively access the same APIs from inside the running program itself, as an automatic benefit of using whatever technologies power Automator? Why can’t I do the equivalent of opening the Smalltalk Inspector on my GUI program? 

---

That is a bit how Xerox PARC experiment in Mesa/Cedar went, and how Wirth got inspired for Oberon.

In what concerns Oberon System, there were no executables, only dynamic loadable modules.

They could be used by other modules, or by the OS itself.

Basically, there were a set of conventions how public procedures/functions should look like for the OS to recognise them as callable from REPL, or mouse actions.

And for mouse actions, they could either act immediately, act upon the current selected text/viewer or present a context menu.

Or how Inferno and Limbo interact with each other, don't stop a Plan 9.

I think the closest you can get back into these ideas is Windows, with COM/.NET/DLLs and PowerShell as orchestrator. 

---

Much of what you're describing reminds me of Dynamic Windows, the UI on Symbolics workstations in the 80s, and CLIM (Common Lisp Interface Manager), an attempt to embody the same ideas in a vendor-neutral standard.

DW/CLIM record text+graphical output in a DOM-like tree structure with references to the underlying application objects plus a "presentation type" and parameters for how the object was presented. The interaction window presents a stream interface rebinding your application's standard-input and standard-output streams, but also implements drawing operations, and the functions 'accept' and 'present' do structured IO against the stream in terms of actual objects. The presentation-type parameter determines both how to parse/print objects and which objects on screen may be selected with the mouse as input. Everything being live objects, the lisp machine usually let you select any object on screen and pop it into an inspector. Commands defined the presentation types of their parameters, again defining how to parse/print arguments and enabling mouse selection. Presentation translators let you define how to convert an object of some type into a context where the UI wanted input of a different type. Commands themselves had a presentation type, so the top-level command processor literally loops printing a prompt, calling 'accept' for a command, then executing the command. Consistently, presentation (to command) translators let you trigger commands directly via clicking objects, drag-and-drop gestures, etc. permitting the UI to function in terms of commands even if the textual 'interactor-pane' was absent.

It's a fascinating paradigm, with some shortcomings. Despite CLIM defining some more traditional GUI widgets, how to integrate then with presentations and the command processor is a bit half-baked. This is a solvable problem. A free implementation called McCLIM is still around and worked on by one or two of the devout.

One of these days I hope to see a good implementation of these ideas in Javascript. A browser DOM is the perfect substrate on which to implement a modernized CLIM-like UI, rather than reinventing everything the hard way on top of X11 like McCLIM attempts. It would certainly do wonders toward spreading the ideas. 

---
---

https://news.ycombinator.com/item?id=22498665

your point about UI complexity reflecting the complexity of the problem + level of control is totally true in regard to most tools - but one of the key points at replit is that we are trying to create a programming environment that appeals to both beginners and experts. we don't want to block out hobbyists. that's really the main point of comparison to Premiere- if someone is interested in learning about film production, you shouldn't have to go to iMovie and then "graduate" to Premiere (or Canva -> Photoshop, SketchUp -> Autocad). tools should grow with you (ideally)!

complexity is always relative but I think the end point here is that there's always room for simplification and shallower learning curves, even for advanced/pro users. 

---

I agree that digital tools don't have to be one thing, as you said they can grow with you (or you change them, like going from textedit to vim or from notepad to sublime text).

While there are solutions that both experts and beginners can benifit from (like Blenders 2.8 UI overhaul), everybody who has done every manual job ever might have learned that the optimized professional tool you used has a learning curve for a reason: when you know how to use it, it is literally faster and gets less in your way than the friendly intuitive and self explainatory variant of the tool.

The kind of dynamic scaling you mentioned is always desireable, but not always possible without introducing considerable complexity into a software — especially in complex domains like editing, audio or 3D the hard thing usually is to develop a language of adjectives and verbs the user can throw onto the objects they work with. And learning this can feel like learning a new language for a reason. And like with languages a simple combination of grunts and gestures might be enough for certain communicative goals, but sometimes it pays off to learn the actual developed language to really express what you want.

The most powerful software that gets in your way the least I ever learned had incredibly steep learning curves, but once you've got it, it felt like speeking a language you mastered.

We are literally commanding machines here — and problems of high complexity paired with a high level of control usually demand a very precise communication. So while I could e.g. imagine an editing suite which interprets your grunt and edits a bunch of videos for you using AI, you are still losing a level of control there: because the computer has to assume what you are unable to tell it. There is literally no way around it.

Ever wondered why the language used in the military, in an operation room or on a film set is so specialized and hard to understand for amateurs? It is because they need that level of control to communicate in order to get the desired result. 

---

Genera on the Lisp Machine is very much an embodiment of this graphical command line (or rather just command) user interface. There's quite a lot to it. CLIM (and the open source McCLIM) is the current realisation of this.

There's some documentation about 'Commands' in CLIM II here for example: http://www.lispworks.com/documentation/lwu41/climuser/GUID_1...

Commands are first class objects which live in command tables and which CLIM can look at to generate bits of UI and allow parameters to be specified. Central to this in CLIM is the idea of presentations - where domain objects are 'presented' onto the screen and the link to the domain object is maintained. That way you can invoke a command on that object in various ways, for example you can type a command name and then click a presented object which is acceptable as a parameter.

The Symbolics S-packages used these concepts to make (I presume) powerful 3d modelling programs back in the day (I haven't used them): https://www.youtube.com/watch?v=gV5obrYaogU - sorry for the poor video quality.

https://twitter.com/rainerjoswig?lang=en (lispm on here) sometimes posts interesting things about Genera 


---
---

https://news.ycombinator.com/item?id=18518807

First off, note that Lisp OSes were made before modern cybersecurity was a concept.

Since everything was Lisp all the way down, and Lisp is a very dynamic language, all calls that comprise the OS are available as plain function calls visible to the interactive REPL, introspection, and modification. They were very developer-oriented and had lots of ways to have live displays of objects, to reuse or interact with them, and very deep debugger and documentation integration, since all code was at the same "level".

Since it's a high level language that does not expose low level memory (although certainly there are obscure implementation-specific Lisp operations for doing so used deep in the guts), corruption of memory at a low crashy level isn't generally a thing. Protections between applications/processes aren't as necessary and it can all remain plain running threads & functions interacting with each other & the OS. 

---

Correct. It's not a process-oriented environment, with crash protection facilities keeping everything at arm's length.

There are namespacing facilities (though I'm not sure how far back in history they go) to discriminate your code's reach, as well as to import the public API from others' code, or in a different manner to bore into their private affairs generally for admin/debugging/tracing/modification. "Global" variables have thread-local, dynamically scoped bindings which are super useful for configuration, redirection, or setting other side-band broad context when calling into shared code.

Also it should be noted that these types of machines (at least from the Symbolics point of view) tended to be single-user workstations, though were still networked to allow remote (and simultaneous) access to its running world, with varying levels of per-connection context.

One of the biggest downsides of ye olde Lisp machines was garbage collection times. Some had facilities to define dedicated heap regions, but generally the GC had to walk the entire workstation heap across all "applications" in 1980s hardware, which wasn't great. But at least as a programmer, you could freely code and only worry about minimizing GC pressure when it became an issue, instead of starting from a required malloc/free perspective and constantly worry about leaks. 

---

> Some had facilities to define dedicated heap regions, but generally the GC had to walk the entire workstation heap across all "applications" in 1980s hardware, which wasn't great.

a stop-the-world global GC was generally tried to be avoided. mid 80s Symbolics Genera had regions for data types (areas), areas for manual memory management, a generational GC, incremental GC (the GC does some work while the programs are running) and an ephemeral GC efficiently tracking memory pages in RAM. For normal use it then looked like global stop-the-world GCs were only used in special situations like saving a GCed image or when running out of free memory. But one often preferred to add more virtual memory then, save the work and reboot. For that one could add paging space when Lisp crashed with an out-of-memory error and continue from there. 
