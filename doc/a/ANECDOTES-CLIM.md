Anecdotes: CLIM, McCLIM
=======================


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

Much of what you're describing reminds me of Dynamic Windows, the UI on Ptrbolics workstations in the 80s, and CLIM (Common Lisp Interface Manager), an attempt to embody the same ideas in a vendor-neutral standard.

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


---
---


https://news.ycombinator.com/item?id=15057548


Blast from the past! I used to work on McCLIM and Climacs as an impressionable youth almost ten years ago. I think the battle for coming up with new ways of defining GUIs is lost (and the web is eating everything anyway), but CLIM was definitely an interesting idea. While it lost a bit in the ability of individual applications to customise their behaviour, it won a lot in the interoperability that was transparently provided by the presentation system. Using "presentations", any on-screen object could be associated with a Lisp object. An application would usually "present" various objects using "presentation types" (image, graph, filename, etc), and other applications could then "accept" input of a given presentation type. If some object of an acceptable type was somewhere on the screen, the user could just click it. Text-based input entry was usually also permitted, assuming the presentation type in question had a textual representation.

I mostly worked on Drei, which was the input editor. It finds use in Climacs (which is probably not worth using if you have Emacs), and the McCLIM Listener, which is really cool. It shows the potential of adding bits of graphics support to a classical REPL, and being able to work interactively with opaque objects.

Using CLIM is a bit like operating an artifact that somehow made its way from an alternative universe. 

---

Yeah, I'm aware of w3m but it would be nice to be able to just render HTML payloads in an Emacs buffer. About a year ago I saw a demo on Youtube of an embedded Webkit in Emacs but I'm not sure what the progress is there.

What I envision is something very close to Gmail's interface. But instead of just email the interface presents a list of items. Each item is of a particular type and each type has an associated renderer. A command box facilitates command entry and the result of each command is a list of these items. When an item is selected a view of the item is rendered. Items can also have tags associated with them such that we can filter item sets by tags. A resulting set of items can be piped to an ensuing command to produce another set of items.

Item renderers don't need to be read only either. For example I could issue a file search command that returned a set of files. By selecting one, if an editor renderer existed I could edit the result in place and issue a save command on the item.

I could take the paradigm even further. I could issue something like a project command that returned a set of projects where a project was really just a tag for an item which had metadata pointing to a directory path. After selecting one of these project items I might be presented with a rendered view of the directory's contents. From there I might be able to execute a build command on this item and the command would use the metadata associated with the item to build the project. The output would be a list of one item with the build results.

I've been thinking about this for a long time and the only apps I can think of that don't fit with this paradigm are apps that actually require a mouse or pen pad for input like photo shop or auto cad.

There are varying technologies which nibble around the edges of such a system but nothing that really implements it fully. Emacs comes close, the command line in a terminal comes close, Gmail exhibits aspects, Enso exhibited aspects but nothing exists which puts all the pieces together. 

---
---

https://news.ycombinator.com/item?id=9038505


I've been thinking hard about this lately, and the first question for me is "What would a 21st Century Lisp Machine mean?"

Lisp Machines were created in part due to the desire to get the most performance possible back in the days when CPUs were made out of discrete low and medium scale integration TTL (there were also ECL hot-rods, but their much greater costs across the board starting with design limited them to proven concepts, like mainframes of proven value, supercomputers, and the Xerox Dorado, after the Alto etc. had proven the worth of the concept).

Everyone was limited: maximum logic speeds were pretty low, you could try to avoid using microcoded synchronous designs, but e.g. Honeywell proved that to be a terrible idea, as noted elsewhere memory was very dear. E.g. the Lisp Machine was conceived not long after Intel shipped the first generally available DRAM chip, a whopping 1,024 bits (which was used along with the first model of the PDP-11 to provide graphics terminals to the MIT-AI PDP-10), etc. etc.

So there was a lot to be said for making a custom TTL CPU optimized for Lisp. And only that, initially: to provide some perspective, the three major improvements of LMI's LAMBDA CPU over the CADR were using Fairchild's FAST family of high speed TTL, stealing one bit from the 8 bits dedicated to tags to double the address space (no doubt a hack enabled by it having a 2 space copying GC), and adding a neat TRW 16 bit integer multiply chip.

The game radically changed when you could fit all of a CPU on a single silicon die. And for a whole bunch of well discussed reasons, to which I would add Symbolics being very badly managed, and LMI killed off by dirty Canadian politics, there was no RISC based Lisp processor, Lisp Machines didn't make the transition to that era. And now CPUs are so fast, so wide, have so much cache ... e.g. more L3 cache than a Lisp Machine of old was likely to have in DRAM, the hardware case isn't compelling. Although I'm following the lowRISC project because they propose to add 2 tag bits to the RISC-V architecture.

So, we're really talking about software, and what was the Lisp Machine in that respect. Well, us partisans of it thought it was the highest leveraged software development platform in existence, akin to supercomputers for leveraging scientists (another field that's changed radically, in part due to technology, in part due to geopolitics changing for the better).

For now, I'll finish this overly long comment by asking if a modern, productive programmer could be so without using a web browser along with the stuff we think of as software development tools. I.e., what would/should the scope of a 21st Century Lisp Machine be? 

---
---


https://news.ycombinator.com/item?id=12703498


> I'm aware of that, but text is a fairly convenient representation of Lisp data.

Some Lisp data does not have a textual representation, it might have a graphical representation or the textual representation may not be very helpful (Lisp data being a graph might be better displayed as a 2d or even 3d graph, than as a textual representation). The Symbolics UI of the REPL/Listener would allow you to interact with the 2d/3d graph as it were Lisp data, which it actually is underneath.

> In fact, I'm unsure what you mean by directly interacting with the data, as you can't have bytes fly from your fingertips, AFAIK.

GNU Emacs pushes characters around.

S-Edit manipulates S-expressions.

GNU Emacs lets you pretend that you edit s-expressions, by pushing characters in a buffer around. But you don't. All you do is push text around.

S-Edit directly manipulates the data. It's a structure editor.

Symbolics Genera uses 'presentations' to record for every output (graphical or not) the original Lisp data. When you interact with the interface, you interact with these data objects. For example in a Listener (the REPL) programs and the Listener itself display presentations, which then can be acted on. It also parses text into data objects and then runs the commands on the data objects and not on text.

SLIME provides a very simple and partial reconstruction of that - which is a nice feature.

> In addition, files are a pretty good metaphor as well: If you want to store your code as something textual, they're indispensible. And sure, you can use image storage and navigate in other ways, but Lisp isn't Smalltalk: The way Lisp is written isn't as unified, so that wouldn't work as well, AFAIK.

Smalltalk does not use data as representation for source code. It uses text as representation for source code.

Interlisp-D is more radical than Smalltalk 80. The Smalltalk editor edits text. The Interlisp-D editor S-Edit edits s-expressions as data.

Interlisp-D treats the files as a code database similar to Smalltalk, but the source code loaded remains data and you can run the program from that data directly via the Lisp interpreter. Smalltalk execution does not provide something like that. The so-called interpreter in Smalltalk executes bytecode. This is different from a Lisp interpreter, which works over the Lisp source data.

The combination of a s-expression editor with an s-expression interpreter (plus optional compilation) is very different, from what Smalltalk 80 did.

When you mark an expression in Smalltalk 80 and execute it via a menu command, then the expression gets compiled to bytecode and the bytecode interpreter then runs it.

If you mark an expression in S-Edit, the s-expression data is extracted from the data structure you edit and you can run that s-expression data with an interpreter, walking directly over that s-expression data. 


---


It feels very different.

Imagine the editor and the runtime work on the same data:

   EDIT <->  data  <-> EVAL

In GNU Emacs it looks like this:

   Emacs EDIT -> text -> Emacs SAVE FILE

   SBCL LOAD FILE -> SBCL READ -> SBCL EXECUTE

or

   Emacs EDIT -> text -> Emacs TRANSFER to SBCL
     -> SBCL READ > SBCL EXECUTE ->
     SBCL generate TEXT -> SBCL TRANSFER to Emacs
     -> Emacs DISPLAY -> text

With presentations it looks for the user that the Emacs side knows the data for the text.

That's a lot of indirection, a lot of conversions, different runtimes, etc.

S-Edit feels like working with clay. A text editor feels like working with instruments, manipulating something which then manipulates the clay and you are watching the result through goggles. 


---

 Neat. But actually, I find that mechanism quite unpleasant, and one I wouldn't like working with. I'm sure it's quite powerful, but so is Vim, and I never really "got" Vim either. I would, in fact, argue that manipulating text has advantages over direct object manipulation: the first of which is that you can more directly edit text, whereas the DEdit interface is all about executing commands on objects. Secondly, you can apply useful transformations to text which don't necessarily make sense to apply to raw datastructures (search + replace, regexes, other handy transforms). Finally, many of the advantages of manipulating a pure datastructure can be had in text as well: see paredit.

But that's just me. You use your cool lispm tools, I'll resign myself to never acheiving ultimate productivity.

But I was resigned to that already, pretty much.

	
	
crististm on Oct 14, 2016 [-]

Text is not "raw data structure" and you may want to think twice that by manipulating text you touch the "data".

From the sidelines, your approach is "all I have are nails and all I need is the biggest hammer you can get me".

	
	
qwertyuiop924 on Oct 14, 2016 [-]

Text is a raw data structure, it's just not the one that's used behind the scenes: Everything is a datastructure. And actually I do touch the data by manipulating text.

>From the sidelines, your approach is "all I have are nails and all I need is the biggest hammer you can get me".

From my perspective, my approach is "that development environment sounds really unpleasant, and while I can appreciare the elegance, I'm having trouble finding the relative upshot."

	
	
crististm on Oct 14, 2016 [-]

When your editor's cursor hovers over "transistor T1" do you in fact touch the transistor on the board over your desk, the transistor in the schematic diagram, or only the text that happens to be "1T rotsisnart" spelled backwards (with no underlying meaning that is)?

Data representation is not the data. It's just this: "re" presenting. And when this representation is disconnected from the source by means of showing only the raw text and not allowing access to the source you get what you describe as "unpleasant".

You may find it enjoyable to work only with raw text but you're missing out on working with what that text is supposed to represent.

	
	
qwertyuiop924 on Oct 14, 2016 [-]

All is representations. Some are closer to the data they are representing than others. I am aware that text is a representation. It is a also a data structure: A data structure which we are better able to manipulate directly with our human senses, and a data structure which tools like DEdit still render to.

>And when this representation is disconnected from the source by means of showing only the raw text and not allowing access to the source you get what you describe as "unpleasant".

First off, pleasantness of an interaction is a matter of personal opinion. If you like DEdit, great: I'm not stopping you. But I don't enjoy DEdit and its like. That's my prerogative.

Secondly, I'm not working "only with raw text." Paredit &co let me manipulate the sexpr data structure more directly (or less directly, depending on how you look at it). Geiser and SLIME let me evaluate and manipulate the code as code. But at the end of the day, I'm also operating on text, so I have all of the textual data tools as well, and because Paredit is actually operating on text, it can be used outside of the context of Lisp: As Perlis said, "It is better to have 100 functions operate on one data structure than 10 functions on 10 data structures." 

---
---


https://news.ycombinator.com/item?id=21961575


https://news.ycombinator.com/item?id=19961812


https://news.ycombinator.com/item?id=15185827


https://news.ycombinator.com/item?id=11712038


https://news.ycombinator.com/item?id=9831429


https://news.ycombinator.com/item?id=7878679
