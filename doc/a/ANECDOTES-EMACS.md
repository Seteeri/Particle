Anecdotes: EMACS
================

Reddit Thread: "Is There anything Emacs CAN't do?"

https://www.reddit.com/r/emacs/comments/3v19uj/is_there_anything_emacs_cant_do

https://www.reddit.com/r/emacs/comments/701v90/guile_needs_a_community_elispemacs_needs_a/


---
---


https://news.ycombinator.com/item?id=22857551


This comment probably will be downvoted since it doesn't represent popular opinion.

I think everyone who has chosen a career of a software developer sooner or later needs to seriously evaluate Lisp and Lisp dialects - Racket, Clojure, CL, Emacs Lisp, Fennel, etc.

Lisp has certain qualities that most other languages don't (perhaps except for Smalltalk).

I have personally learned, tried, and implemented working software in numerous different programming languages. I'm not trying to brag about it, that's not the point. The point I'm trying to make is the following:

I've seen many programs - systems, small, medium, and of significant size, and honestly, only systems written in Lisp can give you this genuine feeling like you are dealing with a garden. Even in systems where things get incredibly messy, you still can organically keep growing them and keep adding more and extend existing stuff. They usually do not crumble under their weight.

Take, for example, Emacs. Arguably it is probably the most extensible piece of software that humanity has ever created. It is wildly malleable. Things you can do with Eisp are borderline insanity; you can change things on the fly without even having to restart Emacs - sometimes, it feels like some sorcery. Last time I checked - I pull something over 400 hundred different packages in my configuration. And somehow things just work. Yes, from time to time, "abstractions start leaking," and things would break, but never to the point that it becomes completely unusable. Check out GitHub stats. Github alone contains an incredible amount of Emacs lisp. A language with one sole purpose - configuration. Basically, it is a glorified YAML for Emacs. And the people who wrote that code - most of them never had met each other. They have never worked in the same org. They didn't have to follow strict guidelines or standards. It is a messy, wild, and lawless world. Emacs ecosystem defies any logic. If you think about it - it should not work at all, yet it does. And it's been working for over 40 years.

Many people. Some brilliant individuals every once in a while would claim: "Well, Lisp is just old. Give language X a few more years, and it gets the same and even better features..." And that's been going on for over six decades now. Ironically, almost every single language that is on top of TIOBE and RedMonk today, has borrowed many ideas from Lisp, except the most fundamental one. The concept of homoiconicity. The benefits of homoiconicity are incredibly underrated.

You may dislike or even hate Lisp, but trust me, years later, when Python is no longer sufficient for our needs; when Java becomes too expensive to maintain; and Javascript transforms into a drastically different language - Lisp still would be used, and systems written in it would be thriving.

Learn Lisp. It will make you a better programmer. That is guaranteed. Don't take my word for it. See what the world known CS experts have to say about it. Not a single one has ever said: "Lisp is a waste of time." 


---
---


https://www.reddit.com/r/emacs/comments/8gdk9p/is_emacs_bloated/


I am using Emacs for a year now, i am not a Coder nor Developer or anything in IT, i think you are absolutely right that getting the starting point is the most difficult. However most users will start using emacs because of a certain package, i think org-mode is one of them, which attracts a lot of non-experienced users. At least for me this was the case. And i think while everyone has a certain interest in why he gets in to emacs he "naturaly" gets the starting point, to stay with the org-mode example: I want to create an manage my notes, and learn latex to fastly export my notes to latex and draft papers. My needs are my compass and my way to dig deeper into emacs. However as emacs is this kind of software which does basically everything and of course you have the freedom of chosing an approach to a task it's difficult to stay focussed. Therefore i think a more reduced and modular emacs would help to stay focussed and slowly discover functionalities, packages, etc. The same is also true for certain emacs packages themself, like (another time staying with the example) org-mode. I don't need a lot of built-in functionalities or at least i don't need them right now, perhaps i don't even know i could want to use them, hence they are kind of invisible, even if they are there. I think it would help to learn the stuff you want easier and at the same time learning more about functionalities and getting a user experience which is finally more customized to fit the users needs. Just as a very "neurotic" last example i get sick of the 100 and everything entries i always get by helm when using M-x, as i don't use a lot of them and therefore i would like them to just be invisible as long as i don't use them...However if bloated or not, it's a great tool: the day i discovered tetris i didn't worked for the rest of the day...


---
---


https://news.ycombinator.com/item?id=23107123


While I think you're right on the whole here about "Emacs people," but you did an interesting thing in the first sentence: you contrasted experienced Emacs user with junior developer. And I think that actually points to a problem: people who are experienced developers but not experienced Emacs users are going to be in much the same position as the OP's description.

While I know the basics of Emacs, every time I try to get serious with it and make it my One True Editor, I end up spending days screwing around with packages and configuration files to try to figure out how to have similar functionality to what I get either out of the box or with fairly minimal effort in arguably lesser editors. I'm sure Emacs can do all of what I need and nearly all of what I want, but most of the time I'm damned if I can figure out how to get there from where I'm starting.

If I was really going to try to give advice on how to make Emacs more popular, it'd center around the package/extension system. It's great that it has package repositories and a built-in management system now, and the documentation for the core editor is terrific... but the documentation for packages is wildly variable, and what's worse, packages interact with one another, depend on one another, and/or conflict with one another in ways that are just utterly mystifying to a newbie.

I don't care about making Emacs "pretty," and while it'd be nice if it used less weird terminology by today's standards, I can deal with it. What I want is sane defaults and good guidance. Spacemacs' concept of layers -- where I can just say "I would like you to install and enable all the crap that lets me smartly edit PHP files, please" -- is absolutely onto something, although I would still argue that it might need to offer a little less choice by default. Don't make me choose whether I want Helm or Ivy because I have no idea, and for God's sake, enable sensible layers/packages by default: assume that yes, I do want autocompletion and git integration and such. If you must, let me click a button to choose between "batteries included" and "advanced" configuration. This is stuff that mainline Emacs should be doing.

And, actually, that's one other thing Emacs could stand to do better: learn from VS Code's configuration system that lets you use dropdowns and checkboxes and simple text fields for nearly everything, and has a button to go into the configuration files when you need it. Yes, I know Emacs has a text-based UI for configuration, too. I've used it. It's bad. Okay? It's just bad. The controls are non-standard and weird, the organization is utterly mystifying to someone who doesn't already understand Emacsology, just... no. Start over.

As for me, well, when Spacemacs moves the LSP layer to its non-development branch, I'll probably give it a try again. Until then, I'm probably gonna keep doing my technical writing in BBEdit and my coding in Visual Code. (I'm probably gonna keep doing my technical writing in BBEdit until I die, but that's a different post.) 


---
---


https://www.reddit.com/r/emacs/comments/ckcijv/what_do_you_think_about_the_future_of_emacs/


I have two complaints:

    single-threaded and blocking: any complex extension must manage external process and be careful with IPC not to block too long.

    non-graphical: yes you can embed images, but editor is still limited to text, so can't really have modes to operate with graphical representation of some structure.
    
    
---


I'd like elisp packages to be able to draw graphical annotations on the buffer. It's hard to predict what people will do with graphics but one example is DrRacket's scope display, graphically connecting variables to their definition. I don't know if I'd actually want that particular feature, but I think it'd be nice if Emacs allowed experimentation with graphics from elisp so that people would be able to try out new ideas that aren't per-character properties.


---


No, but I do want to write/have:

    graphically display structure of AST you are editing (even prose has AST and theotetically can be typed and manipulated as a tree)

    Concrete Syntax Tree typographically not dependent on fixed-width fonts, and the view being updated automatically in your style as you edit the AST.

    contextual fade in/out and resizing of dependent/relevant types/code/data flows
    
    
---


I suspect that until emacs, as a project, starts prioritizing improvements to beginner-friendliness, its user base will continue to dwindle. Right now, a new emacs user is hit with a foreign/old-fashioned UI, completely nonstandard keybindings with no visual hints unless they adopt a totally nonstandard menu-driven workflow, and an editing runtime with its own set of foundational concepts (what is a major mode and how do I C-g), and they have to navigate all of these more or less steep learning curves simultaneously. If they have a lot of motivation and spare time (I bounced off emacs about 3 times before it stuck, and I am quite far to the "I would like to learn this old computer system on its own terms" side of the spectrum), it can work. If they have a lot of mentorship from friends/coworkers/etc, it can work (FWIW, I have never been in an office with another emacs user in my professional life). Otherwise, becoming an emacs user takes enormous buy-in and effort before seeing any benefits from the switch. Unfortunately, this seems to be a social/political problem rather than a technical one. With nothing mitigating the inherently-hard-but-still-much-harder-than-necessary learning curve, I think emacs use will dwindle to negligible levels and eventually die.

That said, I think the broader emacs community has come up with a lot of good work on this stuff its own; I can see a world where spacemacs/doom/centaur/prelude/etc catch on in certain programming communities and become de facto entry points into the broader emacs ecosystem. This seems especially likely if either GNU emacs or remacs can make significant headway into laggy and blocking operations, which are more frustrating to user expectations than they were when emacs' architecture was developed. Remacs improving non-linux emacsen could also help enormously.

The most likely outcome is probably in the middle, though: emacs remains a niche program, whose programming and extension model is so beautiful and useful that it maintains a cult following among 1-5% of professional developers, and whose learning curve scares off the other 90-odd percent.
