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
