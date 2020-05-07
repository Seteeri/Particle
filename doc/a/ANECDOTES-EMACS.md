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
