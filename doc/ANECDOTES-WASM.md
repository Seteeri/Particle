Anecdotes: WASM
===============


https://news.ycombinator.com/item?id=21722421


Even if we don't lose the HTML-centered model, we are probably going to lose control of our browsers. Eventually somebody is going to ship a product that's nothing more than a browser implemented in WASM that runs inside your browser. The "inner browser" won't have content filtering, privacy controls, DOM inspector, or a Javascript debugger (for the Javascript engine running on the "inner browser") that you can interact with. You'll have to agree to let them run arbitrary code on your machine to even view the "website". There will be no "browse w/o Javascript" option in that future.

The kind of jerks who liked adding Javascript to block right-clicking, blocking "Paste" into password fields, etc, are going to absolutely love using the browser-in-a-browser product to deliver their "website".

For the inevitable replies: Yes-- you can already do this with minified Javascript. WASM, being targeted for performance, is just going to make this kind of asshattery faster. 


---


Unfortunately, I must agree with you that there are definitely some very troubling implications to how WebAssembly might be used to build more effective walled gardens on the web.

I can easily imagine a "platform" WASM module which acts as a runtime for other WASM modules built by "app" developers. This Platform module can be easily cached by FAANG or other big commercial interests, similar to AMP by Google (maybe even be pre-bundled into the browser?). The only way to discover, download and run these other apps is through this curated Platform module. All this could be rendered through something like the Canvas API instead of the DOM, which again is managed on a low level by the Platform and in turn exposes higher level API's for the Apps. The Platform also has built in support for Ad networks, tracking, etc., which cannot be disabled without disabling the whole ecosystem of apps. And of course, like any good play/app store, it is completely incompatible with anything else, leading to new levels of Balkanization of the web.

I hope that this isn't the case, and I'm completely wrong about this. But I just can't shake the feeling that as a community, we are championing WebAssembly as purely a performance win, without considering how big commercial interests might seek to exploit this new technology.

Edit: typo with AMP 


---


> Could we ever lose the HTML centered model?

This is what people should be worried about, not replacing JS. It became a kind of popular hot-take for a while to say that separation of concerns was a mistake, and that's not how apps get built in the real world, and what we really need is a way to encapsulate all of our DOM and CSS in JS. We need to start pushing back against that idea and keep emphasizing that separation of concerns is really important for end users.

HTML is the interface you write to. It's not a document layout language for authoring, it is a render target that is understandable and manipulable by the end-user. It's a fantastic idea that enables a lot of user-land innovation, and it's one of the biggest reasons why the web is still a relatively good platform to interact with as an end-user.

A lot of architecture decisions on the web haven't aged well, but separating content, styling, and logic was a fantastic architecture decision that is still as relevant today as it ever was. And the rise of the web as an application platform has only made it more important, not less. 


---
---


https://news.ycombinator.com/item?id=20458173



But it's the other way around. Can you distribute a native application on macOS without Apple's blessing? No, you can't (not when Catalina will be out anyway).

Yes you can:

https://forums.macrumors.com/threads/unsigned-apps-catalyst-...

WASM is the next best thing to an open, yet secure, platform.

So, instead of walled gardens with signed and checked applications (bad). We have to run untrusted and unchecked code of vendors who run code to track your movements around the web and in their applications? (Even worse)

Desktop Linux aside

Maybe we should make Linux an acceptable platform for more people rather than optimizing for proprietary web applications that you don't even own a copy of anymore. 


---


> Maybe we should make Linux an acceptable platform for more people rather than optimizing for proprietary web applications that you don't even own a copy of anymore.

Won't ever happen for two reasons: 1) The Linux Desktop community would have to admit that there are better ways to do things, and 2) They'd have to agree on what those things are. 20 years of Linux Desktop history show these things to be impossible.

Theoretically, a new community could spring up and put a new userland on top of the Linux kernel, like Android did, but it would seem like the people who have the interest in doing such a thing (like myself) lack enough time, talent, and/or ability to organize sufficiently to pull it off. Not to mention that we'd all have to agree too.


---
---


https://news.ycombinator.com/item?id=16468963


I'm only interested in owning one of these devices when I actually own it. That is when the device is actually working for me, not the corporation who sold it to me. I'd be very interested in a personal assistant/ daemon that wasn't part of some walled-garden ecosystem and would actually put the benefit of its user ahead of the profits of corporation. If I ask my daemon to find me a product, I want it to compare prices across all online vendors to find the best deal for me, not just default to purchasing from Amazon. I also want complete control over the information it shares with third-parties, and I want to directly benefit from any information sharing as my data is valuable and if folks want it they're going to have to provide me some benefit in exchange.


---
---

https://news.ycombinator.com/item?id=22901541


