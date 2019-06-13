Widgets
======

All widgets are based on an underlying tree structure and can be broken
down into nodes = push button

They can be linked together to propagate a signal.

Nodes allow widgets to be recomposed.

Can apply ops to pointer or pointee - use quote to apply to ptr?

If node = push button && character = node then how do you "push" a character?
-> Nothing happens - user must define it...where do types come in?
-> Or it evals it since strings are symbols which have values...default = NIL

Random thought:
Left Click = 1
Right Click = 0
Wheel = (0 to 1)

Maybe store parts in symbol list?

What makes a button different from text is simply the border/shading

Maintain pointer concept

Least Astonishment Principle

Pointers propagate same signal

-----------

Text/Labels
- multiline? -> more visual issue; newline means offset relative to last character

[T] --> [E] --> [X] --> [T] <-- [*]

Cycle Button/Drop-down List/Radio Buttons (single state change)
- iterate through a list basically

single/double linked list

[TXT-1|4] <-- [*]
|
[TXT-2|3] <-- [_]
|
[TXT-3|2] <-- [_] 
|
[TXT-4|1] <-- [_]

(no cycle...since if at start or end, can't go up/dn -> more predictable/consistent behavior)

[UP] = (nth list)
[DN] = (cdr)

Propagation: [*] --> [TXT-#] 
                 1/0
                 
If list large, use binary tree or skip list

-----------

Combo Box - combo of text field + drop-down/list box

-----------

Check Box (mutli state change)
- duplicate pointers -> double pointers

              [*]
                |
[TXT-1|4] <-- [*]
|               |
[TXT-2|3] <-- [*]
|               |
[TXT-3|2] <-- [*]
|               |
[TXT-4|1] <-- [*]

To unselect, pop item

              [*]
                |
[TXT-1|4] <-- [*]
|               |
[TXT-2|3]       |
|               |
[TXT-3|2] <-- [*]
|               |
[TXT-4|1] <-- [*]

---------

Scrollers - continuous

Scrollbar
- dir indicates page movement

(obj) -> [*] --> [*] --> [*] -> (obj)

Slider -> Discrete -> Cycle Button
- note the pointers are not linked
- pointers at ends? or values? ends makes more sense
  [*]    [*]    [*]    [*]
  
[*] --> [1] -- [2] -- [3] -- [4] --> [*]
         |
        [*]

Progress Bar
- similar to scrollbar...continous

[*] --> [%%] --> [*]

------------

Grid/Table/Matrix/Tree?
- more of display issue since still just a list?
- or shared links

link right-down

[G] --> [_] --> [_] --> [_]
  |       |       |       |
[_] --> [_] --> [_] --> [_]
  |       |       |       |
[_] --> [_] --> [_] --> [_]
  |       |       |       |
[_] --> [_] --> [_] --> [_]