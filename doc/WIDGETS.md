Widgets
======

All widgets are based on an underlying tree structure and can be broken
down into nodes.

**Explain core widgets and corresponding nodal structure**

Use icons for L,T,B, etc. instead of letters but for now use letters

------

Labels - cannot modify - check in backspace etc - user can manually change it ofc
         call it god mode???

[L]-[]...

Text Field/Box

[F]-[]...

[A]-[]...

Cycle Button

[B]-[U]-[P]
 |
[F]-[]...
 |
[B]-[D]-[N]

Drop-down List - similar to Radio Buttons but condensed; text fields + button
               - redundant?

[B]-[]...
 |
[D]-[] Label 1

Hide until needed to be seen
[] Label 2
[] Label 3
[] Label 4

List Box - same as checkbox essentially

[I]

Combo Box - combo of text field + drop-down/list box

-----------

Push Button - user adds node which will change state
              repeated adding = repeated pushing which creates a chain - natural undo
[B]-[]...

Radio Button -  user adds node to one of the labels
[R]
 |
-[] Label 1
 |
-[] Label 2
 | 
-[] Label 3
 | 
-[] Label 4

Check Box - user adds node to any of the labels
[C]
 |
-[] Label 1
 |
-[] Label 2
 |
-[] Label 3
 |
-[] Label 4

---------

Scrollers

Scrollbar

[S]----[<>]----[S]

Slider (use preexisting edges to cue user?)

[S]--[1]--[2]--[3]--[4]--[S]
      |     |    |     |
     [^]

Progress Bar

[P]--[%%]--[P]

------------

Grid View

link right-down
[G]--[]--[]--[]
|    |   |   |
[]->[]--[]--[]
|    |   |   |
[]->[]--[]--[]
|    |   |   |
[]->[]--[]--[]
|    |   |   |
[]->[]--[]--[]

Tree View

[]--[]--[]
     |
    []--[]--[]
     |   |
    []  []--[]--[]
     |       |
    []      []--[]