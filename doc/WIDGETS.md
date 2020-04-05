Widgets
=======

All widgets are based on an underlying tree structure and can be broken
down into nodes = push button

They can be linked together to propagate a signal.

Nodes allow widgets to be recomposed.

# Text/Labels
- another list
- multiline is a list of lists

# Button

    [Y]  [X]  '*
         Cmd

Select + Eval (or call func)

# Cycle Button/Drop-down List/Radio Buttons (single state change)
- iterate through a list

    [Y]
      
    [Y]  [X] '*
         Opt
      
    [Y]  [X]  NIL
         Opt
      
    [Y]  [X]  NIL
         Opt
      
    [Y]  [X]  NIL
         Opt

    NIL
    
Selected item has a Cdr

-----------

# Combo Box 
- combo of text field + drop-down/list box

-----------

# Check Box

    [Y]
      
    [Y]  [X]  T
         Opt
      
    [Y]  [X]  T
         Opt
      
    [Y]  [X]  T
         Opt
      
    [Y]  [X]  T
         Opt

    NIL

To unselect, set Cdr

    [Y]
      
    [Y]  [X]  T
         Opt
      
    [Y]  [X]  NIL
         Opt
      
    [Y]  [X]  NIL
         Opt
      
    [Y]  [X]  NIL
         Opt
         
    NIL

Selected item has T

---------

# Scrollers - continuous

## Scrollbar
- adj ends to change sensitivity

    [*] --> [*] --> [*]


## Slider
- similar to radio buttons with discrete values
      
    [*] --> [1] -- [2] -- [3] -- [4] --> [*]
             |
            [*]

## Progress Bar
- similar to scrollbar...continous

    [*] --> [X] --> [*]
            %%

------------

# Grid/Table/Matrix/Tree?
- two dimensional list or lists of lists
- multiple ways to structure data

[G] --> [_] --> [_] --> [_]
  |       |       |       |
[_] --> [_] --> [_] --> [_]
  |       |       |       |
[_] --> [_] --> [_] --> [_]
  |       |       |       |
[_] --> [_] --> [_] --> [_]
