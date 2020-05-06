Widgets
=======

Conventional widgets are based on an underlying tree structure (or class hierarchy) and in Particle are broken down into symbols or cons pairs

# Text/Labels

* native Lisp data


# Button

* equivalent to native eval

      [Y]  [X]  '*
           Cmd

         
# Radio Buttons

* list of symbols or pairs where only one item can have a Cdr

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


# Check Box

To select, set Cdr to T (or Any)

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

To unselect, set Cdr to NIL

      [Y]
        
      [Y]  [X]  T (or Any)
           Opt
        
      [Y]  [X]  NIL
           Opt
        
      [Y]  [X]  NIL
           Opt
        
      [Y]  [X]  NIL
           Opt
          
      NIL


# Combo Box 

* options stored in property list
* to see possible options, user prints out property list
    
# Scrollers

## Scrollbar * continous
* uses scroll symbols

      [start] -> [scrub] -> [end]

* scrub points to object meant to be adjusted

## Slider * discrete
* similar to radio buttons with discrete values
      
      [*] --> [1] -* [2] -* [3] -* [4] --> [*]
              |
              [*]

* continous same as a scrollbar with different graphics
            
## Progress Bar
* similar to scrollbar except scrub shows a value and cannot be moved by user

      [start] --> [X] --> [end]
                  %%


# Table

* two dimensional list or lists of lists
* multiple ways to structure data
  * list of column lists
  * list of row lists
  * list of assoc lists
* larger datasets warrant binary trees and external databases

      [Y] [ ]  [ ]  [ ]  [ ]  NIL
      |         A    B    C
      |
      [Y] [ ]  [ ]  [ ]  [ ]  NIL
      |   1    A1   B1   C1
      |
      [Y] [ ]  [ ]  [ ]  [ ]  NIL
      |   2    A2   B2   C2
      |
      [Y] [ ]  [ ]  [ ]  [ ]  NIL
      |   3    A3   B3   C3
      |
      NIL
