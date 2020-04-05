User-Interface
==============

*Talk about lay person's understanding of lists versus a programmer's understanding
of a (singly-linked) list*

*Talk about this relation to the learning curve*

# Command Matrix

    +-------+--------------------------------------------------------
    | CMD   |                           Pointer
    +-------+-------------------+--------------------+---------------
    |       | Pair/Atom         | Car/Atom           | NIL
    +-------+-------------------+--------------------+---------------
    | NL    | List w. Pair      | List w. Car        | Empty list
    +-------+-------------------+--------------------+---------------
    | ASCII | Ins-back, Mov-Cdr | Write-car, Mov-Car | Same as left
    +-------+-------------------+--------------------+---------------
    | NIL   | Same as above     | Same as above      | Ins or Wr; def Ins; Wr does nothing; make opt
    +-------+-------------------+--------------------+---------------

## Basics

### Data, Lists, and Atoms

Most people are already familiar with the concept of a list. Anytime one creates
an outline of sorts, one is creating a list or in computer science terms, a data
structure.

    I. Title
       A. Title
          1. Title
             i. Title
             I. Title
          2. Title
       B. Title
    II. Title
      ...
    III.
    ...

`I`'s content consists of a `Title` and body (`A`, `B`, ...), and then links to `II`. The
same pattern repeats itself throughout.

    I. Title
       A. Title
       B. Title
    II.
      ...
    ...

    I. Title
       Body
    ...

Because the user is already familiar with Roman numerals (or at least the symbols
less than ten), the user implicitly knows that `I` proceeds to `II` and so on, 
without needing any sort of explicit depiction of that relationship.

The indentation of the body indicates that it and its content belong to the
outer indented heading.

Now, take for example how a programmer could see the same outline 
as a data structure called a singly-linked list with a more verbose representation:

    [L]  [A]  [A]
         I.   Title

         [L]  [A]  [A]
              A.   Title

              [L]  [A]  [A]    NIL
                   1.   Title

                   [L]  [A]  [A]    NIL
                        i.   Title

                   NIL

              [L]  [A]  [A]    NIL
                   2.   Title
                   
              NIL

         [L]  [A]  [A]
              A.   Title

              [L]  [A]  [A]    NIL
                   1.   Title

                   [L]  [A]  [A]    NIL
                        i.   Title
                   
                   NIL
                   
              [L]  [A]  [A]    NIL
                   2.   Title
                   
              NIL
         NIL
         
    [L]  [A]  [A]
         II.  Title
         
         ...
         
    ...

* There are two types of data: lists and atoms
* Lists can contain a combination of lists and atoms
  * Every list ends with the atom NIL
* Atoms are either numbers or strings
  * NIL is an atom, albeit a special one, as it represents an empty list
  * There are subtypes of symbols but that is for another discussion

### Pointers

* Pointer can be either above or below an item in a list, depending on what it
is pointing to
* If above, an ASCII key will insert a key before the current item, and move the
pointer above the next item
* If below, an ASCII key will replace the item with input key, and move the pointer
below the next item
* Pointers themselves are symbols
* All pointers begin with an asterisk: `*0` `*1` etc.
* There can exists as many pointers as desired, however, there exists one root
master pointer: `*0`.

## Layouts

There exists 2 possible layouts, x or y:

### X Layout

Atom:

    [X] CDR
    CAR

List:

    [X] [X] CDR
    CAR CAR

    ...

### Y Layout

Atom:

    [Y] CAR
    CDR

List:

    [Y] CAR
    [Y] CAR
    CDR

    ...

### Defaults

The default layout for atoms is X since text is generally read horizontally.

The default layout for lists is Y:
* More compact layout
* Visual distinction of lists
* More intuitive to most users

In other words, every list is indented on a newline like in an outline.

A combined layout from the first example:

    [Y]  [X]  [X]
         I.   Title

         [Y]  [X]  [X]
              A.   Title
         NIL
    NIL

## Newline Patterns

A new list is placed on a new line for either layout:

### X Layout

    [X]  [X]  [X]  NIL
    A    B    
              [X]  NIL
              C

* Follows default layout

### Y Layout

    [X]  [X]
    A    B    

    [Y]  [X]  NIL
    NIL  C   

* If no pair after, implies following pair is on newline

These combinations are also possible, but to be generally avoided, due to the difficulty 
in quickly visually distinguishing a list; typically used for special forms.

### Y Layout / Same Line:

    [X]  [X]  [Y]  [X]  NIL
    A    B    NIL  C

### X Layout / New Line:

    [X]  [X]
    A    B    

    [X]  NIL

    [X]  NIL
    C

## Nested Lists

    (a b (c))

    [X]  [X]
    A    B    
    
    0
    [Y]  [X]  NIL
    NIL  C
  
Call make-nl:
  
    (a b ((c)))
    
    [X]  [X]
    A    B    

             0
    [Y]  [X] NIL
    NIL
    
         [Y]  [X]  NIL
         NIL  C

## Atoms

*Manipulating strings and numbers*

## List

*Manipulating lists*

# Undo System

# PicoLisp Internals (Advanced)

*For reference*

* Use adr to get encoded pointer - decode to get ptr or for num data
  * shift left: NIL = (adr NIL) = 273780<<4 = 4380480
  * left shift = (>> -4 8786312637524)
  * (cons NIL NIL) = 72 215 66 0 0 0 0 0 | 72 215 66 0 0 0 0 0

* Atoms
  * Symbols
    * Internal:
      * CAR: Name, PL (poss)
      * CDR: NIL or VAL
    * Transient:
      * CAR: Name encoded in num in pointer (> word = cons cells/list)
      * CDR: Ptr to Car
      * Anon: show ptr
        * CAR: 0 (num as pointer)
          * (= (cons 0 NIL) (box))  -> struct eq, returns NIL but actually same
          * (== (cons 0 NIL) (box)) -> ptr eq, NIL
        * CDR: NIL
    * External: block address relative to loaded database
    * NIL: just another symbol sort of
      * NIL = "" = ()
  * Number: show number cells, shortnum=pointer, large numbers=cons
    * largest short number is 1,152,921,504,606,846,976
    * greater than that will be broken into lists

* Pair: show dot and CAR/CDR
