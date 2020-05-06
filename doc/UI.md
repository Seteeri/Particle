User-Interface
==============

# Command Matrix

# Basic Concepts

    +--------------+--------------+
    | Programmer   | Layperson    |
    +--------------+--------------+
    | List         | Outline/Tree |
    +--------------+--------------+
    | Atom: Number | Number       |
    +--------------+--------------+
    | Atom: Symbol | Hyperlink    |
    +--------------+--------------+
    | Atom: String | Text         |
    +--------------+--------------+

* There are two types of data in the "world": lists and atoms
* Lists contain a mix of lists and/or atoms
* Atoms are one of three types: numbers, strings, symbols
  * Every list ends with the atom NIL which will be explained in the next section
    
## Lists

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


## Symbols

* Symbols are similar to hyperlinks in a web page in that they have a representation (the visible text) and they also can link to other data that is not explicitly visible (generally visible when hovering over the text).
* It is best to think of strings/text as links without a link or value.

* `NIL` is a special type; it is both an atom and a list
  * It has multiple representations:
    * `""` : empty string
    * `()` : empty list
    * `NIL` : symbol, represents False value (`T` represents True)

* There are subtypes of symbols but that is for another discussion
  * Widgets which will be discussed in a later section


#### Pointers

* A pointer is conceptually the same as a traditional mouse cursor or pointer used to select objects
* Pointers are represented as text, instead of a cursor to be able to refer to it more conveniently by name
  * How would you refer to a mouse cursor in a conventional system?
* A pointer can be either above or below an item in a list
  * above: ASCII key will insert a key before the current item, and move the pointer above the next item
    * Same as normal typing
  * below: ASCII key will replace the item, and move the pointer below the next item
    * Same as normal typing with insert/overwrite mode on
* By convention, all pointer names begin with an asterisk: `*0` `*1` etc.
* There can exists as many pointers as desired, however, there exists one root master pointer: `*0`.
  * It should not be deleted

Basic Operations:

* Select one atom or list
  * Point to atom
  * Point to start of list

* Select part of list
  * Set `start` pointer and `end` pointer to desired data
  * Put `start` and `end` pointer in a list
  * Point `*0` pointer to that list

  
Compound Operations:
  
Basic operations can be combined to perform more advanced selections:

* Select non-contiguous atoms in a list
* Select mixture of above and lists

...and others.

This can be achieved by using list operations:

Given:

         *1        *2        *3
    [ ]  [ ]  [ ]  [ ]  [ ]  [ ]
          a    b    c    d    e

Select sublist:
(setq *0 '((*1 *2)))

Select atoms:
(setq *0 '(*1 *2 *3))

Select both sublist *1-*2, and atom *3:
(setq *0 '((*1 *2) *3))


## Layouts

Objects are linked; the linked object is either drawn to the right of or underneath the original object.

Thus there exists 2 possible layouts, x or y:

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

         
# Basic Operations


## Cut Copy Paste (CUA Ops)


## Undo System


## Input/Output

### Files and Databases


# Advanced

# Namespaces

# OpenGL

# PicoLisp Internals

*For reference*

LISP in a Nutshell:

* Lisp is based on s-expressions which are used for code and data.
* S-expressions consists of atoms and pairs
* Atoms and pairs are both made of cells consisting of two tagged pointers
* Atoms are either symbols or numbers
  * Symbols include strings
* Pairs whose CDR points to another pair creats a list
  * Proper lists end with NIL
  
*Note to self: research relationship between binary trees, general trees,
directed acyclic graphs*
https://stackoverflow.com/questions/16860566/s-expression-for-directed-acyclic-graph

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
