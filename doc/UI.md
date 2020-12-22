User-Interface
==============

Ostensibly, it resembles an outliner, however, it integrates various computing concepts and UI designs from CLIs, shells, REPLs, notebooks, WMs/DEs, creative coding, mindmapping, wikis and note-taking programs into a single object-oriented (more operatively, symbolic) interface; *it is not a visual programming language*.

Advanced:

* Fuzzy string matching
  * Tag suggestions
* Code-specific support
* Revision control
  * Tree diffs
* Encryption - text and block-based
* OCR
* Ink-pen input; drawn annotations
* Collaboration - separate related project

Interfaces:

* Touch/Mobile - power constraints and form factor need to be adressed
* AR/VR - I do have a design for AR...

# Basic Concepts

                    +-----------+-----------+
                    |                       |
                  Atom                     List
                    |
                    |
              +-----+-----+
              |           |
         Atom/Number  Atom/Symbol
                          |
                          |
     +--------+-----------+-----------+
     |        |           |           |
    NIL       *       Transient    External

Note, asterisk is another type of atom/symbol called an internal symbol which will be for a later discussion.

Or how a layperson might view the concepts:
    
    +------------------------+--------------------------------------+
    | Data Type              | Layperson                            |
    +------------------------+--------------------------------------+
    | List                   | Directory/folder                     | 
    |                        | with files as the "atoms"            |
    +------------------------+--------------------------------------+
    | Atom: Number           | Integers like 1, 2, 3                |
    +------------------------+--------------------------------------+
    | Atom: External Symbol  | Hyperlink with a name and link       |
    +------------------------+--------------------------------------+
    | Atom: Transient Symbol | String or text                       |
    +------------------------+--------------------------------------+

* There are two types of data in the "world": lists and atoms
* Lists contain a mix of lists and/or atoms
  * Similar to a folder that can contain files and/or folders and so on.
* Every list ends with the atom `NIL`
  * It's as if every folder, had a file named `NIL` in it, which no matter how you sorted the folder view, the `NIL` file was always last.
* Atoms are one of three types: `NIL`, internal, and transient
  * For this discussion, the following nomencalture will be interchangably used: 
    * External = link
    * Transient = string
  * `NIL` represents nothingness or false or "No"
    * `NIL` = computer speak for "No"
    * As if you were asked a question by the computer, instead of saying "No", you would say `NIL`
      * "Yes" or to say something is true, is represented by the symbol `T` and the opposite of `NIL`
      * Technically speaking, *any* data (including lists) that is not `NIL` is considered true.
    
*show brief examples*
*show with color*
    
## Lists

The concept of a list is intuitive and typically learned early on; the standard 5 paragraph essay in school etc. Anytime one creates an outline of sorts, one is creating a list or in computer science terms, a data structure.

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

*use actual outline example*
    
`I`'s content consists of a `Title` and body (`A`, `B`, ...). In the user's mind, the association is indicated by visual cues - all indented content underneath each bullet, can be considered belonging to that bullet or concept. The same pattern repeats itself recursively and serially.

    I. Title
       A. Title
       B. Title
    II.
      ...
    ...

    I. Title
       Body
    ...

Because the user is already familiar with the counting system, the user implicitly knows that `I` proceeds to `II` and so on, without needing additional explicit depiction of that relationship. 

Now, take for example how a programmer could see the same outline as a data structure, called a singly-linked list, with a more verbose representation (L#=List, depth level; A=Atom):

    [L1]  [A]  [A]
          I.   Title

          [L2]  [A]  [A]
                A.   Title

                [L3]  [A]  [A]    NIL
                      1.   Title
 
                      [L4]  [A]  [A]    NIL
                            i.   Title

                      NIL

                [L3]  [A]  [A]    NIL
                      2.   Title
                   
                NIL

          [L2]  [A]  [A]
                A.   Title

                [L3]  [A]  [A]    NIL
                      1.   Title

                      [L4]  [A]  [A]    NIL
                            i.   Title

                      NIL

                [L3]  [A]  [A]    NIL
                      2.   Title
                    
                NIL
              
          NIL
         
    [L1]  [A]  [A]
          II.  Title
         
         ...
         
    ...

Breaking this down serially into lists:


    [L1]  [A]  [A]                            List 1 contains 4 items: A, A, L2, L2, NIL
          I.   Title

          [L2]  [A]  [A]                      List 2 contains 5 items: A, A, L3, L3, NIL
                A.   Title

                [L3]  [A]  [A]    NIL         List 3 contains 4 items: A, A, L4, NIL
                      1.   Title
 
                      [L4]  [A]  [A]    NIL   List 4 contains 3 items: A, A, NIL
                            i.   Title

                      NIL

                [L3]  [A]  [A]    NIL
                      2.   Title
                   
                NIL

          [L2]  [A]  [A]
                A.   Title

                [L3]  [A]  [A]    NIL
                      1.   Title

                      [L4]  [A]  [A]    NIL
                            i.   Title

                      NIL

                [L3]  [A]  [A]    NIL
                      2.   Title
                    
                NIL
              
          NIL

The next section will go further into the atoms like "A" and `NIL` etc.
          
## Symbols

                    +-----------+-----------+
                    |                       |
                  Atom                     List
                    |
                    |
              +-----+-----+
              |           |
         Atom/Number  Atom/Symbol
                          |
                          |
     +--------+-----------+-----------+
     |        |           |           |
    NIL       *       Transient    External

* Symbols are similar to hyperlinks in a web page in that they have a name (the visible string or text) and they also link to other data through a URL
  * In computer speak, we call the *webpage* the URL points to, the *value* of the symbol.
  * The URL would be another symbol.

* External symbols are equivalent to hyperlinks.
  
* Plain text or strings can be thought of as hyperlinks without any value since the string itself is its meaning, i.e. the word represents itself
  * In computer speak, the symbol points to itself.
  * It is simplest to think of strings/text as links without a link or value.
  * They can also be turned into external symbols or hyperlinks by being given a value.

* `NIL` is a special type; it is both a symbol (atom) and a list
  * It is equivalent to having a bunch of hyperlinks that link to the same page, except this page is the page seen when the server can't find the URL.
  * It has multiple representations which all have the same value:
    * `""` (empty string)
    * `()` (empty list)
    * `NIL`(symbol)
  * For now, we will focus only on `NIL`.
  * `NIL` as generally marks the end of a list.

* There are subtypes of symbols that will be discussed elsewhere
  * Widgets which will be discussed in a later section

The next section will talk about pointers, which can be considered a type of symbol.
  
#### Pointers

* A pointer is Particle's version of a traditional mouse cursor or pointer used to select objects.
* With the hyperlink analogy, a pointer is a link, or "points", to any sort of data; that data is the "selected" object.
  * The pointer can point to a list or a string or another symbol, or even another pointer.
* Pointers are represented as text, instead of a cursor to be able to refer to it more conveniently by name
  * For example, how would you refer to a mouse cursor in a conventional system?

* There are two basic types of pointer symbols: `↑*car` , `↓*cdr`
* A pointer can be either above or below an item in a list (indicated by the arrow in the name)
* The data pointed to is indicated by the arrow
* When a user switches pointers, they will automatically change color to yellow to indicate it is active.

*show image*


* The position changes the effect of the command on what it is pointing to
  * ↑ : ASCII key will insert a character before the current item, and move the pointer above the next item
    * Same as normal typing
  * ↓ : ASCII key will replace the character, and move the pointer below the next item
    * Same as normal typing with insert/overwrite mode on

* There can exists as many pointers as desired with the number appended in the name
  * If a user deletes all the pointers, they will be unable to perform commands.
* The user can see all pointers available in the `*ptr` list.
* The `*ptr` list can also be used to change pointers.
  
Basic Commands:
  
* To switch pointers
  * Mov pointer to pointer
  * Use switch-ptr command
  
Basic Selections:

* To select one atom or list
  * Mov pointer to atom or start of list (the dot before first item)
  * Pass pointer symbol to command

* To select part of list
  * Set `start` pointer and `end` pointer to desired data in a list
  * Put `start` and `end` pointer in a list
  * Pass pointer list to command

Advanced Selections:
  
Basic operations can be combined to perform more advanced selections:

* Select non-contiguous atoms in a list
* Select mixture of atoms and lists

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

Now armed with the basic knowledge, more commands can be used, which will be discussed next.

# Basic Operations

    +-------------------+---------------------------------------------+
    | Key Binding       | Command (CAR/CDR)                           |
    +-------------------+---------------------------------------------+
    | ASCII             | Make single-character strings: "a" "b" ...  |
    +-------------------+---------------------------------------------+
    | Enter             | Make newline char                           |
    +-------------------+---------------------------------------------+
    | Backspace         | Del before data                             |
    +-------------------+---------------------------------------------+
    | Delete            | Del after data                              |
    +-------------------+---------------------------------------------+
    | L-Alt + Space     | Convert atom: str <-> sym, num <-> str      |    
    +-------------------+---------------------------------------------+
    | R-Alt + Enter     | Make list / Make newline                    |
    +-------------------+---------------------------------------------+
    | R-Alt + Backspace | Del newline                                 |
    +-------------------+---------------------------------------------+

TODO:
* Mov cursor...
* Mov Start/End of List
* Mov Start/End of Line
* Make NIL

*Add GIF anims*

Command keys are based on spatial relationship, aka physically group related functionality together than mnemonics which are limited

# Illustrated Guide

## Cmd-Pointer Matrices (ATOM/PAIR)

  * factor in parent cell layout
  * remember, del pair behind ptr

  STR:
  
    * 2 opts for x/y-b: treat NIL as pair or atom
      * mak-str-x-a/b = repl atom
      * con-b-pair-x/mak-str-y-b = ins pair
      * -> treat as list so same as typing
      * -> also not common cdr non-NIL
      * usr can use alt-reg to repl atom
    
    +---+------------------+-----------------------+
    |   |       CAR        |          CDR          |
    |---+------------------+-----------------------+
    | S | (c) -> (d)       | (c) -> (d c)          |
    |---+------------------+-----------------------+
    |   |                  |     *               * |
    | X | [ ] .  ->  [ ] . | [ ] .  ->  [ ]  [ ] . |
    |   |  *          *    |  .          .    .    |
    |---+------------------+-----------------------|
    |   | [y] *  ->  [y] * | [y] .  ->  [y] .      |
    | Y |  .          .    |  *             *      |
    |   |                  |  .         [ ] .      |
    |   |                  |             .         |
    +---+------------------+-----------------------+

    +---+---------------------------------+-------------------------------+
    |   |               CAR               |              CDR              |
    +---+---------------------------------+-------------------------------+
    | S | ((c)) -> ((d c))                | (c) -> (d c)                  |
    +---+---------------------------------+-------------------------------+
    |   | # Y-Pair car = X-Pair (DO)      | # Y-Pair cdr = X-Pair         |
    |   |                                 |                               |
    |   |       *                    *    | [y] .  ->  [y]                |
    |   | [y]  [ ] .  ->  [y]  [ ]  [ ] . |  *          *                 |
    |   |       .               .    .    | [ ] .      [ ]  [ ] .         |
    |   |                                 |  .          .    .            |
    |   |  .               .              |                               |
    | Y +---------------------------------+-------------------------------+
    |   | # Y-Pair car = Y-Pair           | # Y-Pair cdr = Y-Pair         |
    |   |                                 |                               |
    |   |       *               *         | [y] .  ->  [y]                |
    |   | [y]  [y] .  ->  [y]  [ ]        |  *          *                 |
    |   |       .               .         | [y] .      [ ]                |
    |   |                                 |  .          .                 |
    |   |  .                   [y] .      |                               |
    |   |                                 |            [y] .              |
    |   |                       .         |             .                 |
    |   |                  .              |                               |
    +---+---------------------------------+-------------------------------+
    |   | # X-Pair car = X-Pair (TODO)    | # X-Pair cdr = X-Pair         |
    |   |                                 |                               |
    |   | [ ]   .  ->  [ ]                |       *                  *    |
    |   |  *            *                 | [ ]  [ ]  ->  [ ]  [ ]  [ ] . |
    |   | [ ] .        [ ]  [ ] .         |  .    .        .    .    .    |
    |   |  .            .    .            |                               |
    | X +---------------------------------+-------------------------------+
    |   | # X-Pair car = Y-Pair (TODO)    | # X-Pair cdr = Y-Pair         |
    |   |                                 |                               |
    |   | [ ]   .  ->  [ ]                | [ ]   ->  [ ]                 |
    |   |  *            *                 |  .         .                  |
    |   | [y] .        [ ]                |  *         *                  |
    |   |  .            .                 | [y] .     [ ]                 |
    |   |                                 |            .                  |
    |   |              [y] .              |  .                            |
    |   |               .                 |           [y] .               |
    |   |                                 |                               |
    |   |                                 |            .                  |
    +---+---------------------------------+-------------------------------+    
  
  LIST:
    
    +---+---------------------------------+---------------------+
    |   |              CAR                |         CDR         |
    |---+---------------------------------+---------------------+
    | S | (c d e) -> (c (d) e)            | (d) -> (d (NIL))    |
    |---+---------------------------------+---------------------+
    |   | [ ]  [ ]  [ ] .  ->  [ ]        |     *               |
    |   |  .    *    .          .         | [ ] .  ->  [ ]      |
    |   |                                 |  .          .       |
    |   |                            *    |                     |
    | X |                      [y]  [ ] . |             *       |
    |   |                            .    |            [y] .    |
    |   |                                 |             .       |
    |   |                      [ ]        |                     |
    |   |                       .         |                     |
    |---+---------------------------------+---------------------|
    |   | [ ]    ->  [ ]                  | [y] .  ->  [y] .    |
    |   |  .          .                   |  *          *       |
    |   |     *            *              |  .         [ ] .    |
    | Y | [y] .      [y]  [ ] .           |             .       |
    |   |                  .              |                     |
    |   |                                 |                     |
    |   | [ ]        [ ]                  |                     |
    |   |  .          .                   |                     |
    +---+---------------------------------+---------------------+

    +---+---------------------------------+-------------------------------+
    |   |               CAR               |              CDR              |
    +---+---------------------------------+-------------------------------+
    | S | (c (d) e) -> (c ((d)) e)        | (c d e) -> (c ((d e)))        |
    +---+---------------------------------+-------------------------------+
    |   | # Y-Pair car = X-Pair           | # Y-Pair cdr = X-Pair         |
    |   |                                 |                               |
    |   |       *               *         | [y] .  ->  [y] .              |
    |   | [y]  [ ] .  ->  [y]  [y]  [ ] . |  *          *                 |
    |   |       .               .    .    | [ ] .      [y]  [ ] .         |
    |   |                                 |  .               .            |
    |   |  .               .              |                               |
    |   |                                 |             .                 |
    |   |                                 |                               |
    |   | # poss repl with X-Pair inst.   | # poss repl with X-Pair inst. |
    | Y +---------------------------------+-------------------------------+
    |   | # Y-Pair car = Y-Pair           | # Y-Pair cdr = Y-Pair         |
    |   |                                 |                               |
    |   |       *               *         | [y] .  ->  [ ]                |
    |   | [y]  [y] .  ->  [y]  [y]  [y] . |  *          .                 |
    |   |       .                         | [y] .            *            |
    |   |                            .    |            [y]  [y] .         |
    |   |  .                              |  .               .            |
    |   |                       .         |                               |
    |   |                                 |             .                 |
    |   |                  .              | # same as X-P=Y-P             |
    +---+---------------------------------+-------------------------------+
    |   | # X-Pair car = X-Pair (TODO)    | # X-Pair cdr = X-Pair         |
    |   |                                 |                               |
    |   | [ ]   .  ->  [ ]                |       *                       |
    |   |  *            *                 | [ ]  [ ] .  ->  [ ]           |
    |   | [ ] .        [ ]                |  .    .          .            |
    |   |  .                              |                  *            |
    |   |              [ ] .              |                 [y]  [ ]  .   |
    |   |               .                 |                       .       |
    |   |                                 |                               |
    |   |                                 |                  .            |
    |   |                                 |                               |
    |   |                                 | # watch lines                 |
    | X +---------------------------------+-------------------------------+
    |   | # X-Pair car = Y-Pair (TODO)    | # X-Pair cdr = Y-Pair         |
    |   |                                 |                               |
    |   | [ ]   .  ->  [ ]                | [ ]   ->  [ ]                 |
    |   |  *            *                 |  .         .                  |
    |   | [y] .        [y]  [y] .         |  *              *             |
    |   |  .                 .            | [y] .     [y]  [y] .          |
    |   |               .                 |                 .             |
    |   |                                 |  .                            |
    |   |                                 |            .                  |
    +---+---------------------------------+-------------------------------+    
    
  BSP:
  
    * repl back with cur
    * cur atom car does nothing
    * check when cdr non-NIL
  
    +---+----------------------------+---------------------------------+
    |   |             CAR            |               CDR               |
    |---+----------------------------+---------------------------------+
    | S | (c d e) -> (c . d)         | (c d e . f) -> (d e . f)        |
    |---+----------------------------+---------------------------------+
    |   |                          * |               *               * |
    | X | [ ]  [ ]  [ ] .  ->  [ ] . | [ ]  [ ]  [ ] .  ->  [ ]  [ ] . |
    |   |  .    *    .          .    |  .    .    .          .    .    |
    |---+----------------------------+---------------------------------|
    |   | [ ]    ->  [ ]             | [ ]    ->  [ ]                  |
    |   |  .          .              |  .          .                   |
    |   |     *       *              |             *                   |
    |   | [y] .       .              | [ ]        [ ]                  |
    | Y |                            |  .          .                   |
    |   |                            |                                 |
    |   | [ ]                        | [y] .       *                   |
    |   |  .                         |                                 |
    |   |                            |  *                              |
    +---+----------------------------+---------------------------------+
    |   | # leave on newline                                           |
    +---+----------------------------+---------------------------------+

    +---+------------------------------+-------------------------------+
    |   |              CAR             |              CDR              |
    +---+------------------------------+-------------------------------+
    | S | (c (d) e) -> (c d)           | (c d e) -> (d e)              |
    +---+------------------------------+-------------------------------+
    |   | # Y-Pair car = X-Pair        | # Y-Pair cdr = X-Pair         | 
    |   |                              |                               |     
    |   | [ ]        ->  [ ]           |             *                 |    
    |   |  .              .            | [y] .  ->  [ ] .              |
    |   |       *         *            |  *          .                 |
    |   | [y]  [ ] .     [ ] .         | [ ]                           |
    |   |       .         .            |  .                            |
    |   |                              |                               |
    |   | [ ]                          |                               |
    |   |  .                           |                               |
    | Y +------------------------------+-------------------------------+
    |   | # Y-Pair car = Y-Pair        | # Y-Pair cdr = Y-Pair         |
    |   |                              |                               |
    |   | [ ]         ->  [ ]          |             *                 |
    |   |  .               .           | [y] .  ->  [y] .              |
    |   |       *          *           |  *          .                 |
    |   | [y]  [y] .      [y] .        | [y] .                         |
    |   |       .          .           |  .                            |
    |   |                              |                               |
    |   |  .                           | # assume Y always nl          |
    +---+------------------------------+-------------------------------+
    |   | # X-Pair car = X-Pair (TODO) | # X-Pair cdr = X-Pair         |      
    |   |                              |                               |
    |   | [ ]     .  ->  [ ]   .       |       *          *            |
    |   |                 *            | [ ]  [ ] .  ->  [ ] .         |
    |   | [ ]   .        [ ] .         |  .    .          .            |
    |   |  *              .            |                               |
    |   | [ ] .                        |                               |
    |   |  .                           |                               |
    |   |                              | # watch lines                 |
    | X +------------------------------+-------------------------------+
    |   | # X-Pair car = Y-Pair (TODO) | # X-Pair cdr = Y-Pair         |
    |   |                              |                               |
    |   | [ ]   .  ->  [ ]             | [ ]   ->  [y] .               |
    |   |  *            *              |  .         .                  |
    |   | [y] .        [y]  [y] .      |  *                            |
    |   |  .                 .         | [y] .                         |
    |   |               .              |                               |
    |   |                              |  .                            |
    |   |                              |                               |
    +---+------------------------------+-------------------------------+    
    
  DEL:
  
    * repl cur with NIL
    
    +---+------------------------------+-------------------------------+
    |   |              CAR             |              CDR              |
    +---+------------------------------+-------------------------------+
    | S | (c (d) e) -> (c d)           | (c d e) -> (d e)              |
    +---+------------------------------+-------------------------------+
    |   | # Y-Pair car = X-Pair        | # Y-Pair cdr = X-Pair         | 
    |   |                              |                               |     
    |   | [ ]        ->  [ ]           | [y] .  ->  [y] .              |    
    |   |  .              .            |  *          *                 |
    |   |       *              *       | [ ]                           |
    |   | [y]  [ ] .     [y]   .       |  .                            |
    |   |       .                      |                               |
    |   |                              |                               |
    |   |  .              .            |                               |
    | Y +------------------------------+-------------------------------+
    |   | # Y-Pair car = Y-Pair        | # Y-Pair cdr = Y-Pair         |
    |   |                              |                               |
    |   | [ ]         ->  [ ]          |                               |
    |   |  .               .           | [y] .  ->  [y] .              |
    |   |       *          *           |  *          *                 |
    |   | [y]  [y] .      [y] .        | [y] .                         |
    |   |       .          .           |  .                            |
    |   |                              |                               |
    |   |  .                           |                               |
    +---+------------------------------+-------------------------------+
    |   | # X-Pair car = X-Pair (TODO) | # X-Pair cdr = X-Pair         |      
    |   |                              |                               |
    |   | [ ]     .  ->  [ ]   .       |       *             *         |
    |   |                 *            | [ ]  [ ] .  ->  [ ] .         |
    |   | [ ]   .        [ ] .         |  .    .          .            |
    |   |  *              .            |                               |
    |   | [ ] .                        |                               |
    |   |  .                           |                               |
    |   |                              | # watch lines                 |
    | X +------------------------------+-------------------------------+
    |   | # X-Pair car = Y-Pair (TODO) | # X-Pair cdr = Y-Pair         |
    |   |                              |                               |
    |   | [ ]         ->  [ ]          | [ ]   ->  [ ]                 |
    |   |       *          *           |  .         .                  |
    |   | [y]  [y] .      [y] .        |  *         *                  |
    |   |       .          .           | [y] .      .                  |
    |   |  .                           |                               |
    |   |                              |  .                            |
    |   |                              |                               |
    |   |                              | # assume Y always nl          |
    +---+------------------------------+-------------------------------+    
        
    
  LINE:

    * Only applies to B
      * For A, poss do same action as B
    * No multi-spaced lines
      * Poss?
      
    +---+---------------------+------------------+
    |   |         CAR         |        CDR       |
    |---+---------------------+------------------+
    |   |                     |                  |
    | X | [ ] .  ->  [ ] .    | [ ] *  ->  [ ]   |
    |   |  *          *       |  .          .    |
    |   |                     |                  |
    |   |                     |             *    |
    |---+---------------------+------------------+
    |   | [ ]    ->  [ ]      | [y] .  ->  [y] . |
    |   |  .          .       |  *               | 
    |   |                     |             *    | 
    |   | [y] *      [y] *    |                  |
    | Y |  .          .       |                  |
    |   |                     |                  |
    +---+---------------------+------------------+
    
    * applies to X/Cdr/X
    
    +---+------------------------------+-----------------------+
    |   |              CAR             |          CDR          |
    +---+------------------------------+-----------------------+
    |   | # Y-Pair car = X-Pair        | # Y-Pair cdr = X-Pair | 
    |   |                              |                       |     
    |   | [ ]        ->  [ ]           | [y] .  ->  [y] .      |    
    |   |  .              .            |  *          *         |
    |   |       *              *       | [ ]        [ ]        |
    |   | [y]  [ ] .     [y]  [ ] .    |  .          .         |
    |   |       .              .       |                       |
    |   |                              |                       |
    |   | [ ]            [ ]           |                       |
    |   |  .              .            |                       |
    | Y +------------------------------+-----------------------+
    |   | # Y-Pair car = Y-Pair        | # Y-Pair cdr = Y-Pair |
    |   |                              |                       |
    |   | [ ]         ->  [ ]          | [y] .  ->  [y] .      |
    |   |  .               .           |  *          *         |
    |   |       *               *      | [y] .      [y] .      |
    |   | [y]  [y] .      [y]  [y] .   |  .          .         |
    |   |       .               .      |                       |
    |   |                              |                       |
    |   |  .               .           |                       |
    +---+------------------------------+-----------------------+
    |   | # X-Pair car = X-Pair (TODO) | # X-Pair cdr = X-Pair |      
    |   |                              |                       |
    |   | [ ]     .  ->  [ ]     .     |       *               |
    |   |                              | [ ]  [ ] .  ->  [ ]   |
    |   | [ ]   .        [ ]   .       |  .    .          .    |
    |   |  *              *            |                  *    |
    |   | [ ] .          [ ] .         |                 [ ] . |
    |   |  .              .            |                  .    |
    | X +------------------------------+-----------------------+
    |   | # X-Pair car = Y-Pair (TODO) | # X-Pair cdr = Y-Pair |
    |   |                              |                       |
    |   | [ ]         ->  [ ]          | [ ]    ->  [ ]        |
    |   |       *               *      |  .          .         |
    |   | [y]  [y] .      [y]  [y] .   |  *          *         |
    |   |       .               .      | [y] .      [y] .      |
    |   |  .               .           |  .          .         |
    +---+------------------------------+-----------------------+    
    

### X <-> Y Pair

* Simply swap Car/Cdr positions
* Note, causes recursive redraw of Car/Cdr cells

## Atoms

Strings:

* Chop
* Pack
* Glue with char
* Split by char
* Make line

Symbols:

* Make NIL

* Convert

## Lists

* Make sublist
* Make list starting with item

# Advanced

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
