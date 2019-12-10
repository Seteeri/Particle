User-Interface
==============

Spacing Implies Relationship:
* Particles = one space (XY)
* CAR/CDR = two spaces (X)
* Cons = four spaces (XY)

* For example, ptr bytes are connected but spacing implies their connection
* Edges are only used for pointers (CAR/CDR)

Two View Modes:
* Cell view
* List view

Data Types (Use ALT):

NUM   = ALT + NUM
SYMS:
  NIL   = I-SYM: "NIL"
          ALT + SPACE
  T-SYM = ASCII
          Create chars until snip command
          Snip command can produce a string or internal symbol
            Tr = (pack chrs)
            In = (intern (pack chrs))
          Then use no snip command to use individual chars which is less common pattern
  I-SYM = See T-Sym
          Or (intern "")
  A-SYM = ALT + ?
          Or type (new) or (box)
CONS:

Unused: ESC, TAB, CAPS, META, etc.

Use arrows to move around, if obj cut off, pan to it

################################################################################

- A vertex represents a cons cell
- A cons cell is made of two pointers, called car & cdr
- These two pointers can point to an atom or to another cons cell
  - Car/Cdr is either pointer or binary
- An atom is an encoded pointer, which is a number or a symbol/string(UTF-8)
- Numbers: shortnum fit in single cell (two pointers), bignum consists of multiple cells
- Symbols: CDR=value, CAR= Nil|Name|PList&Name
  - If only name and no plist, then CAR would point to cons cell containing name
  - If bignum, then contains multiple cons cells (list with encoded numbers)
  - If only plist and no name, then CAR would point to a plist with last cons CDR=Nil
    which is where name would normally be

* Use adr to get encoded pointer - decode to get ptr or for num data
  - shift left: NIL = (adr NIL) = 273780<<4 = 4380480
  - left shift = (>> -4 8786312637524)
  - (cons NIL NIL) = 72 215 66 0 0 0 0 0 | 72 215 66 0 0 0 0 0

* Given (setq A (cons 'B 'C))
* Symbol ptr, points to CDR so (sym 'A) = ptr to CDR = ptr of (cons 'B 'C)
* (adr A) = cons address
* (adr 'A) = ?
* quote def: any doQuote(any x) {return cdr(x);}
* Doing (adr (- adr-sym 8)) = infinite loop

         +-----+-----+
         | CAR | CDR |
         +-----+-----+
         TAIL     HEAD

Typing characters produces transient symbol characters with the CDR being shown
by default (the value).

"h" "e" "l" "l" "o"

              +--------+
              |        |
              V        |
      +-------+-----+  |
      |  'H'  | PTR |--+
      +---+---+-----+

Links represent pointers
- Each char is individual symbol so not linked
- If chars were in a list, there'd be lines

With chars, can move independently since they are different symbols and thus
different memory blocks. If they are to be moved together, they must be packed
and vice versa for the opposite.

User can choose to see CAR/CDR/CONS:
- Atoms
  - Symbols (CDR:VALUE)
    - Internal: show name
      - CAR: Name, PL (poss)
      - CDR: NIL or VAL
    - Transient: show name (VAL is symbol itself)
      - CAR: Name in encoded pointer (> word = cons cells/list)
      - CDR: Ptr to CAR/Symbol
      - Anon: show ptr
        - CAR: 0 (NUM as encoded pointer)
          - (= (cons 0 NIL) (box))  -> struct eq, returns NIL but actually same
          - (== (cons 0 NIL) (box)) -> ptr eq, NIL
        - CDR: NIL
    - External: Internal
    - NIL: just another symbol sort of...show NIL/""/()...only defined once
  - Number: show number cells, shortnum=pointer, large numbers=cons
    - largest short number is 1,152,921,504,606,846,976
    - greater than that will be broken into lists
    - have to use struct to get cells (do later)
    - how to differentiate numbers vs chars?
      - either use strings or colors
      - worry about this later
- Cons
  - Pair: show CAR/CDR...for lists it works also recursively


Abstract:

              +--------+
              |        |
              V        |
      +-------+-----+  |
      |  'H'  | PTR |--+
      +---+---+-----+

  * Single cons cell = contiguous memory of 16 bytes = 2 words
  * Pointers are separate sections of contiguous memory linked together
  * Poss laterally convert between integer-binary-hex (number formats)
  * Basically, type can be derived graphically/structurally
  * Could put CAR/CDR together and color to differentiate (or other method)

* Cons cell has two ptrs
  * = 16 bytes = 128-bits
* Ptr is interpreted: num, sym/cons, or cons
  * Num can be enc ptr or list of cons cells where CAR is enc ptr
* The interpretation is generally what the user modifies
  * Mod # -> Ptr changes, Mod Ptr -> # changes


################################################################################

                            +--------+
                            |        |
                            V        |
    +-----+-----+     +-----+-----+  |
    |  |  | VAL +---> | 'A' |  |  |--+
    +--+--+-----+     +--+--+-----+
       |                 
    "Data"
                            

(any "(list 0 abc \"def\" (box) NIL)")

  12345678  87654321    12345678  87654321  ->  12345678  87654321  ->  12345678  87654321  ->
  |                     |                       |                       |                     
  V                     V                       V                       V                     
  12345678  87654321    12345678  87654321      12345678  87654321      12345678  87654321    
  V                     V         V             V         V             V---------+           
  list                  0         NIL           abc       NIL           "a"                  

  I-SYM/FN              NUM                     I-SYM                   T-SYM                
  (traverse)           (bignum traverse)


  12345678  87654321  ->  12345678  87654321  ->  NIL
  |                       |
  V                       V
  12345678  87654321      NIL
  V         V
  0         NIL

  A-SYM                   NIL


(cons (cons) (cons))

  12345678  87654321  ->  12345678  87654321
  |                       |         |
  V                       V         V
  12345678  87654321      NIL       NIL    
  V         V
  NIL       NIL


(cons 1 2)

  12345678  87654321
  |         |       
  V         V       
  1         2
  

(cons (cons 1) (cons 2))

  12345678  87654321  ->  12345678  87654321
  |                       |         |
  V                       V         V
  12345678  87654321      2         NIL
  V         V
  1         NIL

################################################################################


              0         0
              |         |
              |         |
       12345789  12345678
       ||||||||  ||||||||
       |
  [PL][.]
    |
    + pos
    + rot
    + sca
    + mm
    + rgba
    + uv
    + off-texel
    |
    CAR/CDR (name)

  * Struct data exists for all the above
  * The problem is representing the vertex data as symbols leads to inf rec.
  * Solution
    -> There is none
    -> Can edit but cannot show

  * CAR-CDR = quad spaced
  * Vert dist = quad spaced
  * Note, Vertex is a symbol whose value is the actual symbol
  * Above pointers cannot be moved individually and will move together
    * 1/2 move with pointers since they are directly based on it
    * So cons cells can move independently of each other
      * Above: if 1/2 were cons cells instead, they could move independently
      of root cons
  * To modify vertex data:
    * Show first level of vertex data
    * Modifying vertex data will modify cons cell
    * To modify the vertex data, must create new cons cell/vertex data for said vertex data
  * Need quote operator!! or is it a mode
    * (quote particle) -> produce vertex data for particle
    * (quote (quote particle)) -> produce vertex data for vertex data

  Vertex Obj/Symbol:

              0                     'a'
              |                     |
              |                     |
       12345789  12345678    12345678<-87654321
                 |      |              |
       +---------+      +--------------+
       |                               |
  [PL][.]                         [PL][.]
    |                                  |
    +...                               +...


      'h'                   'i'
       |                     |
  [PL][.]                   ...
    |
    +--[][NIL]
       |
       [...][GL]
       |
       ...

  * Each of the cons cells above is a vertex
    -> Inf recursion
    -> Vertex is lowest level - AKA the interface
       -> Simple to understand
       -> If data's rep has multiple ASCII keys, any char can be operated on
    -> Use transformers to produce another Vertex of a different type
       * Ex: show pointers
    -> Possible to edit Vertex itself by editing the class

  * ASCII key = anonymous symbol or object symbol
    * Vertex symbol will store A as one of the following in its VAL:
      * Encode A as a  number (CTRL/ALT)
      * Encode A as ptr to transient symbol (default)
    * POSS: Use mod key to build string
      * Hold CTRL/ALT, type alphas, release -> 'hi' instead of 'h' 'i'
      * Hold CTRL/ALT, type nums, release -> 123 instead of 1 2 3
    * Types are encoded in pointers to the cons cell
      * So a cons cell can have any data in it however, how it is addressed
      determines its type
      * This is why (= (cons 0 NIL) (box)) -> NIL
      * Ptr to cons has 0 offset
      * Ptr to box has 8 offset

  * Opaque pointers, such as pointers to built-in functions - no descendants
  * Ptrs = no descendant
  * Nums/Syms = descendant

  Modifications:
  * Changing pointer digits will cause descendants to regenerate
  * Changing symbol values or numbers will cause predecessors to regenerate

  Composite View (show CAR):

  12345678  ->  12345678  ->  12345678  ->  12345678  ->  12345678  ->  12345678
  |             |             |             |             |             |
  V             V             V             V             V             V
  list          0             abc           "def"         0             NIL

  * Again, an opaque pointer will not have a descendant
  * Cons will show first part only


  Atomic View:

  list -> 0 -> abc -> "def" -> ${12345678} -> NIL [-> NIL]


Numbers vs Pointers:

  12345678  12345678   (cons cell with two opaque pointers)

  12345678  12345678   (cons cell with two numbers)
  |         |
  V         V
  1         2


Esc = 1
Backspace = 14
Tab = 15
Q = 16
A = 30
Z = 44
Space = 57
CAPS = 58
Win = 125
LShift = 42
RShift = 54
LCtrl = 29
RCtrl = 97
LAlt = 56
RAlt = 100
~/` = 41
# = # + 1 (1=2)
F1 = 59
F2 = 60

########
WORKFLOW

1. Manx
2. Strings/Symbols
3. Numbers/Strings
4. Lists/Cons

# MANEUVERING

L/R move between atoms
U/D move between nested lists

https://www.reddit.com/r/emacs/comments/3sfmkz/could_this_be_a_pareditsmartparens_killer/


# STRINGS/INTERN

1. Push strings
2. Pack/replace strings; save point

3. Push strings
2. Pack/replace strings; save point

Repeat...

Example:

-> H e l l o
-> Hello

-> Hello W o r l d
-> Hello World

OR

-> H e l l o   W o r l d
-> Hello World

Workflow is useful for mixing lists
So to do: (pack "abc" "def")

1. Push p,a,c,k
2. Pack/Replace->Intern

-> p a c k
-> "pack"
-> pack

-> a b c
-> "abc"

-> d e f
-> "def"

-> <eval>

# NUMBERS

Similar...

To go from (1 2 3) -> 123 

-> 1 2 3
-> Pack/Format/Replace

NUM <-> STR

straightforward

# LISTS

Making lists

1. Use CTRL+TAB/CAPS to create empty list
