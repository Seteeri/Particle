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
  Use T-Sym
  Or select two pieces of data and use shortcut
  ALT + ENTER
  Need list command also
  
ALT+TAB?
  
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

      +-----------------+
      |                 |
      V                 |
      +-------+-----+   |
      |  'H'  | PTR |---+
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

      (default transient symbol)

      +---------------------+
      |                     |
      V                     |
      +--------+--------+   |
      |  'H'   |   PTR  |---+
      +---+----+--------+

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

(list 1 2) = (cons 1 (cons 2 NIL))

.
|   |
PTR PTR
|    |
1    .
     |   |
     PTR PTR
     |   |
     2   NIL

* Move cons (.) will move PTR with it but not the value if value=cons

PTR . PTR
|       |
1       2

Draw the Heap:
* Create 3 particles for every byte (0-255)
  * One cell = 16*3 = 48 particles
* GL index = (* Heap index (+ 208 48))

* Want user to work on particles which is an arbitrary unit of information
* Particles can represent a cons cell down to a single bit
* Particle has a ptr and corresponding val
  * Val can be another ptr

( ((1 2 3 4 5 6 7 8) 1)
  ((1 2 3 4 5 6 7 8) 2) )

Cons
  CAR
    251-252-253-254-255-256-257-258 
    1                              
  CDR
    251-252-253-254-255-256-257-258
    2

Tecnically VAL need not exist if user modifies raw ptrs

Cons
  CAR
    2  0 0 0 0 0 0 0
  CDR
    18 0 0 0 0 0 0 0
    
Figure out how to represent bytes, then how to represent its val or interpretation

So 123 etc is each a particle with its own vertex data
Ofc 123 can also be hex or oct or bin

* Say above is (cons 0 1) -> ((2 0 0 0 0 0 0 0) (18 0 0 0 0 0 0 0))
* If modify a byte, number will change
* Create 3 slots for each number
* If user wants to modify entire ptr at once:
  * Create cons with numbers and use cpy function
  * Or use a pointer

. -> CAR -> 2000000 -> VAL
  -> CDR -> 1800000 -> VAL
  
Bytes connect to following byte; double space between bytes to delimit

Create vertex syms/cons for the target cons cell bytes?
- Toggle on quote: on vertex data to modify vertex data
  * This will change the particles to vertex data of vertex data
- Toggle off quote:
  * This will change particles back to og vertex data with updated view
  * Old vertex data will be GC'd

Cons: ( ((2__) (0__) (0__) (0__) (0__) (0__) (0__) (0__))
        ((18_) (0__) (0__) (0__) (0__) (0__) (0__) (0__)) )

With Value:

Cons: ( ( ((2__) (0__) (0__) (0__) (0__) (0__) (0__) (0__))
          VAL-CAR)
        ( ((2__) (0__) (0__) (0__) (0__) (0__) (0__) (0__))
          VAL-CAR)
          
LATTER makes more sense

>>>> (48+2)*208
9984 bytes = 9.75 kb per cons cell
So 1 GB VRAM can draw 107,546 particles


#################################################

EXAMPLE: (any "(list 0 abc \"def\" (box) NIL)") :

  Quantum View:

  Raw Binary


  Element View:

  12345678  87654321    12345678  87654321  ->  12345678  87654321  ->  12345678  87654321  ->  12345678  87654321  ->  12345678  87654321  ->  NIL
  |                     |                       |                       |                       |                       |
  V                     V                       V                       V                       V                       V
  12345678  87654321    12345678  87654321      12345678  87654321      12345678  87654321      12345678  87654321      NIL
  V                     V         V             V         V             V---------+             V         V
  list                  0         NIL           abc       NIL           "abc"                   0         NIL

  I-SYM/FN              NUM                     I-SYM                   T-SYM                   A-SYM                   NIL


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

  -> Flip model since we work on symbol names
  -> User chooses option for top: Atoms, CAR/CDR, Pointers, Binary


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


QUESTIONS:

  * How to show spaces?

Strings = main interface = symbols, so strings = symbols
Symbols are then composed of cons cells...

Strings -> Cons cells
        -> Numbers (bignum=cons cells)

Note, characters are stored in short num (1-7 bytes) or big num (8 bytes)

Convert numbers by typing name vs pressing digits?
-> Numbers faster when sequential but thats less frequent
  - In that case, easier to write program to produce it
-> Numbers faster when only typing numbers, but when mixed,
   typing name faster since fingers remain near home keys

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