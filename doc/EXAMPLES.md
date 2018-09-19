Examples
========

## Overview

### Lists
```
Delta list - YZ
{.}

XZ: Not independent list

Code list - XY
{d}-{e}-{f}-{u}-{n}
 |
 |                {t}
 |                 |              
{defun}-{fn}-{.}-{t}
          |
         {f}-{n}
```

### Operations
```
Op #1:
- Change name "fn" -> "ab"

YZ:         { }---{.}
             |
XZ:         {fn} (rotate this along X axis towards -Z so no longer visible)
             |
XY: {defun}-{ab}-{.}-{t}
```

```
Op #2:
- Change name "ab" -> "uv"

<<-- new changes added to front
YZ:         {.}---{.}---{.}
             |     |
XZ:         {ab}--{fn}
             |
XY: {defun}-{uv}-{.}-{t}
```

```
(top view)

/\
X              <-Z->
\/               
    {.}---{.}---{.}---{.}   (Fns)
{}   |     |     |     |
 |   |     |     |     |
{}--{ab}--{fn}   |     |  
 |               |     |    
{}               |     |    (Inputs)
 |               |     |
{}-------------{nil}   |
 |                     |
{foo}------------------'

        (Output)
```

Better way:
```
/\
X              <-Z->
\/               
    {ab}---{cd}--{t}--{foo}-{.} (Inputs)
{}   |      |     |     |
 |   |      |     |     |
{}--{#'r}--{#'r}  |     |  
 |                |     |    
{}                |     |           (Fn)
 |                |     |
{}--------------{#'r}   | 
 |                      |
{foo}-----------------{#'a}

        (Output)
```

What happens when code list changes?
-> Remove code code and collapse timeline? Or empty the timeline also 
   (serialize as needed, etc.)?
```
   {.}
{.}
```

## Timeline

``{.}`` indicates a branch

This is considered the master branch ``{.}``:
```
{.}-{a}-{b}-{c}-{d}
```

This would represent multiple branches, ``a``, ``b``, ``c``, ``d`` based on 
``master`` branch.
```
{.}-{.}-{.}-{.}-{.}
     |   |   |   |
    {a} {b} {c} {d}
```

Note, redo, would move to C:
```
{.}-{a}-{b}-{c}-{d}
             |
            {*}        
```

Commit A, B, C, D, revert to B:
```
(a b c d)

{.}-{a}-{b}-{c}-{d}
         |
        {*}
```

Commit E,F:
```
(a (b (c d)) e f)

{.}-{a}-{.}
         |
        {b}
         |
         +--{.}
         |   |
        {e} {c}
         |   |
    {*}-{f} {d}
```
* Remember C- is based directly on B, not A (only through B...).
* Take items afer undo atom and, then replace undo atom with a list containing 
  undo atom and the list, respectively.
  ```    
  (a b c d e f) => (a b) (c) (d e f)
  (c) (d e f) => (c (d e f))
  (a b) (c (d e f)) => (a b (c (d e f)))
  ```
          
Revert to B:
```
(a (b (c d) e f))

{.}-{a}-{.}
         |
    {*}-{b}
         |
         +--{.}
         |   |
        {e} {c}
         |   |
        {f} {d}
```

Commit G:
```
(a (b ((c d) e f) g))

{.}-{a}-{.}
         |
        {b}       
         |
         +--{.}
         |   |
         |   +--{.}
         |   |   |
         |  {e} {c}
         |   |   |
         |  {f} {d}
         |
        {g}
         |
        {*}
```
* Repeat

Revert to A and commit H:
```
(a (b ((c d) e f) g) h)

            {*}
             |
{.}-{a}-{.}-{h}
         |
        {b}       
         |
         +--{.}
         |   |
         |   +--{.}
         |   |   |
         |  {e} {c}
         |   |   |
         |  {f} {d}
         |
        {g}
```
* Repeat...
* If we kept undoing to A and adding, it would end up nesting b in lists.
  ```
  (a (b ((c d) e f) g) )
  (a ((b ((c d) e f) g)) h)
  (a (((b ((c d) e f) g)) h) i)
  (a ((((b ((c d) e f) g)) h) i) j)
  
  vs

  (a (b ((c d) e f) g) h i j)
  ```
* Can calculate the nested lists based on how many elements there are after it
  so only create list if it isn't in a list
* From the above, the following can be read: 
  * C->B->A
  * E->B->A
  * G->B->A
  * H->A
