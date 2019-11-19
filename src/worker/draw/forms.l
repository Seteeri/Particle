#{
  Layout depends on data structure...

  Symbol Functions:
  * de - first three items on same line
  * let - 1st/2nd item on same line; 2nd item is a list or atom...
  * prog/with - cdr on different lines

  Control Flow:
  * if/when/unless
  * loop/for/while/do
  * cond/case - like prog...
  * and/or/not - separate line for each

  * Poss add option to split after specific length like 2-3 etc

  Other:
  * Strings - newline or line break

  Default to drawing horizontally for lists

  Specific formatting for specific structures allow for faster
  readability, in other words, fast identification of blocks of code
  or in this case, lists
}#


(de draw-with (List Depth X-Root)

  # Rename to draw-hor-one

  (draw-chars (char 183) *col-base-0 NIL)
  (adv-ptr)
  
  (let ((X Y) (get *vert-ptr 'pos))
  
    (for Any (head 2 List)
      (adv-ptr)
      (set-y-ptr-2 Y)
      (draw-any Any Depth X))
    
    # Move pointer under with, indent
    (set-x-ptr-2 X)
    (adv-ptr 3.0)
    
    (draw-list-one (nth List 3) Depth X)))


(de draw-prog (List Depth X-Root)
  #{
    (prog
      A
      B)
  }#

  (draw-chars (char 183) *col-base-0 NIL)
  (adv-ptr)
  
  (let ((X Y) (get *vert-ptr 'pos))
  
    (adv-ptr)
    (set-y-ptr-2 Y)
    (draw-any (car List) Depth X)
    
    # Move pointer under with, indent
    (set-x-ptr-2 X)
    (adv-ptr 3.0)
    
    (draw-list-one (cdr List) Depth X)))
  

(de draw-if (List Depth X-Root)
  # Draw first two items subsequently
  # Draw third item under second item
  
  (draw-chars (char 183) *col-base-0 NIL)
  (adv-ptr 2.0)

  T)


(de draw-cond (List Depth X-Root)
  # Draw cond, draw prog
  #{
    (cond ((= A) 1)
          ((= B) 2)
          ((= C) 3))
  }#
  
  (draw-chars (char 183) *col-base-0 NIL)
  (adv-ptr 2.0)

  (let ((X Y) (get *vert-ptr 'pos))

    (draw-atom (car List) Depth X)
    (adv-ptr))
  
  (let ((X Y) (get *vert-ptr 'pos))

    # Loop through remainder

    (let? Rem (nth List 2)
      (until (not Rem)
        (let ((Any Prg) (car Rem))
          (draw-any Any Depth X)
          (adv-ptr)
          (draw-any Prg Depth X))
        (setq Rem (cdr Rem))
        (when Rem
          (set-x-ptr-2 X)
          (nl-ptr))))))


(de draw-de (List Depth X-Root)
  # Draw first three items, then rest as a list

  (draw-chars (char 183) *col-base-0 NIL)
  (adv-ptr)
    
  (let ((X Y) (get *vert-ptr 'pos))
  
    (for Any (head 3 List)
      (adv-ptr)
      (set-y-ptr-2 Y)
      (draw-any Any Depth X))
      
    (draw-let-prog (nth List 4) Depth X)))


(de draw-cons (List Depth X-Root)

  (draw-chars (char 183) *col-base-0 NIL)
  (adv-ptr)

  (let ((X Y) (get *vert-ptr 'pos))
    (for Any List
      (adv-ptr)
      (set-y-ptr-2 Y)
      (draw-any Any Depth X))))


(de draw-= (List Depth X-Root)

  (draw-chars (char 183) *col-base-0 NIL)
  (adv-ptr)

  (let ((X Y) (get *vert-ptr 'pos))
    (for Any List
      (adv-ptr)
      (set-y-ptr-2 Y)
      (draw-any Any Depth X))))


(de draw-let (List Depth X-Root)

  (draw-chars (char 183) *col-base-0 NIL)
  (adv-ptr 2.0)

  (let ((X Y) (get *vert-ptr 'pos))

    (draw-atom (car List) Depth X)
    (adv-ptr)

    # Must determine width of local vars for prg

    (if (lst? (get List 2))
        (draw-let-list List Depth X)
        (draw-let-var List Depth X))))


(de draw-let-list (List Depth X-Root)

  (draw-chars (char 183) *col-base-0 NIL)
  (adv-ptr 2.0)

  (let ((X Y) (get *vert-ptr 'pos))
    # Loop two at a time:
    (let R (get List 2)
      (until (not R)
        (setq A (car R))
        (setq B (cadr R))
        (setq R (cddr R))
        (let D (+ Depth 1.0)
          (draw-any A D X-Root)
          (adv-ptr)
          (draw-any B D X-Root))
        (when R
          (nl-ptr)
          (set-x-ptr-2 X))))

    # same Depth since still in let list
    # must use X-Root
    (draw-let-prog (nth List 3) Depth X-Root)))


(de draw-let-var (List Depth X-Root)

  # Instead of using x-root, get from ptr

  # TODO: Refactor this using let

  (draw-any (get List 2) Depth X-Root)

  (adv-ptr)

  (draw-any (get List 3) Depth X-Root)

  (draw-let-prog (nth List 4) Depth X-Root))


(de draw-let-prog (List Depth X-Root)

  # Instead of using x-root, get from ptr

  # Newline will be emitted when list is drawn

  # Move ptr under let
  (set-x-ptr-2 X-Root)

  # Indent ptr 4 spaces
  (adv-ptr 2.0)

  # Remainder can be either list or atom
  # NTH/CDR will return a list although it is not the start of a list but
  # the continuation/remainder of one

  (let ((X Y) (get *vert-ptr 'pos))
    (draw-list-one List
                   Depth
                   X)))
