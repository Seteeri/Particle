#{
  Layout depends on data structure...

  Symbol Functions:
  * de - first three items on same line
  * let - ...
  * with - ...
  * prog - ...

  Control Flow:
  * if/when/unless
  * loop/while/do
    * for - more complicated
  * cond
  * case/casq/state

  * Poss add option to split after specific length like 2-3 etc

  Other:
  * Strings - newline or line break

  Specific formatting for specific structures allow for faster
  readability, in other words, fast identification of blocks of code
  or in this case, lists
}#

(de draw-for (List Depth X-Root)  
  (if (sym? (get List 2))
      (draw-for-three List Depth X-Root)
      (draw-for-two List Depth X-Root)))


(de draw-for-three (List Depth X-Root)
  # Draw first three

  (draw-chars (char 183) *col-base-0 NIL)
  (adv-ptr 2.0)
  
  (let ((X Y) (get *vert-ptr 'pos))
  
    (draw-list-x (head 3 List) Depth X Y)

    # Move pointer under with, indent
    (set-x-ptr-2 X)
    (adv-ptr 2.0)
    (nl-ptr)
    
    (draw-list-col (nth List 4) Depth X)))
  
  
(de draw-for-two (List Depth X-Root)
  # Draw first two - same as draw-with
  (draw-with List Depth X-Root))


(de draw-with (List Depth X-Root)

  # Rename to draw-hor-one?

  (draw-chars (char 183) *col-base-0 NIL)
  (adv-ptr 2.0)
  
  (let ((X Y) (get *vert-ptr 'pos))
  
    (draw-list-x (head 2 List) Depth X Y)

    # Move pointer under with, indent
    (set-x-ptr-2 X)
    (adv-ptr 3.0)
    
    (draw-list-col (nth List 3) Depth X)))


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
    (nl-ptr)
    
    (draw-list-col (cdr List) Depth X)))


(de draw-cond-1 (List Depth X-Root)
  # Draw cond prg row
  
  (draw-chars (char 183) *col-base-0 NIL)
  (adv-ptr 2.0)

  (let ((X Y) (get *vert-ptr 'pos))

    (draw-atom (car List) Depth X)
    (adv-ptr))
  
  (let ((X Y) (get *vert-ptr 'pos))

    # Loop through remainder

    (let? Rem (cdr List)
      (until (not Rem)

        # Actually a list
      
        (draw-list-col2-dot (car Rem) Depth X)
           
        (setq Rem (cdr Rem))
        (when Rem
          (set-x-ptr-2 X)
          (nl-ptr))))))


(de draw-cond-2 (List Depth X-Root)
  # Draw cond prg col
  
  (draw-chars (char 183) *col-base-0 NIL)
  (adv-ptr 2.0)

  (let ((X Y) (get *vert-ptr 'pos))

    (draw-atom (car List) Depth X)
    (adv-ptr))
  
  (let ((X Y) (get *vert-ptr 'pos))

    # Loop through remainder

    (let? Rem (cdr List)
      (until (not Rem)

        # Draw first part, then second part under
        
        (draw-chars (char 183) *col-base-0 NIL)
        (adv-ptr 2.0)
        
        (let ((Any Prg) (car Rem))
          (draw-any Any Depth X)
          (set-x-ptr-2 X)
          (nl-ptr)
          (adv-ptr 2.0)
          (draw-any Prg Depth X))
        
        (setq Rem (cdr Rem))
        (when Rem
          (set-x-ptr-2 X)
          (nl-ptr))))))


(de draw-de (List Depth X-Root)
  # Draw first three items, then rest as a list
  # Can also b used to define variables: (de *var . 123)
  # In this case list of de, (*var . 123)

  (draw-chars (char 183) *col-base-0 NIL)
  (adv-ptr 2.0)
    
  (let ((X Y) (get *vert-ptr 'pos))
            
    (draw-list-x (head 2 List) Depth X Y)
    
    (adv-ptr)
    
    # TODO: Need to make sure not nil or single item
    # Can't use draw any since it will draw list using default layout
    (if (lst? (get List 3))
      (draw-list-col-dot (get List 3) Depth X-Root)
      (draw-any (get List 3) Depth X-Root))
               
    (draw-let-prog (nth List 4) Depth X-Root)))
    

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
    # REFACTOR THIS
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

  # Move ptr under let and indent
  (set-x-ptr-2 X-Root)
  (adv-ptr 2.0)
  (nl-ptr)  

  # Remainder can be either list or atom
  # NTH/CDR will return a list although it is not the start of a list but
  # the continuation/remainder of one

  (let ((X Y) (get *vert-ptr 'pos))
    (draw-list-col List
                   Depth
                   X)))
