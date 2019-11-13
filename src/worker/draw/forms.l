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
  
  Function Calls:
  * prin* - vertically align arguments after string
  * fn - (+ 1 2 3) = (+ 1
                        2
                        3)
    * poss add option to split after specific length like 2-3 etc
  
  Other:
  * Strings - newline or line break
  
  Default to drawing horizontally for lists
  
  Specific formatting for specific structures allow for faster
  readability, in other words, fast identification of blocks of code
  or in this case, lists
}#

(de draw-de (List Depth) T)
(de draw-cond (List Depth) T)
(de draw-prog (List Depth) T)
(de draw-width (List Depth) T)


(de draw-let (List Depth X-Root)

  (draw-chars (char 183) *col-base-0 NIL)
  (adv-ptr)
  
  (draw-atom (car List))
  (adv-ptr)

  # If second atom is a list:
  #   draw pairs
  #   draw two atoms
  
  # Must determine width of local vars for prg
  
  (if (lst? (get List 2))
      (draw-let-f1 List Depth X-Root)
      (draw-let-f2 List Depth X-Root)))


(de draw-let-f1 (List Depth X-Root)
  
  (let ((X Y) (get *vert-ptr 'pos))
    # Loop two at a time:
    (let R (get List 2)
      (until (not R)
        (setq A (car R))
        (setq B (cadr R))
        (setq R (cddr R))
        (draw-any A (+ Depth 1.0)) (adv-ptr)
        (draw-any B (+ Depth 1.0))
        (when R
          (nl-ptr)
          (set-x-ptr-2 X))))

    #{
      # Draw horizontally
      
      (set-y-ptr-2 Y)  
      # Draw list will produce newline before drawing
      (nl-ptr -1.0)    
      (adv-ptr 6.0)
    }#

    (set-x-ptr-2 (+ X-Root (*/ Depth *adv-vert 1.0)))
    (adv-ptr 3.0)
    
    (draw-list (nth List 3)
               (+ Depth 1.0)
               (get (get *vert-ptr 'pos) 1))))


(de draw-let-f2 (List Depth X-Root)
  
  # Same as above...except drawing only 1 pair
  
  (draw-any (get List 2)) # a
  (adv-ptr)
  (draw-any (get List 3)) # (+ 1 2)
  (adv-ptr)
          
  # Newline, Return feed, Indent two spaces
  
  (set-x-ptr-2 (+ X-Root (*/ Depth *adv-vert 1.0)))
  (adv-ptr 3.0)
  
  # Remainder can be either list or atom
  # NTH/CDR will return a list although it is not the start of a list but
  # the continuation/remainder of one
  
  (draw-list (nth List 4)
             (+ Depth 1.0)
             (get (get *vert-ptr 'pos) 1)))
