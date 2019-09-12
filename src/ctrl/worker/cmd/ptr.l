#{
  Pointer
  
  - Placement is relative to ref
  - Selection is ref
    - So to do below, must select first in list
  - Mod + WASD
    - Left:ASCII + Right:R-Ctrl

  * New - ?
  
  * Place - repositions pointer semantically
    * Lists point one direction
    * Options:
      * Below prev/next in list: R-Ctrl + A/D
      * Below above/below in list: R-Ctrl + W/S
    * Default is R
    * Example:

      To do:
      
      (a
       b
       c)

       vs

       (a b c)

       First:

       -> a
          a
          
       -> b
          a
          b

       -> c
          a
          b
          c
      
  
  * Select - R-Alt+WASD
    * L/R - move list items
    * U/D - move lists

      (*1 2 (3 4) 5 6)

      -> R = 2
      -> R = 5
      -> R = 6
      -> ...

      -> R = 2
      -> D = 3
      -> R = 4
      -> R = no move

      -> R = 2
      -> D = 3
      -> R = 4
      -> U = 5
      -> R = 6
      -> R = no move
         * User can move up to timeline list and create new list

        (1 2 (3 4) 5 6 *)
                     

        (1 2 (3 4) 5 6) *
                        

        (1 2 (3 4) 5 6) (*)
                        

        * Press up anytime while on 3 or 4 to move to prev list in list or prev item (2) in parent list
        * Press dn anytime while on 3 or 4 to move to next list in list or next item (5) in parent list


    * Shift + Alt + L/R ? - move to start/end item in list
    * Shift + Alt + U/D ? - move to top-level list

    Slurp+Dir / Barf+Dir:
    * Ctrl + Shift + Alt + L/R - push prev/next item in list
    * Ctrl + Shift + Alt + U/D - append/con prev/next list

  * Hold both R-Alt + R-Ctrl to move together
    - Or by itself to move cursor to pointer

  EXAMPLE
  
    * To do let:

      (let (a b
            c d
            e f)
        T)

      - overall: let, nl, Alt+S, a, b, Ctrl+S x2, c, d, Ctrl+S x2, e, f, Alt+W, Ctrl+S x2, T, Alt+W
      
        = type let, new list, enter child list, type a, type b, place below prev x2,
          type c, type d, place below prev x2,
          type e, type f,
          enter parent list, place below prev x2, type T, enter parent list
          
        - When modifying?
          -> Example: from a; add x after b, y after d, z after f
             - goto mode place after: R-Ctrl+D
             - goto b: R-Alt+D
             - cons "x": x
             - goto d: R-Alt+D
             - cons "y": y
             - goto f: R-Alt+D
             - cons "z": z
             
        - remember, after new sexpr, it is selected...
        
        - note new str is set to new selection, but place is physically elsewhere
          - this indicates two different cursors!
          - @=cursor=place
          - *=pointer=selection (or color object)
          - similar to Blender's 3D cursor vs selection highlight, except...
          poss multiple cursors and pointers and can ref to same types to
          build compounds to do multi-select or multi-place so with the above
          so can select multiple and can place multiple

      - Or turn all strings into syms at the end
}#


############################
# Placement (Semantic Space)

# A/D = mode -> place prev/after
# - rather than always place after, and move selection
# W/S = mode -> place above/below

(de cmd-place-up (Keysym)
  (println "Control-R + W: Place Up")
  (setq *place-cur 'above))
  
(de cmd-place-dn (Keysym)
  (println "Control-R + S: Place Dn")
  (setq *place-cur 'under))


(de cmd-place-le (Keysym)
  (println "Control-R + A: Place Le")
  (setq *place-cur 'before))
  
(de cmd-place-ri (Keysym)
  (println "Control-R + D: Place Ri")
  (setq *place-cur 'after))


#########################
# Selection (User Space?)

(de cmd-select () T)

(de cmd-select-up (Keysym)
  (println "Alt-R + W: Select Up")
  
  # Search for parent list (store prev list in *list-pre-ptr)
  T)
  
(de cmd-select-dn (Keysym)
  (println "Alt-R + S: Select Dn")
  
  # Search for subsequent list in current list
  # Store current list in prev list, before moving
  
  T)


# NEXT
# (f d s a)
#  * ->

# PREV
# (f d s a)
#     <- *


(de cmd-select-le (Keysym)
  (println "Alt-R + A: Select Le")

  # Have to get location in current list to get previous...not efficient
  # or...make particle doubly-linked list
  # or...r-tree search

  # Traverse list until current particle found
  # While iterating save, prev

  #(pretty *timeline) (println *part-ptr) (prinl)

  (let (Prev NIL
        Part (car *timeline))

    (until (or (= Part *part-ptr)
               (= Part 'TIMELINE)
               (not Part))
      (setq Prev Part)
      (setq Part (get Part 'next)))

    (setq *part-ptr Prev)
    (adv-ptr 4.0)))

  
(de cmd-select-ri (Keysym)
  (println "Alt-R + D: Select Ri")

  # Move to next item
  # Check current particle next value

  (unless (= *part-ptr 'TIMELINE)
    (with *part-ptr
      (println "*part-ptr:next" (: next))
      (if (= (: next) 'TIMELINE)
        (prog
          (println "1" "NEXT=" 'TIMELINE "CUR=" (: data))
          #(setq *atom-pre-ptr *part-ptr)
          (setq *part-ptr 'TIMELINE))
        (prog
          (println "2" "NEXT=" (get (: next) 'data) "CUR=" (: data))
          #(setq *atom-pre-ptr *part-ptr)
          (setq *part-ptr (: next))
          (adv-ptr -4.0))))))


#################
# Euclidean Space
# or grid move

(de cmd-translate-ptr (Cnt Move)

  (with *vert-ptr
      (=: pos (list (+ (get (: pos) 1) (get Move 1))
                    (+ (get (: pos) 2) (get Move 2))
                    (get (: pos) 3))))

  (update-model-matrix> *vert-ptr)

  (cmd-update-vert *ipc *vert-ptr))


(de cmd-translate-ptr-px (Keysym) (cmd-translate-ptr 1 (list (*/ *adv-vert 2.0 1.0)   0.0)))
(de cmd-translate-ptr-nx (Keysym) (cmd-translate-ptr 1 (list (*/ *adv-vert -2.0 1.0)  0.0)))
(de cmd-translate-ptr-py (Keysym) (cmd-translate-ptr 2 (list 0.0 (*/ (+ *adv-vert *adv-vert) 2.0 1.0))))
(de cmd-translate-ptr-ny (Keysym) (cmd-translate-ptr 2 (list 0.0 (*/ (+ *adv-vert *adv-vert) -2.0 1.0))))